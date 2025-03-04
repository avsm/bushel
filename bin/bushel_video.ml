[@@@warning "-26-27-32"]
module J = Ezjsonm

(* JSON helper functions *)
let map_dict fn y =
  match J.get_dict y with
  | kv -> `O (List.map (fun (k,v) -> k, (fn k v)) kv)
  | exception _e -> y

let iter_dict fn y =
  match J.get_dict y with
  | kv -> List.iter (fun (k,v) -> fn k v) kv
  | exception _e -> ()

(* Shadow keys for Yaml *)
let get_shadow y k =
  let sk = "_"^k in
  match J.find_opt y [k] with
  | Some v -> v
  | None -> J.find y [sk]

let update_shadow y k v =
  let sk = "_"^k in
  J.update y [sk] (Some v)

let rec merge_shadows (y:J.value) =
  match y with
  | `A x -> `A (List.map merge_shadows x)
  | `O x -> `O (List.map (fun (k,v) ->
      match k with
      | k when String.starts_with ~prefix:"_" k ->
          String.sub k 1 (String.length k - 1), merge_shadows v
      | k-> k, (merge_shadows v)) x)
  | `Null | `Bool _ | `Float _ | `String _ -> y

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs_threaded.enable ();
  Logs.Src.set_level Cohttp_eio.src (Some Debug)

open Cohttp_eio

let null_auth ?ip:_ ~host:_ _ =
  Ok None (* Warning: use a real authenticator in your code! *)

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () |> Result.get_ok in
  fun uri raw ->
    let host =
      Uri.host uri |>
      Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let fetch ~sw client url =
  let resp, body = Client.get ~sw client (Uri.of_string url) in
  if Http.Status.compare resp.status `OK = 0 then
    Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
  else failwith (Fmt.str "Unexpected HTTP status: %a" Http.Status.pp resp.status)

module SM = Map.Make(String)

let vid_to_shadow_yaml v =
  let open Bushel.Video in
  `O [
    "_description", `String v.description;
    "_published_date", `String (v.published_date |> Ptime.to_rfc3339);
    "_title", `String v.title;
    "_url", `String v.url;
    "_uuid", `String v.uuid;
    "_slug", `String v.slug;
  ]

let append_video_yaml_to_yaml v y =
  let v = vid_to_shadow_yaml v in
  match J.get_list Fun.id y with
  | l -> `A (v :: l)
  | exception _ -> `A [v]

(* Check if the video exists in vids by looking for the same url field, and if it doesnt add a new Yaml record to y *)
let add_vids vids existing_vid_urls y =
  List.fold_left (fun y v ->
    prerr_endline v.Bushel.Video.title;
    if List.mem v.Bushel.Video.url existing_vid_urls then y
    else append_video_yaml_to_yaml v y
  ) y vids

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client = Client.make ~https:(Some (https ~authenticator:null_auth)) net in
  Eio.Switch.run @@ fun sw ->
  let base = "https://crank.recoil.org" in
  let j = fetch ~sw client (base ^ "/api/v1/video-channels/anil/videos?count=100") in
  let j = Ezjsonm.from_string j in
  let total = Ezjsonm.find j ["total"] |> Ezjsonm.get_int in
  Eio.traceln "Total videos: %d" total;
  let one_video j =
    let title = Ezjsonm.find j ["name"] |> Ezjsonm.get_string in
    let slug = Ezjsonm.find j ["id"] |> Ezjsonm.get_int |> string_of_int in
    let url = Ezjsonm.find j ["url"] |> Ezjsonm.get_string in
    let uuid = Ezjsonm.find j ["uuid"] |> Ezjsonm.get_string in
    let embed = base ^ (Ezjsonm.find j ["embedPath"] |> Ezjsonm.get_string) in
    let description =
      try
         Ezjsonm.find j ["description"] |> Ezjsonm.get_string
      with _ -> "" in
    let published_date =
      try
         Ezjsonm.find j ["originallyPublishedAt"] |> Ezjsonm.get_string
      with _ ->
         Ezjsonm.find j ["publishedAt"] |> Ezjsonm.get_string
    in
    let published_date = Ptime.of_rfc3339 published_date |> Result.get_ok |> fun (c,_,_) -> c in
    Eio.traceln "Title: %s, URL: %s" title url;
    {Bushel.Video.description;published_date;title;url;embed;uuid;slug;talk=false;paper=None;project=None}
  in
  let vids = Ezjsonm.find j ["data"] |> Ezjsonm.get_list one_video in
  let by = Yaml_unix.of_file_exn (Fpath.v "bushel/videos.yml") in
  let byt = Bushel.Video.ts_of_yaml by in
  let existing_vid_urls = List.fold_left (fun acc b -> b.Bushel.Video.url :: acc) [] byt in
  let by = add_vids vids existing_vid_urls by in
  Yaml_unix.to_file_exn (Fpath.v "bushel/videos.yml") by;
  ()
