[@@@warning "-26-27-32"]
module J = Ezjsonm

open Lwt.Infix
open Cohttp_lwt_unix
open Cmdliner

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

let fetch url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  if resp.status = `OK then
    Cohttp_lwt.Body.to_string body
  else
    Lwt.fail_with (Fmt.str "Unexpected HTTP status: %d" (Cohttp.Code.code_of_status resp.status))

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
  match Ezjsonm.get_list Fun.id y with
  | l -> `A (v :: l)
  | exception _ -> `A [v]

(* Check if the video exists in vids by looking for the same url field, and if it doesnt add a new Yaml record to y *)
let add_vids vids existing_vid_urls y =
  List.fold_left (fun y v ->
    prerr_endline v.Bushel.Video.title;
    if List.mem v.Bushel.Video.url existing_vid_urls then y
    else append_video_yaml_to_yaml v y
  ) y vids

let process_videos output_dir overwrite =
  let base = "https://crank.recoil.org" in
  fetch (base ^ "/api/v1/video-channels/anil/videos?count=100") >>= fun j ->
  let j = Ezjsonm.from_string j in
  let total = Ezjsonm.find j ["total"] |> Ezjsonm.get_int in
  Logs.info (fun f -> f "Total videos: %d" total);
  let one_video j =
    let title = Ezjsonm.find j ["name"] |> Ezjsonm.get_string in
    let slug = Ezjsonm.find j ["id"] |> Ezjsonm.get_int |> string_of_int in
    let url = Ezjsonm.find j ["url"] |> Ezjsonm.get_string in
    let uuid = Ezjsonm.find j ["uuid"] |> Ezjsonm.get_string in
    let _embed = base ^ (Ezjsonm.find j ["embedPath"] |> Ezjsonm.get_string) in
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
    Logs.info (fun f -> f "Title: %s, URL: %s" title url);
    {Bushel.Video.description; published_date; title; url; uuid; slug; talk=false; paper=None; project=None; tags=[]}
  in
  let vids = Ezjsonm.find j ["data"] |> Ezjsonm.get_list one_video in
  let by = Yaml_unix.of_file_exn (Fpath.v (output_dir ^ "/videos.yml")) in
  let byt = 
    match by with
    | `A lst -> List.map (fun yaml -> Bushel.Video.t_of_yaml ~description:"" yaml) lst
    | _ -> []
  in
  let existing_vid_urls = List.fold_left (fun acc b -> b.Bushel.Video.url :: acc) [] byt in
  let by = add_vids vids existing_vid_urls by in
  if overwrite then
    Yaml_unix.to_file_exn (Fpath.v (output_dir ^ "/videos.yml")) by
  else
    Lwt.return ()

let output_dir =
  let doc = "Output directory" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT_DIR" ~doc)

let overwrite =
  let doc = "Overwrite existing files" in
  Arg.(value & flag & info ["overwrite"] ~doc)

let cmd =
  let doc = "Fetch and process videos" in
  let info = Cmd.info "bushel_video" ~doc in
  Cmd.v info (Term.(const (fun output_dir overwrite -> Lwt_main.run (process_videos output_dir overwrite)) $ output_dir $ overwrite))

let () =
  exit (Cmd.eval cmd)
