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

(* Helper function to parse video from YAML *)
let parse_video_from_yaml yaml =
  let get_shadow_string fields k =
    match J.find_opt yaml ["_" ^ k] with
    | Some (`String v) -> v
    | _ -> 
        match J.find_opt yaml [k] with
        | Some (`String v) -> v
        | _ -> failwith ("invalid yaml field: " ^ k)
  in
  
  let slug = get_shadow_string yaml "uuid" in
  let title = get_shadow_string yaml "title" in
  let url = get_shadow_string yaml "url" in
  let uuid = get_shadow_string yaml "uuid" in
  let description = 
    match J.find_opt yaml ["_description"] with
    | Some (`String v) -> v
    | _ -> 
      match J.find_opt yaml ["description"] with
      | Some (`String v) -> v
      | _ -> ""
  in
  let published_date =
    let date_str = get_shadow_string yaml "published_date" in
    match Ptime.of_rfc3339 date_str with
    | Ok (date, _, _) -> date
    | Error _ -> 
        Fmt.epr "Warning: could not parse date '%s'\n" date_str;
        let span = Ptime.Span.of_d_ps (0, 0L) |> Option.get in
        Ptime.of_span span |> Option.get
  in
  
  let talk =
    match J.find_opt yaml ["talk"] with
    | Some (`Bool v) -> v
    | _ -> false
  in
  
  let tags =
    match J.find_opt yaml ["tags"] with
    | Some l -> J.get_list J.get_string l
    | _ -> []
  in
  
  let paper =
    match J.find_opt yaml ["paper"] with
    | Some (`String v) -> Some v
    | _ -> None
  in
  
  let project =
    match J.find_opt yaml ["project"] with
    | Some (`String v) -> Some v
    | _ -> None
  in
  
  {Bushel.Video.slug; title; tags; published_date; uuid; description; talk; paper; project; url}

let process_videos output_dir overwrite =
  let base_url = "https://crank.recoil.org" in
  let channel = "anil" in
  Peertube.fetch_channel_videos base_url channel >>= fun response ->
  Logs.info (fun f -> f "Total videos: %d" response.total);
  let vids = List.map (fun video ->
    let (description, published_date, title, url, uuid, slug) = 
      Peertube.to_bushel_video video 
    in
    Logs.info (fun f -> f "Title: %s, URL: %s" title url);
    {Bushel.Video.description; published_date; title; url; uuid; slug; talk=false; paper=None; project=None; tags=[]}
  ) response.data in
  let by = Yaml_unix.of_file_exn (Fpath.v (output_dir ^ "/videos.yml")) in
  let byt = 
    match by with
    | `A lst -> List.map parse_video_from_yaml lst
    | _ -> []
  in
  let existing_vid_urls = List.fold_left (fun acc b -> b.Bushel.Video.url :: acc) [] byt in
  let by = add_vids vids existing_vid_urls by in
  if overwrite then
    Lwt.return @@ Yaml_unix.to_file_exn (Fpath.v (output_dir ^ "/videos.yml")) by
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
