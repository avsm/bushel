(** PeerTube API client implementation
    TODO:claude *)

open Lwt.Infix

module J = Ezjsonm

(** Type representing a PeerTube video *)
type video = {
  id: int;
  uuid: string;
  name: string;
  description: string option;
  url: string;
  embed_path: string;
  published_at: Ptime.t;
  originally_published_at: Ptime.t option;
  thumbnail_path: string option;
  tags: string list;
}

(** Type for PeerTube API response containing videos *)
type video_response = {
  total: int;
  data: video list;
}

(** Parse a date string to Ptime.t, defaulting to epoch if invalid *)
let parse_date str =
  match Ptime.of_rfc3339 str with
  | Ok (date, _, _) -> date
  | Error _ -> 
      Fmt.epr "Warning: could not parse date '%s'\n" str;
      (* Default to epoch time *)
      let span_opt = Ptime.Span.of_d_ps (0, 0L) in
      match span_opt with
      | None -> failwith "Internal error: couldn't create epoch time span"
      | Some span ->
        match Ptime.of_span span with
        | Some t -> t
        | None -> failwith "Internal error: couldn't create epoch time"

(** Extract a string field from JSON, returns None if not present or not a string *)
let get_string_opt json path =
  try Some (J.find json path |> J.get_string)
  with _ -> None

(** Extract a string list field from JSON, returns empty list if not present *)
let get_string_list json path =
  try 
    let tags_json = J.find json path in
    J.get_list J.get_string tags_json
  with _ -> []

(** Parse a single video from PeerTube JSON *)
let parse_video json =
  let id = J.find json ["id"] |> J.get_int in
  let uuid = J.find json ["uuid"] |> J.get_string in
  let name = J.find json ["name"] |> J.get_string in
  let description = get_string_opt json ["description"] in
  let url = J.find json ["url"] |> J.get_string in
  let embed_path = J.find json ["embedPath"] |> J.get_string in
  
  (* Parse dates *)
  let published_at = 
    J.find json ["publishedAt"] |> J.get_string |> parse_date 
  in
  
  let originally_published_at =
    match get_string_opt json ["originallyPublishedAt"] with
    | Some date -> Some (parse_date date)
    | None -> None
  in
  
  let thumbnail_path = get_string_opt json ["thumbnailPath"] in
  let tags = get_string_list json ["tags"] in
  
  { id; uuid; name; description; url; embed_path; 
    published_at; originally_published_at; 
    thumbnail_path; tags }

(** Parse a PeerTube video response *)
let parse_video_response json =
  let total = J.find json ["total"] |> J.get_int in
  let videos_json = J.find json ["data"] in
  let data = J.get_list parse_video videos_json in
  { total; data }

(** Fetch videos from a PeerTube instance channel with pagination support
    @param count Number of videos to fetch per page
    @param start Starting index for pagination (0-based)
    @param base_url Base URL of the PeerTube instance
    @param channel Channel name to fetch videos from
    @return A Lwt promise with the video response
    TODO:claude *)
let fetch_channel_videos ?(count=20) ?(start=0) base_url channel =
  let open Cohttp_lwt_unix in
  let url = Printf.sprintf "%s/api/v1/video-channels/%s/videos?count=%d&start=%d" 
              base_url channel count start in
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  if resp.status = `OK then
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    let json = J.from_string body_str in
    Lwt.return (parse_video_response json)
  else
    let status_code = Cohttp.Code.code_of_status resp.status in
    Lwt.fail_with (Fmt.str "HTTP error: %d" status_code)

(** Fetch all videos from a PeerTube instance channel using pagination
    @param page_size Number of videos to fetch per page
    @param max_pages Maximum number of pages to fetch (None for all pages)
    @param base_url Base URL of the PeerTube instance
    @param channel Channel name to fetch videos from
    @return A Lwt promise with all videos combined
    TODO:claude *)
let fetch_all_channel_videos ?(page_size=20) ?max_pages base_url channel =
  let rec fetch_pages start acc _total_count =
    fetch_channel_videos ~count:page_size ~start base_url channel >>= fun response ->
    let all_videos = acc @ response.data in
    
    (* Determine if we need to fetch more pages *)
    let fetched_count = start + List.length response.data in
    let more_available = fetched_count < response.total in
    let under_max_pages = match max_pages with
      | None -> true
      | Some max -> (start / page_size) + 1 < max
    in
    
    if more_available && under_max_pages then
      fetch_pages fetched_count all_videos response.total
    else
      Lwt.return all_videos
  in
  fetch_pages 0 [] 0

(** Fetch detailed information for a single video by UUID
    @param base_url Base URL of the PeerTube instance
    @param uuid UUID of the video to fetch
    @return A Lwt promise with the complete video details
    TODO:claude *)
let fetch_video_details base_url uuid =
  let open Cohttp_lwt_unix in
  let url = Printf.sprintf "%s/api/v1/videos/%s" base_url uuid in
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  if resp.status = `OK then
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    let json = J.from_string body_str in
    (* Parse the single video details *)
    Lwt.return (parse_video json)
  else
    let status_code = Cohttp.Code.code_of_status resp.status in
    Lwt.fail_with (Fmt.str "HTTP error: %d" status_code)

(** Convert a PeerTube video to Bushel.Video.t compatible structure *)
let to_bushel_video video =
  let description = Option.value ~default:"" video.description in
  let published_date = video.originally_published_at |> Option.value ~default:video.published_at in
  (description, published_date, video.name, video.url, video.uuid, string_of_int video.id)

(** Get the thumbnail URL for a video *)
let thumbnail_url base_url video =
  match video.thumbnail_path with
  | Some path -> Some (base_url ^ path)
  | None -> None

(** Download a thumbnail to a file
    @param base_url Base URL of the PeerTube instance
    @param video The video to download the thumbnail for
    @param output_path Path where to save the thumbnail
    @return A Lwt promise with unit on success *)
let download_thumbnail base_url video output_path =
  match thumbnail_url base_url video with
  | None ->
      Lwt.return (Error (`Msg (Printf.sprintf "No thumbnail available for video %s" video.uuid)))
  | Some url ->
      let open Cohttp_lwt_unix in
      Client.get (Uri.of_string url) >>= fun (resp, body) ->
      if resp.status = `OK then
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        Lwt.catch
          (fun () ->
            let oc = open_out_bin output_path in
            output_string oc body_str;
            close_out oc;
            Lwt.return (Ok ()))
          (fun exn ->
            Lwt.return (Error (`Msg (Printf.sprintf "Failed to write thumbnail: %s"
              (Printexc.to_string exn)))))
      else
        let status_code = Cohttp.Code.code_of_status resp.status in
        Lwt.return (Error (`Msg (Printf.sprintf "HTTP error downloading thumbnail: %d" status_code)))