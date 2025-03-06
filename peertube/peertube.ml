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

(** Fetch videos from a PeerTube instance channel *)
let fetch_channel_videos ?(count=100) base_url channel =
  let open Cohttp_lwt_unix in
  let url = Printf.sprintf "%s/api/v1/video-channels/%s/videos?count=%d" 
              base_url channel count in
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  if resp.status = `OK then
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    let json = J.from_string body_str in
    Lwt.return (parse_video_response json)
  else
    let status_code = Cohttp.Code.code_of_status resp.status in
    Lwt.fail_with (Fmt.str "HTTP error: %d" status_code)

(** Convert a PeerTube video to Bushel.Video.t compatible structure *)
let to_bushel_video video =
  let description = Option.value ~default:"" video.description in
  let published_date = video.originally_published_at |> Option.value ~default:video.published_at in
  (description, published_date, video.name, video.url, video.uuid, string_of_int video.id)