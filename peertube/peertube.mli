(** PeerTube API client interface
    TODO:claude *)

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

(** Parse a single video from PeerTube JSON *)
val parse_video : Ezjsonm.value -> video

(** Parse a PeerTube video response *)
val parse_video_response : Ezjsonm.value -> video_response

(** Fetch videos from a PeerTube instance channel *)
val fetch_channel_videos : ?count:int -> string -> string -> video_response Lwt.t

(** Convert a PeerTube video to Bushel.Video.t compatible structure 
    Returns (description, published_date, title, url, uuid, slug) *)
val to_bushel_video : video -> string * Ptime.t * string * string * string * string