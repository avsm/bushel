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

(** Fetch videos from a PeerTube instance channel with pagination support
    @param count Number of videos to fetch per page (default: 20)
    @param start Starting index for pagination (0-based) (default: 0)
    @param base_url Base URL of the PeerTube instance
    @param channel Channel name to fetch videos from *)
val fetch_channel_videos : ?count:int -> ?start:int -> string -> string -> video_response Lwt.t

(** Fetch all videos from a PeerTube instance channel using pagination
    @param page_size Number of videos to fetch per page (default: 20)
    @param max_pages Maximum number of pages to fetch (None for all pages)
    @param base_url Base URL of the PeerTube instance
    @param channel Channel name to fetch videos from *)
val fetch_all_channel_videos : ?page_size:int -> ?max_pages:int -> string -> string -> video list Lwt.t

(** Fetch detailed information for a single video by UUID
    @param base_url Base URL of the PeerTube instance
    @param uuid UUID of the video to fetch *)
val fetch_video_details : string -> string -> video Lwt.t

(** Convert a PeerTube video to Bushel.Video.t compatible structure
    Returns (description, published_date, title, url, uuid, slug) *)
val to_bushel_video : video -> string * Ptime.t * string * string * string * string

(** Get the thumbnail URL for a video
    @param base_url Base URL of the PeerTube instance
    @param video The video to get the thumbnail URL for *)
val thumbnail_url : string -> video -> string option

(** Download a thumbnail to a file
    @param base_url Base URL of the PeerTube instance
    @param video The video to download the thumbnail for
    @param output_path Path where to save the thumbnail *)
val download_thumbnail : string -> video -> string -> (unit, [> `Msg of string]) result Lwt.t