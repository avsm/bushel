(** Karakeep API client interface *)

(** Type representing a Karakeep bookmark *)
type bookmark = {
  id: string;
  title: string option;
  url: string;
  note: string option;
  created_at: Ptime.t;
  updated_at: Ptime.t option;
  favourited: bool;
  archived: bool;
  tags: string list;
  tagging_status: string option;
  summary: string option;
  content: (string * string) list;
  assets: (string * string) list;
}

(** Type for Karakeep API response containing bookmarks *)
type bookmark_response = {
  total: int;
  data: bookmark list;
}

(** Parse a single bookmark from Karakeep JSON *)
val parse_bookmark : Ezjsonm.value -> bookmark

(** Parse a Karakeep bookmark response *)
val parse_bookmark_response : Ezjsonm.value -> bookmark_response

(** Fetch bookmarks from a Karakeep instance with pagination support
    @param api_key API key for authentication
    @param limit Number of bookmarks to fetch per page (default: 50)
    @param offset Starting index for pagination (0-based) (default: 0)
    @param include_content Whether to include full content (default: true)
    @param filter_tags Optional list of tags to filter by
    @param base_url Base URL of the Karakeep instance
    @return A Lwt promise with the bookmark response *)
val fetch_bookmarks : 
  api_key:string -> 
  ?limit:int -> 
  ?offset:int -> 
  ?include_content:bool ->
  ?filter_tags:string list ->
  string -> 
  bookmark_response Lwt.t

(** Fetch all bookmarks from a Karakeep instance using pagination
    @param api_key API key for authentication
    @param page_size Number of bookmarks to fetch per page (default: 50)
    @param max_pages Maximum number of pages to fetch (None for all pages)
    @param filter_tags Optional list of tags to filter by
    @param base_url Base URL of the Karakeep instance
    @return A Lwt promise with all bookmarks combined *)
val fetch_all_bookmarks : 
  api_key:string -> 
  ?page_size:int -> 
  ?max_pages:int -> 
  ?filter_tags:string list ->
  string -> 
  bookmark list Lwt.t

(** Fetch detailed information for a single bookmark by ID
    @param api_key API key for authentication
    @param base_url Base URL of the Karakeep instance
    @param bookmark_id ID of the bookmark to fetch
    @return A Lwt promise with the complete bookmark details *)
val fetch_bookmark_details : 
  api_key:string ->
  string -> 
  string -> 
  bookmark Lwt.t

(** Convert a Karakeep bookmark to Bushel.Link.t compatible structure *)
val to_bushel_link : bookmark -> Bushel.Link.t

(** Fetch an asset from the Karakeep server as a binary string
    @param api_key API key for authentication
    @param base_url Base URL of the Karakeep instance
    @param asset_id ID of the asset to fetch
    @return A Lwt promise with the binary asset data *)
val fetch_asset :
  api_key:string ->
  string ->
  string ->
  string Lwt.t

(** Get the asset URL for a given asset ID
    @param base_url Base URL of the Karakeep instance
    @param asset_id ID of the asset
    @return The full URL to the asset *)
val get_asset_url :
  string ->
  string ->
  string