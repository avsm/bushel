(** Karakeep API client implementation *)

open Lwt.Infix

module J = Ezjsonm

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
}

(** Type for Karakeep API response containing bookmarks *)
type bookmark_response = {
  total: int;
  data: bookmark list;
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
    let items_json = J.find json path in
    J.get_list (fun tag -> J.find tag ["name"] |> J.get_string) items_json
  with _ -> []

(** Extract a boolean field from JSON, with default value *)
let get_bool_def json path default =
  try J.find json path |> J.get_bool
  with _ -> default

(** Parse a single bookmark from Karakeep JSON *)
let parse_bookmark json =
  Printf.eprintf "%s\n%!" (J.value_to_string json);
  let id = J.find json ["id"] |> J.get_string in
  
  (* Title can be null *)
  let title = 
    try Some (J.find json ["title"] |> J.get_string)
    with _ -> None
  in
  Printf.eprintf "%s -> %s\n%!" id (match title with None -> "???"  | Some v -> v);
  (* Get URL - first try direct URL field *)
  let url = 
    try J.find json ["url"] |> J.get_string
    with _ -> 
      (* Try content.url if direct url not found *)
      try J.find json ["content"; "url"] |> J.get_string
      with _ -> failwith "No URL found in bookmark"
  in
  
  let note = get_string_opt json ["note"] in
  
  (* Parse dates *)
  let created_at = 
    try J.find json ["createdAt"] |> J.get_string |> parse_date 
    with _ -> 
      try J.find json ["created_at"] |> J.get_string |> parse_date
      with _ -> failwith "No creation date found"
  in
  
  let updated_at = 
    try Some (J.find json ["updatedAt"] |> J.get_string |> parse_date)
    with _ -> 
      try Some (J.find json ["modifiedAt"] |> J.get_string |> parse_date)
      with _ -> None
  in
  
  let favourited = get_bool_def json ["favourited"] false in
  let archived = get_bool_def json ["archived"] false in
  let tags = get_string_list json ["tags"] in
  
  { id; title; url; note; created_at; updated_at; favourited; archived; tags }

(** Parse a Karakeep bookmark response *)
let parse_bookmark_response json =
  (* The response format is different based on endpoint, need to handle both structures *)
  try 
    (* Standard list format with total count *)
    let total = J.find json ["total"] |> J.get_int in
    let bookmarks_json = J.find json ["data"] in
    let data = J.get_list parse_bookmark bookmarks_json in
    { total; data }
  with _ -> 
    try
      (* Format with bookmarks array *)
      let bookmarks_json = J.find json ["bookmarks"] in
      let data = J.get_list parse_bookmark bookmarks_json in
      { total = List.length data; data }
    with _ ->
      try
        (* Alternate format without total (for endpoints like /tags/<id>/bookmarks) *)
        let bookmarks_json = json in
        let data = J.get_list parse_bookmark bookmarks_json in
        { total = List.length data; data }
      with e ->
        Fmt.epr "Error parsing Karakeep response: %s\n" (Printexc.to_string e);
        { total = 0; data = [] }

(** Fetch bookmarks from a Karakeep instance with pagination support *)
let fetch_bookmarks ~api_key ?(limit=50) ?(offset=0) ?(include_content=true) ?filter_tags base_url =
  let open Cohttp_lwt_unix in
  
  (* Base URL for bookmarks API *)
  let url = Printf.sprintf "%s/api/v1/bookmarks?limit=%d&offset=%d&includeContent=%b" 
              base_url limit offset include_content in
  
  (* Add tags filter if provided *)
  let url = match filter_tags with
    | Some tags -> 
        let tags_param = String.concat "," tags in
        url ^ "&tags=" ^ tags_param
    | None -> url
  in
  
  (* Set up headers with API key *)
  let headers = Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Authorization" ("Bearer " ^ api_key) in
  
  (* Make the request *)
  Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
  if resp.status = `OK then
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    let json = J.from_string body_str in
    Lwt.return (parse_bookmark_response json)
  else
    let status_code = Cohttp.Code.code_of_status resp.status in
    Lwt.fail_with (Fmt.str "HTTP error: %d" status_code)

(** Fetch all bookmarks from a Karakeep instance using pagination *)
let fetch_all_bookmarks ~api_key ?(page_size=50) ?max_pages ?filter_tags base_url =
  let rec fetch_pages offset acc _total_count =
    fetch_bookmarks ~api_key ~limit:page_size ~offset ?filter_tags base_url >>= fun response ->
    let all_bookmarks = acc @ response.data in
    
    (* Determine if we need to fetch more pages *)
    let fetched_count = offset + List.length response.data in
    let more_available = fetched_count < response.total in
    let under_max_pages = match max_pages with
      | None -> true
      | Some max -> (offset / page_size) + 1 < max
    in
    
    if more_available && under_max_pages then
      fetch_pages fetched_count all_bookmarks response.total
    else
      Lwt.return all_bookmarks
  in
  fetch_pages 0 [] 0

(** Fetch detailed information for a single bookmark by ID *)
let fetch_bookmark_details ~api_key base_url bookmark_id =
  let open Cohttp_lwt_unix in
  let url = Printf.sprintf "%s/api/v1/bookmarks/%s" base_url bookmark_id in
  
  (* Set up headers with API key *)
  let headers = Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Authorization" ("Bearer " ^ api_key) in
    
  Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
  if resp.status = `OK then
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    let json = J.from_string body_str in
    Lwt.return (parse_bookmark json)
  else
    let status_code = Cohttp.Code.code_of_status resp.status in
    Lwt.fail_with (Fmt.str "HTTP error: %d" status_code)

(** Convert a Karakeep bookmark to Bushel.Link.t compatible structure *)
let to_bushel_link bookmark =
  let description = 
    match bookmark.title with
     | Some title when title <> "" -> title
     | _ -> bookmark.url
  in
  let date = Ptime.to_date bookmark.created_at in
  let metadata = [("karakeep_id", bookmark.id)] in
  { Bushel.Link.url = bookmark.url; date; description; metadata }
