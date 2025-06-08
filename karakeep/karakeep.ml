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
  tagging_status: string option;
  summary: string option;
  content: (string * string) list;
  assets: (string * string) list;
}

(** Type for Karakeep API response containing bookmarks *)
type bookmark_response = {
  total: int;
  data: bookmark list;
  next_cursor: string option;
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
  (* Remove debug prints for production *)
  (* Printf.eprintf "%s\n%!" (J.value_to_string json); *)
  
  let id = 
    try J.find json ["id"] |> J.get_string 
    with e -> 
      Printf.eprintf "Error parsing bookmark ID: %s\n" (Printexc.to_string e);
      Printf.eprintf "JSON: %s\n" (J.value_to_string json);
      failwith "Unable to parse bookmark ID"
  in
  
  (* Title can be null *)
  let title = 
    try Some (J.find json ["title"] |> J.get_string)
    with _ -> None
  in
  (* Remove debug prints for production *)
  (* Printf.eprintf "%s -> %s\n%!" id (match title with None -> "???"  | Some v -> v); *)
  (* Get URL - try all possible locations *)
  let url = 
    try J.find json ["url"] |> J.get_string  (* Direct url field *)
    with _ -> try
      J.find json ["content"; "url"] |> J.get_string  (* Inside content.url *)
    with _ -> try
      J.find json ["content"; "sourceUrl"] |> J.get_string  (* Inside content.sourceUrl *)
    with _ -> 
      (* For assets/PDF type links *)
      match J.find_opt json ["content"; "type"] with
      | Some (`String "asset") ->
          (* Extract URL from sourceUrl in content *)
          (try J.find json ["content"; "sourceUrl"] |> J.get_string
           with _ -> 
             (match J.find_opt json ["id"] with
              | Some (`String id) -> "karakeep-asset://" ^ id
              | _ -> failwith "No URL or asset ID found in bookmark"))
      | _ ->
          (* Debug output to understand what we're getting *)
          Printf.eprintf "Bookmark JSON structure: %s\n" (J.value_to_string json);
          failwith "No URL found in bookmark"
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
  
  (* Extract additional metadata *)
  let tagging_status = get_string_opt json ["taggingStatus"] in
  let summary = get_string_opt json ["summary"] in
  
  (* Extract content details *)
  let content =
    try
      let content_json = J.find json ["content"] in
      let rec extract_fields acc = function
        | [] -> acc
        | (k, v) :: rest ->
            let value = match v with
              | `String s -> s
              | `Bool b -> string_of_bool b
              | `Float f -> string_of_float f
              | `Null -> "null"
              | _ -> "complex_value" (* For objects and arrays *)
            in
            extract_fields ((k, value) :: acc) rest
      in
      match content_json with
      | `O fields -> extract_fields [] fields
      | _ -> []
    with _ -> []
  in
  
  (* Extract assets *)
  let assets =
    try
      let assets_json = J.find json ["assets"] in
      J.get_list (fun asset_json ->
        let id = J.find asset_json ["id"] |> J.get_string in
        let asset_type = 
          try J.find asset_json ["assetType"] |> J.get_string
          with _ -> "unknown"
        in
        (id, asset_type)
      ) assets_json
    with _ -> []
  in
  
  { id; title; url; note; created_at; updated_at; favourited; archived; tags; 
    tagging_status; summary; content; assets }

(** Parse a Karakeep bookmark response *)
let parse_bookmark_response json =
  (* The response format is different based on endpoint, need to handle both structures *)
  (* Print the whole JSON structure for debugging *)
  Printf.eprintf "Full response JSON: %s\n" (J.value_to_string json);
  
  try 
    (* Standard list format with total count *)
    let total = J.find json ["total"] |> J.get_int in
    let bookmarks_json = J.find json ["data"] in
    Printf.eprintf "Found bookmarks in data array\n";
    let data = J.get_list parse_bookmark bookmarks_json in
    
    (* Try to extract nextCursor if available *)
    let next_cursor = 
      try Some (J.find json ["nextCursor"] |> J.get_string)
      with _ -> None
    in
    
    { total; data; next_cursor }
  with e1 -> 
    Printf.eprintf "First format parse error: %s\n" (Printexc.to_string e1);
    try
      (* Format with bookmarks array *)
      let bookmarks_json = J.find json ["bookmarks"] in
      Printf.eprintf "Found bookmarks in bookmarks array\n";
      let data = 
        try J.get_list parse_bookmark bookmarks_json 
        with e ->
          Printf.eprintf "Error parsing bookmarks array: %s\n" (Printexc.to_string e);
          Printf.eprintf "First bookmark sample: %s\n" 
            (try J.value_to_string (List.hd (J.get_list (fun x -> x) bookmarks_json))
             with _ -> "Could not extract sample");
          []
      in
      
      (* Try to extract nextCursor if available *)
      let next_cursor = 
        try Some (J.find json ["nextCursor"] |> J.get_string)
        with _ -> None
      in
      
      { total = List.length data; data; next_cursor }
    with e2 ->
      Printf.eprintf "Second format parse error: %s\n" (Printexc.to_string e2);
      try
        (* Check if it's an error response *)
        let error = J.find json ["error"] |> J.get_string in
        let message = 
          try J.find json ["message"] |> J.get_string
          with _ -> "Unknown error"
        in
        Printf.eprintf "API Error: %s - %s\n" error message;
        { total = 0; data = []; next_cursor = None }
      with _ ->
        try
          (* Alternate format without total (for endpoints like /tags/<id>/bookmarks) *)
          Printf.eprintf "Trying alternate array format\n";
          
          (* Debug the structure to identify the format *)
          Printf.eprintf "JSON structure keys: %s\n" 
            (match json with
             | `O fields -> 
                 String.concat ", " (List.map (fun (k, _) -> k) fields)
             | _ -> "not an object");
          
          (* Check if it has a nextCursor but bookmarks are nested differently *)
          if J.find_opt json ["nextCursor"] <> None then begin
            Printf.eprintf "Found nextCursor, checking alternate structures\n";
            
            (* Try different bookmark container paths *)
            let bookmarks_json =
              try Some (J.find json ["data"])
              with _ -> None
            in
            
            match bookmarks_json with
            | Some json_array ->
                Printf.eprintf "Found bookmarks in data field\n";
                begin try
                  let data = J.get_list parse_bookmark json_array in
                  let next_cursor = 
                    try Some (J.find json ["nextCursor"] |> J.get_string)
                    with _ -> None
                  in
                  { total = List.length data; data; next_cursor }
                with e ->
                  Printf.eprintf "Error parsing bookmarks from data: %s\n" (Printexc.to_string e);
                  { total = 0; data = []; next_cursor = None }
                end
            | None ->
                Printf.eprintf "No bookmarks found in alternate structure\n";
                { total = 0; data = []; next_cursor = None }
          end
          else begin
            (* Check if it's an array at root level *)
            match json with
            | `A _ ->
                let data = 
                  try J.get_list parse_bookmark json
                  with e ->
                    Printf.eprintf "Error parsing root array: %s\n" (Printexc.to_string e);
                    []
                in
                { total = List.length data; data; next_cursor = None }
            | _ -> 
                Printf.eprintf "Not an array at root level\n";
                { total = 0; data = []; next_cursor = None }
          end
        with e3 ->
          Printf.eprintf "Third format parse error: %s\n" (Printexc.to_string e3);
          { total = 0; data = []; next_cursor = None }

(** Helper function to consume and return response body data *)
let consume_body body =
  Cohttp_lwt.Body.to_string body >>= fun _ ->
  Lwt.return_unit

(** Fetch bookmarks from a Karakeep instance with pagination support *)
let fetch_bookmarks ~api_key ?(limit=50) ?(offset=0) ?cursor ?(include_content=true) ?filter_tags base_url =
  let open Cohttp_lwt_unix in
  
  (* Base URL for bookmarks API *)
  let url_base = Printf.sprintf "%s/api/v1/bookmarks?limit=%d&includeContent=%b" 
                  base_url limit include_content in
  
  (* Add pagination parameter - either cursor or offset *)
  let url = 
    match cursor with
    | Some cursor_value -> 
        url_base ^ "&cursor=" ^ cursor_value
    | None -> 
        url_base ^ "&offset=" ^ string_of_int offset
  in
  
  (* Add tags filter if provided *)
  let url = match filter_tags with
    | Some tags when tags <> [] -> 
        (* URL encode each tag and join with commas *)
        let encoded_tags = 
          List.map (fun tag -> 
            Uri.pct_encode ~component:`Query_key tag
          ) tags 
        in
        let tags_param = String.concat "," encoded_tags in
        Printf.eprintf "Adding tags filter: %s\n" tags_param;
        url ^ "&tags=" ^ tags_param
    | _ -> url
  in
  
  (* Set up headers with API key *)
  let headers = Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Authorization" ("Bearer " ^ api_key) in
  
  Printf.eprintf "Fetching bookmarks from: %s\n" url;

  (* Make the request *)
  Lwt.catch
    (fun () ->
      Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
      if resp.status = `OK then
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        Printf.eprintf "Received %d bytes of response data\n" (String.length body_str);
        
        Lwt.catch
          (fun () ->
            let json = J.from_string body_str in
            Lwt.return (parse_bookmark_response json)
          )
          (fun e ->
            Printf.eprintf "JSON parsing error: %s\n" (Printexc.to_string e);
            Printf.eprintf "Response body (first 200 chars): %s\n" 
              (if String.length body_str > 200 then String.sub body_str 0 200 ^ "..." else body_str);
            Lwt.fail e
          )
      else
        let status_code = Cohttp.Code.code_of_status resp.status in
        consume_body body >>= fun _ ->
        Printf.eprintf "HTTP error %d\n" status_code;
        Lwt.fail_with (Fmt.str "HTTP error: %d" status_code)
    )
    (fun e ->
      Printf.eprintf "Network error: %s\n" (Printexc.to_string e);
      Lwt.fail e
    )

(** Fetch all bookmarks from a Karakeep instance using pagination *)
let fetch_all_bookmarks ~api_key ?(page_size=50) ?max_pages ?filter_tags base_url =
  let rec fetch_pages page_num cursor acc _total_count =
    (* Use cursor if available, otherwise use offset-based pagination *)
    (match cursor with
     | Some cursor_str -> fetch_bookmarks ~api_key ~limit:page_size ~cursor:cursor_str ?filter_tags base_url 
     | None -> fetch_bookmarks ~api_key ~limit:page_size ~offset:(page_num * page_size) ?filter_tags base_url)
    >>= fun response ->
    
    let all_bookmarks = acc @ response.data in
    
    (* Determine if we need to fetch more pages *)
    let more_available = 
      match response.next_cursor with
      | Some _ -> true  (* We have a cursor, so there are more results *)
      | None -> 
          (* Fall back to offset-based check *)
          let fetched_count = (page_num * page_size) + List.length response.data in
          fetched_count < response.total
    in
    
    let under_max_pages = match max_pages with
      | None -> true
      | Some max -> page_num + 1 < max
    in
    
    if more_available && under_max_pages then
      fetch_pages (page_num + 1) response.next_cursor all_bookmarks response.total
    else
      Lwt.return all_bookmarks
  in
  fetch_pages 0 None [] 0

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
    consume_body body >>= fun () ->
    Lwt.fail_with (Fmt.str "HTTP error: %d" status_code)

(** Get the asset URL for a given asset ID *)
let get_asset_url base_url asset_id =
  Printf.sprintf "%s/api/assets/%s" base_url asset_id

(** Fetch an asset from the Karakeep server as a binary string *)
let fetch_asset ~api_key base_url asset_id =
  let open Cohttp_lwt_unix in
  
  let url = get_asset_url base_url asset_id in
  
  (* Set up headers with API key *)
  let headers = Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Authorization" ("Bearer " ^ api_key) in
  
  Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
  if resp.status = `OK then
    Cohttp_lwt.Body.to_string body
  else
    let status_code = Cohttp.Code.code_of_status resp.status in
    consume_body body >>= fun () ->
    Lwt.fail_with (Fmt.str "Asset fetch error: %d" status_code)

(** Create a new bookmark in Karakeep with optional tags *)
let create_bookmark ~api_key ~url ?title ?note ?tags ?(favourited=false) ?(archived=false) base_url =
  let open Cohttp_lwt_unix in
  
  (* Prepare the bookmark request body *)
  let body_obj = [
    ("type", `String "link");
    ("url", `String url);
    ("favourited", `Bool favourited);
    ("archived", `Bool archived);
  ] in
  
  (* Add optional fields *)
  let body_obj = match title with
    | Some title_str -> ("title", `String title_str) :: body_obj
    | None -> body_obj
  in
  
  let body_obj = match note with
    | Some note_str -> ("note", `String note_str) :: body_obj
    | None -> body_obj
  in
  
  (* Convert to JSON *)
  let body_json = `O body_obj in
  let body_str = J.to_string body_json in
  
  (* Set up headers with API key *)
  let headers = Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Authorization" ("Bearer " ^ api_key)
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
  in
  
  (* Helper function to ensure we consume all response body data *)
  let consume_body body =
    Cohttp_lwt.Body.to_string body >>= fun _ ->
    Lwt.return_unit
  in
  
  (* Create the bookmark *)
  let url_endpoint = Printf.sprintf "%s/api/v1/bookmarks" base_url in
  Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body_str) (Uri.of_string url_endpoint) >>= fun (resp, body) ->
  
  if resp.status = `Created || resp.status = `OK then
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    let json = J.from_string body_str in
    let bookmark = parse_bookmark json in
    
    (* If tags are provided, add them to the bookmark *)
    (match tags with
     | Some tag_list when tag_list <> [] ->
         (* Prepare the tags request body *)
         let tag_objects = List.map (fun tag_name -> 
           `O [("tagName", `String tag_name)]
         ) tag_list in
         
         let tags_body = `O [("tags", `A tag_objects)] in
         let tags_body_str = J.to_string tags_body in
         
         (* Add tags to the bookmark *)
         let tags_url = Printf.sprintf "%s/api/v1/bookmarks/%s/tags" base_url bookmark.id in
         Client.post ~headers ~body:(Cohttp_lwt.Body.of_string tags_body_str) (Uri.of_string tags_url) >>= fun (resp, body) ->
         
         (* Always consume the response body *)
         consume_body body >>= fun () ->
         
         if resp.status = `OK then
           (* Fetch the bookmark again to get updated tags *)
           fetch_bookmark_details ~api_key base_url bookmark.id
         else
           (* Return the bookmark without tags if tag addition failed *)
           Lwt.return bookmark
     | _ -> Lwt.return bookmark)
  else
    let status_code = Cohttp.Code.code_of_status resp.status in
    Cohttp_lwt.Body.to_string body >>= fun error_body ->
    Lwt.fail_with (Fmt.str "Failed to create bookmark. HTTP error: %d. Details: %s" status_code error_body)

(** Convert a Karakeep bookmark to Bushel.Link.t compatible structure *)
let to_bushel_link ?base_url bookmark =
  (* Try to find the best title from multiple possible sources *)
  let description = 
    match bookmark.title with
    | Some title when title <> "" -> title
    | _ -> 
        (* Check if there's a title in the content *)
        let content_title = List.assoc_opt "title" bookmark.content in
        match content_title with
        | Some title when title <> "" && title <> "null" -> title
        | _ -> bookmark.url
  in
  let date = Ptime.to_date bookmark.created_at in
  
  (* Build comprehensive metadata from all available fields *)
  let metadata = 
    (match bookmark.summary with Some s -> [("summary", s)] | None -> []) @
    (match bookmark.tagging_status with Some s -> [("tagging_status", s)] | None -> []) @
    (List.map (fun (id, asset_type) -> ("asset_" ^ asset_type, id)) bookmark.assets) @
    (List.filter_map (fun (k, v) -> 
      if List.mem k ["type"; "url"; "title"; "screenshotAssetId"; "favicon"] 
      then Some ("content_" ^ k, v) else None) bookmark.content)
  in
  
  (* Create karakeep_id if base_url is provided *)
  let karakeep_id = 
    match base_url with
    | Some url -> Some { Bushel.Link.remote_url = url; id = bookmark.id }
    | None -> 
        (* For backward compatibility, try to extract from metadata *)
        Some { Bushel.Link.remote_url = "unknown"; id = bookmark.id }
  in
  
  (* Extract any tags for additional context *)
  let bushel_slugs = 
    (* If there are tags that start with "bushel:", use them as slugs *)
    List.filter_map (fun tag ->
      if String.starts_with ~prefix:"bushel:" tag then
        Some (String.sub tag 7 (String.length tag - 7))
      else
        None
    ) bookmark.tags
  in
  
  { Bushel.Link.url = bookmark.url; date; description; metadata; karakeep_id; bushel_slugs }
