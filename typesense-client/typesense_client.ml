open Lwt.Syntax
open Cohttp_lwt_unix

(** TODO:claude Standalone Typesense client for OCaml *)

(** Configuration for Typesense client *)
type config = {
  endpoint : string;
  api_key : string;
}

(** Error types for Typesense operations *)
type error = 
  | Http_error of int * string
  | Json_error of string
  | Connection_error of string

let pp_error fmt = function
  | Http_error (code, msg) -> Fmt.pf fmt "HTTP %d: %s" code msg
  | Json_error msg -> Fmt.pf fmt "JSON error: %s" msg
  | Connection_error msg -> Fmt.pf fmt "Connection error: %s" msg

(** TODO:claude Create authentication headers for Typesense API *)
let auth_headers api_key =
  Cohttp.Header.of_list [
    ("X-TYPESENSE-API-KEY", api_key);
    ("Content-Type", "application/json");
  ]

(** TODO:claude Make HTTP request to Typesense API *)
let make_request ?(meth=`GET) ?(body="") config path =
  let uri = Uri.of_string (config.endpoint ^ path) in
  let headers = auth_headers config.api_key in
  let body = if body = "" then `Empty else `String body in
  Lwt.catch (fun () ->
    let* resp, body = Client.call ~headers ~body meth uri in
    let status = Cohttp.Code.code_of_status (Response.status resp) in
    let* body_str = Cohttp_lwt.Body.to_string body in
    if status >= 200 && status < 300 then
      Lwt.return_ok body_str
    else
      Lwt.return_error (Http_error (status, body_str))
  ) (fun exn ->
    Lwt.return_error (Connection_error (Printexc.to_string exn))
  )

(** TODO:claude Search result types *)
type search_result = {
  id: string;
  title: string;
  content: string;
  score: float;
  collection: string;
  highlights: (string * string list) list;
  document: Ezjsonm.value;  (* Store raw document for flexible field access *)
}

type search_response = {
  hits: search_result list;
  total: int;
  query_time: float;
}

(** TODO:claude Parse search result from JSON *)
let parse_search_result collection json =
  let open Ezjsonm in
  let document = get_dict json |> List.assoc "document" in
  let highlights = try get_dict json |> List.assoc "highlights" with _ -> `A [] in
  let score = try get_dict json |> List.assoc "text_match" |> get_float with _ -> 0.0 in
  
  let id = get_dict document |> List.assoc "id" |> get_string in
  let title = try get_dict document |> List.assoc "title" |> get_string with _ -> "" in
  let content = try
    match collection with
    | "papers" -> get_dict document |> List.assoc "abstract" |> get_string
    | "projects" -> get_dict document |> List.assoc "description" |> get_string
    | "news" -> get_dict document |> List.assoc "content" |> get_string
    | "videos" -> get_dict document |> List.assoc "description" |> get_string
    | "notes" -> get_dict document |> List.assoc "content" |> get_string
    | "ideas" -> get_dict document |> List.assoc "description" |> get_string
    | "contacts" -> get_dict document |> List.assoc "name" |> get_string
    | _ -> ""
  with _ -> "" in
  
  let parse_highlights highlights =
    try
      get_list (fun h ->
        let field = get_dict h |> List.assoc "field" |> get_string in
        let snippets = get_dict h |> List.assoc "snippets" |> get_list get_string in
        (field, snippets)
      ) highlights
    with _ -> []
  in
  
  { id; title; content; score; collection; highlights = parse_highlights highlights; document }

(** TODO:claude Parse search response from JSON *)
let parse_search_response collection json =
  let open Ezjsonm in
  let hits = get_dict json |> List.assoc "hits" |> get_list (parse_search_result collection) in
  let total = get_dict json |> List.assoc "found" |> get_int in
  let query_time = get_dict json |> List.assoc "search_time_ms" |> get_float in
  { hits; total; query_time }

(** TODO:claude Search a single collection *)
let search_collection config collection_name query ?(limit=10) ?(offset=0) () =
  let escaped_query = Uri.pct_encode query in
  let query_fields = match collection_name with
    | "papers" -> "title,abstract,authors"
    | "projects" -> "title,description"
    | "news" -> "title,content"
    | "videos" -> "title,description"
    | "notes" -> "title,content"
    | "ideas" -> "title,description"
    | "contacts" -> "name,names"
    | _ -> "title,content,description,abstract"
  in
  let path = Printf.sprintf "/collections/%s/documents/search?q=%s&query_by=%s&per_page=%d&page=%d&highlight_full_fields=%s"
    collection_name escaped_query query_fields limit ((offset / limit) + 1) query_fields in
  let* result = make_request config path in
  match result with
  | Ok response_str ->
    (try
      let json = Ezjsonm.from_string response_str in
      let search_response = parse_search_response collection_name json in
      Lwt.return_ok search_response
    with exn ->
      Lwt.return_error (Json_error (Printexc.to_string exn)))
  | Error err -> Lwt.return_error err

(** TODO:claude Helper function to drop n elements from list *)
let rec drop n lst =
  if n <= 0 then lst
  else match lst with
  | [] -> []
  | _ :: tl -> drop (n - 1) tl

(** TODO:claude Helper function to take n elements from list *)
let rec take n lst =
  if n <= 0 then []
  else match lst with
  | [] -> []
  | hd :: tl -> hd :: take (n - 1) tl

(** TODO:claude Multisearch result types *)
type multisearch_response = {
  results: search_response list;
}

(** TODO:claude Parse multisearch response from JSON *)
let parse_multisearch_response json =
  let open Ezjsonm in
  let results_json = get_dict json |> List.assoc "results" |> get_list (fun r -> r) in
  let results = List.mapi (fun i result_json ->
    let collection_name = match i with
      | 0 -> "contacts"
      | 1 -> "news" 
      | 2 -> "notes"
      | 3 -> "papers"
      | 4 -> "projects"
      | 5 -> "ideas"
      | 6 -> "videos"
      | _ -> "unknown"
    in
    parse_search_response collection_name result_json
  ) results_json in
  { results }

(** TODO:claude Perform multisearch across all collections *)
let multisearch config query ?(limit=10) () =
  let collections = ["contacts"; "news"; "notes"; "papers"; "projects"; "ideas"; "videos"] in
  let query_by_collection = [
    ("contacts", "name,names,email,handle,github,twitter,url");
    ("news", "title,content,source,author,category,tags");
    ("notes", "title,content,tags,synopsis");
    ("papers", "title,authors,abstract,journal,tags");
    ("projects", "title,description,languages,license,status,tags");
    ("ideas", "title,description,level,status,project,supervisors,tags");
    ("videos", "title,description,channel,platform,tags");
  ] in
  
  let searches = List.map (fun collection ->
    let query_by = List.assoc collection query_by_collection in
    Ezjsonm.dict [
      ("collection", Ezjsonm.string collection);
      ("q", Ezjsonm.string query);
      ("query_by", Ezjsonm.string query_by);
      ("exclude_fields", Ezjsonm.string "embedding");
      ("per_page", Ezjsonm.int limit);
    ]
  ) collections in
  
  let body = Ezjsonm.dict [("searches", Ezjsonm.list (fun x -> x) searches)] |> Ezjsonm.value_to_string in
  let* result = make_request ~meth:`POST ~body config "/multi_search" in
  
  match result with
  | Ok response_str ->
    (try
      let json = Ezjsonm.from_string response_str in
      let multisearch_resp = parse_multisearch_response json in
      Lwt.return_ok multisearch_resp
    with exn ->
      Lwt.return_error (Json_error (Printexc.to_string exn)))
  | Error err -> Lwt.return_error err

(** TODO:claude Combine multisearch results into single result set *)
let combine_multisearch_results (multisearch_resp : multisearch_response) ?(limit=10) ?(offset=0) () =
  (* Collect all hits from all collections *)
  let all_hits = List.fold_left (fun acc response ->
    response.hits @ acc
  ) [] multisearch_resp.results in
  
  (* Sort by score descending *)
  let sorted_hits = List.sort (fun a b -> Float.compare b.score a.score) all_hits in
  
  (* Apply offset and limit *)
  let dropped_hits = drop offset sorted_hits in
  let final_hits = take limit dropped_hits in
  
  (* Calculate totals *)
  let total = List.fold_left (fun acc response -> acc + response.total) 0 multisearch_resp.results in
  let query_time = List.fold_left (fun acc response -> acc +. response.query_time) 0.0 multisearch_resp.results in
  
  { hits = final_hits; total; query_time }

(** TODO:claude List all collections *)
let list_collections config =
  let* result = make_request config "/collections" in
  match result with
  | Ok response_str ->
    (try
      let json = Ezjsonm.from_string response_str in
      let collections = Ezjsonm.get_list (fun c ->
        let name = Ezjsonm.get_dict c |> List.assoc "name" |> Ezjsonm.get_string in
        let num_docs = Ezjsonm.get_dict c |> List.assoc "num_documents" |> Ezjsonm.get_int in
        (name, num_docs)
      ) json in
      Lwt.return_ok collections
    with exn ->
      Lwt.return_error (Json_error (Printexc.to_string exn)))
  | Error err -> Lwt.return_error err

(** TODO:claude Pretty printer utilities *)

(** Extract field value from JSON document or return empty string if not found *)
let extract_field_string document field =
  try
    let open Ezjsonm in
    get_dict document |> List.assoc field |> get_string
  with _ -> ""

(** Extract field value from JSON document as string list or return empty list if not found *)
let extract_field_string_list document field =
  try
    let open Ezjsonm in
    get_dict document |> List.assoc field |> get_list get_string
  with _ -> []

(** Extract field value from JSON document as boolean or return false if not found *)
let extract_field_bool document field =
  try
    let open Ezjsonm in
    get_dict document |> List.assoc field |> get_bool
  with _ -> false

(** Format authors list for display *)
let format_authors authors =
  match authors with
  | [] -> ""
  | [single] -> single
  | _first :: rest when List.length rest <= 2 -> String.concat ", " authors
  | first :: _rest -> Printf.sprintf "%s et al." first

(** Format date for display *)
let format_date date_str =
  match date_str with
  | "" -> ""
  | d when String.length d >= 10 -> String.sub d 0 10  (* Take YYYY-MM-DD part *)
  | d -> d

(** Format tags for display *)
let format_tags tags =
  match tags with
  | [] -> ""
  | ts when List.length ts <= 3 -> String.concat ", " ts
  | ts -> Printf.sprintf "%s (+%d more)" (String.concat ", " (take 2 ts)) (List.length ts - 2)

(** TODO:claude One-line pretty printer for search results *)
let pp_search_result_oneline (result : search_result) =
  let document = result.document in
  
  match result.collection with
  | "papers" ->
    let authors = extract_field_string_list document "authors" in
    let date = extract_field_string document "date" in
    let journal = extract_field_string_list document "journal" in
    let journal_str = match journal with [] -> "" | j :: _ -> Printf.sprintf " (%s)" j in
    Printf.sprintf "📄 %s — %s%s %s" 
      result.title 
      (format_authors authors)
      journal_str
      (format_date date)
      
  | "videos" ->
    let date = extract_field_string document "published_date" in
    let uuid = extract_field_string document "uuid" in
    let is_talk = extract_field_bool document "is_talk" in
    let talk_indicator = if is_talk then "🎤" else "🎬" in
    let url = extract_field_string document "url" in
    let url_display = if url = "" then "" else Printf.sprintf " <%s>" url in
    Printf.sprintf "%s %s — %s [%s]%s" 
      talk_indicator
      result.title 
      (format_date date)
      (if uuid = "" then result.id else uuid)
      url_display
      
  | "projects" ->
    let start_year = extract_field_string document "start_year" in
    let tags = extract_field_string_list document "tags" in
    let tags_str = match tags with [] -> "" | ts -> Printf.sprintf " #%s" (format_tags ts) in
    Printf.sprintf "🚀 %s — %s%s" 
      result.title 
      (if start_year = "" then "" else Printf.sprintf "(%s) " start_year)
      tags_str
      
  | "news" ->
    let date = extract_field_string document "date" in
    let url = extract_field_string document "url" in
    let url_display = if url = "" then "" else Printf.sprintf " <%s>" url in
    Printf.sprintf "📰 %s — %s%s" 
      result.title 
      (format_date date)
      url_display
      
  | "notes" ->
    let date = extract_field_string document "date" in
    let tags = extract_field_string_list document "tags" in
    let tags_str = match tags with [] -> "" | ts -> Printf.sprintf " #%s" (format_tags ts) in
    Printf.sprintf "📝 %s — %s%s" 
      result.title 
      (format_date date)
      tags_str
      
  | "ideas" ->
    let project = extract_field_string document "project" in
    let level = extract_field_string document "level" in
    let status = extract_field_string document "status" in
    let year = extract_field_string document "year" in
    Printf.sprintf "💡 %s — %s%s%s %s" 
      result.title 
      (if project = "" then "" else Printf.sprintf "[%s] " project)
      (if level = "" then "" else Printf.sprintf "(%s) " level)
      (if status = "" then "" else Printf.sprintf "%s " status)
      year
      
  | "contacts" ->
    let names = extract_field_string_list document "names" in
    let handle = extract_field_string document "handle" in
    let email = extract_field_string document "email" in
    let github = extract_field_string document "github" in
    let name_str = match names with [] -> result.title | n :: _ -> n in
    let contact_info = [
      (if handle = "" then "" else Printf.sprintf "@%s" handle);
      (if email = "" then "" else email);
      (if github = "" then "" else Printf.sprintf "github:%s" github);
    ] |> List.filter (fun s -> s <> "") |> String.concat " " in
    Printf.sprintf "👤 %s — %s" 
      name_str 
      contact_info
      
  | _ -> Printf.sprintf "[%s] %s" result.collection result.title