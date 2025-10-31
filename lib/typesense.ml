open Lwt.Syntax
open Cohttp_lwt_unix

(** TODO:claude Typesense API client for Bushel *)

type config = {
  endpoint : string;
  api_key : string;
  openai_key : string;
}

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

(** TODO:claude Create a collection with given schema *)
let create_collection config (schema : Ezjsonm.value) =
  let body = Ezjsonm.value_to_string schema in
  make_request ~meth:`POST ~body config "/collections"

(** TODO:claude Check if collection exists *)
let collection_exists config name =
  let* result = make_request config ("/collections/" ^ name) in
  match result with
  | Ok _ -> Lwt.return true
  | Error (Http_error (404, _)) -> Lwt.return false
  | Error _ -> Lwt.return false

(** TODO:claude Delete a collection *)
let delete_collection config name =
  make_request ~meth:`DELETE config ("/collections/" ^ name)

(** TODO:claude Upload documents to a collection in batch *)
let upload_documents config collection_name (documents : Ezjsonm.value list) =
  let jsonl_lines = List.map (fun doc -> Ezjsonm.value_to_string doc) documents in
  let body = String.concat "\n" jsonl_lines in
  make_request ~meth:`POST ~body config 
    (Printf.sprintf "/collections/%s/documents/import?action=upsert" collection_name)


(** TODO:claude Convert Bushel objects to Typesense documents *)

(** TODO:claude Helper function to truncate long strings for embedding *)
let truncate_for_embedding ?(max_chars=20000) text =
  if String.length text <= max_chars then text
  else String.sub text 0 max_chars

(** TODO:claude Helper function to convert Ptime to Unix timestamp *)
let ptime_to_timestamp ptime =
  let span = Ptime.to_span ptime in
  let seconds = Ptime.Span.to_int_s span in
  match seconds with
  | Some s -> Int64.of_int s
  | None -> 0L

(** TODO:claude Helper function to convert date tuple to Unix timestamp *)
let date_to_timestamp (year, month, day) =
  match Ptime.of_date (year, month, day) with
  | Some ptime -> ptime_to_timestamp ptime
  | None -> 0L

(** Resolve author handles to full names in a list *)
let resolve_author_list contacts authors =
  List.map (fun author ->
    (* Strip '@' prefix if present *)
    let handle =
      if String.length author > 0 && author.[0] = '@' then
        String.sub author 1 (String.length author - 1)
      else
        author
    in
    (* Try to look up as a contact handle *)
    match Contact.find_by_handle contacts handle with
    | Some contact -> Contact.name contact
    | None -> author (* Keep original if not found *)
  ) authors

let contact_to_document (contact : Contact.t) =
  let open Ezjsonm in
  let safe_string_list_from_opt = function
    | Some s -> [s]
    | None -> []
  in
  dict [
    ("id", string (Contact.handle contact));
    ("handle", string (Contact.handle contact));
    ("name", string (Contact.name contact));
    ("names", list string (Contact.names contact));
    ("email", list string (safe_string_list_from_opt (Contact.email contact)));
    ("icon", list string (safe_string_list_from_opt (Contact.icon contact)));
    ("github", list string (safe_string_list_from_opt (Contact.github contact)));
    ("twitter", list string (safe_string_list_from_opt (Contact.twitter contact)));
    ("url", list string (safe_string_list_from_opt (Contact.url contact)));
  ]

let paper_to_document entries (paper : Paper.t) =
  let date_tuple = Paper.date paper in
  let contacts = Entry.contacts entries in

  (* Helper to extract string arrays from JSON, handling both single strings and arrays *)
  let extract_string_array_from_json json_field_name =
    try
      (* Access the raw JSON from the paper record *)
      let paper_json = Paper.raw_json paper in
      let value = Ezjsonm.get_dict paper_json |> List.assoc json_field_name in
      match value with
      | `String s -> [s]
      | `A l -> List.filter_map (function `String s -> Some s | _ -> None) l
      | _ -> []
    with _ -> []
  in

  (* Resolve author handles to full names *)
  let authors = resolve_author_list contacts (Paper.authors paper) in

  (* Convert abstract markdown to plain text *)
  let abstract = Md.markdown_to_plaintext entries (Paper.abstract paper) |> truncate_for_embedding in

  (* Extract publication metadata *)
  let bibtype = Paper.bibtype paper in
  let metadata =
    try
      match bibtype with
      | "article" -> Printf.sprintf "Journal: %s" (Paper.journal paper)
      | "inproceedings" -> Printf.sprintf "Proceedings: %s" (Paper.journal paper)
      | "misc" | "techreport" -> Printf.sprintf "Preprint: %s" (Paper.journal paper)
      | _ -> Printf.sprintf "%s: %s" (String.capitalize_ascii bibtype) (Paper.journal paper)
    with _ -> bibtype
  in

  (* Get bibtex from raw JSON *)
  let bibtex =
    try
      let paper_json = Paper.raw_json paper in
      Ezjsonm.get_dict paper_json
      |> List.assoc "bibtex"
      |> Ezjsonm.get_string
    with _ -> ""
  in

  let thumbnail_url = Entry.thumbnail entries (`Paper paper) in
  Ezjsonm.dict [
    ("id", Ezjsonm.string (Paper.slug paper));
    ("title", Ezjsonm.string (Paper.title paper));
    ("authors", Ezjsonm.list Ezjsonm.string authors);
    ("abstract", Ezjsonm.string abstract);
    ("metadata", Ezjsonm.string metadata);
    ("bibtex", Ezjsonm.string bibtex);
    ("date", Ezjsonm.string (let y, m, d = date_tuple in Printf.sprintf "%04d-%02d-%02d" y m d));
    ("date_timestamp", Ezjsonm.int64 (date_to_timestamp date_tuple));
    ("tags", Ezjsonm.list Ezjsonm.string (Paper.tags paper));
    ("doi", Ezjsonm.list Ezjsonm.string (extract_string_array_from_json "doi"));
    ("pdf_url", Ezjsonm.list Ezjsonm.string (extract_string_array_from_json "pdf_url"));
    ("journal", Ezjsonm.list Ezjsonm.string (extract_string_array_from_json "journal"));
    ("related_projects", Ezjsonm.list Ezjsonm.string (Paper.project_slugs paper));
    ("thumbnail_url", Ezjsonm.string (Option.value ~default:"" thumbnail_url));
  ]

let project_to_document entries (project : Project.t) =
  let open Ezjsonm in
  (* Use January 1st of start year as the date for sorting *)
  let date_timestamp = date_to_timestamp (project.start, 1, 1) in

  (* Convert body markdown to plain text *)
  let description = Md.markdown_to_plaintext entries (Project.body project) |> truncate_for_embedding in

  let thumbnail_url = Entry.thumbnail entries (`Project project) in
  dict [
    ("id", string project.slug);
    ("title", string (Project.title project));
    ("description", string description);
    ("start", int project.start);
    ("finish", option int project.finish);
    ("start_year", int project.start);
    ("date", string (Printf.sprintf "%04d-01-01" project.start));
    ("date_timestamp", int64 date_timestamp);
    ("tags", list string (Project.tags project));
    ("thumbnail_url", string (Option.value ~default:"" thumbnail_url));
  ]

let video_to_document entries (video : Video.t) =
  let open Ezjsonm in
  let datetime = Video.datetime video in
  let safe_string_list_from_opt = function
    | Some s -> [s]
    | None -> []
  in

  (* Convert body markdown to plain text *)
  let description = Md.markdown_to_plaintext entries (Video.body video) |> truncate_for_embedding in

  (* Resolve paper and project slugs to titles *)
  let paper_title = match Video.paper video with
    | Some slug ->
        (match Entry.lookup entries (":" ^ slug) with
         | Some entry -> Some (Entry.title entry)
         | None -> Some slug) (* Fallback to slug if not found *)
    | None -> None
  in
  let project_title = match Video.project video with
    | Some slug ->
        (match Entry.lookup entries (":" ^ slug) with
         | Some entry -> Some (Entry.title entry)
         | None -> Some slug) (* Fallback to slug if not found *)
    | None -> None
  in

  let thumbnail_url = Entry.thumbnail entries (`Video video) in
  dict [
    ("id", string (Video.slug video));
    ("title", string (Video.title video));
    ("description", string description);
    ("published_date", string (Ptime.to_rfc3339 datetime));
    ("date", string (Ptime.to_rfc3339 datetime));
    ("date_timestamp", int64 (ptime_to_timestamp datetime));
    ("url", string (Video.url video));
    ("uuid", string (Video.uuid video));
    ("is_talk", bool (Video.talk video));
    ("paper", list string (safe_string_list_from_opt paper_title));
    ("project", list string (safe_string_list_from_opt project_title));
    ("tags", list string video.tags);
    ("thumbnail_url", string (Option.value ~default:"" thumbnail_url));
  ]

let note_to_document entries (note : Note.t) =
  let open Ezjsonm in
  let datetime = Note.datetime note in
  let safe_string_list_from_opt = function
    | Some s -> [s]
    | None -> []
  in

  (* Convert body markdown to plain text *)
  let content = Md.markdown_to_plaintext entries (Note.body note) |> truncate_for_embedding in

  let thumbnail_url = Entry.thumbnail entries (`Note note) in
  let word_count = Note.words note in
  dict [
    ("id", string (Note.slug note));
    ("title", string (Note.title note));
    ("date", string (Ptime.to_rfc3339 datetime));
    ("date_timestamp", int64 (ptime_to_timestamp datetime));
    ("content", string content);
    ("tags", list string (Note.tags note));
    ("draft", bool (Note.draft note));
    ("synopsis", list string (safe_string_list_from_opt (Note.synopsis note)));
    ("thumbnail_url", string (Option.value ~default:"" thumbnail_url));
    ("words", int word_count);
  ]

let idea_to_document entries (idea : Idea.t) =
  let open Ezjsonm in
  let contacts = Entry.contacts entries in
  (* Use January 1st of the year as the date for sorting *)
  let date_timestamp = date_to_timestamp (Idea.year idea, 1, 1) in

  (* Convert body markdown to plain text *)
  let description = Md.markdown_to_plaintext entries (Idea.body idea) |> truncate_for_embedding in

  (* Resolve supervisor and student handles to full names *)
  let supervisors = resolve_author_list contacts (Idea.supervisors idea) in
  let students = resolve_author_list contacts (Idea.students idea) in

  (* Resolve project slug to project title *)
  let project_title =
    match Entry.lookup entries (Idea.project idea) with
    | Some entry -> Entry.title entry
    | None -> Idea.project idea (* Fallback to slug if not found *)
  in

  let thumbnail_url = Entry.thumbnail entries (`Idea idea) in
  dict [
    ("id", string idea.slug);
    ("title", string (Idea.title idea));
    ("description", string description);
    ("level", string (Idea.level_to_string (Idea.level idea)));
    ("project", string project_title);
    ("status", string (Idea.status_to_string (Idea.status idea)));
    ("year", int (Idea.year idea));
    ("date", string (Printf.sprintf "%04d-01-01" (Idea.year idea)));
    ("date_timestamp", int64 date_timestamp);
    ("supervisors", list string supervisors);
    ("students", list string students);
    ("tags", list string idea.tags);
    ("thumbnail_url", string (Option.value ~default:"" thumbnail_url));
  ]

(** TODO:claude Helper function to add embedding field to schema *)
let add_embedding_field_to_schema schema config embedding_from_fields =
  let open Ezjsonm in
  let fields = get_dict schema |> List.assoc "fields" |> get_list (fun f -> f) in
  let embedding_field = dict [
    ("name", string "embedding");
    ("type", string "float[]");
    ("embed", dict [
      ("from", list string embedding_from_fields);
      ("model_config", dict [
        ("model_name", string "openai/text-embedding-3-small");
        ("api_key", string config.openai_key);
      ]);
    ]);
  ] in
  let updated_fields = fields @ [embedding_field] in
  let updated_schema = 
    List.map (fun (k, v) ->
      if k = "fields" then (k, list (fun f -> f) updated_fields)
      else (k, v)
    ) (get_dict schema)
  in
  dict updated_schema

(** TODO:claude Upload all bushel objects to their respective collections *)
let upload_all config entries =
  let* () = Lwt_io.write Lwt_io.stdout "Uploading bushel data to Typesense\n" in

  let contacts = Entry.contacts entries in
  let papers = Entry.papers entries in
  let projects = Entry.projects entries in
  let notes = Entry.notes entries in
  let videos = Entry.videos entries in
  let ideas = Entry.ideas entries in

  let collections = [
    ("contacts", add_embedding_field_to_schema Contact.typesense_schema config ["name"; "names"], (List.map contact_to_document contacts : Ezjsonm.value list));
    ("papers", add_embedding_field_to_schema Paper.typesense_schema config ["title"; "abstract"; "authors"], (List.map (paper_to_document entries) papers : Ezjsonm.value list));
    ("videos", add_embedding_field_to_schema Video.typesense_schema config ["title"; "description"], (List.map (video_to_document entries) videos : Ezjsonm.value list));
    ("projects", add_embedding_field_to_schema Project.typesense_schema config ["title"; "description"; "tags"], (List.map (project_to_document entries) projects : Ezjsonm.value list));
    ("notes", add_embedding_field_to_schema Note.typesense_schema config ["title"; "content"; "tags"], (List.map (note_to_document entries) notes : Ezjsonm.value list));
    ("ideas", add_embedding_field_to_schema Idea.typesense_schema config ["title"; "description"; "tags"], (List.map (idea_to_document entries) ideas : Ezjsonm.value list));
  ] in

  let upload_collection ((name, schema, documents) : string * Ezjsonm.value * Ezjsonm.value list) =
    let* () = Lwt_io.write Lwt_io.stdout (Fmt.str "Processing collection: %s\n" name) in
    let* exists = collection_exists config name in
    let* () = 
      if exists then (
        let* () = Lwt_io.write Lwt_io.stdout (Fmt.str "Collection %s exists, deleting...\n" name) in
        let* result = delete_collection config name in
        match result with
        | Ok _ -> Lwt_io.write Lwt_io.stdout (Fmt.str "Deleted collection %s\n" name)
        | Error err -> 
          let err_str = Fmt.str "%a" pp_error err in
          Lwt_io.write Lwt_io.stdout (Fmt.str "Failed to delete collection %s: %s\n" name err_str)
      ) else
        Lwt.return_unit
    in
    let* () = Lwt_io.write Lwt_io.stdout (Fmt.str "Creating collection %s with %d documents\n" name (List.length documents)) in
    let* result = create_collection config schema in
    match result with
    | Ok _ ->
      let* () = Lwt_io.write Lwt_io.stdout (Fmt.str "Created collection %s\n" name) in
      if documents = [] then
        Lwt_io.write Lwt_io.stdout (Fmt.str "No documents to upload for %s\n" name)
      else (
        let* result = upload_documents config name documents in
        match result with
        | Ok response -> 
          (* Count successes and failures *)
          let lines = String.split_on_char '\n' response in
          let successes = List.fold_left (fun acc line -> 
            if String.contains line ':' && Str.string_match (Str.regexp ".*success.*true.*") line 0 then acc + 1 else acc) 0 lines in
          let failures = List.fold_left (fun acc line -> 
            if String.contains line ':' && Str.string_match (Str.regexp ".*success.*false.*") line 0 then acc + 1 else acc) 0 lines in
          let* () = Lwt_io.write Lwt_io.stdout (Fmt.str "Upload results for %s: %d successful, %d failed out of %d total\n" 
            name successes failures (List.length documents)) in
          if failures > 0 then
            let* () = Lwt_io.write Lwt_io.stdout (Fmt.str "Failed documents in %s:\n" name) in
            let failed_lines = List.filter (fun line -> Str.string_match (Str.regexp ".*success.*false.*") line 0) lines in
            Lwt_list.iter_s (fun line -> Lwt_io.write Lwt_io.stdout (line ^ "\n")) failed_lines
          else
            Lwt.return_unit
        | Error err -> 
          let err_str = Fmt.str "%a" pp_error err in
          Lwt_io.write Lwt_io.stdout (Fmt.str "Failed to upload documents to %s: %s\n" name err_str)
      )
    | Error err ->
      let err_str = Fmt.str "%a" pp_error err in
      Lwt_io.write Lwt_io.stdout (Fmt.str "Failed to create collection %s: %s\n" name err_str)
  in

  Lwt_list.iter_s upload_collection collections

(** TODO:claude Re-export search types from Typesense_client *)
type search_result = Typesense_client.search_result = {
  id: string;
  title: string;
  content: string;
  score: float;
  collection: string;
  highlights: (string * string list) list;
  document: Ezjsonm.value;
}

type search_response = Typesense_client.search_response = {
  hits: search_result list;
  total: int;
  query_time: float;
}

(** TODO:claude Convert bushel config to client config *)
let to_client_config (config : config) =
  Typesense_client.{ endpoint = config.endpoint; api_key = config.api_key }

(** TODO:claude Search a single collection *)
let search_collection (config : config) collection_name query ?(limit=10) ?(offset=0) () =
  let client_config = to_client_config config in
  let* result = Typesense_client.search_collection client_config collection_name query ~limit ~offset () in
  match result with
  | Ok response -> Lwt.return_ok response
  | Error (Typesense_client.Http_error (code, msg)) -> Lwt.return_error (Http_error (code, msg))
  | Error (Typesense_client.Json_error msg) -> Lwt.return_error (Json_error msg)
  | Error (Typesense_client.Connection_error msg) -> Lwt.return_error (Connection_error msg)

(** TODO:claude Search across all collections - use client multisearch *)
let search_all (config : config) query ?(limit=10) ?(offset=0) () =
  let client_config = to_client_config config in
  let* result = Typesense_client.multisearch client_config query ~limit:50 () in
  match result with
  | Ok multisearch_resp ->
    let combined_response = Typesense_client.combine_multisearch_results multisearch_resp ~limit ~offset () in
    Lwt.return_ok combined_response
  | Error (Typesense_client.Http_error (code, msg)) -> Lwt.return_error (Http_error (code, msg))
  | Error (Typesense_client.Json_error msg) -> Lwt.return_error (Json_error msg)
  | Error (Typesense_client.Connection_error msg) -> Lwt.return_error (Connection_error msg)

(** TODO:claude List all collections *)
let list_collections (config : config) =
  let client_config = to_client_config config in
  let* result = Typesense_client.list_collections client_config in
  match result with
  | Ok collections -> Lwt.return_ok collections
  | Error (Typesense_client.Http_error (code, msg)) -> Lwt.return_error (Http_error (code, msg))
  | Error (Typesense_client.Json_error msg) -> Lwt.return_error (Json_error msg)
  | Error (Typesense_client.Connection_error msg) -> Lwt.return_error (Connection_error msg)

(** TODO:claude Re-export multisearch types from Typesense_client *)
type multisearch_response = Typesense_client.multisearch_response = {
  results: search_response list;
}

(** TODO:claude Perform multisearch across all collections *)
let multisearch (config : config) query ?(limit=10) () =
  let client_config = to_client_config config in
  let* result = Typesense_client.multisearch client_config query ~limit () in
  match result with
  | Ok multisearch_resp -> Lwt.return_ok multisearch_resp
  | Error (Typesense_client.Http_error (code, msg)) -> Lwt.return_error (Http_error (code, msg))
  | Error (Typesense_client.Json_error msg) -> Lwt.return_error (Json_error msg)
  | Error (Typesense_client.Connection_error msg) -> Lwt.return_error (Connection_error msg)

(** TODO:claude Combine multisearch results into single result set *)
let combine_multisearch_results (multisearch_resp : multisearch_response) ?(limit=10) ?(offset=0) () =
  Typesense_client.combine_multisearch_results multisearch_resp ~limit ~offset ()

(** TODO:claude Load configuration from files *)
let load_config_from_files () =
  let read_file_if_exists filename =
    if Sys.file_exists filename then
      let ic = open_in filename in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Some (String.trim content)
    else None
  in
  
  let endpoint = match read_file_if_exists ".typesense-url" with
    | Some url -> url
    | None -> "http://localhost:8108"
  in
  
  let api_key = match read_file_if_exists ".typesense-key" with
    | Some key -> key
    | None -> 
      try Sys.getenv "TYPESENSE_API_KEY"
      with Not_found -> ""
  in
  
  let openai_key = match read_file_if_exists ".openrouter-api" with
    | Some key -> key
    | None -> 
      try Sys.getenv "OPENAI_API_KEY"
      with Not_found -> ""
  in
  
  { endpoint; api_key; openai_key }

(** TODO:claude Re-export pretty printer from Typesense_client *)
let pp_search_result_oneline = Typesense_client.pp_search_result_oneline
