open Lwt.Syntax
open Cohttp_lwt_unix

(** TODO:claude Typesense API client for Bushel *)

type config = {
  endpoint : string;
  api_key : string;
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

let paper_to_document (paper : Paper.t) =
  let date_tuple = Paper.date paper in
  
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
  
  Ezjsonm.dict [
    ("id", Ezjsonm.string (Paper.slug paper));
    ("title", Ezjsonm.string (Paper.title paper));
    ("authors", Ezjsonm.list Ezjsonm.string (Paper.authors paper));
    ("abstract", Ezjsonm.string (Paper.abstract paper));
    ("date", Ezjsonm.string (let y, m, d = date_tuple in Printf.sprintf "%04d-%02d-%02d" y m d));
    ("date_timestamp", Ezjsonm.int64 (date_to_timestamp date_tuple));
    ("tags", Ezjsonm.list Ezjsonm.string (Paper.tags paper));
    ("doi", Ezjsonm.list Ezjsonm.string (extract_string_array_from_json "doi"));
    ("pdf_url", Ezjsonm.list Ezjsonm.string (extract_string_array_from_json "pdf_url"));
    ("journal", Ezjsonm.list Ezjsonm.string (extract_string_array_from_json "journal"));
    ("related_projects", Ezjsonm.list Ezjsonm.string (Paper.project_slugs paper));
  ]

let project_to_document (project : Project.t) =
  let open Ezjsonm in
  (* Use January 1st of start year as the date for sorting *)
  let date_timestamp = date_to_timestamp (project.start, 1, 1) in
  dict [
    ("id", string project.slug);
    ("title", string (Project.title project));
    ("description", string (Project.body project));  (* Use body as description *)
    ("start_year", int project.start);
    (* Skip finish_year for now due to type issues *)
    ("date", string (Printf.sprintf "%04d-01-01" project.start));
    ("date_timestamp", int64 date_timestamp);
    ("tags", list string (Project.tags project));
  ]

let news_to_document (news : News.t) =
  let open Ezjsonm in
  let datetime = News.datetime news in
  dict [
    ("id", string (News.slug news));
    ("title", string (News.title news));
    ("content", string (News.body news));  (* Use body as content *)
    ("date", string (Ptime.to_rfc3339 datetime));
    ("date_timestamp", int64 (ptime_to_timestamp datetime));
    ("tags", list string (News.tags news));
    ("url", string (News.site_url news));
  ]

let video_to_document (video : Video.t) =
  let open Ezjsonm in
  let datetime = Video.datetime video in
  let safe_string_list_from_opt = function
    | Some s -> [s]
    | None -> []
  in
  dict [
    ("id", string (Video.slug video));
    ("title", string (Video.title video));
    ("description", string (Video.body video));
    ("published_date", string (Ptime.to_rfc3339 datetime));
    ("date", string (Ptime.to_rfc3339 datetime));
    ("date_timestamp", int64 (ptime_to_timestamp datetime));
    ("url", string (Video.url video));
    ("uuid", string (Video.uuid video));
    ("is_talk", bool (Video.talk video));
    ("paper", list string (safe_string_list_from_opt (Video.paper video)));
    ("project", list string (safe_string_list_from_opt (Video.project video)));
    ("tags", list string video.tags);
  ]

let note_to_document (note : Note.t) =
  let open Ezjsonm in
  let datetime = Note.datetime note in
  let safe_string_list_from_opt = function
    | Some s -> [s]
    | None -> []
  in
  dict [
    ("id", string (Note.slug note));
    ("title", string (Note.title note));
    ("date", string (Ptime.to_rfc3339 datetime));
    ("date_timestamp", int64 (ptime_to_timestamp datetime));
    ("content", string (Note.body note));
    ("tags", list string (Note.tags note));
    ("draft", bool (Note.draft note));
    ("synopsis", list string (safe_string_list_from_opt (Note.synopsis note)));
    ("titleimage", option string (Note.titleimage note));
  ]

let idea_to_document (idea : Idea.t) =
  let open Ezjsonm in
  (* Use January 1st of the year as the date for sorting *)
  let date_timestamp = date_to_timestamp (Idea.year idea, 1, 1) in
  dict [
    ("id", string idea.slug);
    ("title", string (Idea.title idea));
    ("description", string (Idea.body idea));  (* Use body as description *)
    ("level", string (Idea.level_to_string (Idea.level idea)));
    ("project", string (Idea.project idea));
    ("status", string (Idea.status_to_string (Idea.status idea)));
    ("year", int (Idea.year idea));
    ("date", string (Printf.sprintf "%04d-01-01" (Idea.year idea)));
    ("date_timestamp", int64 date_timestamp);
    ("supervisors", list string (Idea.supervisors idea));
    ("tags", list string idea.tags);
  ]

(** TODO:claude Load bushel data from directory *)
let load_bushel_data data_dir =
  let map_md base subdir fn =
    let dir = base ^ "/data/" ^ subdir in
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun e -> fn dir e)
  in
  let map_category base c fn = map_md base c (fun dir e -> fn @@ Filename.concat dir e) in
  
  let load_contacts base = map_category base "contacts" Contact.of_md in
  let load_projects base = map_category base "projects" Project.of_md in
  let load_notes base = map_category base "notes" Note.of_md in
  let load_news base = map_category base "news" News.of_md in
  let load_ideas base = map_category base "ideas" Idea.of_md in
  let load_videos base = map_category base "videos" Video.of_md in
  
  let load_papers base =
    Sys.readdir (base ^ "/data/papers")
    |> Array.to_list
    |> List.filter (fun slug -> Sys.is_directory (base ^ "/data/papers/" ^ slug))
    |> List.map (fun slug ->
      Sys.readdir (base ^ "/data/papers/" ^ slug)
      |> Array.to_list
      |> List.filter (fun ver -> Filename.check_suffix ver ".md")
      |> List.map (fun ver ->
        let ver = Filename.chop_extension ver in
        Paper.of_md ~slug ~ver (base ^ "/data/papers/" ^ slug ^ "/" ^ ver ^ ".md")))
    |> List.flatten
    |> Paper.tv
  in
  
  (load_contacts data_dir, load_papers data_dir, load_projects data_dir,
   load_news data_dir, load_videos data_dir, load_notes data_dir, load_ideas data_dir)

(** TODO:claude Upload all bushel objects to their respective collections *)
let upload_all config data_dir =
  let* () = Lwt_io.write Lwt_io.stdout (Fmt.str "Loading bushel data from %s\n" data_dir) in
  
  let (contacts, papers, projects, news, videos, notes, ideas) = load_bushel_data data_dir in

  let collections = [
    ("contacts", Contact.typesense_schema, (List.map contact_to_document contacts : Ezjsonm.value list));
    ("papers", Paper.typesense_schema, (List.map paper_to_document papers : Ezjsonm.value list));
    ("videos", Video.typesense_schema, (List.map video_to_document videos : Ezjsonm.value list));
    ("projects", Project.typesense_schema, (List.map project_to_document projects : Ezjsonm.value list));
    ("news", News.typesense_schema, (List.map news_to_document news : Ezjsonm.value list));
    ("notes", Note.typesense_schema, (List.map note_to_document notes : Ezjsonm.value list));
    ("ideas", Idea.typesense_schema, (List.map idea_to_document ideas : Ezjsonm.value list));
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