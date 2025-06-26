open Cmdliner
open Lwt.Infix
open Printf

(* Type for person response *)
type person = {
  id: string;
  name: string;
  thumbnailPath: string option;
}

(* Parse a person from JSON *)
let parse_person json =
  let open Ezjsonm in
  let id = find json ["id"] |> get_string in
  let name = find json ["name"] |> get_string in
  let thumbnailPath = 
    try Some (find json ["thumbnailPath"] |> get_string)
    with _ -> None
  in
  { id; name; thumbnailPath }

(* Parse a list of people from JSON response *)
let parse_people_response json =
  let open Ezjsonm in
  get_list parse_person json

(* Read API key from file *)
let read_api_key file =
  let ic = open_in file in
  let key = input_line ic in
  close_in ic;
  key

(* Search for a person by name *)
let search_person base_url api_key name =
  let open Cohttp_lwt_unix in
  let headers = Cohttp.Header.init_with "X-Api-Key" api_key in
  let encoded_name = Uri.pct_encode name in
  let url = Printf.sprintf "%s/api/search/person?name=%s" base_url encoded_name in
  
  Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
  if resp.status = `OK then
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    let json = Ezjsonm.from_string body_str in
    Lwt.return (parse_people_response json)
  else
    let status_code = Cohttp.Code.code_of_status resp.status in
    Lwt.fail_with (Printf.sprintf "HTTP error: %d" status_code)

(* Download thumbnail for a person *)
let download_thumbnail base_url api_key person_id output_path =
  let open Cohttp_lwt_unix in
  let headers = Cohttp.Header.init_with "X-Api-Key" api_key in
  let url = Printf.sprintf "%s/api/people/%s/thumbnail" base_url person_id in
  
  Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
  match resp.status with
  | `OK ->
    Cohttp_lwt.Body.to_string body >>= fun img_data ->
    (* Ensure output directory exists *)
    (try
       let dir = Filename.dirname output_path in
       if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
       Lwt.return_unit
     with _ -> Lwt.return_unit) >>= fun () ->
    Lwt_io.with_file ~mode:Lwt_io.output output_path
      (fun oc -> Lwt_io.write oc img_data) >>= fun () ->
    Lwt.return_ok output_path
  | _ ->
    let status_code = Cohttp.Code.code_of_status resp.status in
    Lwt.return_error (Printf.sprintf "HTTP error: %d" status_code)

(* Get face for a single contact *)
let get_face_for_contact base_url api_key output_dir contact =
  let name = Bushel.Contact.name contact in
  let handle = Bushel.Contact.handle contact in
  let output_path = Filename.concat output_dir (handle ^ ".jpg") in
  
  (* Skip if file already exists *)
  if Sys.file_exists output_path then
    Lwt.return (`Skipped (sprintf "Thumbnail for '%s' already exists at %s" name output_path))
  else begin
    printf "Processing contact: %s (handle: %s)\n%!" name handle;
    search_person base_url api_key name >>= function
    | [] -> 
        Lwt.return (`Error (sprintf "No person found with name '%s'" name))
    | person :: _ ->
        download_thumbnail base_url api_key person.id output_path >>= function
        | Ok path -> 
            Lwt.return (`Ok (sprintf "Saved thumbnail for '%s' to %s" name path))
        | Error err -> 
            Lwt.return (`Error (sprintf "Error for '%s': %s" name err))
  end

(* Process all contacts or a specific one *)
let process_contacts base_dir output_dir specific_handle api_key base_url =
  printf "Loading Bushel database from %s\n%!" base_dir;
  let db = Bushel.load base_dir in
  let contacts = Bushel.Entry.contacts db in
  printf "Found %d contacts\n%!" (List.length contacts);
  
  (* Ensure output directory exists *)
  if not (Sys.file_exists output_dir) then Unix.mkdir output_dir 0o755;
  
  (* Filter contacts based on specific_handle if provided *)
  let contacts_to_process = 
    match specific_handle with
    | Some handle -> 
        begin match Bushel.Contact.find_by_handle contacts handle with
        | Some contact -> [contact]
        | None -> 
            eprintf "No contact found with handle '%s'\n%!" handle;
            []
        end
    | None -> contacts
  in
  
  (* Process each contact *)
  let results = Lwt_main.run begin
    Lwt_list.map_s 
      (fun contact -> 
         get_face_for_contact base_url api_key output_dir contact >>= fun result ->
         Lwt.return (Bushel.Contact.handle contact, result))
      contacts_to_process
  end in
  
  (* Print summary *)
  let ok_count = List.length (List.filter (fun (_, r) -> match r with `Ok _ -> true | _ -> false) results) in
  let error_count = List.length (List.filter (fun (_, r) -> match r with `Error _ -> true | _ -> false) results) in
  let skipped_count = List.length (List.filter (fun (_, r) -> match r with `Skipped _ -> true | _ -> false) results) in
  
  printf "\nSummary:\n";
  printf "  Successfully processed: %d\n" ok_count;
  printf "  Errors: %d\n" error_count;
  printf "  Skipped (already exist): %d\n" skipped_count;
  
  (* Print detailed results *)
  if error_count > 0 then begin
    printf "\nError details:\n";
    List.iter (fun (handle, result) ->
      match result with
      | `Error msg -> printf "  %s: %s\n" handle msg
      | _ -> ())
      results;
  end;
  
  if ok_count > 0 || skipped_count > 0 then 0 else 1

(* Command line interface *)

(* Export the term for use in main bushel.ml *)
let term =
  Term.(
    const (fun base_dir output_dir handle api_key_file base_url ->
      try
        let api_key = read_api_key api_key_file in
        process_contacts base_dir output_dir handle api_key base_url
      with e -> 
        eprintf "Error: %s\n%!" (Printexc.to_string e);
        1
    ) $ Bushel_common.base_dir $ Bushel_common.output_dir ~default:"." $ Bushel_common.handle_opt $ 
      Bushel_common.api_key_file ~default:".photos-api" $ 
      Bushel_common.url_term ~default:"https://photos.recoil.org" ~doc:"Base URL of the Immich instance")

let cmd =
  let info = Cmd.info "faces" ~doc:"Retrieve face thumbnails for Bushel contacts from Immich" in
  Cmd.v info term

(* Main entry point removed - accessed through bushel_main.ml *)
