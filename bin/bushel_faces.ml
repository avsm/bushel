open Cmdliner
open Lwt.Infix

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
    Lwt_io.with_file ~mode:Lwt_io.output output_path
      (fun oc -> Lwt_io.write oc img_data) >>= fun () ->
    Lwt.return_ok output_path
  | _ ->
    let status_code = Cohttp.Code.code_of_status resp.status in
    Lwt.return_error (Printf.sprintf "HTTP error: %d" status_code)

(* Main function to retrieve face for a name *)
let get_face base_url api_key name output_dir =
  Lwt_main.run begin
    search_person base_url api_key name >>= fun people ->
    match people with
    | [] -> 
        Lwt.return (`Error (false, Printf.sprintf "No person found with name '%s'" name))
    | person :: _ ->
        let output_filename = Printf.sprintf "%s/%s.jpg" output_dir (String.map (fun c -> if c = ' ' then '_' else c) name) in
        download_thumbnail base_url api_key person.id output_filename >>= function
        | Ok path -> 
            Lwt.return (`Ok (Printf.sprintf "Saved thumbnail to %s" path))
        | Error err -> 
            Lwt.return (`Error (false, err))
  end

(* Command line interface *)
let name_arg =
  let doc = "Name of the person to find" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"NAME")

let output_dir_arg =
  let doc = "Output directory for thumbnails (defaults to current directory)" in
  Arg.(value & opt string "." & info ["o"; "output-dir"] ~doc)

let api_key_file_arg =
  let doc = "File containing the Immich API key (defaults to .photos-api)" in
  Arg.(value & opt string ".photos-api" & info ["k"; "key-file"] ~doc)

let base_url_arg =
  let doc = "Base URL of the Immich instance" in
  Arg.(value & opt string "https://photos.recoil.org" & info ["u"; "url"] ~doc)

let faces_cmd =
  let doc = "Retrieve face thumbnails from Immich" in
  let info = Cmd.info "bushel-faces" ~doc in
  Cmd.v info Term.(
    const (fun name output_dir api_key_file base_url ->
      try
        let api_key = read_api_key api_key_file in
        match get_face base_url api_key name output_dir with
        | `Ok msg -> 
            print_endline msg; 
            0
        | `Error (_, err) -> 
            prerr_endline err; 
            1
      with e -> 
        prerr_endline (Printexc.to_string e); 
        1
    ) $ name_arg $ output_dir_arg $ api_key_file_arg $ base_url_arg)

let () = exit (Cmd.eval' faces_cmd)
