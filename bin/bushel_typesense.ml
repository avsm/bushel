open Cmdliner

(** TODO:claude Bushel Typesense upload binary *)

let endpoint =
  let doc = "Typesense server endpoint URL" in
  Arg.(value & opt string "http://localhost:8108" & info ["endpoint"; "e"] ~doc)

let api_key =
  let doc = "Typesense API key for authentication" in
  Arg.(value & opt string "" & info ["api-key"; "k"] ~doc)

let data_dir =
  let doc = "Directory containing bushel data files" in
  Arg.(value & opt string "." & info ["data-dir"; "d"] ~doc)

let overwrite =
  let doc = "Overwrite existing collections" in
  Arg.(value & flag & info ["overwrite"; "o"] ~doc)

(** TODO:claude Main upload function *)
let upload endpoint api_key data_dir overwrite =
  if api_key = "" then (
    Printf.eprintf "Error: API key is required. Use --api-key or set TYPESENSE_API_KEY environment variable.\n";
    exit 1
  );
  
  let config = Bushel.Typesense.{ endpoint; api_key } in
  
  Printf.printf "Uploading bushel data to Typesense at %s\n" endpoint;
  Printf.printf "Data directory: %s\n" data_dir;
  Printf.printf "Overwrite mode: %b\n" overwrite;
  
  Lwt_main.run (
    Lwt.catch (fun () ->
      Bushel.Typesense.upload_all config data_dir
    ) (fun exn ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      exit 1
    )
  )

(** TODO:claude Command line interface setup *)
let cmd =
  let doc = "Upload bushel collections to Typesense search engine" in
  let man = [
    `S Manpage.s_description;
    `P "Upload all bushel object types (contacts, papers, projects, news, videos, notes, ideas) to a Typesense search engine instance.";
    `P "The API key can be provided via --api-key flag or TYPESENSE_API_KEY environment variable.";
    `S Manpage.s_examples;
    `P "Upload to local Typesense instance:";
    `Pre "  bushel-typesense --api-key xyz123 --data-dir /path/to/data";
    `P "Upload to remote Typesense instance:";
    `Pre "  bushel-typesense --endpoint https://search.example.com --api-key xyz123";
  ] in
  let info = Cmd.info "bushel-typesense" ~doc ~man in
  Cmd.v info Term.(const upload $ endpoint $ api_key $ data_dir $ overwrite)

let () =
  (* Check for API key in environment if not provided *)
  let api_key_env = try Some (Sys.getenv "TYPESENSE_API_KEY") with Not_found -> None in
  match api_key_env with
  | Some key when key <> "" ->
    (* Override the api_key argument with environment variable *)
    let api_key = Arg.(value & opt string key & info ["api-key"; "k"] ~doc:"Typesense API key") in
    let cmd = 
      let doc = "Upload bushel collections to Typesense search engine" in
      let info = Cmd.info "bushel-typesense" ~doc in
      Cmd.v info Term.(const upload $ endpoint $ api_key $ data_dir $ overwrite)
    in
    exit (Cmd.eval cmd)
  | _ ->
    exit (Cmd.eval cmd)