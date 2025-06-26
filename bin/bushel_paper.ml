module ZT = Zotero_translation
open Lwt.Infix
open Printf
module J = Ezjsonm
open Cmdliner


let _authors b j =
  let keys = J.get_dict j in
  let authors = J.get_list J.get_string (List.assoc "author" keys) in
  let a = 
    List.fold_left (fun acc a ->
        match Bushel.Entry.lookup_by_name b a with
        | Some c -> `String ("@" ^ (Bushel.Contact.handle c)) :: acc
        | None -> failwith (sprintf "author %s not found" a)
      ) [] authors
    in
  J.update j ["author"] (Some (`A a))

let of_doi zt ~base_dir ~slug ~version doi =
  ZT.json_of_doi zt ~slug doi  >>= fun j ->
  let papers_dir = Printf.sprintf "%s/papers/%s" base_dir slug in
  (* Ensure papers directory exists *)
  (try Unix.mkdir papers_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  
  (* Extract abstract from JSON data *)
  let abstract = try
    let keys = Ezjsonm.get_dict (j :> Ezjsonm.value) in
    match List.assoc_opt "abstract" keys with
    | Some abstract_json -> Some (Ezjsonm.get_string abstract_json)
    | None -> None
  with _ -> None in
  
  (* Remove abstract from frontmatter - it goes in body *)
  let keys = Ezjsonm.get_dict (j :> Ezjsonm.value) in
  let filtered_keys = List.filter (fun (k, _) -> k <> "abstract") keys in
  let json_without_abstract = `O filtered_keys in
  
  (* Use library function to generate YAML with abstract in body *)
  let content = Bushel.Paper.to_yaml ?abstract ~ver:version json_without_abstract in
  
  let filename = Printf.sprintf "%s.md" version in
  let filepath = Filename.concat papers_dir filename in
  let oc = open_out filepath in
  output_string oc content;
  close_out oc;
  Printf.printf "Created paper file: %s\n" filepath;
  Lwt.return ()

let slug_arg =
  let doc = "Slug for the entry." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SLUG" ~doc)

let version_arg =
  let doc = "Version of the entry." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"VERSION" ~doc)

let doi_arg =
  let doc = "DOI of the entry." in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"DOI" ~doc)

(* Export the term for use in main bushel.ml *)
let term =
  Term.(const (fun base slug version doi ->
    let zt = ZT.v "http://svr-avsm2-eeg-ce:1969" in
    Lwt_main.run @@ of_doi zt ~base_dir:base ~slug ~version doi; 0
  ) $ Bushel_common.base_dir $ slug_arg $ version_arg $ doi_arg)

let cmd =
  let doc = "Generate paper entry from DOI" in
  let info = Cmd.info "paper" ~doc in
  Cmd.v info term

(* Main entry point removed - accessed through bushel_main.ml *)
