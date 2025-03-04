module ZT = Zotero_translation
open Lwt.Infix
open Printf
module J = Ezjsonm
open Cmdliner

let post_to_jekyll j =
  sprintf "---\n%s---\n" (Yaml.to_string_exn j)

let authors b j =
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

let of_doi zt b ~slug doi =
  ZT.json_of_doi zt ~slug doi  >>= fun j ->
  let j = authors b (j:>Ezjsonm.value) in 
  print_endline (post_to_jekyll (j :> Yaml.value));
  Lwt.return ()

let base_arg =
  let doc = "Base directory." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"BASE" ~doc)

let slug_arg =
  let doc = "Slug for the entry." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"SLUG" ~doc)

let doi_arg =
  let doc = "DOI of the entry." in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"DOI" ~doc)

let cmd =
  let doc = "Bushel Paper CLI" in
  let term = Term.(const (fun base slug doi ->
    let e = Bushel.load base in
    let zt = ZT.v "http://svr-avsm2-eeg-ce:1969" in
    Lwt_main.run @@ of_doi zt e ~slug doi
  ) $ base_arg $ slug_arg $ doi_arg) in
  Cmd.v (Cmd.info "bushel_paper" ~doc) term

let () =
  exit (Cmd.eval cmd)
