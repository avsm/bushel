module ZT = Zotero_translation
open Lwt.Infix
open Printf
module J = Ezjsonm

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

let () =
  (* TODO cmdliner *)
  let base = Sys.argv.(1) in
  let slug = Sys.argv.(2) in
  let doi = Sys.argv.(3) in
  let e = Bushel.load base in
  let zt = ZT.v "http://svr-avsm2-eeg-ce:1969" in
  Lwt_main.run @@ of_doi zt e ~slug doi
