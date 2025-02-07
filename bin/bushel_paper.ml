module ZT = Zotero_translation
open Lwt.Infix

let print_json j =
  print_endline (Ezjsonm.to_string j)

let of_doi zt doi =
  ZT.json_of_doi zt doi  >>= fun j ->
  print_json j;
  Lwt.return ()

let () =
  (* TODO cmdliner *)
  let doi = Sys.argv.(1) in
  let zt = ZT.v "http://svr-avsm2-eeg-ce:1969" in
  Lwt_main.run @@ of_doi zt doi
