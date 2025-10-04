open Cmdliner
open Printf

(** TODO:claude Generate bibtex entry from paper data *)
let generate_bibtex_entry paper =
  let open Bushel.Paper in
  (* Use slug as the bibtex key/label *)
  let bibkey = slug paper in
  let bibtype = try bibtype paper with _ -> "misc" in
  let title = try title paper with _ -> "Untitled" in
  let authors =
    let auth_list = try authors paper with _ -> [] in
    String.concat " and " auth_list
  in
  let year = try year paper with _ -> 0 in

  (* Build the bibtex entry *)
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (sprintf "@%s{%s,\n" bibtype bibkey);
  Buffer.add_string buf (sprintf "  title = {%s},\n" title);
  Buffer.add_string buf (sprintf "  author = {%s},\n" authors);
  Buffer.add_string buf (sprintf "  year = {%d}" year);

  (* Add optional fields *)
  (match String.lowercase_ascii bibtype with
  | "article" ->
    (try
      Buffer.add_string buf (sprintf ",\n  journal = {%s}" (journal paper))
    with _ -> ());
    (match volume paper with
    | Some v -> Buffer.add_string buf (sprintf ",\n  volume = {%s}" v)
    | None -> ());
    (match issue paper with
    | Some i -> Buffer.add_string buf (sprintf ",\n  number = {%s}" i)
    | None -> ());
    (match pages paper with
    | "" -> ()
    | p -> Buffer.add_string buf (sprintf ",\n  pages = {%s}" p))
  | "inproceedings" ->
    (try
      Buffer.add_string buf (sprintf ",\n  booktitle = {%s}" (booktitle paper))
    with _ -> ());
    (match pages paper with
    | "" -> ()
    | p -> Buffer.add_string buf (sprintf ",\n  pages = {%s}" p));
    (match publisher paper with
    | "" -> ()
    | p -> Buffer.add_string buf (sprintf ",\n  publisher = {%s}" p))
  | "techreport" ->
    (try
      Buffer.add_string buf (sprintf ",\n  institution = {%s}" (institution paper))
    with _ -> ());
    (match number paper with
    | Some n -> Buffer.add_string buf (sprintf ",\n  number = {%s}" n)
    | None -> ())
  | "book" ->
    (match publisher paper with
    | "" -> ()
    | p -> Buffer.add_string buf (sprintf ",\n  publisher = {%s}" p));
    (try
      Buffer.add_string buf (sprintf ",\n  isbn = {%s}" (isbn paper))
    with _ -> ())
  | _ -> ());

  (* Add DOI if available *)
  (match doi paper with
  | Some d -> Buffer.add_string buf (sprintf ",\n  doi = {%s}" d)
  | None -> ());

  (* Add URL if available *)
  (match url paper with
  | Some u -> Buffer.add_string buf (sprintf ",\n  url = {%s}" u)
  | None -> ());

  Buffer.add_string buf "\n}\n";
  Buffer.contents buf

(** TODO:claude Main function to export bibtex for all papers *)
let export_bibtex base_dir output_file latest_only =
  (* Load all papers *)
  let bushel = Bushel.load base_dir in
  let papers = Bushel.Entry.papers bushel in

  (* Filter to only latest versions if requested *)
  let papers =
    if latest_only then
      List.filter (fun p -> p.Bushel.Paper.latest) papers
    else
      papers
  in

  (* Sort papers by year (most recent first) *)
  let papers = List.sort Bushel.Paper.compare papers in

  (* Generate bibtex for each paper *)
  let bibtex_entries = List.map generate_bibtex_entry papers in
  let bibtex_content = String.concat "\n" bibtex_entries in

  (* Output to file or stdout *)
  match output_file with
  | None ->
    print_string bibtex_content;
    0
  | Some file ->
    let oc = open_out file in
    output_string oc bibtex_content;
    close_out oc;
    printf "Bibtex exported to %s (%d entries)\n" file (List.length papers);
    0

(** TODO:claude Command line arguments *)
let output_file_arg =
  let doc = "Output file for bibtex (defaults to stdout)" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)

let latest_only_arg =
  let doc = "Export only the latest version of each paper" in
  Arg.(value & flag & info ["latest"] ~doc)

(** TODO:claude Command term *)
let term =
  Term.(const export_bibtex $ Bushel_common.base_dir $ output_file_arg $ latest_only_arg)

let cmd =
  let doc = "Export bibtex for all papers" in
  let info = Cmd.info "bibtex" ~doc in
  Cmd.v info term