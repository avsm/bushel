open Cmdliner
open Bushel

(** Generate a roguedoi identifier using Crockford base32 encoding *)
let generate_roguedoi () =
  Random.self_init ();
  (* Generate a 10-character roguedoi with checksum and split every 5 chars *)
  let id = Crockford.generate ~length:10 ~split_every:5 ~checksum:true () in
  Printf.sprintf "10.59999/%s" id

(** Add DOI to a specific note's frontmatter if it doesn't already have one *)
let add_doi_to_note note_path =
  let content = In_channel.with_open_bin note_path In_channel.input_all in
  (* Check if note already has a doi: field *)
  let has_doi = try
    let _ = String.index content 'd' in
    let re = Str.regexp "^doi:" in
    let lines = String.split_on_char '\n' content in
    List.exists (fun line -> Str.string_match re (String.trim line) 0) lines
  with Not_found -> false
  in
  if has_doi then begin
    Fmt.pr "%a: Note already has a DOI, skipping@."
      (Fmt.styled `Yellow Fmt.string) note_path;
    false
  end else begin
    let roguedoi = generate_roguedoi () in
    (* Parse the file to extract frontmatter *)
    match String.split_on_char '\n' content with
    | "---" :: rest ->
        (* Find the end of frontmatter *)
        let rec find_end_fm acc = function
          | [] -> None
          | "---" :: body_lines -> Some (List.rev acc, body_lines)
          | line :: lines -> find_end_fm (line :: acc) lines
        in
        (match find_end_fm [] rest with
         | Some (fm_lines, body_lines) ->
             (* Add doi field to frontmatter *)
             let new_fm = fm_lines @ [Printf.sprintf "doi: %s" roguedoi] in
             let new_content =
               String.concat "\n" (["---"] @ new_fm @ ["---"] @ body_lines)
             in
             Out_channel.with_open_bin note_path (fun oc ->
               Out_channel.output_string oc new_content
             );
             Fmt.pr "%a: Added DOI %a@."
               (Fmt.styled `Green Fmt.string) note_path
               (Fmt.styled `Cyan Fmt.string) roguedoi;
             true
         | None ->
             Fmt.epr "%a: Could not parse frontmatter@."
               (Fmt.styled `Red Fmt.string) note_path;
             false)
    | _ ->
        Fmt.epr "%a: No frontmatter found@."
          (Fmt.styled `Red Fmt.string) note_path;
        false
  end

(** Main command implementation *)
let note_doi_cmd () base_dir dry_run =
  let entries = load base_dir in
  let notes = Entry.notes entries in

  (* Filter for perma notes without DOI *)
  let perma_notes = List.filter (fun n ->
    Note.perma n && Option.is_none (Note.doi n)
  ) notes in

  if perma_notes = [] then begin
    Fmt.pr "No permanent notes without DOI found.@.";
    0
  end else begin
    Fmt.pr "@[<v>";
    Fmt.pr "%a: Found %d permanent notes without DOI@.@."
      (Fmt.styled `Bold Fmt.string) "Info"
      (List.length perma_notes);

    let count = ref 0 in
    List.iter (fun note ->
      let slug = Note.slug note in
      let note_path = Printf.sprintf "%s/data/notes/%s.md" base_dir slug in
      Fmt.pr "Processing %a (%a)...@,"
        (Fmt.styled `Cyan Fmt.string) slug
        (Fmt.styled `Faint Fmt.string) (Note.title note);

      if not dry_run then begin
        if add_doi_to_note note_path then
          incr count
      end else begin
        let roguedoi = generate_roguedoi () in
        Fmt.pr "  Would add DOI: %a@,"
          (Fmt.styled `Cyan Fmt.string) roguedoi;
        incr count
      end
    ) perma_notes;

    Fmt.pr "@.";
    if dry_run then
      Fmt.pr "%a: Would add DOI to %d notes (dry run)@."
        (Fmt.styled `Bold Fmt.string) "Summary"
        !count
    else
      Fmt.pr "%a: Added DOI to %d notes@."
        (Fmt.styled `Bold Fmt.string) "Summary"
        !count;
    Fmt.pr "@]@.";
    0
  end

(** Command line interface definition *)
let dry_run_flag =
  let doc = "Show what would be done without making changes" in
  Arg.(value & flag & info ["n"; "dry-run"] ~doc)

let term =
  Term.(const note_doi_cmd $ Bushel_common.setup_term $ Bushel_common.base_dir $ dry_run_flag)

let cmd =
  let doc = "Generate and add DOI identifiers to permanent notes" in
  let man = [
    `S Manpage.s_description;
    `P "This command generates roguedoi identifiers using Crockford base32 encoding \
        and adds them to the frontmatter of permanent notes (notes with perma: true) \
        that don't already have a DOI.";
    `P "Roguedoi format: 10.59999/xxxxx-xxxxx where x is a Crockford base32 character.";
    `S Manpage.s_options;
  ] in
  let info = Cmd.info "note-doi" ~doc ~man in
  Cmd.v info term
