open Cmdliner
open Bushel

(** Check if an entry has a thumbnail *)
let has_thumbnail entries entry =
  match Entry.thumbnail_slug entries entry with
  | Some _ -> true
  | None -> false

(** Check if an entry has a synopsis or description *)
let has_synopsis = function
  | `Paper p -> Paper.abstract p <> ""  (* Papers have abstracts *)
  | `Note n -> Note.synopsis n <> None  (* Notes have optional synopsis *)
  | `Idea _ -> true  (* Ideas don't have synopsis field *)
  | `Project _ -> true  (* Projects don't have synopsis field *)
  | `Video _ -> true  (* Videos don't have synopsis field *)

(** Check if an entry has tags *)
let has_tags = function
  | `Paper p -> Paper.tags p <> []
  | `Note n -> Note.tags n <> []
  | `Idea i -> i.Idea.tags <> []  (* Access record field directly *)
  | `Project p -> Project.tags p <> []
  | `Video v -> v.Video.tags <> []  (* Access record field directly *)

(** Entry with broken references *)
type entry_with_broken_refs = {
  entry : Entry.entry;
  broken_slugs : string list;
  broken_contacts : string list;
}

(** Find entries missing thumbnails *)
let find_missing_thumbnails entries =
  let all = Entry.all_entries entries in
  List.filter (fun entry -> not (has_thumbnail entries entry)) all

(** Find entries missing synopsis *)
let find_missing_synopsis entries =
  let all = Entry.all_entries entries in
  List.filter (fun entry -> not (has_synopsis entry)) all

(** Find entries missing tags *)
let find_missing_tags entries =
  let all = Entry.all_entries entries in
  List.filter (fun entry -> not (has_tags entry)) all

(** Find entries with broken slugs or contact handles *)
let find_broken_references entries =
  let all = Entry.all_entries entries in
  List.filter_map (fun entry ->
    let body = Entry.body entry in
    let broken_slugs, broken_contacts = Md.validate_references entries body in
    if broken_slugs <> [] || broken_contacts <> [] then
      Some { entry; broken_slugs; broken_contacts }
    else
      None
  ) all

(** Print a list of entries *)
let print_entries title entries_list =
  if entries_list <> [] then begin
    Fmt.pr "@.%a (%d):@," (Fmt.styled `Bold Fmt.string) title (List.length entries_list);
    List.iter (fun entry ->
      let slug = Entry.slug entry in
      let type_str = Entry.to_type_string entry in
      let title = Entry.title entry in
      Fmt.pr "  %a %a - %a@,"
        (Fmt.styled `Cyan Fmt.string) slug
        (Fmt.styled `Faint Fmt.string) (Printf.sprintf "(%s)" type_str)
        Fmt.string title
    ) entries_list
  end

(** Print entries with broken references *)
let print_broken_references title entries_with_broken_refs =
  if entries_with_broken_refs <> [] then begin
    Fmt.pr "@.%a (%d):@," (Fmt.styled `Bold Fmt.string) title (List.length entries_with_broken_refs);
    List.iter (fun { entry; broken_slugs; broken_contacts } ->
      let slug = Entry.slug entry in
      let type_str = Entry.to_type_string entry in
      let entry_title = Entry.title entry in
      Fmt.pr "  %a %a - %a@,"
        (Fmt.styled `Cyan Fmt.string) slug
        (Fmt.styled `Faint Fmt.string) (Printf.sprintf "(%s)" type_str)
        Fmt.string entry_title;
      if broken_slugs <> [] then
        Fmt.pr "    %a %a@,"
          (Fmt.styled `Red Fmt.string) "Broken slugs:"
          (Fmt.list ~sep:Fmt.comma Fmt.string) broken_slugs;
      if broken_contacts <> [] then
        Fmt.pr "    %a %a@,"
          (Fmt.styled `Red Fmt.string) "Broken contacts:"
          (Fmt.list ~sep:Fmt.comma Fmt.string) broken_contacts;
    ) entries_with_broken_refs
  end

(** Main missing command implementation *)
let missing_cmd () base_dir check_thumbnails check_synopsis check_tags check_refs =
  let entries = load base_dir in

  let count = ref 0 in

  if check_thumbnails then begin
    let missing = find_missing_thumbnails entries in
    print_entries "Entries missing thumbnails" missing;
    count := !count + List.length missing
  end;

  if check_synopsis then begin
    let missing = find_missing_synopsis entries in
    print_entries "Entries missing synopsis" missing;
    count := !count + List.length missing
  end;

  if check_tags then begin
    let missing = find_missing_tags entries in
    print_entries "Entries missing tags" missing;
    count := !count + List.length missing
  end;

  if check_refs then begin
    let broken = find_broken_references entries in
    print_broken_references "Entries with broken references" broken;
    (* Count total number of broken references, not just entries *)
    let broken_count = List.fold_left (fun acc { broken_slugs; broken_contacts; _ } ->
      acc + List.length broken_slugs + List.length broken_contacts
    ) 0 broken in
    count := !count + broken_count
  end;

  if !count = 0 then
    Fmt.pr "@.No missing metadata or broken references found.@."
  else
    Fmt.pr "@.Total issues found: %d@." !count;

  0

(** Command line arguments *)
let thumbnails_flag =
  let doc = "Check for entries missing thumbnails" in
  Arg.(value & flag & info ["thumbnails"; "t"] ~doc)

let synopsis_flag =
  let doc = "Check for entries missing synopsis" in
  Arg.(value & flag & info ["synopsis"; "s"] ~doc)

let tags_flag =
  let doc = "Check for entries missing tags" in
  Arg.(value & flag & info ["tags"; "g"] ~doc)

let refs_flag =
  let doc = "Check for broken slugs and contact handles" in
  Arg.(value & flag & info ["refs"; "r"] ~doc)

let term =
  Term.(const (fun setup base thumbnails synopsis tags refs ->
    (* If no flags specified, check everything *)
    let check_all = not (thumbnails || synopsis || tags || refs) in
    missing_cmd setup base
      (check_all || thumbnails)
      (check_all || synopsis)
      (check_all || tags)
      (check_all || refs)
  ) $ Bushel_common.setup_term $ Bushel_common.base_dir $ thumbnails_flag $ synopsis_flag $ tags_flag $ refs_flag)

let cmd =
  let doc = "List entries with missing metadata or broken references" in
  let man = [
    `S Manpage.s_description;
    `P "This command scans all entries and reports any that are missing thumbnails, synopsis, tags, or have broken slugs/contact handles.";
    `P "By default, all checks are performed. Use flags to select specific checks.";
    `S Manpage.s_options;
    `S Manpage.s_examples;
    `P "Check for all issues:";
    `Pre "  $(mname) $(tname)";
    `P "Check only for missing thumbnails:";
    `Pre "  $(mname) $(tname) --thumbnails";
    `P "Check for missing synopsis and tags:";
    `Pre "  $(mname) $(tname) --synopsis --tags";
    `P "Check only for broken references:";
    `Pre "  $(mname) $(tname) --refs";
  ] in
  let info = Cmd.info "missing" ~doc ~man in
  Cmd.v info term
