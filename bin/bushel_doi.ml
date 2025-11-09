module ZT = Zotero_translation
open Lwt.Infix
module J = Ezjsonm
open Cmdliner

(* Extract all DOIs from notes by scanning for doi.org URLs *)
let extract_dois_from_notes notes =
  let doi_url_pattern = Re.Perl.compile_pat "https?://(?:dx\\.)?doi\\.org/([^)\\s\"'>]+)" in
  let dois = ref [] in
  List.iter (fun note ->
    let body = Bushel.Note.body note in
    let matches = Re.all doi_url_pattern body in
    List.iter (fun group ->
      try
        let encoded_doi = Re.Group.get group 1 in
        let doi = Uri.pct_decode encoded_doi in
        if not (List.mem doi !dois) then
          dois := doi :: !dois
      with _ -> ()
    ) matches
  ) notes;
  !dois

(* Resolve a single DOI via Zotero and convert to doi_entry *)
let resolve_doi zt doi =
  Printf.printf "Resolving DOI: %s\n%!" doi;
  Lwt.catch
    (fun () ->
      ZT.json_of_doi zt ~slug:"temp" doi >>= fun json ->
      try
        let keys = J.get_dict (json :> J.value) in
        let title = J.find json ["title"] |> J.get_string in
        let authors = J.find json ["author"] |> J.get_list J.get_string in
        let year = J.find json ["year"] |> J.get_string |> int_of_string in
        let bibtype = J.find json ["bibtype"] |> J.get_string in
        let publisher =
          try
            (* Try journal first, then booktitle, then publisher *)
            match List.assoc_opt "journal" keys with
            | Some j -> J.get_string j
            | None ->
              match List.assoc_opt "booktitle" keys with
              | Some b -> J.get_string b
              | None ->
                match List.assoc_opt "publisher" keys with
                | Some p -> J.get_string p
                | None -> ""
          with _ -> ""
        in
        let entry = Bushel.Doi_entry.create_resolved ~doi ~title ~authors ~year ~bibtype ~publisher in
        Printf.printf "  ✓ Resolved: %s (%d)\n%!" title year;
        Lwt.return entry
      with e ->
        Printf.eprintf "  ✗ Failed to parse response for %s: %s\n%!" doi (Printexc.to_string e);
        Lwt.return (Bushel.Doi_entry.create_failed ~doi ~error:(Printexc.to_string e))
    )
    (fun exn ->
      Printf.eprintf "  ✗ Failed to resolve %s: %s\n%!" doi (Printexc.to_string exn);
      Lwt.return (Bushel.Doi_entry.create_failed ~doi ~error:(Printexc.to_string exn))
    )

let run base_dir force =
  Printf.printf "Loading bushel database...\n%!";
  let entries = Bushel.load base_dir in
  let notes = Bushel.Entry.notes entries in

  Printf.printf "Scanning %d notes for DOI URLs...\n%!" (List.length notes);
  let found_dois = extract_dois_from_notes notes in
  Printf.printf "Found %d unique DOIs\n%!" (List.length found_dois);

  let data_dir = Bushel.Entry.data_dir entries in
  let doi_yml_path = Filename.concat data_dir "doi.yml" in
  Printf.printf "Loading existing DOI cache from %s...\n%!" doi_yml_path;
  let existing_entries = Bushel.Doi_entry.load doi_yml_path in
  Printf.printf "Loaded %d cached DOI entries\n%!" (List.length existing_entries);

  (* Filter DOIs that need resolution *)
  let dois_to_resolve =
    List.filter (fun doi ->
      match Bushel.Doi_entry.find_by_doi existing_entries doi with
      | Some _ when not force ->
        Printf.printf "Skipping %s (already cached)\n%!" doi;
        false
      | Some _ when force ->
        Printf.printf "Re-resolving %s (--force)\n%!" doi;
        true
      | Some _ -> false  (* Catch-all for Some case *)
      | None -> true
    ) found_dois
  in

  if List.length dois_to_resolve = 0 then begin
    Printf.printf "No DOIs to resolve!\n%!";
    0
  end else begin
    Printf.printf "Resolving %d DOI(s)...\n%!" (List.length dois_to_resolve);

    let zt = ZT.v "http://svr-avsm2-eeg-ce:1969" in

    (* Resolve all DOIs *)
    let resolved_entries_lwt =
      Lwt_list.map_s (resolve_doi zt) dois_to_resolve
    in

    let new_entries = Lwt_main.run resolved_entries_lwt in

    (* Merge with existing entries *)
    let all_entries =
      if force then
        (* Replace existing entries with new ones *)
        let updated_dois = List.map (fun e -> e.Bushel.Doi_entry.doi) new_entries in
        let kept_existing = List.filter (fun e -> not (List.mem e.Bushel.Doi_entry.doi updated_dois)) existing_entries in
        kept_existing @ new_entries
      else
        existing_entries @ new_entries
    in

    (* Save updated cache *)
    Printf.printf "Saving %d total entries to %s...\n%!" (List.length all_entries) doi_yml_path;
    Bushel.Doi_entry.save doi_yml_path all_entries;

    Printf.printf "Done!\n%!";
    0
  end

let force_flag =
  let doc = "Force re-resolution of already cached DOIs" in
  Arg.(value & flag & info ["force"; "f"] ~doc)

let term =
  Term.(const run $ Bushel_common.base_dir $ force_flag)

let cmd =
  let doc = "Resolve DOIs found in notes via Zotero Translation Server" in
  let info = Cmd.info "doi-resolve" ~doc in
  Cmd.v info term
