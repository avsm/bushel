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

(* Extract publisher URLs from notes (Elsevier, ScienceDirect, IEEE, Nature, ACM, Sage, UPenn, Springer, Taylor & Francis) *)
let extract_publisher_urls_from_notes notes =
  (* Matches publisher URLs: linkinghub.elsevier.com, sciencedirect.com/science/article, ieeexplore.ieee.org, nature.com, journals.sagepub.com, garfield.library.upenn.edu, link.springer.com, tandfonline.com/doi, and dl.acm.org/doi/10.* URLs *)
  let publisher_pattern = Re.Perl.compile_pat "https?://(?:(?:www\\.)?(?:linkinghub\\.elsevier\\.com|(?:www\\.)?xsciencedirect\\.com/science/article|ieeexplore\\.ieee\\.org|nature\\.com|journals\\.sagepub\\.com|garfield\\.library\\.upenn\\.edu|link\\.springer\\.com)/[^)\\s\"'>]+|(?:dl\\.acm\\.org|(?:www\\.)?tandfonline\\.com)/doi(?:/pdf)?/10\\.[^)\\s\"'>]+)" in
  let urls = ref [] in
  List.iter (fun note ->
    let body = Bushel.Note.body note in
    let matches = Re.all publisher_pattern body in
    List.iter (fun group ->
      try
        let url = Re.Group.get group 0 in
        if not (List.mem url !urls) then
          urls := url :: !urls
      with _ -> ()
    ) matches
  ) notes;
  !urls

(* Resolve a single DOI via Zotero and convert to doi_entry *)
let resolve_doi zt ~verbose doi =
  Printf.printf "Resolving DOI: %s\n%!" doi;
  let doi_url = Printf.sprintf "https://doi.org/%s" doi in
  Lwt.catch
    (fun () ->
      ZT.json_of_doi zt ~slug:"temp" doi >>= fun json ->
      if verbose then begin
        Printf.printf "  Raw Zotero response:\n%s\n%!" (J.value_to_string json)
      end;
      try
        let keys = J.get_dict (json :> J.value) in
        let title = J.find json ["title"] |> J.get_string in
        let authors = J.find json ["author"] |> J.get_list J.get_string in
        let year = J.find json ["year"] |> J.get_string |> int_of_string in
        let bibtype = J.find json ["bibtype"] |> J.get_string in
        let publisher =
          try
            (* Try journal first, then booktitle, then proceedingsTitle, then publisher *)
            match List.assoc_opt "journal" keys with
            | Some j -> J.get_string j
            | None ->
              match List.assoc_opt "booktitle" keys with
              | Some b -> J.get_string b
              | None ->
                match List.assoc_opt "proceedingsTitle" keys with
                | Some pt -> J.get_string pt
                | None ->
                  match List.assoc_opt "publisher" keys with
                  | Some p -> J.get_string p
                  | None -> ""
          with _ -> ""
        in
        let entry = Bushel.Doi_entry.create_resolved ~doi ~title ~authors ~year ~bibtype ~publisher ~source_urls:[doi_url] () in
        Printf.printf "  ✓ Resolved: %s (%d)\n%!" title year;
        Lwt.return entry
      with e ->
        Printf.eprintf "  ✗ Failed to parse response for %s: %s\n%!" doi (Printexc.to_string e);
        Lwt.return (Bushel.Doi_entry.create_failed ~doi ~error:(Printexc.to_string e) ~source_urls:[doi_url] ())
    )
    (fun exn ->
      Printf.eprintf "  ✗ Failed to resolve %s: %s\n%!" doi (Printexc.to_string exn);
      Lwt.return (Bushel.Doi_entry.create_failed ~doi ~error:(Printexc.to_string exn) ~source_urls:[doi_url] ())
    )

(* Resolve a publisher URL via Zotero /web endpoint *)
let resolve_url zt ~verbose url =
  Printf.printf "Resolving URL: %s\n%!" url;
  Lwt.catch
    (fun () ->
      (* Use Zotero's resolve_url which calls /web endpoint with the URL directly *)
      ZT.resolve_url zt url >>= function
      | Error (`Msg err) ->
        Printf.eprintf "  ✗ Failed to resolve URL: %s\n%!" err;
        Lwt.return (Bushel.Doi_entry.create_failed ~doi:url ~error:err ~source_urls:[url] ())
      | Ok json ->
        if verbose then begin
          Printf.printf "  Raw Zotero response:\n%s\n%!" (J.value_to_string json)
        end;
        try
          (* Extract metadata from the JSON response *)
          let json_list = match json with
            | `A lst -> lst
            | single -> [single]
          in
          match json_list with
          | [] ->
            Printf.eprintf "  ✗ Empty response\n%!";
            Lwt.return (Bushel.Doi_entry.create_failed ~doi:url ~error:"Empty response" ~source_urls:[url] ())
          | item :: _ ->
            (* Extract DOI if present, otherwise use URL *)
            let doi = try J.find item ["DOI"] |> J.get_string with _ ->
              try J.find item ["doi"] |> J.get_string with _ -> url
            in
            let title = try J.find item ["title"] |> J.get_string with _ ->
              "Unknown Title"
            in
            (* Extract authors from Zotero's "creators" field *)
            let authors = try
              J.find item ["creators"] |> J.get_list (fun creator_obj ->
                try
                  let last_name = J.find creator_obj ["lastName"] |> J.get_string in
                  let first_name = try J.find creator_obj ["firstName"] |> J.get_string with _ -> "" in
                  if first_name = "" then last_name else first_name ^ " " ^ last_name
                with _ -> "Unknown Author"
              )
            with _ -> []
            in
            (* Extract year from Zotero's "date" field *)
            (* Handles both ISO format "2025-07" and text format "November 28, 2023" *)
            let year = try
              let date_str = J.find item ["date"] |> J.get_string in
              (* First try splitting on '-' for ISO dates like "2025-07" or "2024-11-04" *)
              let parts = String.split_on_char '-' date_str in
              match parts with
              | year_str :: _ when String.length year_str = 4 ->
                (try int_of_string year_str with _ -> 0)
              | _ ->
                (* Try splitting on space and comma for dates like "November 28, 2023" *)
                let space_parts = String.split_on_char ' ' date_str in
                let year_candidate = List.find_opt (fun s ->
                  let s = String.trim (String.map (fun c -> if c = ',' then ' ' else c) s) in
                  String.length s = 4 && String.for_all (function '0'..'9' -> true | _ -> false) s
                ) space_parts in
                (match year_candidate with
                 | Some year_str -> int_of_string (String.trim year_str)
                 | None -> 0)
            with _ -> 0
            in
            (* Extract type/bibtype from Zotero's "itemType" field *)
            let bibtype = try J.find item ["itemType"] |> J.get_string with _ -> "article" in
            (* Extract publisher/journal from Zotero's "publicationTitle" or "proceedingsTitle" field *)
            let publisher = try
              J.find item ["publicationTitle"] |> J.get_string
            with _ ->
              try J.find item ["proceedingsTitle"] |> J.get_string
              with _ -> ""
            in
            (* Include both the original URL and the DOI URL in source_urls *)
            let doi_url = if doi = url then [] else [Printf.sprintf "https://doi.org/%s" doi] in
            let source_urls = url :: doi_url in
            let entry = Bushel.Doi_entry.create_resolved ~doi ~title ~authors ~year ~bibtype ~publisher ~source_urls () in
            Printf.printf "  ✓ Resolved: %s (%d) [DOI: %s]\n%!" title year doi;
            Lwt.return entry
        with e ->
          Printf.eprintf "  ✗ Failed to parse response: %s\n%!" (Printexc.to_string e);
          Lwt.return (Bushel.Doi_entry.create_failed ~doi:url ~error:(Printexc.to_string e) ~source_urls:[url] ())
    )
    (fun exn ->
      Printf.eprintf "  ✗ Failed to resolve %s: %s\n%!" url (Printexc.to_string exn);
      Lwt.return (Bushel.Doi_entry.create_failed ~doi:url ~error:(Printexc.to_string exn) ~source_urls:[url] ())
    )

let run base_dir force verbose =
  Printf.printf "Loading bushel database...\n%!";
  let entries = Bushel.load base_dir in
  let notes = Bushel.Entry.notes entries in

  Printf.printf "Scanning %d notes for DOI URLs...\n%!" (List.length notes);
  let found_dois = extract_dois_from_notes notes in
  Printf.printf "Found %d unique DOIs\n%!" (List.length found_dois);

  Printf.printf "Scanning %d notes for publisher URLs...\n%!" (List.length notes);
  let found_urls = extract_publisher_urls_from_notes notes in
  Printf.printf "Found %d unique publisher URLs\n%!" (List.length found_urls);

  let data_dir = Bushel.Entry.data_dir entries in
  let doi_yml_path = Filename.concat data_dir "doi.yml" in
  Printf.printf "Loading existing DOI cache from %s...\n%!" doi_yml_path;
  let existing_entries = Bushel.Doi_entry.load doi_yml_path in
  Printf.printf "Loaded %d cached DOI entries\n%!" (List.length existing_entries);

  (* Filter DOIs that need resolution *)
  let dois_to_resolve =
    List.filter (fun doi ->
      match Bushel.Doi_entry.find_by_doi_including_ignored existing_entries doi with
      | Some _ when not force ->
        Printf.printf "Skipping DOI %s (already cached)\n%!" doi;
        false
      | Some _ when force ->
        Printf.printf "Re-resolving DOI %s (--force)\n%!" doi;
        true
      | Some _ -> false  (* Catch-all for Some case *)
      | None -> true
    ) found_dois
  in

  (* Filter URLs that need resolution *)
  let urls_to_resolve =
    List.filter (fun url ->
      match Bushel.Doi_entry.find_by_url_including_ignored existing_entries url with
      | Some _ when not force ->
        Printf.printf "Skipping URL %s (already cached)\n%!" url;
        false
      | Some _ when force ->
        Printf.printf "Re-resolving URL %s (--force)\n%!" url;
        true
      | Some _ -> false  (* Catch-all for Some case *)
      | None -> true
    ) found_urls
  in

  if List.length dois_to_resolve = 0 && List.length urls_to_resolve = 0 then begin
    Printf.printf "No DOIs or URLs to resolve!\n%!";
    0
  end else begin
    Printf.printf "Resolving %d DOI(s) and %d URL(s)...\n%!" (List.length dois_to_resolve) (List.length urls_to_resolve);

    let zt = ZT.v "http://svr-avsm2-eeg-ce:1969" in

    (* Resolve all DOIs *)
    let resolved_doi_entries_lwt =
      Lwt_list.map_s (resolve_doi zt ~verbose) dois_to_resolve
    in

    (* Resolve all publisher URLs *)
    let resolved_url_entries_lwt =
      Lwt_list.map_s (resolve_url zt ~verbose) urls_to_resolve
    in

    let new_doi_entries = Lwt_main.run resolved_doi_entries_lwt in
    let new_url_entries = Lwt_main.run resolved_url_entries_lwt in
    let new_entries = new_doi_entries @ new_url_entries in

    (* Merge with existing entries, combining source_urls for entries with the same DOI *)
    let all_entries =
      if force then
        (* Replace existing entries with new ones - match by DOI *)
        let is_updated entry =
          List.exists (fun new_e ->
            new_e.Bushel.Doi_entry.doi = entry.Bushel.Doi_entry.doi
          ) new_entries
        in
        let kept_existing = List.filter (fun e -> not (is_updated e)) existing_entries in
        kept_existing @ new_entries
      else
        (* Merge new entries with existing ones, combining source_urls *)
        let merged = ref existing_entries in
        List.iter (fun new_entry ->
          match Bushel.Doi_entry.find_by_doi_including_ignored !merged new_entry.Bushel.Doi_entry.doi with
          | Some existing_entry ->
            (* DOI already exists - merge the entries by combining source_urls and preserving ignore flag *)
            let combined = Bushel.Doi_entry.merge_entries existing_entry new_entry in
            merged := combined :: (List.filter (fun e -> e.Bushel.Doi_entry.doi <> new_entry.Bushel.Doi_entry.doi) !merged)
          | None ->
            (* New DOI - add it *)
            merged := new_entry :: !merged
        ) new_entries;
        !merged
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

let verbose_flag =
  let doc = "Show raw Zotero API responses for debugging" in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

let term =
  Term.(const run $ Bushel_common.base_dir $ force_flag $ verbose_flag)

let cmd =
  let doc = "Resolve DOIs found in notes via Zotero Translation Server" in
  let info = Cmd.info "doi-resolve" ~doc in
  Cmd.v info term
