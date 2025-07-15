open Cmdliner
open Lwt.Infix

(* Helper function for logging with proper flushing *)
let log fmt = Fmt.kstr (fun s -> prerr_string s; flush stderr) fmt
let log_verbose verbose fmt = 
  if verbose then Fmt.kstr (fun s -> prerr_string s; flush stderr) fmt 
  else Fmt.kstr (fun _ -> ()) fmt

(* Initialize a new links.yml file or ensure it exists *)
let init_links_file links_file =
  if Sys.file_exists links_file then
    print_endline (Fmt.str "Links file %s already exists" links_file)
  else begin
    (* Create an empty links file *)
    Bushel.Link.save_links_file links_file [];
    print_endline (Fmt.str "Created empty links file: %s" links_file)
  end;
  0

(* Update links.yml from Karakeep *)
let update_from_karakeep base_url api_key_opt tag links_file download_assets =
  match api_key_opt with
  | None ->
      prerr_endline "Error: API key is required.";
      prerr_endline "Please provide one with --api-key or create a ~/.karakeep-api file.";
      1
  | Some api_key ->
      let assets_dir = "data/assets" in
      
      (* Run the Lwt program *)
      Lwt_main.run (
        print_endline (Fmt.str "Fetching links from %s with tag '%s'..." base_url tag);
        
        (* Prepare tag filter *)
        let filter_tags = if tag = "" then [] else [tag] in
        
        (* Fetch bookmarks from Karakeep with error handling *)
        Lwt.catch
          (fun () ->
            Karakeep.fetch_all_bookmarks ~api_key ~filter_tags base_url >>= fun bookmarks ->
            
            print_endline (Fmt.str "Retrieved %d bookmarks from Karakeep" (List.length bookmarks));
            
            (* Read existing links if file exists *)
            let existing_links = Bushel.Link.load_links_file links_file in
            
            (* Convert bookmarks to bushel links *)
            let new_links = List.map (fun bookmark ->
              Karakeep.to_bushel_link ~base_url bookmark
            ) bookmarks in
            
            (* Merge with existing links - keep existing dates (karakeep dates may be unreliable) *)
            let merged_links = Bushel.Link.merge_links existing_links new_links in
            
            (* Save the updated links file *)
            Bushel.Link.save_links_file links_file merged_links;
            
            print_endline (Fmt.str "Updated %s with %d links" links_file (List.length merged_links));
            
            (* Download assets if requested *)
            if download_assets then begin
              print_endline "Downloading assets for bookmarks...";
              
              (* Ensure the assets directory exists *)
              (try Unix.mkdir assets_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
              
              (* Process each bookmark with assets *)
              Lwt_list.iter_s (fun bookmark ->
                (* Extract asset IDs from bookmark *)
                let assets = bookmark.Karakeep.assets in
                
                (* Skip if no assets *)
                if assets = [] then
                  Lwt.return_unit
                else
                  (* Process each asset *)
                  Lwt_list.iter_s (fun (asset_id, asset_type) ->
                    let asset_dir = Fmt.str "%s/%s" assets_dir asset_id in
                    let asset_file = Fmt.str "%s/asset.bin" asset_dir in
                    let meta_file = Fmt.str "%s/metadata.json" asset_dir in
                    
                    (* Skip if the asset already exists *)
                    if Sys.file_exists asset_file then
                      Lwt.return_unit
                    else begin
                      (* Create the asset directory *)
                      (try Unix.mkdir asset_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
                      
                      (* Download the asset *)
                      print_endline (Fmt.str "Downloading %s asset %s..." asset_type asset_id);
                      Karakeep.fetch_asset ~api_key base_url asset_id >>= fun data ->
                      
                      (* Guess content type based on first bytes *)
                      let content_type = 
                        if String.length data >= 4 && String.sub data 0 4 = "\x89PNG" then
                          "image/png"
                        else if String.length data >= 3 && String.sub data 0 3 = "\xFF\xD8\xFF" then
                          "image/jpeg"
                        else if String.length data >= 4 && String.sub data 0 4 = "%PDF" then
                          "application/pdf"
                        else
                          "application/octet-stream"
                      in
                      
                      (* Write the asset data *)
                      Lwt_io.with_file ~mode:Lwt_io.Output asset_file (fun oc ->
                        Lwt_io.write oc data
                      ) >>= fun () ->
                      
                      (* Write metadata file *)
                      let metadata = Fmt.str "{\n  \"contentType\": \"%s\",\n  \"assetType\": \"%s\"\n}" 
                        content_type asset_type in
                      Lwt_io.with_file ~mode:Lwt_io.Output meta_file (fun oc ->
                        Lwt_io.write oc metadata
                      )
                    end
                  ) assets
              ) bookmarks >>= fun () ->
              
              print_endline "Asset download completed.";
              Lwt.return 0
            end else
              Lwt.return 0
          )
          (fun exn ->
            prerr_endline (Fmt.str "Error fetching bookmarks: %s" (Printexc.to_string exn));
            Lwt.return 1
          )
      )

(* Extract outgoing links from Bushel entries *)
let update_from_bushel bushel_dir links_file include_domains exclude_domains =
  (* Parse domain filters if provided *)
  let include_domains_list = match include_domains with
    | None -> []
    | Some s -> String.split_on_char ',' s |> List.map String.trim
  in
  
  let exclude_domains_list = match exclude_domains with
    | None -> []
    | Some s -> String.split_on_char ',' s |> List.map String.trim
  in
  
  (* Show filter settings if any *)
  if include_domains_list <> [] then
    print_endline (Fmt.str "Including only domains: %s" (String.concat ", " include_domains_list));
  
  if exclude_domains_list <> [] then
    print_endline (Fmt.str "Excluding domains: %s" (String.concat ", " exclude_domains_list));
  
  (* Load all entries from the bushel directory *)
  let notes_dir = Filename.concat bushel_dir "data/notes" in
  
  (* Make sure the notes directory exists *)
  if not (Sys.file_exists notes_dir) then begin
    prerr_endline (Fmt.str "Error: Notes directory %s does not exist" notes_dir);
    exit 1
  end;
  
  (* Load all entries with fallback *)
  print_endline (Fmt.str "Loading entries from %s..." bushel_dir);
  
  let entries_data = Bushel.load bushel_dir in
  let all_entries = Bushel.Entry.all_entries entries_data in
  print_endline (Fmt.str "Loaded %d entries" (List.length all_entries));
  
  (* Extract outgoing links from all entries *)
  print_endline "Extracting outgoing links...";
  let extracted_links = ref [] in
  
  (* Process each entry *)
  List.iter (fun entry ->
    let entry_body = Bushel.Entry.body entry in
    let entry_slug = Bushel.Entry.slug entry in
    
    (* Skip empty bodies *)
    if entry_body <> "" then begin
      let links = Bushel.Entry.extract_external_links entry_body in
      if links <> [] then begin
        (* Add each link from this entry *)
        List.iter (fun url ->
          (* Try to extract domain from URL *)
          let domain = 
            try
              let uri = Uri.of_string url in
              match Uri.host uri with
              | Some host -> host
              | None -> "unknown"
            with _ -> "unknown"
          in
          
          (* Filter by domain if filters are specified *)
          let include_by_domain =
            if include_domains_list <> [] then
              List.exists (fun filter -> 
                domain = filter || String.ends_with ~suffix:filter domain
              ) include_domains_list
            else true
          in
          
          let exclude_by_domain =
            List.exists (fun filter -> 
              domain = filter || String.ends_with ~suffix:filter domain
            ) exclude_domains_list
          in
          
          if include_by_domain && not exclude_by_domain then begin
            let date = Bushel.Entry.date entry in
            
            (* Extract tags from the entry *)
            let entry_tags = Bushel.Tags.tags_of_ent entries_data entry in
            let tag_strings = List.map Bushel.Tags.to_string entry_tags in
            
            let link = { 
              Bushel.Link.url; 
              date; 
              description = ""; 
              karakeep = None;
              bushel = Some { 
                Bushel.Link.slugs = [entry_slug]; 
                tags = tag_strings 
              };
            } in
            extracted_links := link :: !extracted_links
          end
        ) links
      end
    end
  ) all_entries;
  
  (* Load existing links *)
  let existing_links = Bushel.Link.load_links_file links_file in
  
  (* Merge with existing links - prefer bushel entry dates *)
  let merged_links = Bushel.Link.merge_links ~prefer_new_date:true existing_links !extracted_links in
  
  (* Save the updated links file *)
  Bushel.Link.save_links_file links_file merged_links;
  
  print_endline (Fmt.str "Added %d extracted links from Bushel to %s" 
    (List.length !extracted_links) links_file);
  print_endline (Fmt.str "Total links in file: %d" (List.length merged_links));
  0

(* Helper function to filter links that don't have karakeep data for a specific remote *)
let filter_links_without_karakeep base_url links =
  List.filter (fun link ->
    match link.Bushel.Link.karakeep with
    | Some { remote_url; _ } when remote_url = base_url -> false
    | _ -> true
  ) links

(* Helper function to apply limit to links if specified *)
let apply_limit_to_links limit links =
  match limit with
  | Some n when n > 0 -> 
      let rec take_n acc count = function
        | [] -> List.rev acc
        | _ when count = 0 -> List.rev acc
        | x :: xs -> take_n (x :: acc) (count - 1) xs
      in
      let limited = take_n [] n links in
      if List.length links > n then
        log "Limited to first %d links (out of %d available)\n" n (List.length links);
      limited
  | _ -> links

(* Helper function to prepare tags for a link *)
let prepare_tags_for_link tag link =
  let slug_tags = 
    match link.Bushel.Link.bushel with
    | Some { slugs; _ } -> List.map (fun slug -> "bushel:" ^ slug) slugs
    | None -> []
  in
  if tag = "" then slug_tags
  else tag :: slug_tags

(* Helper function to create batches for parallel processing *)
let create_batches max_concurrent links =
  let rec create_batches_aux links acc =
    match links with
    | [] -> List.rev acc
    | _ ->
        let batch, rest = 
          if List.length links <= max_concurrent then
            links, []
          else
            let rec take n lst batch =
              if n = 0 || lst = [] then List.rev batch, lst
              else take (n-1) (List.tl lst) (List.hd lst :: batch)
            in
            take max_concurrent links []
        in
        create_batches_aux rest (batch :: acc)
  in
  create_batches_aux links []

(* Helper function to upload a single link to Karakeep *)
let upload_single_link api_key base_url tag verbose updated_links link =
  let url = link.Bushel.Link.url in
  let title = 
    if link.Bushel.Link.description <> "" then 
      Some link.Bushel.Link.description 
    else None 
  in
  let tags = prepare_tags_for_link tag link in
  
  if verbose then begin
    log "  Uploading: %s\n" url;
    if tags <> [] then 
      log "    Tags: %s\n" (String.concat ", " tags);
    if title <> None then 
      log "    Title: %s\n" (Option.get title);
  end else begin
    log "Uploading: %s\n" url;
  end;
  
  (* Create the bookmark with tags *)
  Lwt.catch
    (fun () ->
      Karakeep.create_bookmark 
        ~api_key 
        ~url 
        ?title 
        ~tags 
        base_url 
      >>= fun bookmark ->
      
      (* Create updated link with karakeep data *)
      let updated_link = {
        link with 
        Bushel.Link.karakeep = 
          Some { 
            Bushel.Link.remote_url = base_url; 
            id = bookmark.id;
            tags = bookmark.tags;
            metadata = [];  (* Will be populated on next sync *)
          }
      } in
      updated_links := updated_link :: !updated_links;
      
      if verbose then
        log "    ✓ Added to Karakeep with ID: %s\n" bookmark.id
      else
        log "  - Added to Karakeep with ID: %s\n" bookmark.id;
      Lwt.return 1 (* Success *)
    )
    (fun exn ->
      if verbose then
        log "    ✗ Error uploading %s: %s\n" url (Printexc.to_string exn)
      else
        log "  - Error uploading %s: %s\n" url (Printexc.to_string exn);
      Lwt.return 0 (* Failure *)
    )

(* Helper function to process a batch of links *)
let process_batch api_key base_url tag verbose updated_links batch_num total_batches batch =
  log_verbose verbose "\nProcessing batch %d/%d (%d links)...\n" 
    (batch_num + 1) total_batches (List.length batch);
  
  (* Process links in this batch concurrently *)
  Lwt_list.map_p (upload_single_link api_key base_url tag verbose updated_links) batch

(* Helper function to update links file with new karakeep data *)
let update_links_file links_file original_links updated_links =
  if !updated_links <> [] then begin
    (* Replace the updated links in the original list *)
    let final_links =
      List.map (fun link ->
        let url = link.Bushel.Link.url in
        let updated = List.find_opt (fun ul -> ul.Bushel.Link.url = url) !updated_links in
        match updated with
        | Some ul -> ul
        | None -> link
      ) original_links
    in
    
    (* Save the updated links file *)
    Bushel.Link.save_links_file links_file final_links;
    
    log "Updated %s with %d new karakeep_ids\n" 
      links_file (List.length !updated_links);
  end

(* Upload links to Karakeep that don't already have karakeep data *)
let upload_to_karakeep base_url api_key_opt links_file tag max_concurrent delay_seconds limit verbose =
  match api_key_opt with
  | None ->
      log "Error: API key is required.\n";
      log "Please provide one with --api-key or create a ~/.karakeep-api file.\n";
      1
  | Some api_key ->
      (* Load links from file *)
      log_verbose verbose "Loading links from %s...\n" links_file;
      let links = Bushel.Link.load_links_file links_file in
      log_verbose verbose "Loaded %d total links\n" (List.length links);
      
      (* Filter links that don't have karakeep data for this remote *)
      log_verbose verbose "Filtering links that don't have karakeep data for %s...\n" base_url;
      let filtered_links = filter_links_without_karakeep base_url links in
      log_verbose verbose "Found %d links without karakeep data\n" (List.length filtered_links);
      
      (* Apply limit if specified *)
      let links_to_upload = apply_limit_to_links limit filtered_links in
  
      if links_to_upload = [] then begin
        log "No links to upload to %s (all links already have karakeep data)\n" base_url;
        0
      end else begin
        log "Found %d links to upload to %s\n" (List.length links_to_upload) base_url;
        
        (* Split links into batches for parallel processing *)
        let batches = create_batches max_concurrent links_to_upload in
        log_verbose verbose "Processing in %d batches of up to %d links each...\n" 
          (List.length batches) max_concurrent;
        log_verbose verbose "Delay between batches: %.1f seconds\n" delay_seconds;
        
        (* Process batches and accumulate updated links *)
        let updated_links = ref [] in
        
        let result = Lwt_main.run (
          Lwt.catch
            (fun () ->
              Lwt_list.fold_left_s (fun (total_count, batch_num) batch ->
                process_batch api_key base_url tag verbose updated_links 
                  batch_num (List.length batches) batch >>= fun results ->
                
                (* Count successes in this batch *)
                let batch_successes = List.fold_left (+) 0 results in
                let new_total = total_count + batch_successes in
                
                log_verbose verbose "  Batch %d complete: %d/%d successful (Total: %d/%d)\n" 
                  (batch_num + 1) batch_successes (List.length batch) new_total (new_total + (List.length links_to_upload - new_total));
                
                (* Add a delay before processing the next batch *)
                if batch_num + 1 < List.length batches then begin
                  log_verbose verbose "  Waiting %.1f seconds before next batch...\n" delay_seconds;
                  Lwt_unix.sleep delay_seconds >>= fun () ->
                  Lwt.return (new_total, batch_num + 1)
                end else
                  Lwt.return (new_total, batch_num + 1)
              ) (0, 0) batches >>= fun (final_count, _) ->
              Lwt.return final_count
            )
            (fun exn ->
              log "Error during upload operation: %s\n" (Printexc.to_string exn);
              Lwt.return 0
            )
        ) in
        
        (* Update the links file with the new karakeep_ids *)
        update_links_file links_file links updated_links;
        
        log "Upload complete. %d/%d links uploaded successfully.\n" 
          result (List.length links_to_upload);
        
        0
      end

(* Common arguments *)
let links_file_arg =
  let doc = "Links YAML file. Defaults to links.yml." in
  Arg.(value & opt string "links.yml" & info ["file"; "f"] ~doc ~docv:"FILE")

let base_url_arg =
  let doc = "Base URL of the Karakeep instance" in
  let default = "https://hoard.recoil.org" in
  Arg.(value & opt string default & info ["url"] ~doc ~docv:"URL")

let api_key_arg =
  let doc = "API key for Karakeep authentication (ak1_<key_id>_<secret>)" in
  let get_api_key () =
    let home = try Sys.getenv "HOME" with Not_found -> "." in
    let key_path = Filename.concat home ".karakeep-api" in
    try
      let ic = open_in key_path in
      let key = input_line ic in
      close_in ic;
      Some (String.trim key)
    with _ -> None
  in
  Arg.(value & opt (some string) (get_api_key ()) & info ["api-key"] ~doc ~docv:"API_KEY")

let tag_arg =
  let doc = "Tag to filter or apply to bookmarks" in
  Arg.(value & opt string "" & info ["tag"; "t"] ~doc ~docv:"TAG")

let download_assets_arg =
  let doc = "Download assets (screenshots, etc.) from Karakeep" in
  Arg.(value & flag & info ["download-assets"; "d"] ~doc)

let base_dir_arg =
  let doc = "Base directory of the Bushel project" in
  Arg.(value & opt string "." & info ["dir"; "d"] ~doc ~docv:"DIR")

let include_domains_arg =
  let doc = "Only include links to these domains (comma-separated list)" in
  Arg.(value & opt (some string) None & info ["include"] ~doc ~docv:"DOMAINS")

let exclude_domains_arg =
  let doc = "Exclude links to these domains (comma-separated list)" in
  Arg.(value & opt (some string) None & info ["exclude"] ~doc ~docv:"DOMAINS")

let concurrent_arg =
  let doc = "Maximum number of concurrent uploads (default: 5)" in
  Arg.(value & opt int 5 & info ["concurrent"; "c"] ~doc ~docv:"NUM")

let delay_arg =
  let doc = "Delay in seconds between batches (default: 1.0)" in
  Arg.(value & opt float 1.0 & info ["delay"] ~doc ~docv:"SECONDS")

let limit_arg =
  let doc = "Limit number of links to upload (for testing)" in
  Arg.(value & opt (some int) None & info ["limit"; "l"] ~doc ~docv:"NUM")

let verbose_arg =
  let doc = "Show detailed progress information during upload" in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

(* Command definitions *)
let init_cmd =
  let doc = "Initialize a new links.yml file" in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(const init_links_file $ links_file_arg)

let karakeep_cmd =
  let doc = "Update links.yml with links from Karakeep" in
  let info = Cmd.info "karakeep" ~doc in
  Cmd.v info Term.(const update_from_karakeep $ base_url_arg $ api_key_arg $ tag_arg $ links_file_arg $ download_assets_arg)

let bushel_cmd =
  let doc = "Update links.yml with outgoing links from Bushel entries" in
  let info = Cmd.info "bushel" ~doc in
  Cmd.v info Term.(const update_from_bushel $ base_dir_arg $ links_file_arg $ include_domains_arg $ exclude_domains_arg)

let upload_cmd =
  let doc = "Upload links without karakeep data to Karakeep" in
  let info = Cmd.info "upload" ~doc in
  Cmd.v info Term.(const upload_to_karakeep $ base_url_arg $ api_key_arg $ links_file_arg $ tag_arg $ concurrent_arg $ delay_arg $ limit_arg $ verbose_arg)

(* Export the term and cmd for use in main bushel.ml *)
let cmd =
  let doc = "Manage links between Bushel and Karakeep" in
  let info = Cmd.info "links" ~doc in
  Cmd.group info [init_cmd; karakeep_cmd; bushel_cmd; upload_cmd]

(* For standalone execution *)
(* Main entry point removed - accessed through bushel_main.ml *)
