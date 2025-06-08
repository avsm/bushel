open Cmdliner
open Lwt.Infix

(* Initialize a new links.yml file or ensure it exists *)
let init_links_file links_file =
  if Sys.file_exists links_file then
    Printf.printf "Links file %s already exists\n" links_file
  else begin
    (* Create an empty links file *)
    Bushel.Link.save_links_file links_file [];
    Printf.printf "Created empty links file: %s\n" links_file
  end;
  0

(* Update links.yml from Karakeep *)
let update_from_karakeep base_url api_key_opt tag links_file download_assets =
  match api_key_opt with
  | None ->
      Printf.eprintf "Error: API key is required.\n";
      Printf.eprintf "Please provide one with --api-key or create a ~/.karakeep-api file.\n";
      1
  | Some api_key ->
      let assets_dir = "data/assets" in
      
      (* Run the Lwt program *)
      Lwt_main.run (
        Printf.printf "Fetching links from %s with tag '%s'...\n" base_url tag;
        
        (* Prepare tag filter *)
        let filter_tags = if tag = "" then [] else [tag] in
        
        (* Fetch bookmarks from Karakeep with error handling *)
        Lwt.catch
          (fun () ->
            Karakeep.fetch_all_bookmarks ~api_key ~filter_tags base_url >>= fun bookmarks ->
            
            Printf.printf "Retrieved %d bookmarks from Karakeep\n" (List.length bookmarks);
            
            (* Read existing links if file exists *)
            let existing_links = Bushel.Link.load_links_file links_file in
            
            (* Convert bookmarks to bushel links *)
            let new_links = List.map (fun bookmark ->
              Karakeep.to_bushel_link ~base_url bookmark
            ) bookmarks in
            
            (* Merge with existing links *)
            let merged_links = Bushel.Link.merge_links existing_links new_links in
            
            (* Save the updated links file *)
            Bushel.Link.save_links_file links_file merged_links;
            
            Printf.printf "Updated %s with %d links\n" links_file (List.length merged_links);
            
            (* Download assets if requested *)
            if download_assets then begin
              Printf.printf "Downloading assets for bookmarks...\n";
              
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
                    let asset_dir = Printf.sprintf "%s/%s" assets_dir asset_id in
                    let asset_file = Printf.sprintf "%s/asset.bin" asset_dir in
                    let meta_file = Printf.sprintf "%s/metadata.json" asset_dir in
                    
                    (* Skip if the asset already exists *)
                    if Sys.file_exists asset_file then
                      Lwt.return_unit
                    else begin
                      (* Create the asset directory *)
                      (try Unix.mkdir asset_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
                      
                      (* Download the asset *)
                      Printf.printf "Downloading %s asset %s...\n" asset_type asset_id;
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
                      let metadata = Printf.sprintf "{\n  \"contentType\": \"%s\",\n  \"assetType\": \"%s\"\n}" 
                        content_type asset_type in
                      Lwt_io.with_file ~mode:Lwt_io.Output meta_file (fun oc ->
                        Lwt_io.write oc metadata
                      )
                    end
                  ) assets
              ) bookmarks >>= fun () ->
              
              Printf.printf "Asset download completed.\n";
              Lwt.return 0
            end else
              Lwt.return 0
          )
          (fun exn ->
            Printf.eprintf "Error fetching bookmarks: %s\n" (Printexc.to_string exn);
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
    Printf.printf "Including only domains: %s\n" (String.concat ", " include_domains_list);
  
  if exclude_domains_list <> [] then
    Printf.printf "Excluding domains: %s\n" (String.concat ", " exclude_domains_list);
  
  (* Load all entries from the bushel directory *)
  let notes_dir = Filename.concat bushel_dir "data/notes" in
  
  (* Make sure the notes directory exists *)
  if not (Sys.file_exists notes_dir) then begin
    Printf.eprintf "Error: Notes directory %s does not exist\n" notes_dir;
    exit 1
  end;
  
  (* Load all entries with fallback *)
  Printf.printf "Loading entries from %s...\n" bushel_dir;
  
  let entries_data = Bushel.load bushel_dir in
  let all_entries = Bushel.Entry.all_entries entries_data in
  Printf.printf "Loaded %d entries\n" (List.length all_entries);
  
  (* Extract outgoing links from all entries *)
  Printf.printf "Extracting outgoing links...\n";
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
  
  (* Merge with existing links *)
  let merged_links = Bushel.Link.merge_links existing_links !extracted_links in
  
  (* Save the updated links file *)
  Bushel.Link.save_links_file links_file merged_links;
  
  Printf.printf "Added %d extracted links from Bushel to %s\n" 
    (List.length !extracted_links) links_file;
  Printf.printf "Total links in file: %d\n" (List.length merged_links);
  0

(* Upload links to Karakeep that don't already have a karakeep_id *)
let upload_to_karakeep base_url api_key_opt links_file tag max_concurrent delay_seconds =
  match api_key_opt with
  | None ->
      Printf.eprintf "Error: API key is required.\n";
      Printf.eprintf "Please provide one with --api-key or create a ~/.karakeep-api file.\n";
      1
  | Some api_key ->
      (* Load links from file *)
      let links = Bushel.Link.load_links_file links_file in
      
      (* Filter links that don't have karakeep data for this remote *)
      let links_to_upload = List.filter (fun link ->
        match link.Bushel.Link.karakeep with
        | Some { remote_url; _ } when remote_url = base_url -> false
        | _ -> true
      ) links in
  
  if links_to_upload = [] then begin
    Printf.printf "No links to upload to %s (all links already have karakeep_ids)\n" base_url;
    0
  end else begin
    Printf.printf "Found %d links to upload to %s\n" (List.length links_to_upload) base_url;
    
    (* Prepare tags - include provided tag and add bushel slugs as tags *)
    let prepare_tags link =
      let slug_tags = 
        match link.Bushel.Link.bushel with
        | Some { slugs; _ } -> List.map (fun slug -> "bushel:" ^ slug) slugs
        | None -> []
      in
      if tag = "" then slug_tags
      else tag :: slug_tags
    in
    
    (* Split links into batches for parallel processing *)
    let rec create_batches links acc =
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
          create_batches rest (batch :: acc)
    in
    
    let batches = create_batches links_to_upload [] in
    Printf.printf "Processing in %d batches of up to %d links each...\n" 
      (List.length batches) max_concurrent;
    
    (* Process batches and accumulate updated links *)
    let updated_links = ref [] in
    
    let result = Lwt_main.run (
      Lwt.catch
        (fun () ->
          Lwt_list.fold_left_s (fun total_count batch ->
            (* Process links in this batch concurrently *)
            Lwt_list.map_p (fun link ->
              let url = link.Bushel.Link.url in
              let title = 
                if link.Bushel.Link.description <> "" then 
                  Some link.Bushel.Link.description 
                else None 
              in
              let tags = prepare_tags link in
              
              Printf.printf "Uploading: %s\n" url;
              
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
                  
                  Printf.printf "  - Added to Karakeep with ID: %s\n" bookmark.id;
                  Lwt.return 1 (* Success *)
                )
                (fun exn ->
                  Printf.eprintf "  - Error uploading %s: %s\n" url (Printexc.to_string exn);
                  Lwt.return 0 (* Failure *)
                )
            ) batch >>= fun results ->
            
            (* Count successes in this batch *)
            let batch_successes = List.fold_left (+) 0 results in
            
            (* Add a delay before processing the next batch *)
            Lwt_unix.sleep delay_seconds >>= fun () ->
            
            (* Return the updated total *)
            Lwt.return (total_count + batch_successes)
          ) 0 batches
        )
        (fun exn ->
          Printf.eprintf "Error during upload operation: %s\n" (Printexc.to_string exn);
          Lwt.return 0
        )
    ) in
    
    (* Update the links file with the new karakeep_ids *)
    if !updated_links <> [] then begin
      (* Replace the updated links in the original list *)
      let final_links =
        List.map (fun link ->
          let url = link.Bushel.Link.url in
          let updated = List.find_opt (fun ul -> ul.Bushel.Link.url = url) !updated_links in
          match updated with
          | Some ul -> ul
          | None -> link
        ) links
      in
      
      (* Save the updated links file *)
      Bushel.Link.save_links_file links_file final_links;
      
      Printf.printf "Updated %s with %d new karakeep_ids\n" 
        links_file (List.length !updated_links);
    end;
    
    Printf.printf "Upload complete. %d/%d links uploaded successfully.\n" 
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
  let doc = "Upload links without karakeep_id to Karakeep" in
  let info = Cmd.info "upload" ~doc in
  Cmd.v info Term.(const upload_to_karakeep $ base_url_arg $ api_key_arg $ links_file_arg $ tag_arg $ concurrent_arg $ delay_arg)

let default_cmd =
  let doc = "Manage links between Bushel and Karakeep" in
  let info = Cmd.info "bushel_links" ~doc in
  Cmd.group info [init_cmd; karakeep_cmd; bushel_cmd; upload_cmd]

let () = exit (Cmd.eval' default_cmd)