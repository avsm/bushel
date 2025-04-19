open Cmdliner
open Lwt.Infix

(* Helper function to update links in a single file *)
let update_links_file file link =
  (* Read existing links if file exists *)
  let existing_links =
    try
      let yaml_str = In_channel.(with_open_bin file input_all) in
      match Yaml.of_string_exn yaml_str with
      | `A links -> List.map Bushel.Link.t_of_yaml links
      | _ -> []
    with _ -> []
  in
  
  (* Check if this URL already exists, if so, remove it *)
  let url = Bushel.Link.url link in
  let filtered_links = List.filter (fun l -> Bushel.Link.url l <> url) existing_links in
  
  (* Add the new link and sort by date (newest first) *)
  let all_links = link :: filtered_links in
  let sorted_links = List.sort Bushel.Link.compare all_links in
  
  (* Write all links to the file *)
  let yaml = `A (List.map Bushel.Link.to_yaml sorted_links) in
  let yaml_str = Yaml.to_string_exn yaml in
  let oc = open_out file in
  output_string oc yaml_str;
  close_out oc;
  file

let link_add url descr date output_file =
  let date =
    match date with
    | Some date_str -> 
        if String.contains date_str '-' then
          Scanf.sscanf date_str "%04d-%02d-%02d" (fun y m d -> (y, m, d))
        else
          Ptime.of_rfc3339 (date_str ^ "T00:00:00Z") 
          |> Result.get_ok 
          |> fun (d, _, _) -> Ptime.to_date d
    | None -> 
        let now = Unix.gettimeofday () in
        let tm = Unix.gmtime now in
        (1900 + tm.tm_year, 1 + tm.tm_mon, tm.tm_mday)
  in
  let link = { Bushel.Link.url; date; description = descr; metadata = [] } in
  let links_file = output_file in
  let filename = update_links_file links_file link in
  Printf.printf "Added link to %s\n" filename;
  0

(* Function to download assets for a bookmark *)
let download_assets base_url api_key assets_dir bookmark =
  Lwt_main.run (
    (* Ensure the assets directory exists *)
    (try Unix.mkdir assets_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    
    (* Extract asset IDs from bookmark metadata *)
    let asset_metadata = List.filter_map (fun (k, v) -> 
      if String.starts_with ~prefix:"asset_" k then
        Some (String.sub k 6 (String.length k - 6), v)
      else if k = "content_screenshotAssetId" then
        Some ("screenshot", v)
      else
        None
    ) bookmark.Bushel.Link.metadata in
    
    (* Process all assets *)
    Lwt_list.iter_s (fun (asset_type, asset_id) ->
      if asset_id <> "null" && asset_id <> "" then begin
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
      end else
        Lwt.return_unit
    ) asset_metadata
  )

(* Function to fetch links from Karakeep and save them *)
let fetch_from_karakeep base_url api_key output_file download_assets_flag =
  let links_file = output_file in
  let assets_dir = "data/assets" in
  
  (* Run the Lwt program *)
  Lwt_main.run (
    (* Fetch bookmarks *)
    Karakeep.fetch_all_bookmarks ~api_key ~filter_tags:[] base_url >>= fun bookmarks ->
    
    (* Read existing links if file exists *)
    let existing_links =
      try
        let yaml_str = In_channel.(with_open_bin links_file input_all) in
        match Yaml.of_string_exn yaml_str with
        | `A links -> List.map Bushel.Link.t_of_yaml links
        | _ -> []
      with _ -> []
    in
    
    (* Keep track of bookmark IDs we've processed to avoid duplicates *)
    let processed_ids = Hashtbl.create (List.length bookmarks) in
    
    (* Process each bookmark and create updated links list *)
    let new_links = 
      List.fold_left (fun acc bookmark ->
        (* Skip already processed bookmark IDs *)
        if Hashtbl.mem processed_ids bookmark.Karakeep.id then acc
        else begin
          Hashtbl.add processed_ids bookmark.Karakeep.id true;
          (Karakeep.to_bushel_link bookmark) :: acc
        end
      ) [] bookmarks
    in
    
    (* Combine with existing links and sort by date (newest first) *)
    let all_links = new_links @ existing_links in
    let sorted_links = List.sort Bushel.Link.compare all_links in
    
    (* Write all links to the file *)
    let yaml = `A (List.map Bushel.Link.to_yaml sorted_links) in
    let yaml_str = Yaml.to_string_exn yaml in
    let oc = open_out links_file in
    output_string oc yaml_str;
    close_out oc;
    
    Printf.printf "Added %d new bookmarks from Karakeep to %s\n" (List.length new_links) links_file;
    Printf.printf "Total links in file: %d\n" (List.length sorted_links);
    
    (* Download assets if requested *)
    if download_assets_flag then begin
      Printf.printf "Downloading assets for new bookmarks...\n";
      (* Ensure the assets directory exists *)
      (try Unix.mkdir assets_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      
      Lwt_list.iter_s (fun link ->
        (* Only process new links *)
        let karakeep_id = 
          try Some (List.assoc "karakeep_id" link.Bushel.Link.metadata)
          with Not_found -> None
        in
        match karakeep_id with
        | Some id when Hashtbl.mem processed_ids id ->
            download_assets base_url api_key assets_dir link;
            Lwt.return_unit
        | _ -> Lwt.return_unit
      ) new_links >>= fun () ->
      
      Printf.printf "Asset download completed.\n";
      Lwt.return 0
    end else
      Lwt.return 0
  )

let url_arg =
  let doc = "URL to add to link collection" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")

let description_arg =
  let doc = "Description of the URL" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"DESCRIPTION")

let date_arg =
  let doc = "Publication date (YYYY-MM-DD format). Defaults to today." in
  Arg.(value & opt (some string) None & info ["date"] ~doc ~docv:"DATE")

let output_file_arg =
  let doc = "Output YAML file (in data/links directory). Defaults to links.yml." in
  Arg.(value & opt string "links.yml" & info ["output"; "o"] ~doc ~docv:"FILE")

let add_cmd =
  let doc = "Add a URL to the link collection" in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const link_add $ url_arg $ description_arg $ date_arg $ output_file_arg)

(* Arguments for karakeep command *)
let base_url_arg =
  let doc = "Base URL of the Karakeep instance" in
  let default = "https://hoard.recoil.org" in
  Arg.(value & opt string default & info ["url"] ~doc ~docv:"URL")

let api_key_arg =
  let doc = "API key for Karakeep authentication (ak1_<key_id>_<secret>)" in
  Arg.(required & opt (some string) None & info ["api-key"] ~doc ~docv:"API_KEY")

let karakeep_output_file_arg =
  let doc = "Output YAML file. Defaults to links.yml." in
  Arg.(value & opt string "links.yml" & info ["output"; "o"] ~doc ~docv:"FILE")

let download_assets_flag_arg =
  let doc = "Download assets (screenshots, etc.) from Karakeep" in
  Arg.(value & flag & info ["download-assets"; "d"] ~doc)

(* Function to create a single bookmark in Karakeep with tags *)
let create_karakeep_bookmark base_url api_key url title note tags favourited archived output_file =
  let tags_list = match tags with
    | None -> []
    | Some tags_str -> String.split_on_char ',' tags_str |> List.map String.trim
  in
  
  (* Run the Lwt program *)
  Lwt_main.run (
    Printf.printf "Creating bookmark for URL: %s\n" url;
    
    (* Create the bookmark with tags *)
    Karakeep.create_bookmark ~api_key ~url ?title ?note ~tags:tags_list ~favourited ~archived base_url >>= fun bookmark ->
    
    (* Convert to bushel link *)
    let bushel_link = Karakeep.to_bushel_link bookmark in
    
    (* Add to local file *)
    let links_file = output_file in
    let _ = update_links_file links_file bushel_link in
    
    Printf.printf "Added bookmark to %s with ID: %s\n" links_file bookmark.id;
    if tags_list <> [] then
      Printf.printf "Applied tags: %s\n" (String.concat ", " tags_list);
    
    Lwt.return 0
  )

(* Arguments for create command *)
let title_arg =
  let doc = "Title for the bookmark (optional)" in
  Arg.(value & opt (some string) None & info ["title"; "t"] ~doc ~docv:"TITLE")

let note_arg =
  let doc = "Note to add to the bookmark (optional)" in
  Arg.(value & opt (some string) None & info ["note"; "n"] ~doc ~docv:"NOTE")

let tags_arg =
  let doc = "Tags to add to the bookmark (comma-separated)" in
  Arg.(value & opt (some string) None & info ["tags"; "T"] ~doc ~docv:"TAGS")

let favourited_flag_arg =
  let doc = "Mark the bookmark as favourited" in
  Arg.(value & flag & info ["favourited"; "f"] ~doc)

let archived_flag_arg =
  let doc = "Mark the bookmark as archived" in
  Arg.(value & flag & info ["archived"; "a"] ~doc)

let create_cmd =
  let doc = "Create a new bookmark in Karakeep" in
  let info = Cmd.info "create" ~doc in
  Cmd.v info Term.(const create_karakeep_bookmark $ base_url_arg $ api_key_arg $ url_arg $ title_arg $ note_arg $ tags_arg $ favourited_flag_arg $ archived_flag_arg $ karakeep_output_file_arg)

let karakeep_cmd =
  let doc = "Fetch bookmarks from Karakeep" in
  let info = Cmd.info "karakeep" ~doc in
  Cmd.v info Term.(const fetch_from_karakeep $ base_url_arg $ api_key_arg $ karakeep_output_file_arg $ download_assets_flag_arg)

(* Function to extract outgoing links from all notes in a Bushel directory *)
let extract_outgoing_links base_dir output_file include_domains exclude_domains =
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
  (* Load all notes from the base directory *)
  let notes_dir = Filename.concat base_dir "data/notes" in
  
  (* Make sure the notes directory exists *)
  if not (Sys.file_exists notes_dir) then begin
    Printf.eprintf "Error: Notes directory %s does not exist\n" notes_dir;
    exit 1
  end;
  
  (* Load all entries with fallback *)
  Printf.printf "Loading entries from %s...\n" base_dir;
  
  let entries_data = 
      Bushel.load base_dir
  in
  
  (* Get all entries *)
  let all_entries = Bushel.Entry.all_entries entries_data in
  Printf.printf "Loaded %d entries\n" (List.length all_entries);
  
  (* Extract outgoing links from all entries *)
  Printf.printf "Extracting outgoing links...\n";
  let outgoing_links = ref [] in
  
  (* Process each entry *)
  List.iter (fun entry ->
    let entry_body = Bushel.Entry.body entry in
    let entry_slug = Bushel.Entry.slug entry in
    
    (* Skip empty bodies *)
    if entry_body <> "" then begin
      let links = Bushel.Entry.extract_external_links entry_body in
      if links <> [] then begin
      
      (* Add each link from this entry as a Bushel.Link *)
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
          let now = Unix.gettimeofday () in
          let tm = Unix.gmtime now in
          let date = (1900 + tm.tm_year, 1 + tm.tm_mon, tm.tm_mday) in
          
          let description = "" in
          let metadata = [
            ("bushel_slug", entry_slug);
          ] in
          let link = { Bushel.Link.url; date; description; metadata } in
          outgoing_links := link :: !outgoing_links
        end
      ) links
    end
    end
  ) all_entries;
  
  (* Read existing links if file exists *)
  let existing_links =
    try
      let yaml_str = In_channel.(with_open_bin output_file input_all) in
      match Yaml.of_string_exn yaml_str with
      | `A links -> List.map Bushel.Link.t_of_yaml links
      | _ -> []
    with _ -> []
  in
  
  (* Create a map from URL to existing link *)
  let url_to_link = Hashtbl.create 100 in
  List.iter (fun link -> 
    let url = Bushel.Link.url link in
    Hashtbl.replace url_to_link url link
  ) existing_links;
  
  (* Process new links, merge with existing ones if URL already exists *)
  let new_links = ref [] in
  let updated_links = ref [] in
  
  List.iter (fun link ->
    let url = Bushel.Link.url link in
    if Hashtbl.mem url_to_link url then begin
      (* URL already exists, merge metadata *)
      let existing_link = Hashtbl.find url_to_link url in
      let existing_metadata = existing_link.Bushel.Link.metadata in
      
      (* Create updated metadata - keep only bushel_slug if present *)
      let updated_metadata = 
        match List.assoc_opt "bushel_slug" existing_metadata with
        | Some slug -> [("bushel_slug", slug)]
        | None -> 
            (* Try to get source_slug from existing metadata and rename it *)
            match List.assoc_opt "source_slug" existing_metadata with
            | Some slug -> [("bushel_slug", slug)]
            | None -> []
      in
      
      (* Create updated link *)
      let updated_link = { existing_link with Bushel.Link.metadata = updated_metadata } in
      Hashtbl.replace url_to_link url updated_link;
      updated_links := updated_link :: !updated_links
    end else begin
      (* New URL, add it *)
      Hashtbl.add url_to_link url link;
      new_links := link :: !new_links
    end
  ) !outgoing_links;
  
  (* Get all links that weren't updated *)
  let unchanged_links = 
    List.filter (fun link ->
      let url = Bushel.Link.url link in
      not (List.exists (fun updated -> Bushel.Link.url updated = url) !updated_links)
    ) existing_links
  in
  
  (* Combine all links and sort by date (newest first) *)
  let all_links = !new_links @ !updated_links @ unchanged_links in
  let sorted_links = List.sort Bushel.Link.compare all_links in
  
  (* Write all links to the file *)
  let yaml = `A (List.map Bushel.Link.to_yaml sorted_links) in
  let yaml_str = Yaml.to_string_exn ~len:1200000 yaml in
  let oc = open_out output_file in
  output_string oc yaml_str;
  close_out oc;
  
  Printf.printf "Added %d new outgoing links to %s\n" (List.length !new_links) output_file;
  Printf.printf "Updated %d existing links with additional sources\n" (List.length !updated_links);
  Printf.printf "Total links in file: %d\n" (List.length sorted_links);
  0

(* Arguments for extract command *)
let base_dir_arg =
  let doc = "Base directory of the Bushel project" in
  Arg.(value & opt string "." & info ["dir"; "d"] ~doc ~docv:"DIR")

let extract_output_file_arg =
  let doc = "Output YAML file. Defaults to outlinks.yml." in
  Arg.(value & opt string "outlinks.yml" & info ["output"; "o"] ~doc ~docv:"FILE")

let include_domains_arg =
  let doc = "Only include links to these domains (comma-separated list). If not specified, all domains are included." in
  Arg.(value & opt (some string) None & info ["include"] ~doc ~docv:"DOMAINS")

let exclude_domains_arg =
  let doc = "Exclude links to these domains (comma-separated list)" in
  Arg.(value & opt (some string) None & info ["exclude"] ~doc ~docv:"DOMAINS")

let extract_cmd =
  let doc = "Extract outgoing links from notes" in
  let info = Cmd.info "extract" ~doc in
  Cmd.v info Term.(const extract_outgoing_links $ base_dir_arg $ extract_output_file_arg $ include_domains_arg $ exclude_domains_arg)

(* Function to upload outlinks to Karakeep *)
let upload_outlinks api_key output_file base_url limit max_concurrent delay_seconds =
  (* Read outlinks from file *)
  let links =
    try
      let yaml_str = In_channel.(with_open_bin output_file input_all) in
      match Yaml.of_string_exn yaml_str with
      | `A links -> List.map Bushel.Link.t_of_yaml links
      | _ -> []
    with _ -> []
  in
  
  (* Take only the specified number of links (or all if limit is 0) *)
  let links_to_upload = 
    if limit > 0 then 
      let rec take n lst acc =
        if n <= 0 || lst = [] then List.rev acc
        else take (n-1) (List.tl lst) ((List.hd lst) :: acc)
      in
      take limit links []
    else 
      links
  in
  
  if links_to_upload = [] then begin
    Printf.printf "No links found in %s or file does not exist.\n" output_file;
    1
  end else begin
    Printf.printf "Uploading %d links to Karakeep...\n" (List.length links_to_upload);
    
    (* Use the provided concurrency and delay parameters *)
    Printf.printf "Using concurrency: %d, delay between batches: %.1f seconds\n"
      max_concurrent delay_seconds;
    
    (* Split links into batches *)
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
    
    (* Process each batch *)
    let total_successes = Lwt_main.run (
      Lwt_list.fold_left_s (fun total_count batch ->
        (* Process links in this batch concurrently *)
        Lwt_list.map_p (fun link ->
          let url = Bushel.Link.url link in
          let description = Bushel.Link.description link in
          let title = if description = "" then None else Some description in
          
          Printf.printf "Uploading: %s\n" url;
          
          (* Create the bookmark with the from_blog tag *)
          Lwt.catch
            (fun () ->
              Karakeep.create_bookmark 
                ~api_key 
                ~url 
                ?title 
                ~tags:["from_blog"] 
                base_url 
              >>= fun bookmark ->
              
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
    ) in
    
    Printf.printf "Upload complete. %d/%d links uploaded successfully.\n" 
      total_successes (List.length links_to_upload);
    
    0
  end

(* Arguments for upload command *)
let upload_limit_arg =
  let doc = "Limit the number of links to upload (0 for all)" in
  Arg.(value & opt int 0 & info ["limit"; "l"] ~doc ~docv:"LIMIT")

let upload_file_arg =
  let doc = "Input YAML file. Defaults to outlinks.yml." in
  Arg.(value & opt string "outlinks.yml" & info ["input"; "i"] ~doc ~docv:"FILE")

let concurrent_arg =
  let doc = "Maximum number of concurrent uploads (default: 5)" in
  Arg.(value & opt int 5 & info ["concurrent"; "c"] ~doc ~docv:"NUM")

let delay_arg =
  let doc = "Delay in seconds between batches (default: 1.0)" in
  Arg.(value & opt float 1.0 & info ["delay"; "d"] ~doc ~docv:"SECONDS")

let upload_cmd =
  let doc = "Upload all outlinks to Karakeep with 'from_blog' tag" in
  let info = Cmd.info "upload" ~doc in
  Cmd.v info Term.(const upload_outlinks $ api_key_arg $ upload_file_arg $ base_url_arg $ upload_limit_arg $ concurrent_arg $ delay_arg)

let default_cmd =
  let doc = "Manage link collection" in
  let info = Cmd.info "bushel_links" ~doc in
  Cmd.group info [add_cmd; karakeep_cmd; create_cmd; extract_cmd; upload_cmd]

let () = exit (Cmd.eval' default_cmd)
