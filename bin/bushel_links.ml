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

(* Function to fetch links from Karakeep and save them *)
let fetch_from_karakeep base_url api_key output_file =
  let links_file = output_file in
  
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

let karakeep_cmd =
  let doc = "Fetch bookmarks from Karakeep" in
  let info = Cmd.info "karakeep" ~doc in
  Cmd.v info Term.(const fetch_from_karakeep $ base_url_arg $ api_key_arg $ karakeep_output_file_arg)

let default_cmd =
  let doc = "Manage link collection" in
  let info = Cmd.info "bushel_links" ~doc in
  Cmd.group info [add_cmd; karakeep_cmd]

let () = exit (Cmd.eval' default_cmd)
