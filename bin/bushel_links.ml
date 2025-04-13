open Cmdliner

let link_add url descr date =
  let date =
    match date with
    | Some date_str -> 
        Ptime.of_rfc3339 (date_str ^ "T00:00:00Z") 
        |> Result.get_ok 
        |> fun (d, _, _) -> Ptime.to_date d
    | None -> 
        let now = Unix.gettimeofday () in
        let tm = Unix.gmtime now in
        (1900 + tm.tm_year, 1 + tm.tm_mon, tm.tm_mday)
  in
  let link = { Bushel.Link.url; date; description = descr } in
  
  (* Ensure directory exists *)
  let dir = "data/links" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  
  (* Create filename based on date and URL hash *)
  let (y, m, d) = date in
  let date_str = Printf.sprintf "%04d-%02d-%02d" y m d in
  let url_hash = Digest.(string url |> to_hex |> fun s -> String.sub s 0 8) in
  let filename = Printf.sprintf "%s/%s-%s.yml" dir date_str url_hash in
  
  (* Write the link to YAML file *)
  let yaml = `A [ Bushel.Link.to_yaml link ] in
  let yaml_str = Yaml.to_string_exn yaml in
  let oc = open_out filename in
  output_string oc yaml_str;
  close_out oc;
  Printf.printf "Added link to %s\n" filename;
  0

let url_arg =
  let doc = "URL to add to link collection" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")

let description_arg =
  let doc = "Description of the URL" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"DESCRIPTION")

let date_arg =
  let doc = "Publication date (YYYY-MM-DD format). Defaults to today." in
  Arg.(value & opt (some string) None & info ["date"] ~doc ~docv:"DATE")

let add_cmd =
  let doc = "Add a URL to the link collection" in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const link_add $ url_arg $ description_arg $ date_arg)

let default_cmd =
  let doc = "Manage link collection" in
  let info = Cmd.info "bushel_links" ~doc in
  Cmd.group info [add_cmd]

let () = exit (Cmd.eval' default_cmd)