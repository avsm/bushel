open Cmdliner

(** TODO:claude Classify papers based on heuristics and update metadata *)
let classify_papers base_dir overwrite =
  let papers_dir = Printf.sprintf "%s/papers" base_dir in
  if not (Sys.file_exists papers_dir) then (
    Printf.eprintf "Papers directory not found: %s\n" papers_dir;
    1
  ) else (
    let paper_dirs = Sys.readdir papers_dir |> Array.to_list in
    List.iter (fun paper_slug ->
      let paper_path = Filename.concat papers_dir paper_slug in
      if Sys.is_directory paper_path then (
        let versions = Sys.readdir paper_path |> Array.to_list 
                     |> List.filter (String.ends_with ~suffix:".md") in
        List.iter (fun version_file ->
          let filepath = Filename.concat paper_path version_file in
          let version = Filename.remove_extension version_file in
          try
            let paper = Bushel.Paper.of_md ~slug:paper_slug ~ver:version filepath in
            let predicted_class = Bushel.Paper.classification paper in
            let class_str = Bushel.Paper.string_of_classification predicted_class in
            Printf.printf "%s/%s: %s\n" paper_slug version class_str;
            
            (* Update the file if overwrite is enabled *)
            if overwrite then (
              let json_data = Bushel.Paper.raw_json paper in
              let keys = Ezjsonm.get_dict json_data in
              let updated_keys = ("classification", `String class_str) :: 
                               (List.filter (fun (k, _) -> k <> "classification") keys) in
              let updated_json = `O updated_keys in
              let abstract = Some (Bushel.Paper.abstract paper) in
              let content = Bushel.Paper.to_yaml ?abstract ~ver:version updated_json in
              let oc = open_out filepath in
              output_string oc content;
              close_out oc;
              Printf.printf "  Updated %s\n" filepath
            )
          with e ->
            Printf.eprintf "Error processing %s: %s\n" filepath (Printexc.to_string e)
        ) versions
      )
    ) paper_dirs;
    0
  )

let overwrite_flag =
  let doc = "Update paper files with classification metadata" in
  Arg.(value & flag & info ["overwrite"] ~doc)

let term =
  Term.(const classify_papers $ Bushel_common.base_dir $ overwrite_flag)

let cmd =
  let doc = "Classify papers as full/short/preprint" in
  let info = Cmd.info "paper-classify" ~doc in
  Cmd.v info term
