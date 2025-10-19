open Cmdliner

(** TODO:claude List completed ideas as markdown bullet list *)
let list_ideas_md base_dir =
  let ideas_dir = Printf.sprintf "%s/ideas" base_dir in
  let contacts_dir = Printf.sprintf "%s/contacts" base_dir in

  if not (Sys.file_exists ideas_dir) then (
    Printf.eprintf "Ideas directory not found: %s\n" ideas_dir;
    1
  ) else (
    (* Load all contacts *)
    let contacts =
      if Sys.file_exists contacts_dir then
        Sys.readdir contacts_dir
        |> Array.to_list
        |> List.filter (String.ends_with ~suffix:".md")
        |> List.filter_map (fun contact_file ->
          let filepath = Filename.concat contacts_dir contact_file in
          try Some (Bushel.Contact.of_md filepath)
          with e ->
            Printf.eprintf "Error loading contact %s: %s\n" filepath (Printexc.to_string e);
            None
        )
      else []
    in

    let idea_files = Sys.readdir ideas_dir
                     |> Array.to_list
                     |> List.filter (String.ends_with ~suffix:".md") in
    let ideas = List.filter_map (fun idea_file ->
      let filepath = Filename.concat ideas_dir idea_file in
      try
        let idea = Bushel.Idea.of_md filepath in
        match Bushel.Idea.status idea with
        | Bushel.Idea.Completed -> Some idea
        | _ -> None
      with e ->
        Printf.eprintf "Error processing %s: %s\n" filepath (Printexc.to_string e);
        None
    ) idea_files in

    (* Sort by year descending *)
    let sorted_ideas = List.sort (fun a b ->
      compare (Bushel.Idea.year b) (Bushel.Idea.year a)
    ) ideas in

    (* Output as markdown bullet list *)
    List.iter (fun idea ->
      let student_names =
        Bushel.Idea.students idea
        |> List.filter_map (fun handle ->
          match Bushel.Contact.find_by_handle contacts handle with
          | Some contact -> Some (Bushel.Contact.name contact)
          | None ->
            Printf.eprintf "Warning: contact not found for handle %s\n" handle;
            Some handle
        )
        |> String.concat ", "
      in
      let level_str = Bushel.Idea.level_to_string (Bushel.Idea.level idea) in
      Printf.printf "- %d: \"%s\", %s (%s)\n"
        (Bushel.Idea.year idea)
        (Bushel.Idea.title idea)
        student_names
        level_str
    ) sorted_ideas;
    0
  )

let term =
  Term.(const list_ideas_md $ Bushel_common.base_dir)

let cmd =
  let doc = "List completed ideas as markdown bullet list" in
  let info = Cmd.info "ideas-md" ~doc in
  Cmd.v info term
