open Cmdliner
open Bushel

(** TODO:claude List all slugs with their types *)
let list_all_slugs entries =
  let all = Entry.all_entries entries in
  (* Sort by slug for consistent output *)
  let sorted = List.sort (fun a b ->
    String.compare (Entry.slug a) (Entry.slug b)
  ) all in
  Fmt.pr "@[<v>";
  Fmt.pr "%a@," (Fmt.styled `Bold Fmt.string) "Available entries:";
  Fmt.pr "@,";
  List.iter (fun entry ->
    let slug = Entry.slug entry in
    let type_str = Entry.to_type_string entry in
    let title = Entry.title entry in
    Fmt.pr "  %a %a - %a@,"
      (Fmt.styled `Cyan Fmt.string) slug
      (Fmt.styled `Faint Fmt.string) (Printf.sprintf "(%s)" type_str)
      Fmt.string title
  ) sorted;
  Fmt.pr "@]@.";
  0

(** TODO:claude Main info command implementation *)
let info_cmd () base_dir slug_opt =
  let entries = load base_dir in
  match slug_opt with
  | None ->
    list_all_slugs entries
  | Some slug ->
    (* Handle contact handles starting with @ *)
    if String.starts_with ~prefix:"@" slug then
      let handle = String.sub slug 1 (String.length slug - 1) in
      match Contact.find_by_handle (Entry.contacts entries) handle with
      | None ->
        Fmt.epr "Error: No contact found with handle '@%s'@." handle;
        1
      | Some contact ->
        Contact.pp Fmt.stdout contact;
        (* Add thumbnail information for contact *)
        (match Entry.contact_thumbnail_slug contact with
         | Some thumb_slug ->
           Fmt.pr "@.@.";
           Fmt.pr "@[<v>%a: %s@," (Fmt.styled `Bold Fmt.string) "Thumbnail Slug" thumb_slug;
           (* Look up the image in srcsetter *)
           (match Entry.lookup_image entries thumb_slug with
            | Some img ->
              let thumbnail_url = Entry.smallest_webp_variant img in
              Fmt.pr "%a: %s@," (Fmt.styled `Bold Fmt.string) "Thumbnail URL" thumbnail_url;
              Fmt.pr "%a: %s@," (Fmt.styled `Bold Fmt.string) "Origin" (Srcsetter.origin img);
              let (w, h) = Srcsetter.dims img in
              Fmt.pr "%a: %dx%d@," (Fmt.styled `Bold Fmt.string) "Dimensions" w h;
              let variants = Srcsetter.variants img in
              if not (Srcsetter.MS.is_empty variants) then begin
                Fmt.pr "%a:@," (Fmt.styled `Bold Fmt.string) "Variants";
                Srcsetter.MS.iter (fun name (vw, vh) ->
                  Fmt.pr "  %s: %dx%d@," name vw vh
                ) variants
              end;
              Fmt.pr "@]"
            | None ->
              Fmt.epr "Warning: Contact thumbnail image not in srcsetter: %s@." thumb_slug;
              Fmt.pr "@]";
              ())
         | None -> ());
        (* Add Typesense JSON *)
        let doc = Typesense.contact_to_document contact in
        Fmt.pr "@.@.";
        Fmt.pr "%a:@," (Fmt.styled `Bold Fmt.string) "Typesense Document";
        Fmt.pr "%s@," (Ezjsonm.value_to_string ~minify:false doc);
        (* Add backlinks information for contact *)
        let backlinks = Bushel.Link_graph.get_backlinks_for_slug handle in
        if backlinks <> [] then begin
          Fmt.pr "@.@.";
          Fmt.pr "%a (%d):@," (Fmt.styled `Bold Fmt.string) "Backlinks" (List.length backlinks);
          List.iter (fun source_slug ->
            match Entry.lookup entries source_slug with
            | Some source_entry ->
              let source_type = Entry.to_type_string source_entry in
              let source_title = Entry.title source_entry in
              Fmt.pr "  %a %a - %a@,"
                (Fmt.styled `Cyan Fmt.string) source_slug
                (Fmt.styled `Faint Fmt.string) (Printf.sprintf "(%s)" source_type)
                Fmt.string source_title
            | None ->
              Fmt.pr "  %a %a@,"
                (Fmt.styled `Cyan Fmt.string) source_slug
                (Fmt.styled `Red Fmt.string) "(not found)"
          ) backlinks
        end;
        Fmt.pr "@.";
        0
    else
      (* Remove leading ':' if present, as slugs are stored without it *)
      let normalized_slug =
        if String.starts_with ~prefix:":" slug
        then String.sub slug 1 (String.length slug - 1)
        else slug
      in
      match Entry.lookup entries normalized_slug with
      | None ->
        Fmt.epr "Error: No entry found with slug '%s'@." slug;
        1
      | Some entry ->
        (match entry with
         | `Paper p -> Paper.pp Fmt.stdout p
         | `Project p -> Project.pp Fmt.stdout p
         | `Idea i -> Idea.pp Fmt.stdout i
         | `Video v -> Video.pp Fmt.stdout v
         | `Note n -> Note.pp Fmt.stdout n);
        (* Add thumbnail information if available *)
        (match Entry.thumbnail_slug entries entry with
         | Some thumb_slug ->
           Fmt.pr "@.@.";
           Fmt.pr "@[<v>%a: %s@," (Fmt.styled `Bold Fmt.string) "Thumbnail Slug" thumb_slug;
           (* Look up the image in srcsetter *)
           (match Entry.lookup_image entries thumb_slug with
            | Some img ->
              let thumbnail_url = Entry.smallest_webp_variant img in
              Fmt.pr "%a: %s@," (Fmt.styled `Bold Fmt.string) "Thumbnail URL" thumbnail_url;
              Fmt.pr "%a: %s@," (Fmt.styled `Bold Fmt.string) "Origin" (Srcsetter.origin img);
              let (w, h) = Srcsetter.dims img in
              Fmt.pr "%a: %dx%d@," (Fmt.styled `Bold Fmt.string) "Dimensions" w h;
              let variants = Srcsetter.variants img in
              if not (Srcsetter.MS.is_empty variants) then begin
                Fmt.pr "%a:@," (Fmt.styled `Bold Fmt.string) "Variants";
                Srcsetter.MS.iter (fun name (vw, vh) ->
                  Fmt.pr "  %s: %dx%d@," name vw vh
                ) variants
              end;
              Fmt.pr "@]"
            | None ->
              Fmt.epr "Warning: Thumbnail image not in srcsetter: %s@." thumb_slug;
              Fmt.pr "@]";
              ())
         | None -> ());
        (* Add Typesense JSON *)
        let doc = match entry with
          | `Paper p -> Typesense.paper_to_document entries p
          | `Project p -> Typesense.project_to_document entries p
          | `Idea i -> Typesense.idea_to_document entries i
          | `Video v -> Typesense.video_to_document entries v
          | `Note n -> Typesense.note_to_document entries n
        in
        Fmt.pr "@.@.";
        Fmt.pr "%a:@," (Fmt.styled `Bold Fmt.string) "Typesense Document";
        Fmt.pr "%s@," (Ezjsonm.value_to_string ~minify:false doc);
        (* Add backlinks information *)
        let backlinks = Bushel.Link_graph.get_backlinks_for_slug normalized_slug in
        if backlinks <> [] then begin
          Fmt.pr "@.@.";
          Fmt.pr "%a (%d):@," (Fmt.styled `Bold Fmt.string) "Backlinks" (List.length backlinks);
          List.iter (fun source_slug ->
            match Entry.lookup entries source_slug with
            | Some source_entry ->
              let source_type = Entry.to_type_string source_entry in
              let source_title = Entry.title source_entry in
              Fmt.pr "  %a %a - %a@,"
                (Fmt.styled `Cyan Fmt.string) source_slug
                (Fmt.styled `Faint Fmt.string) (Printf.sprintf "(%s)" source_type)
                Fmt.string source_title
            | None ->
              Fmt.pr "  %a %a@,"
                (Fmt.styled `Cyan Fmt.string) source_slug
                (Fmt.styled `Red Fmt.string) "(not found)"
          ) backlinks
        end;
        (* Add references information for notes *)
        (match entry with
         | `Note n ->
           let default_author = match Contact.find_by_handle (Entry.contacts entries) "avsm" with
             | Some c -> c
             | None -> failwith "Default author 'avsm' not found"
           in
           let references = Md.note_references entries default_author n in
           if references <> [] then begin
             Fmt.pr "@.@.";
             Fmt.pr "%a (%d):@," (Fmt.styled `Bold Fmt.string) "References" (List.length references);
             List.iter (fun (doi, citation, _is_paper) ->
               Fmt.pr "  %a: %s@,"
                 (Fmt.styled `Cyan Fmt.string) doi
                 citation
             ) references
           end
         | _ -> ());
        Fmt.pr "@.";
        0

(** TODO:claude Command line interface definition *)
let slug_arg =
  let doc = "The slug of the entry to display (with or without leading ':'), or contact handle (with '@' prefix). If not provided, lists all available slugs." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"SLUG" ~doc)

let term =
  Term.(const info_cmd $ Bushel_common.setup_term $ Bushel_common.base_dir $ slug_arg)

let cmd =
  let doc = "Display all information for a given slug" in
  let info = Cmd.info "info" ~doc in
  Cmd.v info term
