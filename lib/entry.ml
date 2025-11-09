type entry =
  [ `Paper of Paper.t
  | `Project of Project.t
  | `Idea of Idea.t
  | `Video of Video.t
  | `Note of Note.t
  ]

type slugs = (string, entry) Hashtbl.t

type t =
  { slugs : slugs
  ; papers : Paper.ts
  ; old_papers : Paper.ts
  ; notes : Note.ts
  ; projects : Project.ts
  ; ideas : Idea.ts
  ; videos : Video.ts
  ; contacts : Contact.ts
  ; images : Srcsetter.ts
  ; doi_entries : Doi_entry.ts
  ; data_dir : string
  }

let contacts { contacts; _ } = contacts
let videos { videos; _ } = videos
let ideas { ideas; _ } = ideas
let papers { papers; _ } = papers
let notes { notes; _ } = notes
let projects { projects; _ } = projects
let images { images; _ } = images
let doi_entries { doi_entries; _ } = doi_entries
let data_dir { data_dir; _ } = data_dir

let v ~papers ~notes ~projects ~ideas ~videos ~contacts ~images ~data_dir =
  let slugs : slugs = Hashtbl.create 42 in
  let papers, old_papers = List.partition (fun p -> p.Paper.latest) papers in
  List.iter (fun n -> Hashtbl.add slugs n.Note.slug (`Note n)) notes;
  List.iter (fun p -> Hashtbl.add slugs p.Project.slug (`Project p)) projects;
  List.iter (fun i -> Hashtbl.add slugs i.Idea.slug (`Idea i)) ideas;
  List.iter (fun v -> Hashtbl.add slugs v.Video.slug (`Video v)) videos;
  List.iter (fun p -> Hashtbl.add slugs p.Paper.slug (`Paper p)) papers;
  (* Load DOI entries from doi.yml *)
  let doi_yml_path = Filename.concat data_dir "doi.yml" in
  let doi_entries = Doi_entry.load doi_yml_path in
  { slugs; papers; old_papers; notes; projects; ideas; videos; images; contacts; doi_entries; data_dir }
;;

let lookup { slugs; _ } slug = Hashtbl.find_opt slugs slug
let lookup_exn { slugs; _ } slug = Hashtbl.find slugs slug

let old_papers { old_papers; _ } = old_papers

let sidebar = function
  | `Note { Note.sidebar = Some s; _ } -> Some s
  | _ -> None
;;

let to_type_string = function
  | `Paper _ -> "paper"
  | `Note _ -> "note"
  | `Project _ -> "project"
  | `Idea _ -> "idea"
  | `Video _ -> "video"
;;

let synopsis = function
  | `Note n -> Note.synopsis n
  | _ -> None
;;

let slug = function
  | `Paper p -> p.Paper.slug
  | `Note n -> n.Note.slug
  | `Project p -> p.Project.slug
  | `Idea i -> i.Idea.slug
  | `Video v -> v.Video.slug
;;

let title = function
  | `Paper p -> Paper.title p
  | `Note n -> Note.title n
  | `Project p -> Project.title p
  | `Idea i -> Idea.title i
  | `Video v -> Video.title v
;;

let body = function
  | `Paper _ -> ""
  | `Note n -> Note.body n
  | `Project p -> Project.body p
  | `Idea i -> Idea.body i
  | `Video _ -> ""
;;

let site_url = function
  | `Paper p -> "/papers/" ^ p.Paper.slug
  | `Note n -> "/notes/" ^ n.Note.slug
  | `Project p -> "/projects/" ^ p.Project.slug
  | `Idea i -> "/ideas/" ^ i.Idea.slug
  | `Video v -> "/videos/" ^ v.Video.slug
;;

(** Extract external URLs from markdown content *)
let extract_external_links md =
  let open Cmarkit in
  let urls = ref [] in
  
  let is_external_url url =
    (* XXX FIXME *)
    let is_bushel_slug = String.starts_with ~prefix:":" in
    let is_tag_slug = String.starts_with ~prefix:"##" in
    if is_bushel_slug url || is_tag_slug url then false
    else 
      try
        let uri = Uri.of_string url in
        match Uri.scheme uri with
        | Some s when s = "http" || s = "https" -> true
        | Some _ -> true  (* Any other scheme is considered external *)
        | None -> false   (* Local references or relative paths *)
      with _ -> false
  in
  
  let inline_mapper _ = function
    | Inline.Link (lb, _) | Inline.Image (lb, _) ->
        let ref = Inline.Link.reference lb in
        (match ref with
        | `Inline (ld, _) ->
            (match Link_definition.dest ld with
            | Some (url, _) when is_external_url url -> 
                urls := url :: !urls;
                Mapper.default
            | _ -> Mapper.default)
        | `Ref (_, _, l) ->
            (* Get the referenced label definition and extract URL if it exists *)
            let defs = Doc.defs (Doc.of_string ~strict:false md) in
            (match Label.Map.find_opt (Label.key l) defs with
            | Some (Link_definition.Def (ld, _)) -> 
                (match Link_definition.dest ld with
                | Some (url, _) when is_external_url url ->
                    urls := url :: !urls
                | _ -> ())
            | _ -> ());
            Mapper.default)
    | Inline.Autolink (autolink, _) ->
        let url = Inline.Autolink.link autolink |> fst in
        if not (Inline.Autolink.is_email autolink) && is_external_url url then
          urls := url :: !urls;
        Mapper.default
    | _ -> Mapper.default
  in
  
  let mapper = Mapper.make ~inline:inline_mapper () in
  let doc = Doc.of_string ~strict:false md in
  let _ = Mapper.map_doc mapper doc in
  List.sort_uniq String.compare !urls

let outgoing_links e = extract_external_links (body e)

let lookup_site_url t slug =
  match lookup t slug with
  | Some ent -> site_url ent
  | None -> ""

let lookup_title t slug =
  match lookup t slug with
  | Some ent -> title ent
  | None -> ""


let date (x : entry) =
  match x with
  | `Paper p -> Paper.date p
  | `Note n -> Note.date n
  | `Project p -> p.Project.start, 1, 1
  | `Idea i -> i.Idea.year, i.Idea.month, 1
  | `Video v -> Video.date v
;;

let datetime v = date v |> Ptime.of_date |> Option.get

let year x =
  match date x with
  | y, _, _ -> y
;;

let is_index_entry = function
  | `Note { Note.index_page; _ } -> index_page
  | _ -> false
;;

let notes_for_slug { notes; _ } slug =
  List.filter (fun n -> match Note.slug_ent n with Some s -> s = slug | None -> false) notes
let all_entries { slugs; _ } = Hashtbl.fold (fun _ v acc -> v :: acc) slugs []

let all_papers { papers; old_papers; _ } =
  List.map (fun x -> `Paper x) (papers @ old_papers)
;;

let compare a b =
  let datetime v = Option.get (Ptime.of_date v) in
  let da = datetime (date a) in
  let db = datetime (date b) in
  if da = db then compare (title a) (title b) else Ptime.compare da db
;;

let lookup_by_name {contacts;_} n =
  match Contact.lookup_by_name contacts n with
  | v -> Some v
  | exception _ -> None

(** Extract the first image URL from markdown text *)
let extract_first_image md =
  let open Cmarkit in
  (* Don't use bushel link resolver to avoid circular dependency *)
  let doc = Doc.of_string md in
  let found_image = ref None in

  let find_image_in_inline _mapper = function
    | Inline.Image (img, _) ->
      (match Inline.Link.reference img with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (url, _) when !found_image = None ->
            found_image := Some url;
            Mapper.default
          | _ -> Mapper.default)
       | _ -> Mapper.default)
    | _ -> Mapper.default
  in

  let mapper = Mapper.make ~inline:find_image_in_inline () in
  let _ = Mapper.map_doc mapper doc in
  !found_image
;;

(** Extract the first video slug from markdown text by looking for bushel video links *)
let extract_first_video entries md =
  let open Cmarkit in
  let doc = Doc.of_string md in
  let found_video = ref None in

  let find_video_in_inline _mapper = function
    | Inline.Link (link, _) ->
      (match Inline.Link.reference link with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (url, _) when !found_video = None && String.starts_with ~prefix:":" url ->
            (* Check if this is a video slug *)
            let slug = String.sub url 1 (String.length url - 1) in
            (match lookup entries slug with
             | Some (`Video v) ->
               found_video := Some (Video.uuid v);
               Mapper.default
             | _ -> Mapper.default)
          | _ -> Mapper.default)
       | _ -> Mapper.default)
    | _ -> Mapper.default
  in

  let mapper = Mapper.make ~inline:find_video_in_inline () in
  let _ = Mapper.map_doc mapper doc in
  !found_video
;;

(** Look up an image in the srcsetter list by slug *)
let lookup_image { images; _ } slug =
  List.find_opt (fun img -> Srcsetter.slug img = slug) images

(** Get the smallest webp variant from a srcsetter image *)
let smallest_webp_variant img =
  let variants = Srcsetter.variants img in
  let webp_variants =
    Srcsetter.MS.bindings variants
    |> List.filter (fun (name, _) -> String.ends_with ~suffix:".webp" name)
  in
  match webp_variants with
  | [] ->
    (* No webp variants - use the name field which is always webp *)
    "/images/" ^ Srcsetter.name img
  | variants ->
    (* Find the variant with the smallest width *)
    let smallest = List.fold_left (fun acc (name, (w, h)) ->
      match acc with
      | None -> Some (name, w, h)
      | Some (_, min_w, _) when w < min_w -> Some (name, w, h)
      | _ -> acc
    ) None variants in
    match smallest with
    | Some (name, _, _) -> "/images/" ^ name
    | None -> "/images/" ^ Srcsetter.name img

(** Get thumbnail slug for a contact *)
let contact_thumbnail_slug contact =
  (* Contact images use just the handle as slug *)
  Some (Contact.handle contact)

(** Get thumbnail URL for a contact - resolved through srcsetter *)
let contact_thumbnail entries contact =
  match contact_thumbnail_slug contact with
  | None -> None
  | Some thumb_slug ->
    match lookup_image entries thumb_slug with
    | Some img -> Some (smallest_webp_variant img)
    | None -> None (* Image not in srcsetter - thumbnails are optional *)

(** Get thumbnail slug for an entry with fallbacks *)
let rec thumbnail_slug entries = function
  | `Paper p ->
    (* Slug is just the paper slug, directory is in the origin path *)
    Some (Paper.slug p)

  | `Video v ->
    (* Videos use their UUID as the slug *)
    Some (Video.uuid v)

  | `Project p ->
    (* Project images use "project-{slug}" format *)
    Some (Printf.sprintf "project-%s" p.Project.slug)

  | `Idea i ->
    let is_active = match Idea.status i with
      | Idea.Available | Idea.Discussion | Idea.Ongoing -> true
      | Idea.Completed | Idea.Expired -> false
    in
    if is_active then
      (* Use first supervisor's face image *)
      let supervisors = Idea.supervisors i in
      match supervisors with
      | sup :: _ ->
        let handle = if String.length sup > 0 && sup.[0] = '@'
          then String.sub sup 1 (String.length sup - 1)
          else sup
        in
        (match Contact.find_by_handle (contacts entries) handle with
         | Some c ->
           (* Contact images use just the handle as slug *)
           Some (Contact.handle c)
         | None ->
           (* Fallback to project thumbnail *)
           let project_slug = Idea.project i in
           (match lookup entries project_slug with
            | Some p -> thumbnail_slug entries p
            | None -> None))
      | [] ->
        (* No supervisors, use project thumbnail *)
        let project_slug = Idea.project i in
        (match lookup entries project_slug with
         | Some p -> thumbnail_slug entries p
         | None -> None)
    else
      (* Use project thumbnail for completed/expired ideas *)
      let project_slug = Idea.project i in
      (match lookup entries project_slug with
       | Some p -> thumbnail_slug entries p
       | None -> None)

  | `Note n ->
    (* Use titleimage if set, otherwise extract first image from body, then try video, otherwise use slug_ent's thumbnail *)
    (match Note.titleimage n with
     | Some slug ->
       (* Always treat titleimage as a bushel slug (without ':' prefix) *)
       Some slug
     | None ->
       (* Extract first image from markdown body *)
       match extract_first_image (Note.body n) with
       | Some url when String.starts_with ~prefix:":" url ->
         Some (String.sub url 1 (String.length url - 1))
       | Some _ -> None
       | None ->
         (* Try extracting first video from markdown body *)
         match extract_first_video entries (Note.body n) with
         | Some video_uuid -> Some video_uuid
         | None ->
           (* Fallback to slug_ent's thumbnail if present *)
           match Note.slug_ent n with
           | Some slug_ent ->
             (match lookup entries slug_ent with
              | Some entry -> thumbnail_slug entries entry
              | None -> None)
           | None -> None)

(** Get thumbnail URL for an entry with fallbacks - resolved through srcsetter *)
let thumbnail entries entry =
  match thumbnail_slug entries entry with
  | None -> None
  | Some thumb_slug ->
    match lookup_image entries thumb_slug with
    | Some img -> Some (smallest_webp_variant img)
    | None ->
      (* For projects, fallback to supervisor faces if project image doesn't exist *)
      (match entry with
       | `Project p ->
         (* Find ideas for this project *)
         let project_ideas = List.filter (fun idea ->
           Idea.project idea = ":" ^ p.Project.slug
         ) (ideas entries) in
         (* Collect all unique supervisors from these ideas *)
         let all_supervisors =
           List.fold_left (fun acc idea ->
             List.fold_left (fun acc2 sup ->
               if List.mem sup acc2 then acc2 else sup :: acc2
             ) acc (Idea.supervisors idea)
           ) [] project_ideas
         in
         (* Split into avsm and others, preferring others first *)
         let (others, avsm) = List.partition (fun sup ->
           let handle = if String.length sup > 0 && sup.[0] = '@'
             then String.sub sup 1 (String.length sup - 1)
             else sup
           in
           handle <> "avsm"
         ) all_supervisors in
         (* Try supervisors in order: others first, then avsm *)
         let ordered_supervisors = others @ avsm in
         (* Try each supervisor's face image *)
         let rec try_supervisors = function
           | [] -> None
           | sup :: rest ->
             let handle = if String.length sup > 0 && sup.[0] = '@'
               then String.sub sup 1 (String.length sup - 1)
               else sup
             in
             (match Contact.find_by_handle (contacts entries) handle with
              | Some c ->
                (match lookup_image entries (Contact.handle c) with
                 | Some img -> Some (smallest_webp_variant img)
                 | None -> try_supervisors rest)
              | None -> try_supervisors rest)
         in
         try_supervisors ordered_supervisors
       | _ -> None)

(** Get thumbnail URL for a note with slug_ent *)
let thumbnail_note_with_ent entries note_item =
  (* Use linked entry's thumbnail if slug_ent is set *)
  match Note.slug_ent note_item with
  | Some slug_ent ->
    (match lookup entries (":" ^ slug_ent) with
     | Some entry -> thumbnail entries entry
     | None ->
       (* Fallback to extracting first image from note body *)
       extract_first_image (Note.body note_item))
  | None ->
    (* No slug_ent, extract from note body *)
    extract_first_image (Note.body note_item)
