(** Bushel mappers for our Markdown extensions and utilities

    This module provides mappers to convert Bushel markdown extensions to different
    output formats. There are two main mappers:

    1. {!make_bushel_inline_mapper} - Full sidenote mode for the main website
       - Converts Bushel links to interactive sidenotes
       - Includes entry previews, contact info, footnotes
       - Used for the main site HTML rendering

    2. {!make_bushel_link_only_mapper} - Plain HTML mode for feeds and simple output
       - Converts Bushel links to regular HTML <a> tags
       - Automatically cleans up link text that contains Bushel slugs
       - Used for Atom feeds, RSS, search indexing
       - Images need .webp extension added (handled by calling code)

    For plain text output (search, LLM), use {!markdown_to_plaintext}.
*)

(* Sidenote data types - reuse existing Bushel types *)
type sidenote_data =
  | Contact_note of Contact.t * string (* contact data + trigger text *)
  | Paper_note of Paper.t * string
  | Idea_note of Idea.t * string
  | Note_note of Note.t * string
  | Project_note of Project.t * string
  | Video_note of Video.t * string
  | Footnote_note of string * Cmarkit.Block.t * string
    (* slug, block content, trigger text *)

type Cmarkit.Inline.t += Side_note of sidenote_data

let authorlink = Cmarkit.Meta.key ()

let make_authorlink label =
  let meta = Cmarkit.Meta.tag authorlink (Cmarkit.Label.meta label) in
  Cmarkit.Label.with_meta meta label
;;

let sluglink = Cmarkit.Meta.key ()

let make_sluglink label =
  let meta = Cmarkit.Meta.tag sluglink (Cmarkit.Label.meta label) in
  Cmarkit.Label.with_meta meta label
;;

let with_bushel_links = function
  | `Def _ as ctx -> Cmarkit.Label.default_resolver ctx
  | `Ref (_, _, (Some _ as def)) -> def
  | `Ref (_, ref, None) ->
    let txt = Cmarkit.Label.key ref in
    (match txt.[0] with
     | '@' -> Some (make_authorlink ref)
     | ':' -> Some (make_sluglink ref)
     | '#' -> if txt.[1] = '#' then Some (make_sluglink ref) else None
     | _ -> None)
;;

let strip_handle s =
  if s.[0] = '@' || s.[0] = ':'
  then String.sub s 1 (String.length s - 1)
  else if s.[0] = '#' && s.[1] = '#'
  then String.sub s 2 (String.length s - 2)
  else s
;;

(* FIXME use Tags *)
let is_bushel_slug = String.starts_with ~prefix:":"
let is_tag_slug link =
  String.starts_with ~prefix:"##" link &&
  not (String.starts_with ~prefix:"###" link)

let is_type_filter_slug = String.starts_with ~prefix:"###"
let is_contact_slug = String.starts_with ~prefix:"@"

let text_of_inline lb =
  let open Cmarkit in
  Inline.to_plain_text ~break_on_soft:false lb
  |> fun r -> String.concat "\n" (List.map (String.concat "") r)
;;

let link_target_is_bushel ?slugs lb =
  let open Cmarkit in
  let ref = Inline.Link.reference lb in
  match ref with
  | `Inline (ld, _) ->
    let dest = Link_definition.dest ld in
    (match dest with
     | Some (url, _) when is_bushel_slug url ->
       (match slugs with
        | Some s -> Hashtbl.replace s url ()
        | _ -> ());
       Some (url, Inline.Link.text lb |> text_of_inline)
     | Some (url, _) when is_tag_slug url ->
       (* Return the tag URL unchanged - will be handled by renderer *)
       Some (url, Inline.Link.text lb |> text_of_inline)
     | Some (url, _) when is_contact_slug url ->
       Some (url, Inline.Link.text lb |> text_of_inline)
     | _ -> None)
  | _ -> None
;;

let image_target_is_bushel lb =
  let open Cmarkit in
  let ref = Inline.Link.reference lb in
  match ref with
  | `Inline (ld, _) ->
    let dest = Link_definition.dest ld in
    (match dest with
     | Some (url, _) when is_bushel_slug url ->
       let alt = Link_definition.title ld in
       let dir =
         Inline.Link.text lb
         |> Inline.to_plain_text ~break_on_soft:false
         |> fun r -> String.concat "\n" (List.map (String.concat "") r)
       in
       Some (url, alt, dir)
     | _ -> None)
  | _ -> None
;;

let rewrite_bushel_link_reference entries slug title meta =
  let open Cmarkit in
  let s = strip_handle slug in
  (* Check if it's a tag, contact, or entry *)
  if is_tag_slug slug then
    (* Tag link - keep the ## prefix in dest for renderer to detect *)
    let txt = Inline.Text (title, meta) in
    let ld = Link_definition.make ~dest:(slug, meta) () in
    let ll = `Inline (ld, meta) in
    let ld = Inline.Link.make txt ll in
    Mapper.ret (Inline.Link (ld, meta))
  else if is_contact_slug slug then
    (* Contact sidenote *)
    match Contact.find_by_handle (Entry.contacts entries) s with
    | Some c ->
        let sidenote = Side_note (Contact_note (c, title)) in
        Mapper.ret sidenote
    | None ->
        (* Contact not found, fallback to regular link *)
        let txt = Inline.Text (title, meta) in
        let ld = Link_definition.make ~dest:("", meta) () in
        let ll = `Inline (ld, meta) in
        let ld = Inline.Link.make txt ll in
        Mapper.ret (Inline.Link (ld, meta))
  else
    (* Check entry type and generate appropriate sidenote *)
    match Entry.lookup entries s with
    | Some (`Paper p) ->
        let sidenote = Side_note (Paper_note (p, title)) in
        Mapper.ret sidenote
    | Some (`Idea i) ->
        let sidenote = Side_note (Idea_note (i, title)) in
        Mapper.ret sidenote
    | Some (`Note n) ->
        let sidenote = Side_note (Note_note (n, title)) in
        Mapper.ret sidenote
    | Some (`Project p) ->
        let sidenote = Side_note (Project_note (p, title)) in
        Mapper.ret sidenote
    | Some (`Video v) ->
        let sidenote = Side_note (Video_note (v, title)) in
        Mapper.ret sidenote
    | None ->
        (* Entry not found, use regular link *)
        let dest = Entry.lookup_site_url entries s in
        let txt = Inline.Text (title, meta) in
        let ld = Link_definition.make ~dest:(dest, meta) () in
        let ll = `Inline (ld, meta) in
        let ld = Inline.Link.make txt ll in
        Mapper.ret (Inline.Link (ld, meta))
;;

let rewrite_bushel_image_reference entries url title dir meta =
  let open Cmarkit in
  let dest =
    match Entry.lookup entries (strip_handle url) with
    | Some ent -> Entry.site_url ent (* This is a video *)
    | None -> Printf.sprintf "/images/%s" (strip_handle url)
  in
  let txt = Inline.Text (dir, meta) in
  let ld = Link_definition.make ?title ~dest:(dest, meta) () in
  let ll = `Inline (ld, meta) in
  let ld = Inline.Link.make txt ll in
  let ent_il = Inline.Image (ld, meta) in
  Mapper.ret ent_il
;;

type Cmarkit.Inline.t += Obsidian_link of string

let rewrite_label_reference_to_obsidian lb meta =
  let open Cmarkit in
  match Inline.Link.referenced_label lb with
  | None -> Mapper.default
  | Some l ->
    let m = Label.meta l in
    (match Meta.find authorlink m with
     | Some () ->
       let slug = Label.key l in
       let target = Printf.sprintf "[[%s]]" slug in
       let txt = Obsidian_link target in
       Mapper.ret txt
     | None ->
       (match Meta.find sluglink m with
        | None -> Mapper.default
        | Some () ->
          let slug = Label.key l in
          if is_bushel_slug slug
          then (
            let target = Printf.sprintf "[[%s]]" (strip_handle slug) in
            let txt = Obsidian_link target in
            Mapper.ret txt)
          else if is_tag_slug slug
          then (
            let target = Printf.sprintf "#%s" (strip_handle slug) in
            let txt = Inline.Text (target, meta) in
            Mapper.ret txt)
          else Mapper.default))
;;

let make_bushel_link_only_mapper _defs entries =
  let open Cmarkit in
  fun _m ->
    function
    | Inline.Link (lb, meta) ->
      (* Convert Bushel link references to regular links (not sidenotes) *)
      (match link_target_is_bushel lb with
       | Some (url, title) ->
         let s = strip_handle url in
         let dest = Entry.lookup_site_url entries s in
         (* If title is itself a Bushel slug, use the entry title instead *)
         let link_text =
           if is_bushel_slug title then
             match Entry.lookup entries (strip_handle title) with
             | Some ent -> Entry.title ent
             | None -> title
           else title
         in
         let txt = Inline.Text (link_text, meta) in
         let ld = Link_definition.make ~dest:(dest, meta) () in
         let ll = `Inline (ld, meta) in
         let ld = Inline.Link.make txt ll in
         Mapper.ret (Inline.Link (ld, meta))
       | None ->
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let m = Label.meta l in
            (* Check for authorlink (contact) first *)
            (match Meta.find authorlink m with
             | Some () ->
               let slug = Label.key l in
               let s = strip_handle slug in
               (match Contact.find_by_handle (Entry.contacts entries) s with
                | Some c ->
                  let name = Contact.name c in
                  (match Contact.best_url c with
                   | Some dest ->
                     let txt = Inline.Text (name, meta) in
                     let ld = Link_definition.make ~dest:(dest, meta) () in
                     let ll = `Inline (ld, meta) in
                     let ld = Inline.Link.make txt ll in
                     Mapper.ret (Inline.Link (ld, meta))
                   | None ->
                     (* No URL for contact, just use name as text *)
                     let txt = Inline.Text (name, meta) in
                     Mapper.ret txt)
                | None ->
                  (* Contact not found, use title as fallback text *)
                  let title = Inline.Link.text lb |> text_of_inline in
                  let txt = Inline.Text (title, meta) in
                  Mapper.ret txt)
             | None ->
               (* Check for sluglink *)
               (match Meta.find sluglink m with
                | Some () ->
                  let slug = Label.key l in
                  if is_bushel_slug slug || is_tag_slug slug || is_contact_slug slug
                  then (
                    let s = strip_handle slug in
                    let dest = Entry.lookup_site_url entries s in
                    let title = Inline.Link.text lb |> text_of_inline in
                    (* If link text is itself a Bushel slug, use the entry title instead *)
                    let link_text =
                      let trimmed = String.trim title in
                      if is_bushel_slug trimmed then
                        match Entry.lookup entries (strip_handle trimmed) with
                        | Some ent -> Entry.title ent
                        | None -> title
                      else title
                    in
                    let txt = Inline.Text (link_text, meta) in
                    let ld = Link_definition.make ~dest:(dest, meta) () in
                    let ll = `Inline (ld, meta) in
                    let ld = Inline.Link.make txt ll in
                    Mapper.ret (Inline.Link (ld, meta)))
                  else Mapper.default
                | None -> Mapper.default))
          | None -> Mapper.default))
    | _ -> Mapper.default
;;

let rewrite_footnote_reference ?footnote_map entries defs lb _meta =
  let open Cmarkit in
  match Inline.Link.referenced_label lb with
  | None -> Mapper.default
  | Some l ->
    (match Inline.Link.reference_definition defs lb with
     | Some (Block.Footnote.Def (fn, _)) ->
       let label_key = Label.key l in
       let slug, trigger_text =
         match footnote_map with
         | Some fm ->
           (match Hashtbl.find_opt fm label_key with
            | Some (slug, text) -> (slug, text)
            | None ->
              let num = Hashtbl.length fm + 1 in
              let slug = Printf.sprintf "fn-%d" num in
              let text = Printf.sprintf "[%d]" num in
              Hashtbl.add fm label_key (slug, text);
              (slug, text))
         | None ->
           (* No map provided, use label key as slug *)
           let slug = Printf.sprintf "fn-%s" (String.sub label_key 1 (String.length label_key - 1)) in
           let text = "[?]" in
           (slug, text)
       in
       (* Process the block to convert Bushel link references to regular links (not sidenotes) *)
       let block = Block.Footnote.block fn in
       let link_mapper = Mapper.make ~inline:(make_bushel_link_only_mapper defs entries) () in
       let processed_block =
         match Mapper.map_block link_mapper block with
         | Some b -> b
         | None -> block
       in
       let sidenote = Side_note (Footnote_note (slug, processed_block, trigger_text)) in
       Mapper.ret sidenote
     | _ -> Mapper.default)

let rewrite_label_reference ?slugs entries lb meta =
  let open Cmarkit in
  match Inline.Link.referenced_label lb with
  | None -> Mapper.default
  | Some l ->
    let m = Label.meta l in
    (match Meta.find authorlink m with
     | Some () ->
       let slug = Label.key l in
       (match Contact.find_by_handle (Entry.contacts entries) (strip_handle slug) with
        | Some c ->
            let trigger_text = Contact.name c in
            let sidenote = Side_note (Contact_note (c, trigger_text)) in
            Mapper.ret sidenote
        | None ->
            (* Contact not found, fallback to text *)
            let txt = Inline.Text ("Unknown Person", meta) in
            Mapper.ret txt)
     | None ->
       (match Meta.find sluglink m with
        | None -> Mapper.default
        | Some () ->
          let slug = Label.key l in
          if is_bushel_slug slug
          then (
            (match slugs with
             | Some s -> Hashtbl.replace s slug ()
             | _ -> ());
            let s = strip_handle slug in
            (* Check entry type and generate appropriate sidenote *)
            match Entry.lookup entries s with
            | Some (`Paper p) ->
                let trigger_text = Entry.lookup_title entries s in
                let sidenote = Side_note (Paper_note (p, trigger_text)) in
                Mapper.ret sidenote
            | Some (`Idea i) ->
                let trigger_text = Entry.lookup_title entries s in
                let sidenote = Side_note (Idea_note (i, trigger_text)) in
                Mapper.ret sidenote
            | Some (`Note n) ->
                let trigger_text = Entry.lookup_title entries s in
                let sidenote = Side_note (Note_note (n, trigger_text)) in
                Mapper.ret sidenote
            | Some (`Project p) ->
                let trigger_text = Entry.lookup_title entries s in
                let sidenote = Side_note (Project_note (p, trigger_text)) in
                Mapper.ret sidenote
            | Some (`Video v) ->
                let trigger_text = Entry.lookup_title entries s in
                let sidenote = Side_note (Video_note (v, trigger_text)) in
                Mapper.ret sidenote
            | None ->
                (* Entry not found, use regular link *)
                let target = Entry.lookup_title entries s in
                let dest = Entry.lookup_site_url entries s in
                let txt = Inline.Text (target, meta) in
                let ld = Link_definition.make ~dest:(dest, meta) () in
                let ll = `Inline (ld, meta) in
                let ld = Inline.Link.make txt ll in
                Mapper.ret (Inline.Link (ld, meta)))
          else if is_tag_slug slug
          then (
            let sh = strip_handle slug in
            (* Use # as dest to prevent navigation, JavaScript will intercept *)
            let target, dest = sh, "#" in
            let txt = Inline.Text (target, meta) in
            let ld = Link_definition.make ~dest:(dest, meta) () in
            let ll = `Inline (ld, meta) in
            let ld = Inline.Link.make txt ll in
            let ent_il = Inline.Link (ld, meta) in
            Mapper.ret ent_il)
          else Mapper.default))
;;

let bushel_inline_mapper_to_obsidian entries _m =
  let open Cmarkit in
  function
  | Inline.Link (lb, meta) ->
    (match link_target_is_bushel lb with
     | None -> rewrite_label_reference_to_obsidian lb meta
     | Some (url, title) -> rewrite_bushel_link_reference entries url title meta)
  | Inline.Image (lb, meta) ->
    (match image_target_is_bushel lb with
     | None -> rewrite_label_reference_to_obsidian lb meta
     | Some (url, alt, dir) -> rewrite_bushel_image_reference entries url alt dir meta)
  | _ -> Mapper.default
;;

let make_bushel_inline_mapper ?slugs ?footnote_map defs entries =
  let open Cmarkit in
  fun _m ->
    function
    | Inline.Link (lb, meta) ->
      (* First check if this is a footnote reference *)
      (match Inline.Link.referenced_label lb with
       | Some l when String.starts_with ~prefix:"^" (Label.key l) ->
         (* This is a footnote reference *)
         rewrite_footnote_reference ?footnote_map entries defs lb meta
       | _ ->
         (* Not a footnote, handle as bushel link *)
         (match link_target_is_bushel ?slugs lb with
          | None -> rewrite_label_reference ?slugs entries lb meta
          | Some (url, title) -> rewrite_bushel_link_reference entries url title meta))
    | Inline.Image (lb, meta) ->
      (match image_target_is_bushel lb with
       | None -> rewrite_label_reference entries lb meta
       | Some (url, alt, dir) -> rewrite_bushel_image_reference entries url alt dir meta)
    | _ -> Mapper.default
;;

let scan_for_slugs entries md =
  let open Cmarkit in
  let slugs = Hashtbl.create 7 in
  let doc = Doc.of_string ~strict:false ~resolver:with_bushel_links md in
  let defs = Doc.defs doc in
  let _ =
    Mapper.map_doc (Mapper.make ~inline:(make_bushel_inline_mapper ~slugs defs entries) ()) doc
  in
  Hashtbl.fold (fun k () a -> k :: a) slugs []
;;

(** Validation mapper that collects broken references *)
let make_validation_mapper entries broken_slugs broken_contacts =
  let open Cmarkit in
  fun _m ->
    function
    | Inline.Link (lb, _meta) ->
      (* Check inline bushel links *)
      (match link_target_is_bushel lb with
       | Some (url, _title) ->
         let s = strip_handle url in
         if is_contact_slug url then
           (* Validate contact handle *)
           (match Contact.find_by_handle (Entry.contacts entries) s with
            | None -> Hashtbl.replace broken_contacts url ()
            | Some _ -> ())
         else if is_bushel_slug url then
           (* Validate entry slug *)
           (match Entry.lookup entries s with
            | None -> Hashtbl.replace broken_slugs url ()
            | Some _ -> ())
         else ();
         Mapper.default
       | None ->
         (* Check referenced label links *)
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let m = Label.meta l in
            (* Check for contact reference *)
            (match Meta.find authorlink m with
             | Some () ->
               let slug = Label.key l in
               let handle = strip_handle slug in
               (match Contact.find_by_handle (Entry.contacts entries) handle with
                | None -> Hashtbl.replace broken_contacts slug ()
                | Some _ -> ());
               Mapper.default
             | None ->
               (* Check for entry slug reference *)
               (match Meta.find sluglink m with
                | None -> Mapper.default
                | Some () ->
                  let slug = Label.key l in
                  if is_bushel_slug slug then (
                    let s = strip_handle slug in
                    match Entry.lookup entries s with
                     | None -> Hashtbl.replace broken_slugs slug ()
                     | Some _ -> ()
                  );
                  Mapper.default))
          | None -> Mapper.default))
    | _ -> Mapper.default
;;

(** Validate all bushel references in markdown and return broken ones *)
let validate_references entries md =
  let open Cmarkit in
  let broken_slugs = Hashtbl.create 7 in
  let broken_contacts = Hashtbl.create 7 in
  let doc = Doc.of_string ~strict:false ~resolver:with_bushel_links md in
  let mapper = Mapper.make ~inline:(make_validation_mapper entries broken_slugs broken_contacts) () in
  let _ = Mapper.map_doc mapper doc in
  let slugs = Hashtbl.fold (fun k () a -> k :: a) broken_slugs [] in
  let contacts = Hashtbl.fold (fun k () a -> k :: a) broken_contacts [] in
  (slugs, contacts)
;;

(** Extract the first image URL from markdown text *)
let extract_first_image md =
  let open Cmarkit in
  (* Don't use bushel link resolver to avoid circular dependency with Entry *)
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

(** Convert markdown text to plain text, resolving bushel links to just their text *)
let markdown_to_plaintext _entries text =
  let open Cmarkit in
  (* Parse markdown with bushel link resolver *)
  let doc = Doc.of_string ~resolver:with_bushel_links text in

  (* Convert document blocks to plain text *)
  let rec block_to_text = function
    | Block.Blank_line _ -> ""
    | Block.Thematic_break _ -> "\n---\n"
    | Block.Paragraph (p, _) ->
      let inline = Block.Paragraph.inline p in
      Inline.to_plain_text ~break_on_soft:false inline
      |> List.map (String.concat "") |> String.concat "\n"
    | Block.Heading (h, _) ->
      let inline = Block.Heading.inline h in
      Inline.to_plain_text ~break_on_soft:false inline
      |> List.map (String.concat "") |> String.concat "\n"
    | Block.Block_quote (bq, _) ->
      let blocks = Block.Block_quote.block bq in
      block_to_text blocks
    | Block.List (l, _) ->
      let items = Block.List'.items l in
      List.map (fun (item, _) ->
        let blocks = Block.List_item.block item in
        block_to_text blocks
      ) items |> String.concat "\n"
    | Block.Code_block (cb, _) ->
      let code = Block.Code_block.code cb in
      String.concat "\n" (List.map Block_line.to_string code)
    | Block.Html_block _ -> ""  (* Skip HTML blocks for search *)
    | Block.Link_reference_definition _ -> ""
    | Block.Ext_footnote_definition _ -> ""
    | Block.Blocks (blocks, _) ->
      List.map block_to_text blocks |> String.concat "\n"
    | _ -> ""
  in
  let blocks = Doc.block doc in
  block_to_text blocks
;;

(** Extract all links from markdown text, including from images *)
let extract_all_links text =
  let open Cmarkit in
  let doc = Doc.of_string ~resolver:with_bushel_links text in
  let links = ref [] in

  let find_links_in_inline _mapper = function
    | Inline.Link (lb, _) | Inline.Image (lb, _) ->
      (* Check for inline link/image destination *)
      (match Inline.Link.reference lb with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (url, _) ->
            links := url :: !links;
            Mapper.default
          | None -> Mapper.default)
       | `Ref _ ->
         (* For reference-style links/images, check if it has a referenced label *)
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let key = Label.key l in
            (* Check if it's a bushel-style link *)
            if String.length key > 0 && (key.[0] = ':' || key.[0] = '@' ||
               (String.length key > 1 && key.[0] = '#' && key.[1] = '#')) then
              links := key :: !links;
            Mapper.default
          | None -> Mapper.default))
    | _ -> Mapper.default
  in

  let mapper = Mapper.make ~inline:find_links_in_inline () in
  let _ = Mapper.map_doc mapper doc in

  (* Deduplicate *)
  let module StringSet = Set.Make(String) in
  StringSet.elements (StringSet.of_list !links)
;;

(* Reference source type for CiTO annotations *)
type reference_source =
  | Paper  (* CitesAsSourceDocument *)
  | Note   (* CitesAsRelated *)
  | External  (* Cites *)

(* Extract references (papers/notes with DOIs) from a note *)
let note_references entries default_author note =
  let refs = ref [] in

  (* Helper to format author name: extract last name from full name *)
  let format_author_last name =
    let parts = String.split_on_char ' ' name in
    List.nth parts (List.length parts - 1)
  in

  (* Helper to format a citation *)
  let format_citation ~authors ~year ~title ~publisher =
    let author_str = match authors with
      | [] -> ""
      | [author] -> format_author_last author ^ " "
      | author :: _ -> (format_author_last author) ^ " et al "
    in
    let pub_str = match publisher with
      | None | Some "" -> ""
      | Some p -> p ^ ". "
    in
    Printf.sprintf "%s(%d). %s. %s" author_str year title pub_str
  in

  (* Check slug_ent if it exists *)
  (match Note.slug_ent note with
   | Some slug ->
     (match Entry.lookup entries slug with
      | Some (`Paper p) ->
        (match Paper.doi p with
         | Some doi ->
           let authors = Paper.authors p in
           let year = Paper.year p in
           let title = Paper.title p in
           let publisher = Some (Paper.publisher p) in
           let citation = format_citation ~authors ~year ~title ~publisher in
           refs := (doi, citation, Paper) :: !refs
         | None -> ())
      | Some (`Note n) ->
        (match Note.doi n with
         | Some doi ->
           let authors = match Note.author n with
             | Some a -> [a]
             | None -> [Contact.name default_author]
           in
           let (year, _, _) = Note.date n in
           let title = Note.title n in
           let publisher = None in
           let citation = format_citation ~authors ~year ~title ~publisher in
           refs := (doi, citation, Note) :: !refs
         | None -> ())
      | _ -> ())
   | None -> ());

  (* Scan body for bushel references *)
  let slugs = scan_for_slugs entries (Note.body note) in
  List.iter (fun slug ->
    (* Strip leading : or @ from slug before lookup *)
    let normalized_slug = strip_handle slug in
    match Entry.lookup entries normalized_slug with
    | Some (`Paper p) ->
      (match Paper.doi p with
       | Some doi ->
         let authors = Paper.authors p in
         let year = Paper.year p in
         let title = Paper.title p in
         let publisher = Some (Paper.publisher p) in
         let citation = format_citation ~authors ~year ~title ~publisher in
         (* Check if doi already exists in refs *)
         if not (List.exists (fun (d, _, _) -> d = doi) !refs) then
           refs := (doi, citation, Paper) :: !refs
       | None -> ())
    | Some (`Note n) ->
      (match Note.doi n with
       | Some doi ->
         let authors = match Note.author n with
           | Some a -> [a]
           | None -> [Contact.name default_author]
         in
         let (year, _, _) = Note.date n in
         let title = Note.title n in
         let publisher = None in
         let citation = format_citation ~authors ~year ~title ~publisher in
         (* Check if doi already exists in refs *)
         if not (List.exists (fun (d, _, _) -> d = doi) !refs) then
           refs := (doi, citation, Note) :: !refs
       | None -> ())
    | _ -> ()
  ) slugs;

  (* Scan body for external DOI URLs and resolve from cache *)
  let body = Note.body note in
  let doi_url_pattern = Re.Perl.compile_pat "https?://(?:dx\\.)?doi\\.org/([^)\\s\"'>]+)" in
  let matches = Re.all doi_url_pattern body in
  let doi_entries = Entry.doi_entries entries in
  List.iter (fun group ->
    try
      let encoded_doi = Re.Group.get group 1 in
      (* URL decode the DOI *)
      let doi = Uri.pct_decode encoded_doi in
      (* Check if doi already exists in refs *)
      if not (List.exists (fun (d, _, _) -> d = doi) !refs) then
        (* Look up in DOI cache *)
        match Doi_entry.find_by_doi doi_entries doi with
        | Some doi_entry when doi_entry.status = Resolved ->
          let citation = format_citation
            ~authors:doi_entry.authors
            ~year:doi_entry.year
            ~title:doi_entry.title
            ~publisher:(Some doi_entry.publisher)
          in
          refs := (doi, citation, External) :: !refs
        | _ ->
          (* Not found in cache, add minimal citation with just the DOI *)
          refs := (doi, doi, External) :: !refs
    with _ -> ()
  ) matches;

  (* Scan body for publisher URLs (Elsevier, ScienceDirect, IEEE, Nature, ACM, Sage, UPenn, Springer, Taylor & Francis, OUP, arXiv) and resolve from cache *)
  let publisher_pattern = Re.Perl.compile_pat "https?://(?:(?:www\\.)?(?:linkinghub\\.elsevier\\.com|(?:www\\.)?sciencedirect\\.com/science/article|ieeexplore\\.ieee\\.org|academic\\.oup\\.com|nature\\.com|journals\\.sagepub\\.com|garfield\\.library\\.upenn\\.edu|link\\.springer\\.com|arxiv\\.org/abs)/[^)\\s\"'>]+|(?:dl\\.acm\\.org|(?:www\\.)?tandfonline\\.com)/doi(?:/pdf)?/10\\.[^)\\s\"'>]+)" in
  let publisher_matches = Re.all publisher_pattern body in
  List.iter (fun group ->
    try
      let url = Re.Group.get group 0 in
      (* Look up in DOI cache by source URL *)
      match Doi_entry.find_by_url doi_entries url with
      | Some doi_entry when doi_entry.status = Resolved ->
        let doi = doi_entry.doi in
        (* Check if this DOI already exists in refs *)
        if not (List.exists (fun (d, _, _) -> d = doi) !refs) then
          let citation = format_citation
            ~authors:doi_entry.authors
            ~year:doi_entry.year
            ~title:doi_entry.title
            ~publisher:(Some doi_entry.publisher)
          in
          refs := (doi, citation, External) :: !refs
      | _ ->
        (* Not found in cache, skip it *)
        ()
    with _ -> ()
  ) publisher_matches;

  (* Filter out the note's own DOI from references *)
  let own_doi = Note.doi note in
  let filtered_refs = List.filter (fun (doi, _, _) ->
    match own_doi with
    | Some own -> doi <> own
    | None -> true
  ) !refs in
  List.rev filtered_refs
;;

