(** Bushel mappers for our Markdown extensions *)

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
let is_tag_slug = String.starts_with ~prefix:"##"

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
       let sh = strip_handle url in
       let uri = Uri.(make ~path:"/news" ~query:[ "t", [ sh ] ] () |> to_string) in
       Some (uri, Inline.Link.text lb |> text_of_inline)
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
  let dest =
    match Entry.lookup entries (strip_handle slug) with
    | Some ent -> Entry.site_url ent
    | None -> slug
  in
  let txt = Inline.Text (title, meta) in
  let ld = Link_definition.make ~dest:(dest, meta) () in
  let ll = `Inline (ld, meta) in
  let ld = Inline.Link.make txt ll in
  let ent_il = Inline.Link (ld, meta) in
  Mapper.ret ent_il
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

let rewrite_label_reference ?slugs entries lb meta =
  let open Cmarkit in
  match Inline.Link.referenced_label lb with
  | None -> Mapper.default
  | Some l ->
    let m = Label.meta l in
    (match Meta.find authorlink m with
     | Some () ->
       let slug = Label.key l in
       let target, dest =
         match Contact.find_by_handle (Entry.contacts entries) (strip_handle slug) with
         | Some c ->
           ( Contact.name c
           , Printf.sprintf
               "bushel:contact:%s:%s"
               (strip_handle slug)
               (Contact.best_url c |> Option.value ~default:"") )
         | None -> "Unknown Person", ""
       in
       let txt = Inline.Text (target, meta) in
       let ld = Link_definition.make ~dest:(dest, meta) () in
       let ll = `Inline (ld, meta) in
       let ld = Inline.Link.make txt ll in
       let author_il = Inline.Link (ld, meta) in
       Mapper.ret author_il
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
            let target, dest =
              match Entry.lookup entries (strip_handle slug) with
              | Some ent -> Entry.title ent, Entry.site_url ent
              | None -> "Unknown Article", ""
            in
            let txt = Inline.Text (target, meta) in
            let ld = Link_definition.make ~dest:(dest, meta) () in
            let ll = `Inline (ld, meta) in
            let ld = Inline.Link.make txt ll in
            let ent_il = Inline.Link (ld, meta) in
            Mapper.ret ent_il)
          else if is_tag_slug slug
          then (
            let sh = strip_handle slug in
            let target, dest =
              sh, Uri.(make ~path:"/news" ~query:[ "t", [ sh ] ] () |> to_string)
            in
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

let bushel_inline_mapper ?slugs entries _m =
  let open Cmarkit in
  function
  | Inline.Link (lb, meta) ->
    (match link_target_is_bushel ?slugs lb with
     | None -> rewrite_label_reference ?slugs entries lb meta
     | Some (url, title) -> rewrite_bushel_link_reference entries url title meta)
  | Inline.Image (lb, meta) ->
    (match image_target_is_bushel lb with
     | None -> rewrite_label_reference entries lb meta
     | Some (url, alt, dir) -> rewrite_bushel_image_reference entries url alt dir meta)
  | _ -> Mapper.default
;;

let scan_for_slugs entries md =
  let open Cmarkit in
  let slugs = Hashtbl.create 7 in
  let _ =
    Doc.of_string ~strict:false ~resolver:with_bushel_links md
    |> Mapper.map_doc (Mapper.make ~inline:(bushel_inline_mapper ~slugs entries) ())
  in
  Hashtbl.fold (fun k () a -> k :: a) slugs []
;;
