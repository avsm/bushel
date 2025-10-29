val make_bushel_inline_mapper
  :  ?slugs:(string, unit) Hashtbl.t
  -> ?footnote_map:(string, string * string) Hashtbl.t
  -> Cmarkit.Label.defs
  -> Entry.t
  -> 'a
  -> Cmarkit.Inline.t
  -> Cmarkit.Inline.t Cmarkit.Mapper.result

val make_bushel_link_only_mapper
  :  Cmarkit.Label.defs
  -> Entry.t
  -> 'a
  -> Cmarkit.Inline.t
  -> Cmarkit.Inline.t Cmarkit.Mapper.result

type Cmarkit.Inline.t += Obsidian_link of string

type sidenote_data =
  | Contact_note of Contact.t * string
  | Paper_note of Paper.t * string
  | Idea_note of Idea.t * string
  | Note_note of Note.t * string
  | Project_note of Project.t * string
  | Video_note of Video.t * string
  | Footnote_note of string * Cmarkit.Block.t * string

type Cmarkit.Inline.t += Side_note of sidenote_data

val bushel_inline_mapper_to_obsidian
  :  Entry.t
  -> 'a
  -> Cmarkit.Inline.t
  -> Cmarkit.Inline.t Cmarkit.Mapper.result

val with_bushel_links
  :  [< `Def of Cmarkit.Label.t option * Cmarkit.Label.t
     | `Ref of 'a * Cmarkit.Label.t * Cmarkit.Label.t option
     ]
  -> Cmarkit.Label.t option

val scan_for_slugs : Entry.t -> string -> string list


val is_bushel_slug : string -> bool
val is_tag_slug : string -> bool
