val bushel_inline_mapper
  :  ?slugs:(string, unit) Hashtbl.t
  -> Entry.t
  -> 'a
  -> Cmarkit.Inline.t
  -> Cmarkit.Inline.t Cmarkit.Mapper.result

type Cmarkit.Inline.t += Obsidian_link of string

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
