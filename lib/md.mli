
val bushel_inline_mapper :
  ?slugs:(string, unit) Hashtbl.t ->
  Entry.t -> 'a -> Cmarkit.Inline.t -> Cmarkit.Inline.t Cmarkit.Mapper.result
val scan_for_slugs : Entry.t -> string -> string list
