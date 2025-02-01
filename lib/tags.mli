type t =
  [ `Contact of string
  | `Set of string
  | `Slug of string
  | `Text of string
  | `Year of int
  ]

val is_text : t -> bool
val is_set : t -> bool
val is_slug : t -> bool
val is_year : t -> bool
val of_string : string -> t
val to_string : t -> string
val to_raw_string : t -> string
val pp : Format.formatter -> t -> unit
val mention_entries : Entry.t -> t list -> Entry.entry list
val tags_of_ent : Entry.t -> Entry.entry -> t list
val tags_of_news : News.t -> t list
val mentions : t list -> t list

val count_tags
  :  ?h:('a, int) Hashtbl.t
  -> ('b -> 'a list)
  -> 'b list
  -> ('a, int) Hashtbl.t
