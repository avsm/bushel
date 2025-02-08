type entry =
  [ `Idea of Idea.t
  | `Note of Note.t
  | `Paper of Paper.t
  | `Project of Project.t
  | `Video of Video.t
  ]

type slugs = (string, entry) Hashtbl.t
type t

val contacts : t -> Contact.ts
val videos : t -> Video.ts
val ideas : t -> Idea.ts
val papers : t -> Paper.ts
val notes : t -> Note.ts
val projects : t -> Project.ts
val images : t -> Srcsetter.ts

val v
  :  papers:Paper.t list
  -> notes:Note.ts
  -> projects:Project.ts
  -> ideas:Idea.ts
  -> videos:Video.ts
  -> news:News.ts
  -> contacts:Contact.ts
  -> images:Srcsetter.ts
  -> t

val lookup : t -> string -> entry option
val lookup_exn : t -> string -> entry
val lookup_news : t -> string -> News.t option
val lookup_by_name : t -> string -> Contact.t option
val old_papers : t -> Paper.ts
val sidebar : [> `Note of Note.t ] -> string option
val to_type_string : entry -> string
val slug : entry -> string
val title : entry -> string
val body : entry -> string

(* FIXME move to view *)
val site_url : entry -> string
val date : entry -> Ptime.date
val datetime : entry -> Ptime.t
val year : entry -> int
val synopsis : entry -> string option

(* FIXME move to separate module *)
type feed =
  [ `Entry of entry
  | `News of News.t * entry
  ]

val feed_date : feed -> Ptime.date
val feed_datetime : feed -> Ptime.t
val feed_title : feed -> string
val feed_url : feed -> string
val feed_compare : feed -> feed -> int
val is_index_entry : entry -> bool
val news_for_slug : t -> string -> News.t list
val news_for_tag : t -> string -> News.t list
val all_entries : t -> entry list
val all_papers : t -> entry list
val compare : entry -> entry -> int
