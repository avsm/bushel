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
val news : t -> News.ts
val data_dir : t -> string

val v
  :  papers:Paper.t list
  -> notes:Note.ts
  -> projects:Project.ts
  -> ideas:Idea.ts
  -> videos:Video.ts
  -> news:News.ts
  -> contacts:Contact.ts
  -> images:Srcsetter.ts
  -> data_dir:string
  -> t

val lookup : t -> string -> entry option
val lookup_exn : t -> string -> entry
val lookup_news : t -> string -> News.t option
val lookup_site_url : t -> string -> string
val lookup_title : t -> string -> string
val lookup_by_name : t -> string -> Contact.t option
val old_papers : t -> Paper.ts
val sidebar : [> `Note of Note.t ] -> string option
val to_type_string : entry -> string
val slug : entry -> string
val title : entry -> string
val body : entry -> string
val extract_external_links : string -> string list
val outgoing_links : entry -> string list

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

(** Look up an image in the srcsetter list by slug *)
val lookup_image : t -> string -> Srcsetter.t option

(** Get the smallest webp variant from a srcsetter image *)
val smallest_webp_variant : Srcsetter.t -> string

(** Get thumbnail slug for a contact *)
val contact_thumbnail_slug : Contact.t -> string option

(** Get thumbnail URL for a contact - resolved through srcsetter *)
val contact_thumbnail : t -> Contact.t -> string option

(** Get thumbnail slug for an entry with fallbacks *)
val thumbnail_slug : t -> entry -> string option

(** Get thumbnail URL for an entry with fallbacks - resolved through srcsetter *)
val thumbnail : t -> entry -> string option

(** Get thumbnail URL for a news entry *)
val thumbnail_news : t -> News.t -> string option
