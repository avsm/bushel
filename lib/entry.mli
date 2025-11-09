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
val doi_entries : t -> Doi_entry.ts
val data_dir : t -> string

val v
  :  papers:Paper.t list
  -> notes:Note.ts
  -> projects:Project.ts
  -> ideas:Idea.ts
  -> videos:Video.ts
  -> contacts:Contact.ts
  -> images:Srcsetter.ts
  -> data_dir:string
  -> t

val lookup : t -> string -> entry option
val lookup_exn : t -> string -> entry
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

val is_index_entry : entry -> bool
val notes_for_slug : t -> string -> Note.t list
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

(** Get thumbnail URL for a note with slug_ent *)
val thumbnail_note_with_ent : t -> Note.t -> string option
