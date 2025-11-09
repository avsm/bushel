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

(** Validate all bushel references in markdown and return broken ones.
    Returns (broken_slugs, broken_contacts) where each list contains
    the full reference string (e.g., ":missing-slug", "@unknown-handle") *)
val validate_references : Entry.t -> string -> string list * string list

(** Extract the first image URL from markdown text *)
val extract_first_image : string -> string option

(** Convert markdown text to plain text, resolving bushel links to just their text *)
val markdown_to_plaintext : 'a -> string -> string

val is_bushel_slug : string -> bool
val is_tag_slug : string -> bool
val is_type_filter_slug : string -> bool
val is_contact_slug : string -> bool
val strip_handle : string -> string

(** Extract all links from markdown text, including from images (internal and external) *)
val extract_all_links : string -> string list

(** Extract references (papers/notes with DOIs) from a note.
    Returns a list of (DOI, citation_string, is_paper) tuples where is_paper
    indicates if the reference is to a paper (true) or a note (false).
    Citation format: "Last, First (Year). Title. Publisher. https://doi.org/the/doi" *)
val note_references : Entry.t -> Contact.t -> Note.t -> (string * string * bool) list
