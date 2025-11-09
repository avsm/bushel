(** DOI entries resolved from external sources via Zotero Translation Server *)

type status =
  | Resolved  (** Successfully resolved from Zotero *)
  | Failed of string  (** Failed to resolve, with error message *)

type t = {
  doi: string;
  title: string;
  authors: string list;
  year: int;
  bibtype: string;  (** article, inproceedings, book, etc *)
  publisher: string;  (** journal/conference/publisher name *)
  resolved_at: string;  (** ISO date when resolved *)
  status: status;
}

type ts = t list

(** Load DOI entries from YAML file *)
val load : string -> ts

(** Save DOI entries to YAML file *)
val save : string -> ts -> unit

(** Convert list to hashtable for fast lookup by DOI *)
val to_map : ts -> (string, t) Hashtbl.t

(** Find entry by DOI *)
val find_by_doi : ts -> string -> t option

(** Create a new resolved entry *)
val create_resolved : doi:string -> title:string -> authors:string list ->
  year:int -> bibtype:string -> publisher:string -> t

(** Create a new failed entry *)
val create_failed : doi:string -> error:string -> t
