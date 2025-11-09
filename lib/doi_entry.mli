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
  source_urls: string list;  (** All URLs that resolve to this DOI (publisher links, doi.org URLs, etc) *)
  status: status;
  ignore: bool;  (** If true, skip this entry when looking up references *)
}

type ts = t list

(** Load DOI entries from YAML file *)
val load : string -> ts

(** Save DOI entries to YAML file *)
val save : string -> ts -> unit

(** Convert list to hashtable for fast lookup by DOI *)
val to_map : ts -> (string, t) Hashtbl.t

(** Find entry by DOI (excludes ignored entries) *)
val find_by_doi : ts -> string -> t option

(** Find entry by source URL (searches through all source_urls, excludes ignored entries) *)
val find_by_url : ts -> string -> t option

(** Find entry by DOI including ignored entries (for resolution checks) *)
val find_by_doi_including_ignored : ts -> string -> t option

(** Find entry by source URL including ignored entries (for resolution checks) *)
val find_by_url_including_ignored : ts -> string -> t option

(** Create a new resolved entry *)
val create_resolved : doi:string -> title:string -> authors:string list ->
  year:int -> bibtype:string -> publisher:string -> ?source_urls:string list -> unit -> t

(** Create a new failed entry *)
val create_failed : doi:string -> error:string -> ?source_urls:string list -> unit -> t

(** Merge two entries with the same DOI, combining their source_urls *)
val merge_entries : t -> t -> t
