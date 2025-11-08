type t
type ts = t list

val v : string -> (ts, string) result
val names : t -> string list
val name : t -> string
val handle : t -> string
val email : t -> string option
val icon : t -> string option
val github : t -> string option
val twitter : t -> string option
val bluesky : t -> string option
val mastodon : t -> string option
val orcid : t -> string option
val url : t -> string option
val atom : t -> string list option
val best_url : t -> string option
val find_by_handle : t list -> string -> t option
val handle_of_name : string -> string
val lookup_by_name : ts -> string -> t
val json_t : t Jsont.t
val compare : t -> t -> int
val of_md : string -> t
val typesense_schema : Ezjsonm.value
val pp : Format.formatter -> t -> unit
