type t =
  { title : string
  ; date : Ptime.date
  ; slug : string
  ; body : string
  ; tags : string list
  ; draft : bool
  ; updated : Ptime.date option
  ; sidebar : string option
  ; index_page : bool
  ; synopsis: string option
  ; titleimage: string option
  ; via : (string * string) option
  ; slug_ent : string option
  ; source : string option
  ; url : string option
  ; author : string option
  ; category : string option
  }

type ts = t list

val link : t -> [> `Ext of string * string | `Local of string ]
val origdate : t -> Ptime.t
val date : t -> Ptime.date
val datetime : t -> Ptime.t
val compare : t -> t -> int
val slug : t -> string
val body : t -> string
val title : t -> string
val draft : t -> bool
val synopsis : t -> string option
val titleimage : t -> string option
val slug_ent : t -> string option
val source : t -> string option
val url : t -> string option
val author : t -> string option
val category : t -> string option
val tags : t -> string list
val sidebar : t -> string option
val lookup : string -> t list -> t
val words : t -> int
val of_md : string -> t
val typesense_schema : Ezjsonm.value
val pp : Format.formatter -> t -> unit
