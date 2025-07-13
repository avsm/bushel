type t =
  { slug : string
  ; title : string
  ; start : int
  ; finish : int option
  ; tags : string list
  ; ideas : string
  ; body : string
  }

type ts = t list

val title : t -> string
val body : t -> string
val ideas : t -> string
val lookup : t list -> string -> t option
val tags : t -> string list
val compare : t -> t -> int
val of_md : string -> t
val typesense_schema : Ezjsonm.value
