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

val tags : t -> string list
val compare : t -> t -> int
val title : t -> string
val body : t -> string
val ideas : t -> string
val of_file : string -> t
val lookup : t list -> string -> t option
