type paper

type t =
  { slug : string
  ; ver : string
  ; paper : paper
  ; abstract : string
  ; latest : bool
  }

type ts = t list

val of_md : slug:string -> ver:string -> string -> t
val tv : t list -> ts
val slug : t -> string
val title : t -> string
val authors : t -> string list
val project_slugs : t -> string list
val slides : t -> string list
val bibtype : t -> string
val journal : t -> string
val doi : t -> string option
val volume : t -> string option
val video : t -> string option
val issue : t -> string option
val url : t -> string option
val best_url : t -> string option
val pages : t -> string
val abstract : t -> string
val institution : t -> string
val number : t -> string option
val editor : t -> string
val isbn : t -> string
val bib : t -> string
val year : t -> int
val publisher : t -> string
val booktitle : t -> string
val tags : t -> string list
val date : t -> int * int * int
val datetime : t -> Ptime.t
val compare : t -> t -> int
val get_papers : slug:string -> ts -> ts
val slugs : ts -> string list
val lookup : ts -> string -> t option
