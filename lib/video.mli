type t =
  { slug : string
  ; title : string
  ; published_date : Ptime.t
  ; uuid : string
  ; description : string
  ; url : string
  ; talk : bool
  ; paper : string option
  ; project : string option
  ; tags : string list
  }

type ts = t list

val compare : t -> t -> int
val url : t -> string
val body : t -> string
val title : t -> string
val uuid : t -> string
val paper : t -> string option
val project : t -> string option
val slug : t -> string
val date : t -> Ptime.date
val datetime : t -> Ptime.t
val talk : t -> bool
val of_md : string -> t
val t_of_yaml : description:string -> Yaml.value -> t
val to_yaml : t -> Yaml.value
val to_file : string -> t -> (unit, [> `Msg of string]) result
val typesense_schema : Ezjsonm.value
val pp : Format.formatter -> t -> unit
