type t = {
  url : string;
  date : Ptime.date;
  description : string;
  metadata : (string * string) list;
}

type ts = t list

val compare : t -> t -> int
val url : t -> string
val date : t -> Ptime.date
val datetime : t -> Ptime.t
val description : t -> string
val of_md : string -> ts
val to_yaml : t -> Yaml.value
val t_of_yaml : Yaml.value -> t
val to_file : string -> t -> (unit, [> `Msg of string]) result