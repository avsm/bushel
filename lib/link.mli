type karakeep_id = {
  remote_url : string;
  id : string;
}

type t = {
  url : string;
  date : Ptime.date;
  description : string;
  metadata : (string * string) list;
  karakeep_id : karakeep_id option;
  bushel_slugs : string list;
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
val load_links_file : string -> ts
val save_links_file : string -> ts -> unit
val merge_links : ts -> ts -> ts