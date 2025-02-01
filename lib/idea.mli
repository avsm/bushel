type level = Any | PartII | MPhil | PhD | Postdoc
type status = Available | Discussion | Ongoing | Completed

val level_of_yaml : Ezjsonm.value -> (level, [> `Msg of string ]) result

val level_to_string : level -> string
val level_to_tag : level -> string
val level_to_yaml : level -> Ezjsonm.value
val status_of_yaml : Ezjsonm.value -> (status, [> `Msg of string ]) result
val status_to_string : status -> string
val status_to_tag : status -> string
val status_to_yaml : status -> Ezjsonm.value

type t = {
  slug : string;
  title : string;
  level : level;
  project : string;
  status : status;
  year : int;
  supervisors : string list;
  students : string list;
  reading : string;
  body : string;
  url : string option;
  tags : string list;
}

type ts = t list
val title : t -> string
val supervisors : t -> string list
val students : t -> string list
val reading : t -> string
val status : t -> status
val level : t -> level
val year : t -> int
val body : t -> string
val project : t -> string
val compare : t -> t -> int
val lookup : t list -> string -> t option

val of_file : string -> t
