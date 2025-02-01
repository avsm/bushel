type t = {
  slug : string;
  title : string;
  published_date : Ptime.t;
  uuid : string;
  description : string;
  url : string;
  talk : bool;
  paper : string option;
  project : string option;
  tags : string list;
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
