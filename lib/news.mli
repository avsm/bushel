type t = {
  slug : string;
  slug_ent : string;
  date : Ptime.date;
  tags : string list;
  title : string;
  body : string;
}
type ts = t list
val slug : t -> string
val slug_ent : t -> string
val tags : t -> string list
val title : t -> string
val date : t -> Ptime.date
val body : t -> string
val site_url : t -> string
val datetime : t -> Ptime.t
val compare : t -> t -> int

val of_md : string -> t
