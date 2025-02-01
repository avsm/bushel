type t = {
  title : string;
  date : Ptime.date;
  slug : string;
  body : string;
  tags : string list;
  updated : Ptime.date option;
  sidebar : string option;
  index_page : bool;
  via : (string * string) option;
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
val tags : t -> string list
val sidebar : t -> string option
val lookup : string -> t list -> t
val words : t -> int
val of_md : string -> t
