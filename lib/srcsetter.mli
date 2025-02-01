module MS : Map.S with type key = string

type t =
  { name : string
  ; slug : string
  ; origin : string
  ; dims : int * int
  ; variants : (int * int) MS.t
  }

val v : string -> string -> string -> (int * int) MS.t -> int * int -> t
val origin : t -> string
val slug : t -> string
val name : t -> string
val dims : t -> int * int
val variants : t -> (int * int) MS.t
val list_to_json : t list -> (string, string) result
val list_of_json : string -> (t list, string) result
val json_t : t Jsont.t
val list : t list Jsont.t
