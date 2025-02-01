module MS = Map.Make (String)

type t =
  { name : string
  ; slug : string
  ; origin : string
  ; dims : int * int
  ; variants : (int * int) MS.t
  }

let v name slug origin variants dims = { name; slug; origin; variants; dims }
let slug { slug; _ } = slug
let origin { origin; _ } = origin
let name { name; _ } = name
let dims { dims; _ } = dims
let variants { variants; _ } = variants

let dims_json_t =
  let open Jsont in
  let dec x y = x, y in
  let enc (w, h) = function
    | 0 -> w
    | _ -> h
  in
  t2 ~dec ~enc uint16
;;

let json_t =
  let open Jsont in
  let open Jsont.Object in
  map ~kind:"Entry" v
  |> mem "name" string ~enc:name
  |> mem "slug" string ~enc:slug
  |> mem "origin" string ~enc:origin
  |> mem "variants" (as_string_map dims_json_t) ~enc:variants
  |> mem "dims" dims_json_t ~enc:dims
  |> finish
;;

let list = Jsont.list json_t
let list_to_json es = Jsont_bytesrw.encode_string list ~format:Jsont.Indent es
let list_of_json = Jsont_bytesrw.decode_string list
