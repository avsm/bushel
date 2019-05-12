type t = string list list

let t =
  let open Irmin.Type in
  list (list string)
