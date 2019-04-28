type t = {
  updated_at : string;
}

let t =
  let open Irmin.Type in
  record "repository" (fun updated_at ->
    { updated_at }
  )
  |+ field "updated_at" string (fun t -> t.updated_at)
  |> sealr

let pp = Irmin.Type.pp_json

let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    Irmin.Type.decode_json t decoder

let merge = Irmin.Merge.(option (idempotent t))
