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
