type t = {
  contents : string;
}

let t =
  let open Irmin.Type in
  record "post" (fun contents ->
    { contents }
  )
  |+ field "contents" string (fun t -> t.contents)
  |> sealr
