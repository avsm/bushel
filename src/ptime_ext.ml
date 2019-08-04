include Ptime

let failwithf = Format.kasprintf failwith

let of_rfc3339_exn s = 
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error (`RFC3339 (_, err)) ->
      failwithf "%a" Ptime.pp_rfc3339_error err

let of_json_exn json =
  match of_rfc3339 (Yojson.Basic.Util.to_string json) with
  | Ok (time, _, _) -> time
  | Error (`RFC3339 (_, err)) -> failwithf "%a" Ptime.pp_rfc3339_error err

let t = 
  Irmin.Type.(map string) of_rfc3339_exn to_rfc3339
