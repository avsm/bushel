module J = Ezjsonm

type status =
  | Resolved
  | Failed of string

type t = {
  doi: string;
  title: string;
  authors: string list;
  year: int;
  bibtype: string;
  publisher: string;
  resolved_at: string;
  status: status;
}

type ts = t list

let create_resolved ~doi ~title ~authors ~year ~bibtype ~publisher =
  let resolved_at =
    let now = Ptime_clock.now () in
    let rfc3339 = Ptime.to_rfc3339 ~space:false ~frac_s:0 now in
    String.sub rfc3339 0 10  (* Extract YYYY-MM-DD *)
  in
  { doi; title; authors; year; bibtype; publisher; resolved_at; status = Resolved }

let create_failed ~doi ~error =
  let resolved_at =
    let now = Ptime_clock.now () in
    let rfc3339 = Ptime.to_rfc3339 ~space:false ~frac_s:0 now in
    String.sub rfc3339 0 10  (* Extract YYYY-MM-DD *)
  in
  { doi; title = ""; authors = []; year = 0; bibtype = ""; publisher = "";
    resolved_at; status = Failed error }

let to_yaml_value entry =
  let status_field = match entry.status with
    | Resolved -> []
    | Failed err -> [("error", `String err)]
  in
  let fields = [
    ("doi", `String entry.doi);
    ("resolved_at", `String entry.resolved_at);
  ] @ status_field in
  let fields = match entry.status with
    | Resolved ->
      fields @ [
        ("title", `String entry.title);
        ("authors", `A (List.map (fun a -> `String a) entry.authors));
        ("year", `Float (float_of_int entry.year));
        ("bibtype", `String entry.bibtype);
        ("publisher", `String entry.publisher);
      ]
    | Failed _ -> fields
  in
  `O fields

let of_yaml_value v =
  try
    let doi = J.find v ["doi"] |> J.get_string in
    let resolved_at = J.find v ["resolved_at"] |> J.get_string in
    let error = try Some (J.find v ["error"] |> J.get_string) with _ -> None in
    match error with
    | Some err ->
      { doi; title = ""; authors = []; year = 0; bibtype = ""; publisher = "";
        resolved_at; status = Failed err }
    | None ->
      let title = J.find v ["title"] |> J.get_string in
      let authors = J.find v ["authors"] |> J.get_list J.get_string in
      let year = J.find v ["year"] |> J.get_float |> int_of_float in
      let bibtype = J.find v ["bibtype"] |> J.get_string in
      let publisher = J.find v ["publisher"] |> J.get_string in
      { doi; title; authors; year; bibtype; publisher; resolved_at; status = Resolved }
  with e ->
    Printf.eprintf "Failed to parse DOI entry: %s\n%!" (Printexc.to_string e);
    failwith "Invalid DOI entry in YAML"

let load path =
  if not (Sys.file_exists path) then
    []
  else
    try
      let yaml_str = In_channel.with_open_text path In_channel.input_all in
      match Yaml.of_string yaml_str with
      | Ok (`A entries) -> List.map of_yaml_value entries
      | Ok _ -> []
      | Error (`Msg e) ->
        Printf.eprintf "Failed to parse %s: %s\n%!" path e;
        []
    with e ->
      Printf.eprintf "Failed to load %s: %s\n%!" path (Printexc.to_string e);
      []

let save path entries =
  let yaml_list = `A (List.map to_yaml_value entries) in
  let yaml_str = Yaml.to_string_exn yaml_list in
  Out_channel.with_open_text path (fun oc ->
    Out_channel.output_string oc yaml_str
  )

let to_map entries =
  let map = Hashtbl.create (List.length entries) in
  List.iter (fun entry -> Hashtbl.add map entry.doi entry) entries;
  map

let find_by_doi entries doi =
  List.find_opt (fun entry -> entry.doi = doi) entries
