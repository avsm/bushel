type t = {
  url : string;
  date : Ptime.date;
  description : string;
}

type ts = t list

let url { url; _ } = url
let date { date; _ } = date
let description { description; _ } = description
let datetime v = Option.get @@ Ptime.of_date @@ date v
let compare a b = Ptime.compare (datetime b) (datetime a)

let t_of_yaml = function
  | `O fields ->
    let url =
      match List.assoc_opt "url" fields with
      | Some (`String v) -> v
      | _ -> failwith "link: missing or invalid url"
    in
    let date =
      match List.assoc_opt "date" fields with
      | Some (`String v) -> 
          v |> Ptime.of_rfc3339 |> Result.get_ok |> fun (a, _, _) -> Ptime.to_date a
      | _ -> failwith "link: missing or invalid date"
    in
    let description =
      match List.assoc_opt "description" fields with
      | Some (`String v) -> v
      | _ -> ""
    in
    { url; date; description }
  | _ -> failwith "invalid yaml"

let read_file file = In_channel.(with_open_bin file input_all)

let of_md fname =
  match Yaml.of_string_exn (read_file fname) with
  | `A links -> 
      List.map t_of_yaml links
  | `O _ as single_link -> 
      [t_of_yaml single_link]
  | _ -> failwith "link_of_md: expected array or object"

let to_yaml t =
  `O [
    ("url", `String t.url);
    ("date", `String (Ptime.to_rfc3339 (Option.get @@ Ptime.of_date t.date)));
    ("description", `String t.description);
  ]

let to_file output_dir t =
  let filename = 
    let (y, m, d) = t.date in
    let hash = Digest.string t.url |> Digest.to_hex in
    let short_hash = String.sub hash 0 8 in
    Printf.sprintf "%04d-%02d-%02d-%s.md" y m d short_hash
  in
  let file_path = Fpath.v (Filename.concat output_dir filename) in
  let yaml = to_yaml t in
  let yaml_str = Yaml.to_string_exn yaml in
  let content = "---\n" ^ yaml_str ^ "---\n" in
  Bos.OS.File.write file_path content