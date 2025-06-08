type karakeep_id = {
  remote_url : string;
  id : string;
}

type t = {
  url : string;
  date : Ptime.date;
  description : string;
  metadata : (string * string) list;
  karakeep_id : karakeep_id option;
  bushel_slugs : string list;
}

type ts = t list

let url { url; _ } = url
let date { date; _ } = date
let description { description; _ } = description
let datetime v = Option.get @@ Ptime.of_date @@ date v
let compare a b = Ptime.compare (datetime b) (datetime a)

(* Convert YAML to Link.t *)
let t_of_yaml = function
  | `O fields ->
    let url =
      match List.assoc_opt "url" fields with
      | Some (`String v) -> v
      | _ -> failwith "link: missing or invalid url"
    in
    let date = 
      match List.assoc_opt "date" fields with
      | Some (`String v) ->  begin
          try
            match Scanf.sscanf v "%04d-%02d-%02d" (fun y m d -> (y, m, d)) with
            | (y, m, d) -> (y, m, d)
          with _ ->
            (* Fall back to RFC3339 parsing for backward compatibility *)
            v |> Ptime.of_rfc3339 |> Result.get_ok |> fun (a, _, _) -> Ptime.to_date a
      end
      | _ -> failwith "link: missing or invalid date"
    in
    let description =
      match List.assoc_opt "description" fields with
      | Some (`String v) -> v
      | _ -> ""
    in
    let metadata =
      match List.assoc_opt "metadata" fields with
      | Some (`O meta_fields) -> 
          List.fold_left (fun acc (k, v) ->
            match v with
            | `String value -> (k, value) :: acc
            | _ -> acc
          ) [] meta_fields
      | _ -> []
    in
    let karakeep_id =
      match List.assoc_opt "karakeep_id" fields with
      | Some (`O ki_fields) ->
          let remote_url = 
            match List.assoc_opt "remote_url" ki_fields with
            | Some (`String v) -> v
            | _ -> failwith "link: invalid karakeep_id.remote_url"
          in
          let id = 
            match List.assoc_opt "id" ki_fields with
            | Some (`String v) -> v
            | _ -> failwith "link: invalid karakeep_id.id"
          in
          Some { remote_url; id }
      | _ -> None
    in
    let bushel_slugs =
      match List.assoc_opt "bushel_slugs" fields with
      | Some (`A items) -> 
          List.fold_left (fun acc item ->
            match item with
            | `String slug -> slug :: acc
            | _ -> acc
          ) [] items
          |> List.rev
      | _ -> 
          (* For backward compatibility, check for a bushel_slug in metadata *)
          match List.assoc_opt "bushel_slug" metadata with
          | Some slug -> [slug]
          | None -> []
    in
    { url; date; description; metadata; karakeep_id; bushel_slugs }
  | _ -> failwith "invalid yaml"

(* Read file contents *)
let read_file file = In_channel.(with_open_bin file input_all)

(* Load links from a YAML file *)
let of_md fname =
  match Yaml.of_string_exn (read_file fname) with
  | `A links -> 
      List.map t_of_yaml links
  | `O _ as single_link -> 
      [t_of_yaml single_link]
  | _ -> failwith "link_of_md: expected array or object"

(* Convert Link.t to YAML *)
let to_yaml t =
  let (year, month, day) = t.date in
  let date_str = Printf.sprintf "%04d-%02d-%02d" year month day in
  
  (* Create base fields *)
  let base_fields = [
    ("url", `String t.url);
    ("date", `String date_str);
  ] @ 
  (if t.description = "" then [] else [("description", `String t.description)])
  in
  
  (* Add metadata if present *)
  let metadata_fields = 
    if t.metadata = [] then []
    else [("metadata", `O (List.map (fun (k, v) -> (k, `String v)) t.metadata))]
  in
  
  (* Add karakeep_id if present *)
  let karakeep_fields =
    match t.karakeep_id with
    | Some { remote_url; id } -> 
        [("karakeep_id", `O [
          ("remote_url", `String remote_url);
          ("id", `String id)
        ])]
    | None -> []
  in
  
  (* Add bushel_slugs if present *)
  let bushel_slugs_fields =
    if t.bushel_slugs = [] then []
    else [("bushel_slugs", `A (List.map (fun slug -> `String slug) t.bushel_slugs))]
  in
  
  `O (base_fields @ metadata_fields @ karakeep_fields @ bushel_slugs_fields)

(* Write a link to a file in the output directory *)
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

(* Load links from a YAML file *)
let load_links_file path =
  try
    let yaml_str = In_channel.(with_open_bin path input_all) in
    match Yaml.of_string_exn yaml_str with
    | `A links -> List.map t_of_yaml links
    | _ -> []
  with _ -> []

(* Save links to a YAML file *)
let save_links_file path links =
  let yaml = `A (List.map to_yaml links) in
  let yaml_str = Yaml.to_string_exn ~len:1200000 yaml in
  let oc = open_out path in
  output_string oc yaml_str;
  close_out oc

(* Merge two lists of links, combining metadata from duplicates *)
let merge_links existing new_links =
  let links_by_url = Hashtbl.create (List.length existing) in
  
  (* Add existing links to hashtable *)
  List.iter (fun link -> 
    Hashtbl.replace links_by_url link.url link
  ) existing;
  
  (* Merge new links with existing ones *)
  List.iter (fun new_link ->
    match Hashtbl.find_opt links_by_url new_link.url with
    | None -> 
        (* New link not in existing links *)
        Hashtbl.add links_by_url new_link.url new_link
    | Some old_link ->
        (* Merge link data, prefer newer data for fields *)
        let title = 
          if new_link.description <> "" then new_link.description 
          else old_link.description 
        in
        
        (* Combine karakeep_id (prefer new over old) *)
        let karakeep_id = 
          match new_link.karakeep_id with
          | Some _ -> new_link.karakeep_id
          | None -> old_link.karakeep_id
        in
        
        (* Combine bushel_slugs (remove duplicates) *)
        let bushel_slugs = 
          List.sort_uniq String.compare (old_link.bushel_slugs @ new_link.bushel_slugs)
        in
        
        (* Combined link *)
        let merged_link = {
          url = new_link.url;
          date = (if compare new_link old_link > 0 then new_link.date else old_link.date);
          description = title;
          metadata = old_link.metadata @ new_link.metadata; (* might need deduplication *)
          karakeep_id;
          bushel_slugs
        } in
        Hashtbl.replace links_by_url new_link.url merged_link
  ) new_links;
  
  (* Convert hashtable back to list and sort by date *)
  Hashtbl.to_seq_values links_by_url 
  |> List.of_seq
  |> List.sort compare