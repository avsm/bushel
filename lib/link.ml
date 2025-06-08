type karakeep_data = {
  remote_url : string;
  id : string;
  tags : string list;
  metadata : (string * string) list;
}

type bushel_data = {
  slugs : string list;
  tags : string list;
}

type t = {
  url : string;
  date : Ptime.date;
  description : string;
  karakeep : karakeep_data option;
  bushel : bushel_data option;
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
    let karakeep =
      match List.assoc_opt "karakeep" fields with
      | Some (`O k_fields) ->
          let remote_url = 
            match List.assoc_opt "remote_url" k_fields with
            | Some (`String v) -> v
            | _ -> failwith "link: invalid karakeep.remote_url"
          in
          let id = 
            match List.assoc_opt "id" k_fields with
            | Some (`String v) -> v
            | _ -> failwith "link: invalid karakeep.id"
          in
          let tags =
            match List.assoc_opt "tags" k_fields with
            | Some (`A tag_list) ->
                List.fold_left (fun acc tag ->
                  match tag with
                  | `String t -> t :: acc
                  | _ -> acc
                ) [] tag_list
                |> List.rev
            | _ -> []
          in
          let metadata =
            match List.assoc_opt "metadata" k_fields with
            | Some (`O meta_fields) -> 
                List.fold_left (fun acc (k, v) ->
                  match v with
                  | `String value -> (k, value) :: acc
                  | _ -> acc
                ) [] meta_fields
            | _ -> []
          in
          Some { remote_url; id; tags; metadata }
      | _ -> None
    in
    let bushel =
      match List.assoc_opt "bushel" fields with
      | Some (`O b_fields) ->
          let slugs =
            match List.assoc_opt "slugs" b_fields with
            | Some (`A slug_list) ->
                List.fold_left (fun acc slug ->
                  match slug with
                  | `String s -> s :: acc
                  | _ -> acc
                ) [] slug_list
                |> List.rev
            | _ -> []
          in
          let tags =
            match List.assoc_opt "tags" b_fields with
            | Some (`A tag_list) ->
                List.fold_left (fun acc tag ->
                  match tag with
                  | `String t -> t :: acc
                  | _ -> acc
                ) [] tag_list
                |> List.rev
            | _ -> []
          in
          Some { slugs; tags }
      | _ -> None
    in
    { url; date; description; karakeep; bushel }
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
  
  (* Add karakeep data if present *)
  let karakeep_fields =
    match t.karakeep with
    | Some { remote_url; id; tags; metadata } -> 
        let karakeep_obj = [
          ("remote_url", `String remote_url);
          ("id", `String id);
        ] in
        let karakeep_obj = 
          if tags = [] then karakeep_obj
          else ("tags", `A (List.map (fun t -> `String t) tags)) :: karakeep_obj
        in
        let karakeep_obj =
          if metadata = [] then karakeep_obj
          else ("metadata", `O (List.map (fun (k, v) -> (k, `String v)) metadata)) :: karakeep_obj
        in
        [("karakeep", `O karakeep_obj)]
    | None -> []
  in
  
  (* Add bushel data if present *)
  let bushel_fields =
    match t.bushel with
    | Some { slugs; tags } -> 
        let bushel_obj = [] in
        let bushel_obj = 
          if slugs = [] then bushel_obj
          else ("slugs", `A (List.map (fun s -> `String s) slugs)) :: bushel_obj
        in
        let bushel_obj =
          if tags = [] then bushel_obj
          else ("tags", `A (List.map (fun t -> `String t) tags)) :: bushel_obj
        in
        if bushel_obj = [] then [] else [("bushel", `O bushel_obj)]
    | None -> []
  in
  
  `O (base_fields @ karakeep_fields @ bushel_fields)

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
        
        (* Combine karakeep data (prefer new over old) *)
        let karakeep = 
          match new_link.karakeep, old_link.karakeep with
          | Some new_k, Some old_k when new_k.remote_url = old_k.remote_url ->
              (* Same remote, merge the data *)
              let merged_metadata =
                let meta_tbl = Hashtbl.create (List.length old_k.metadata) in
                List.iter (fun (k, v) -> Hashtbl.replace meta_tbl k v) old_k.metadata;
                List.iter (fun (k, v) -> Hashtbl.replace meta_tbl k v) new_k.metadata;
                Hashtbl.fold (fun k v acc -> (k, v) :: acc) meta_tbl []
              in
              let merged_tags = List.sort_uniq String.compare (old_k.tags @ new_k.tags) in
              Some { new_k with metadata = merged_metadata; tags = merged_tags }
          | Some new_k, _ -> Some new_k
          | None, old_k -> old_k
        in
        
        (* Combine bushel data *)
        let bushel = 
          match new_link.bushel, old_link.bushel with
          | Some new_b, Some old_b ->
              (* Merge slugs and tags *)
              let merged_slugs = List.sort_uniq String.compare (old_b.slugs @ new_b.slugs) in
              let merged_tags = List.sort_uniq String.compare (old_b.tags @ new_b.tags) in
              Some { slugs = merged_slugs; tags = merged_tags }
          | Some new_b, _ -> Some new_b
          | None, old_b -> old_b
        in
        
        (* Combined link *)
        let merged_link = {
          url = new_link.url;
          date = (if compare new_link old_link > 0 then new_link.date else old_link.date);
          description = title;
          karakeep;
          bushel
        } in
        Hashtbl.replace links_by_url new_link.url merged_link
  ) new_links;
  
  (* Convert hashtable back to list and sort by date *)
  Hashtbl.to_seq_values links_by_url 
  |> List.of_seq
  |> List.sort compare