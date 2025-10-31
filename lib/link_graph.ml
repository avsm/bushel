module StringSet = Set.Make(String)

type entry_type = [ `Paper | `Project | `Note | `Idea | `Video | `Contact ]

type internal_link = {
  source: string;
  target: string;
  target_type: entry_type;
}

type external_link = {
  source: string;
  domain: string;
  url: string;
}

type link_graph = {
  (* All links *)
  mutable internal_links: internal_link list;
  mutable external_links: external_link list;

  (* Indices for efficient queries *)
  outbound: (string, StringSet.t) Hashtbl.t;
  backlinks: (string, StringSet.t) Hashtbl.t;
  external_by_entry: (string, StringSet.t) Hashtbl.t;
  external_by_domain: (string, StringSet.t) Hashtbl.t; (* domain -> source slugs *)
}

let empty_graph () = {
  internal_links = [];
  external_links = [];
  outbound = Hashtbl.create 256;
  backlinks = Hashtbl.create 256;
  external_by_entry = Hashtbl.create 256;
  external_by_domain = Hashtbl.create 64;
}

(* Global storage for the link graph *)
let current_graph : link_graph option ref = ref None

let set_graph graph = current_graph := Some graph
let get_graph () = !current_graph

let entry_type_to_string = function
  | `Paper -> "paper"
  | `Project -> "project"
  | `Note -> "note"
  | `Idea -> "idea"
  | `Video -> "video"
  | `Contact -> "contact"

(* Query functions *)

let get_outbound graph slug =
  try StringSet.elements (Hashtbl.find graph.outbound slug)
  with Not_found -> []

let get_backlinks graph slug =
  try StringSet.elements (Hashtbl.find graph.backlinks slug)
  with Not_found -> []

let get_external_links graph slug =
  try StringSet.elements (Hashtbl.find graph.external_by_entry slug)
  with Not_found -> []

let get_entries_linking_to_domain graph domain =
  try StringSet.elements (Hashtbl.find graph.external_by_domain domain)
  with Not_found -> []

(* Query functions that use the global graph *)

let get_backlinks_for_slug slug =
  match !current_graph with
  | None -> []
  | Some graph -> get_backlinks graph slug

let get_outbound_for_slug slug =
  match !current_graph with
  | None -> []
  | Some graph -> get_outbound graph slug

let get_external_links_for_slug slug =
  match !current_graph with
  | None -> []
  | Some graph -> get_external_links graph slug

(* Pretty printing *)

let pp_internal_link ppf (link : internal_link) =
  Fmt.pf ppf "%s -> %s (%s)"
    link.source
    link.target
    (entry_type_to_string link.target_type)

let pp_external_link ppf (link : external_link) =
  Fmt.pf ppf "%s -> %s (%s)"
    link.source
    link.domain
    link.url

let pp_graph ppf graph =
  Fmt.pf ppf "@[<v>Internal links: %d@,External links: %d@,Entries with outbound: %d@,Entries with backlinks: %d@]"
    (List.length graph.internal_links)
    (List.length graph.external_links)
    (Hashtbl.length graph.outbound)
    (Hashtbl.length graph.backlinks)

let entry_type_of_entry = function
  | `Paper _ -> `Paper
  | `Project _ -> `Project
  | `Note _ -> `Note
  | `Idea _ -> `Idea
  | `Video _ -> `Video
  | `Contact _ -> `Contact

let extract_domain url =
  try
    let uri = Uri.of_string url in
    match Uri.host uri with
    | Some host -> host
    | None -> "unknown"
  with _ -> "unknown"

let add_to_set_hashtbl tbl key value =
  let current =
    try Hashtbl.find tbl key
    with Not_found -> StringSet.empty
  in
  Hashtbl.replace tbl key (StringSet.add value current)

let build_link_graph entries =
  let graph = empty_graph () in

  (* Helper to add internal link *)
  let add_internal_link source target target_type =
    let link = { source; target; target_type } in
    graph.internal_links <- link :: graph.internal_links;
    add_to_set_hashtbl graph.outbound source target;
    add_to_set_hashtbl graph.backlinks target source
  in

  (* Helper to add external link *)
  let add_external_link source url =
    let domain = extract_domain url in
    let link = { source; domain; url } in
    graph.external_links <- link :: graph.external_links;
    add_to_set_hashtbl graph.external_by_entry source url;
    add_to_set_hashtbl graph.external_by_domain domain source
  in

  (* Process each entry *)
  let process_entry entry =
    let source_slug = Entry.slug entry in

    (* Get all links from this entry's markdown content *)
    let md_content = Entry.body entry in
    let all_links = Md.extract_all_links md_content in

    List.iter (fun link ->
      if Md.is_bushel_slug link then
        (* Internal bushel link *)
        let target_slug = Md.strip_handle link in
        match Entry.lookup entries target_slug with
        | Some target_entry ->
          let target_type = entry_type_of_entry target_entry in
          add_internal_link source_slug target_slug target_type
        | None -> ()
      else if Md.is_contact_slug link then
        (* Contact link *)
        let handle = Md.strip_handle link in
        match Contact.find_by_handle (Entry.contacts entries) handle with
        | Some c ->
          let target_slug = Contact.handle c in
          add_internal_link source_slug target_slug `Contact
        | None -> ()
      else if Md.is_tag_slug link then
        (* Skip tag links *)
        ()
      else if String.starts_with ~prefix:"http://" link ||
              String.starts_with ~prefix:"https://" link then
        (* External link *)
        add_external_link source_slug link
      else
        (* Skip other links (relative paths, etc) *)
        ()
    ) all_links
  in

  (* Process all entries *)
  List.iter process_entry (Entry.all_entries entries);

  (* Deduplicate links *)
  let module LinkSet = Set.Make(struct
    type t = internal_link
    let compare (a : internal_link) (b : internal_link) =
      match String.compare a.source b.source with
      | 0 -> String.compare a.target b.target
      | n -> n
  end) in

  let module ExtLinkSet = Set.Make(struct
    type t = external_link
    let compare (a : external_link) (b : external_link) =
      match String.compare a.source b.source with
      | 0 -> String.compare a.url b.url
      | n -> n
  end) in

  graph.internal_links <- LinkSet.elements (LinkSet.of_list graph.internal_links);
  graph.external_links <- ExtLinkSet.elements (ExtLinkSet.of_list graph.external_links);

  graph

(* Export for visualization *)

let to_json graph entries =
  (* Build nodes *)
  let entry_nodes = List.map (fun entry ->
    let slug = Entry.slug entry in
    let title = Entry.title entry in
    let entry_type = entry_type_of_entry entry in
    `O [
      ("id", `String slug);
      ("title", `String title);
      ("type", `String (entry_type_to_string entry_type));
      ("group", `String "entry");
    ]
  ) (Entry.all_entries entries) in

  (* Build domain nodes from external links *)
  let domain_map = Hashtbl.create 64 in
  List.iter (fun link ->
    if not (Hashtbl.mem domain_map link.domain) then
      Hashtbl.add domain_map link.domain ()
  ) graph.external_links;

  let domain_nodes = Hashtbl.fold (fun domain () acc ->
    (`O [
      ("id", `String ("domain:" ^ domain));
      ("title", `String domain);
      ("type", `String "domain");
      ("group", `String "domain");
    ]) :: acc
  ) domain_map [] in

  let all_nodes = entry_nodes @ domain_nodes in

  (* Build internal links *)
  let internal_links_json = List.map (fun (link : internal_link) ->
    `O [
      ("source", `String link.source);
      ("target", `String link.target);
      ("type", `String "internal");
    ]
  ) graph.internal_links in

  (* Build external links (entry -> domain) *)
  let external_links_json = List.map (fun (link : external_link) ->
    `O [
      ("source", `String link.source);
      ("target", `String ("domain:" ^ link.domain));
      ("type", `String "external");
    ]
  ) graph.external_links in

  let all_links = internal_links_json @ external_links_json in

  let json = `O [
    ("nodes", `A all_nodes);
    ("links", `A all_links);
  ] in

  Ezjsonm.to_string json
