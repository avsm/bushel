module J = Ezjsonm

type paper = Ezjsonm.value

type t =
  { slug : string
  ; ver : string
  ; paper : paper
  ; abstract : string
  ; latest : bool
  }

type ts = t list

let key y k = J.find y [ k ]

let slugs ts =
  List.fold_left (fun acc t -> if List.mem t.slug acc then acc else t.slug :: acc) [] ts
;;

let slug { slug; _ } = slug
let title { paper; _ } : string = key paper "title" |> J.get_string
let authors { paper; _ } : string list = key paper "author" |> J.get_list J.get_string

let project_slugs { paper; _ } : string list =
  try key paper "projects" |> J.get_list J.get_string with
  | _ -> []
;;

let slides { paper; _ } : string list =
  try key paper "slides" |> J.get_list J.get_string with
  | _ -> []
;;

let bibtype { paper; _ } : string = key paper "bibtype" |> J.get_string

let journal { paper; _ } =
  try key paper "journal" |> J.get_string with
  | Not_found ->
    failwith
      (Printf.sprintf "no journal found for %s\n%!" (Ezjsonm.value_to_string paper))
;;

(** TODO:claude Helper to extract raw JSON *)
let raw_json { paper; _ } = paper

let doi { paper; _ } =
  try Some (key paper "doi" |> J.get_string) with
  | _ -> None
;;

let volume { paper; _ } =
  try Some (key paper "volume" |> J.get_string) with
  | _ -> None
;;

let video { paper; _ } =
  try Some (key paper "video" |> J.get_string) with
  | _ -> None
;;

let issue { paper; _ } =
  try Some (key paper "number" |> J.get_string) with
  | _ -> None
;;

let url { paper; _ } =
  try Some (key paper "url" |> J.get_string) with
  | _ -> None
;;

let pages { paper; _ } = try key paper "pages" |> J.get_string with _ -> ""
let abstract { abstract; _ } = abstract

let institution { paper; _ } =
  try key paper "institution" |> J.get_string with
  | Not_found ->
    failwith
      (Printf.sprintf "no institution found for %s\n%!" (Ezjsonm.value_to_string paper))
;;

let number { paper; _ } =
  try Some (key paper "number" |> J.get_string) with
  | Not_found -> None
;;

let editor { paper; _ } = key paper "editor" |> J.get_string
let isbn { paper; _ } = key paper "isbn" |> J.get_string
let bib { paper; _ } = key paper "bib" |> J.get_string
let year { paper; _ } = key paper "year" |> J.get_string |> int_of_string

let publisher { paper; _ } =
  try key paper "publisher" |> J.get_string with
  | Not_found -> ""
;;

let booktitle { paper; _ } =
  let r = key paper "booktitle" |> J.get_string |> Bytes.of_string in
  Bytes.set r 0 (Char.lowercase_ascii (Bytes.get r 0));
  String.of_bytes r
;;

let date { paper; _ } =
  let m =
    try
      match String.lowercase_ascii (key paper "month" |> J.get_string) with
      | "jan" -> 1
      | "feb" -> 2
      | "mar" -> 3
      | "apr" -> 4
      | "may" -> 5
      | "jun" -> 6
      | "jul" -> 7
      | "aug" -> 8
      | "sep" -> 9
      | "oct" -> 10
      | "nov" -> 11
      | "dec" -> 12
      | _ -> 1
    with
    | Not_found -> 1
  in
  let y =
    try key paper "year" |> J.get_string |> int_of_string with
    | Not_found ->
      failwith (Printf.sprintf "no year found for %s" (Ezjsonm.value_to_string paper))
  in
  y, m, 1
;;

let datetime p = Option.get @@ Ptime.of_date @@ date p

let compare p2 p1 =
  let d1 =
    Ptime.of_date
      (try date p1 with
       | _ -> 1977, 1, 1)
    |> Option.get
  in
  let d2 =
    Ptime.of_date
      (try date p2 with
       | _ -> 1977, 1, 1)
    |> Option.get
  in
  Ptime.compare d1 d2
;;

let get_papers ~slug ts =
  List.filter (fun p -> p.slug = slug && p.latest <> true) ts |> List.sort compare
;;

let read_file file = In_channel.(with_open_bin file input_all)

let of_md ~slug ~ver fname =
  (* TODO fix Jekyll_post to not error on no date *)
  let fname' = "2000-01-01-" ^ Filename.basename fname in
  match Jekyll_post.of_string ~fname:fname' (read_file fname) with
  | Error (`Msg m) -> failwith ("paper_of_md: " ^ m)
  | Ok jp ->
    let fields = jp.Jekyll_post.fields |> Jekyll_format.fields_to_yaml in
    let { Jekyll_post.body; _ } = jp in
    { slug; ver; abstract = body; paper = fields; latest = false }
;;

let tv (l : t list) =
  let h = Hashtbl.create 7 in
  List.iter
    (fun { slug; ver; _ } ->
      match Hashtbl.find_opt h slug with
      | None -> Hashtbl.add h slug [ ver ]
      | Some l ->
        let l = ver :: l in
        let l = List.sort Stdlib.compare l in
        Hashtbl.replace h slug l)
    l;
  List.map
    (fun p ->
      let latest = Hashtbl.find h p.slug |> List.rev |> List.hd in
      let latest = p.ver = latest in
      { p with latest })
    l
;;

let lookup ts slug = List.find_opt (fun t -> t.slug = slug && t.latest) ts

let tag_of_bibtype bt =
  match String.lowercase_ascii bt with
  | "article" -> "journal"
  | "inproceedings" -> "conference"
  | "techreport" -> "report"
  | "misc" -> "preprint"
  | "book" -> "book"
  | x -> x
;;

let tags { paper; _ } =
  let tags f =
    try key paper f |> J.get_list J.get_string with
    | _ -> []
  in
  let core = tags "tags" in
  let extra = tags "keywords" in
  let projects = tags "projects" in
  let ty = [ key paper "bibtype" |> J.get_string |> tag_of_bibtype ] in
  List.flatten [ core; extra; ty; projects ]
;;

let best_url p =
  if Sys.file_exists (Printf.sprintf "static/papers/%s.pdf" (slug p))
  then Some (Printf.sprintf "/papers/%s.pdf" (slug p))
  else url p
;;

(** TODO:claude Classification types for papers *)
type classification = Full | Short | Preprint

let string_of_classification = function
  | Full -> "full"
  | Short -> "short" 
  | Preprint -> "preprint"

let classification_of_string = function
  | "full" -> Full
  | "short" -> Short
  | "preprint" -> Preprint
  | _ -> Full (* default to full if unknown *)

(** TODO:claude Get classification from paper metadata, with fallback to heuristic *)
let classification { paper; _ } =
  try 
    key paper "classification" |> J.get_string |> classification_of_string
  with _ -> 
    (* Fallback to heuristic classification based on venue/bibtype/title *)
    let bibtype = try key paper "bibtype" |> J.get_string with _ -> "" in
    let journal = try key paper "journal" |> J.get_string |> String.lowercase_ascii with _ -> "" in
    let booktitle = try key paper "booktitle" |> J.get_string |> String.lowercase_ascii with _ -> "" in
    let title_str = try key paper "title" |> J.get_string |> String.lowercase_ascii with _ -> "" in
    
    (* Helper function to check if text contains any of the patterns *)
    let contains_any text patterns =
      List.exists (fun pattern ->
        let regex = Re.Perl.compile_pat ~opts:[`Caseless] pattern in
        Re.execp regex text
      ) patterns
    in
    
    (* Check for preprint indicators *)
    let bibtype_lower = String.lowercase_ascii bibtype in
    if contains_any journal ["arxiv"] || contains_any booktitle ["arxiv"] || bibtype_lower = "misc" || bibtype_lower = "techreport"
    then Preprint
    (* Check for workshop/short paper indicators including in title *)
    else if contains_any journal ["workshop"; "wip"; "poster"; "demo"; "hotdep"; "short"] ||
            contains_any booktitle ["workshop"; "wip"; "poster"; "demo"; "hotdep"; "short"] ||
            contains_any title_str ["poster"]
    then Short
    (* Default to full paper (journal or conference) *)
    else Full

(** TODO:claude Check if paper is marked as selected *)
let selected { paper; _ } =
  try 
    let keys = J.get_dict paper in
    match List.assoc_opt "selected" keys with
    | Some (`Bool true) -> true
    | Some (`String "true") -> true
    | _ -> false
  with _ -> false

(** TODO:claude Get note field from paper metadata *)
let note { paper; _ } =
  try 
    let keys = J.get_dict paper in
    match List.assoc_opt "note" keys with
    | Some note_json -> Some (J.get_string note_json)
    | None -> None
  with _ -> None

(* TODO:claude *)
let to_yaml ?abstract ~ver:_ json_data =
  (* Don't add version - it's inferred from filename *)
  let frontmatter = Yaml.to_string_exn json_data in
  match abstract with
  | Some abs -> 
    (* Trim leading/trailing whitespace and normalize blank lines *)
    let trimmed_abs = String.trim abs in
    let normalized_abs = 
      (* Replace 3+ consecutive newlines with exactly 2 newlines *)
      Re.replace_string (Re.compile (Re.seq [Re.char '\n'; Re.char '\n'; Re.rep1 (Re.char '\n')])) ~by:"\n\n" trimmed_abs
    in
    if normalized_abs = "" then
      Printf.sprintf "---\n%s---\n" frontmatter
    else
      Printf.sprintf "---\n%s---\n\n%s\n" frontmatter normalized_abs
  | None -> Printf.sprintf "---\n%s---\n" frontmatter

(* TODO:claude *)
let typesense_schema =
  let open Ezjsonm in
  dict [
    ("name", string "papers");
    ("fields", list (fun d -> dict d) [
      [("name", string "id"); ("type", string "string")];
      [("name", string "title"); ("type", string "string")];
      [("name", string "authors"); ("type", string "string[]")];
      [("name", string "abstract"); ("type", string "string")];
      [("name", string "date"); ("type", string "string")];
      [("name", string "date_timestamp"); ("type", string "int64")];
      [("name", string "tags"); ("type", string "string[]"); ("facet", bool true)];
      [("name", string "doi"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "arxiv_id"); ("type", string "string"); ("optional", bool true)];
      [("name", string "pdf_url"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "thumbnail_url"); ("type", string "string"); ("optional", bool true)];
      [("name", string "journal"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "related_projects"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "related_talks"); ("type", string "string[]"); ("optional", bool true)];
    ]);
    ("default_sorting_field", string "date_timestamp");
  ]
