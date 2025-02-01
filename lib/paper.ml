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

let pages { paper; _ } = key paper "pages" |> J.get_string
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
  | Not_found -> "FIXME"
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
