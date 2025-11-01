open Entry

type t =
  [ `Slug of string (* :foo points to the specific slug foo *)
  | `Contact of string (* @foo points to contact foo *)
  | `Set of string (* #papers points to all Paper entries *)
  | `Text of string (* foo points to a free text "foo" *)
  | `Year of int (* a number between 1900--2100 is interpreted as a year *)
  ]

let is_text = function
  | `Text _ -> true
  | _ -> false
;;

let is_slug = function
  | `Slug _ -> true
  | _ -> false
;;

let is_set = function
  | `Set _ -> true
  | _ -> false
;;

let is_year = function
  | `Year _ -> true
  | _ -> false
;;

let of_string s : t =
  if String.length s < 2 then invalid_arg ("Tag.of_string: " ^ s);
  match s.[0] with
  | ':' ->
    let slug = String.sub s 1 (String.length s - 1) in
    `Slug slug
  | '@' -> failwith "TODO add contacts to entries"
  | '#' ->
    let cl = String.sub s 1 (String.length s - 1) in
    `Set cl
  | _ ->
    (try
       let x = int_of_string s in
       if x > 1900 && x < 2100 then `Year x else `Text s
     with
     | _ -> `Text s)
;;

let of_string_list l = List.map of_string l

let to_string = function
  | `Slug t -> ":" ^ t
  | `Contact c -> "@" ^ c
  | `Set s -> "#" ^ s
  | `Text t -> t
  | `Year y -> string_of_int y
;;

let to_raw_string = function
  | `Slug t -> t
  | `Contact c -> c
  | `Set s -> s
  | `Text t -> t
  | `Year y -> string_of_int y
;;

let pp ppf t = Fmt.string ppf (to_string t)

let tags_of_ent _entries ent : t list =
  match ent with
  | `Paper p -> of_string_list @@ Paper.tags p
  | `Video v -> of_string_list v.Video.tags
  | `Project p -> of_string_list @@ Project.tags p
  | `Note n -> of_string_list @@ Note.tags n
  | `Idea i -> of_string_list i.Idea.tags
;;

let mentions tags =
  List.filter
    (function
      | `Contact _ | `Slug _ -> true
      | _ -> false)
    tags
;;

let mention_entries entries tags =
  let lk t =
   try Some (lookup_exn entries t)
   with Not_found -> Printf.eprintf "mention_entries not found: %s\n%!" t; None
  in
  List.filter_map
    (function
      | `Slug t -> lk t
      | _ -> None)
    tags
;;

let count_tags ?h fn vs =
  let h =
    match h with
    | Some h -> h
    | None -> Hashtbl.create 42
  in
  List.iter
    (fun ent ->
      List.iter
        (fun tag ->
          match Hashtbl.find_opt h tag with
          | Some num -> Hashtbl.replace h tag (num + 1)
          | None -> Hashtbl.add h tag 1)
        (fn ent))
    vs;
  h
;;
