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

let tags_of_ent entries ent : t list =
  let year_tag =
    let y, _, _ = Entry.date ent in
    `Year y
  in
  let body_slugs = List.map of_string (Md.scan_for_slugs entries (Entry.body ent)) in
  let to_tag a : t = `Slug a in
  let to_sort a : t = `Set a in
  let tags =
    match ent with
    | `Paper p when p.Paper.latest -> to_sort "papers" :: (of_string_list @@ Paper.tags p)
    | `Paper p -> of_string_list @@ Paper.tags p
    | `Video v ->
      let tags = of_string_list v.Video.tags in
      let base =
        to_sort "videos"
        ::
        (match Video.paper v, Video.project v with
         | Some a, None -> to_tag a :: tags
         | None, Some b -> to_tag b :: tags
         | Some a, Some b -> to_tag a :: to_tag b :: tags
         | None, None -> tags)
      in
      if v.Video.talk then to_sort "talks" :: base else base
    | `Project p -> to_sort "projects" :: (of_string_list @@ Project.tags p)
    | `Note n -> to_sort "notes" :: (of_string_list @@ Note.tags n)
    | `Idea i ->
      to_sort "ideas"
      :: of_string (Idea.status_to_tag i.Idea.status)
      :: of_string (Idea.level_to_tag i.Idea.level)
      :: to_tag i.Idea.project
      :: of_string_list i.Idea.tags
  in
  (to_tag (Entry.slug ent) :: year_tag :: body_slugs) @ tags
;;

let tags_of_news news =
  let year_tag =
    let y, _, _ = News.date news in
    `Year y
  in
  year_tag :: List.map of_string news.News.tags
;;

let mentions tags =
  List.filter
    (function
      | `Contact _ | `Slug _ -> true
      | _ -> false)
    tags
;;

let mention_entries entries tags =
  List.filter_map
    (function
      | `Slug t -> Some (lookup_exn entries t)
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
