type entry =
  [ `Paper of Paper.t
  | `Project of Project.t
  | `Idea of Idea.t
  | `Video of Video.t
  | `Note of Note.t
  ]

type feed =
  [ `News of News.t * entry
  | `Entry of entry
  ]

type slugs = (string, entry) Hashtbl.t

type t =
  { slugs : slugs
  ; papers : Paper.ts
  ; old_papers : Paper.ts
  ; notes : Note.ts
  ; projects : Project.ts
  ; ideas : Idea.ts
  ; videos : Video.ts
  ; news : News.ts
  ; contacts : Contact.ts
  }

let contacts { contacts; _ } = contacts
let videos { videos; _ } = videos
let ideas { ideas; _ } = ideas
let papers { papers; _ } = papers
let notes { notes; _ } = notes
let projects { projects; _ } = projects

let v ~papers ~notes ~projects ~ideas ~videos ~news ~contacts =
  let slugs : slugs = Hashtbl.create 42 in
  let papers, old_papers = List.partition (fun p -> p.Paper.latest) papers in
  List.iter (fun n -> Hashtbl.add slugs n.Note.slug (`Note n)) notes;
  List.iter (fun p -> Hashtbl.add slugs p.Project.slug (`Project p)) projects;
  List.iter (fun i -> Hashtbl.add slugs i.Idea.slug (`Idea i)) ideas;
  List.iter (fun v -> Hashtbl.add slugs v.Video.slug (`Video v)) videos;
  List.iter (fun p -> Hashtbl.add slugs p.Paper.slug (`Paper p)) papers;
  { slugs; papers; old_papers; notes; projects; ideas; videos; news; contacts }
;;

let lookup { slugs; _ } slug = Hashtbl.find_opt slugs slug
let lookup_exn { slugs; _ } slug = Hashtbl.find slugs slug
let lookup_news { news; _ } s = List.find_opt (fun { News.slug; _ } -> slug = s) news
let old_papers { old_papers; _ } = old_papers

let sidebar = function
  | `Note { Note.sidebar = Some s; _ } -> Some s
  | _ -> None
;;

let to_type_string = function
  | `Paper _ -> "paper"
  | `Note _ -> "note"
  | `Project _ -> "project"
  | `Idea _ -> "idea"
  | `Video _ -> "video"
;;

let slug = function
  | `Paper p -> p.Paper.slug
  | `Note n -> n.Note.slug
  | `Project p -> p.Project.slug
  | `Idea i -> i.Idea.slug
  | `Video v -> v.Video.slug
;;

let title = function
  | `Paper p -> Paper.title p
  | `Note n -> Note.title n
  | `Project p -> Project.title p
  | `Idea i -> Idea.title i
  | `Video v -> Video.title v
;;

let body = function
  | `Paper _ -> ""
  | `Note n -> Note.body n
  | `Project p -> Project.body p
  | `Idea i -> Idea.body i
  | `Video _ -> ""
;;

let site_url = function
  | `Paper p -> "/papers/" ^ p.Paper.slug
  | `Note n -> "/notes/" ^ n.Note.slug
  | `Project p -> "/projects/" ^ p.Project.slug
  | `Idea i -> "/ideas/" ^ i.Idea.slug
  | `Video v -> "/videos/" ^ v.Video.slug
;;

let date (x : entry) =
  match x with
  | `Paper p -> Paper.date p
  | `Note n -> Note.date n
  | `Project p -> p.Project.start, 1, 1
  | `Idea i -> i.Idea.year, 1, 1
  | `Video v -> Video.date v
;;

let datetime v = date v |> Ptime.of_date |> Option.get

let year x =
  match date x with
  | y, _, _ -> y
;;

let feed_date (x : feed) =
  match x with
  | `News (n, _) -> News.date n
  | `Entry e -> date e
;;

let feed_datetime x = feed_date x |> Ptime.of_date |> Option.get

let feed_title (x : feed) =
  match x with
  | `News (n, _) -> News.title n
  | `Entry e -> title e
;;

let feed_url (x : feed) =
  match x with
  | `News (n, _) -> News.site_url n
  | `Entry e -> site_url e
;;

let is_index_entry = function
  | `Note { Note.index_page; _ } -> index_page
  | _ -> false
;;

let feed_compare b a =
  let datetime v = Option.get (Ptime.of_date v) in
  let da = datetime (feed_date a) in
  let db = datetime (feed_date b) in
  if da = db then compare (feed_title a) (feed_title b) else Ptime.compare da db
;;

let news_for_slug { news; _ } slug = List.filter (fun n -> n.News.slug_ent = slug) news
let news_for_tag { news; _ } tag = List.filter (fun n -> List.mem tag n.News.tags) news
let all_entries { slugs; _ } = Hashtbl.fold (fun _ v acc -> v :: acc) slugs []

let compare a b =
  let datetime v = Option.get (Ptime.of_date v) in
  let da = datetime (date a) in
  let db = datetime (date b) in
  if da = db then compare (title a) (title b) else Ptime.compare da db
;;
