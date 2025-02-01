module Contact = Contact
module Idea = Idea
module News = News
module Note = Note
module Paper = Paper
module Project = Project
module Video = Video
module Tags = Tags
module Entry = Entry
module Util = Util
module Srcsetter = Srcsetter
module Md = Md

let map_md base subdir fn =
  let dir = base ^ "/data/" ^ subdir in
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".md")
  |> List.map (fun e -> fn dir e)
;;

let map_category base c fn = map_md base c (fun dir e -> fn @@ Filename.concat dir e)
let load_contacts base = map_category base "contacts" Contact.of_md
let load_projects base = map_category base "projects" Project.of_md
let load_notes base = map_category base "notes" Note.of_md
let load_news base = map_category base "news" News.of_md
let load_ideas base = map_category base "ideas" Idea.of_md
let load_videos base = map_category base "videos" Video.of_md

let load_images base =
  try
    Srcsetter.list_of_json (Util.read_file (base ^ "/images/index.json")) |> Result.get_ok
  with
  | _ -> [] (* FIXME log *)
;;

let load_papers base =
  Sys.readdir (base ^ "/data/papers")
  |> Array.to_list
  |> List.filter (fun slug -> Sys.is_directory (base ^ "/data/papers/" ^ slug))
  |> List.map (fun slug ->
    Sys.readdir ("data/papers/" ^ slug)
    |> Array.to_list
    |> List.filter (fun ver -> Filename.check_suffix ver ".md")
    |> List.map (fun ver ->
      Paper.of_md ~slug ~ver (base ^ "/data/papers/" ^ slug ^ "/" ^ ver)))
  |> List.flatten
  |> Paper.tv
;;

let load base =
  let images = load_images base in
  let papers = load_papers base in
  let contacts = load_contacts base in
  let projects = load_projects base in
  let notes = load_notes base in
  let news = load_news base in
  let ideas = load_ideas base in
  let videos = load_videos base in
  Entry.v ~images ~papers ~notes ~projects ~ideas ~videos ~news ~contacts
;;
