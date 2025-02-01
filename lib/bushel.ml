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

let load base =
  let images =
    try
      Srcsetter.list_of_json (Util.read_file (base ^ "/images/index.json"))
      |> Result.get_ok
    with
    | _ -> [] (* FIXME log *)
  in
  let papers =
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
  in
  let map_category c fn = map_md base c (fun dir e -> fn @@ Filename.concat dir e) in
  let contacts = map_category "contacts" Contact.of_md in
  let projects = map_category "projects" Project.of_md in
  let notes = map_category "notes" Note.of_md in
  let news = map_category "news" News.of_md in
  let ideas = map_category "ideas" Idea.of_md in
  let videos = map_category "videos" Video.of_md in
  Entry.v ~images ~papers ~notes ~projects ~ideas ~videos ~news ~contacts
;;
