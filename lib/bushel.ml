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

let load _base =
  let images =
    (* FIXME add to entries *)
    try Srcsetter.list_of_json (Util.read_file "images/index.json") |> Result.get_ok with
    | _ -> [] (* FIXME log *)
  in
  let papers =
    Sys.readdir "data/papers"
    |> Array.to_list
    |> List.filter (fun slug -> Sys.is_directory ("data/papers/" ^ slug))
    |> List.map (fun slug ->
      Sys.readdir ("data/papers/" ^ slug)
      |> Array.to_list
      |> List.filter (fun ver -> Filename.check_suffix ver ".md")
      |> List.map (fun ver -> Paper.of_md ~slug ~ver ("data/papers/" ^ slug ^ "/" ^ ver)))
    |> List.flatten
    |> Paper.tv
  in
  let contacts =
    Sys.readdir "data/contacts"
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun f -> Contact.of_md ("data/contacts/" ^ f))
  in
  let projects =
    Sys.readdir "data/projects"
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun f -> Project.of_file ("data/projects/" ^ f))
  in
  let notes =
    Sys.readdir "data/notes"
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun f -> Note.of_md ("data/notes/" ^ f))
  in
  let news =
    Sys.readdir "data/news"
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun f -> News.of_md ("data/news/" ^ f))
  in
  let ideas =
    Sys.readdir "data/ideas"
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun f -> Idea.of_file ("data/ideas/" ^ f))
  in
  let videos =
    Sys.readdir "data/videos"
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun f -> Video.of_md ("data/videos/" ^ f))
  in
  Entry.v ~images ~papers ~notes ~projects ~ideas ~videos ~news ~contacts
;;
