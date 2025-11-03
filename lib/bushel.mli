(** Bushel *)

module Contact = Contact
module Idea = Idea
module Note = Note
module Paper = Paper
module Project = Project
module Video = Video
module Tags = Tags
module Link = Link
module Entry = Entry
module Util = Util
module Md = Md
module Srcsetter = Srcsetter
module Typesense = Typesense
module Link_graph = Link_graph
module Description = Description

val load_contacts : string -> Contact.ts
val load_projects : string -> Project.ts
val load_notes : string -> Note.ts
val load_ideas : string -> Idea.ts
val load_videos : string -> Video.ts
val load_images : string -> Srcsetter.ts
val load_papers : string -> Paper.ts
val load : string -> Entry.t
