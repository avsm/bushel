(** Generate descriptive text for bushel entries *)

(** Format a date as "Month Year" *)
val format_date : int * int * int -> string

(** Generate a descriptive sentence for a paper with date string *)
val paper_description : Paper.t -> date_str:string -> string

(** Generate a descriptive sentence for a note with date string and lookup function *)
val note_description : Note.t -> date_str:string -> lookup_fn:(string -> string option) -> string

(** Generate a descriptive sentence for an idea with date string *)
val idea_description : Idea.t -> date_str:string -> string

(** Generate a descriptive sentence for a video with date string and lookup function *)
val video_description : Video.t -> date_str:string -> lookup_fn:(string -> string option) -> string

(** Generate a descriptive sentence for a project *)
val project_description : Project.t -> string
