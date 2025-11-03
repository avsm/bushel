(** Generate descriptive text for bushel entries *)

(* Helper to format a date as "Month Year" *)
let format_date date =
  let (year, month, _day) = date in
  let month_name = match month with
    | 1 -> "January" | 2 -> "February" | 3 -> "March" | 4 -> "April"
    | 5 -> "May" | 6 -> "June" | 7 -> "July" | 8 -> "August"
    | 9 -> "September" | 10 -> "October" | 11 -> "November" | 12 -> "December"
    | _ -> ""
  in
  Printf.sprintf "%s %d" month_name year

(* Generate a descriptive sentence for a paper *)
let paper_description (p : Paper.t) ~date_str =
  let venue = match String.lowercase_ascii (Paper.bibtype p) with
    | "inproceedings" -> Paper.booktitle p
    | "article" -> Paper.journal p
    | "book" ->
      let pub = Paper.publisher p in
      if pub = "" then "Book" else "Book by " ^ pub
    | "techreport" ->
      (try "Technical report at " ^ Paper.institution p
       with _ -> "Technical report")
    | "misc" ->
      let pub = Paper.publisher p in
      if pub = "" then "Working paper" else "Working paper at " ^ pub
    | _ -> "Publication"
  in
  Printf.sprintf "Paper in %s (%s)" venue date_str

(* Generate a descriptive sentence for a note *)
let note_description (n : Note.t) ~date_str ~lookup_fn =
  match Note.slug_ent n with
  | Some slug_ent ->
    (match lookup_fn slug_ent with
     | Some related_title ->
       Printf.sprintf "Note about %s (%s)" related_title date_str
     | None -> Printf.sprintf "Research note (%s)" date_str)
  | None -> Printf.sprintf "Research note (%s)" date_str

(* Generate a descriptive sentence for an idea *)
let idea_description (i : Idea.t) ~date_str =
  let status_str = String.lowercase_ascii (Idea.status_to_string (Idea.status i)) in
  let level_str = Idea.level_to_string (Idea.level i) in
  Printf.sprintf "Research idea (%s, %s level, %s)" status_str level_str date_str

(* Generate a descriptive sentence for a video *)
let video_description (v : Video.t) ~date_str ~lookup_fn =
  let video_type = if Video.talk v then "Talk video" else "Video" in
  let context = match Video.paper v with
    | Some paper_slug ->
      (match lookup_fn paper_slug with
       | Some title -> Printf.sprintf " about %s" title
       | None -> "")
    | None ->
      (match Video.project v with
       | Some project_slug ->
         (match lookup_fn project_slug with
          | Some title -> Printf.sprintf " about %s" title
          | None -> "")
       | None -> "")
  in
  Printf.sprintf "%s%s (%s)" video_type context date_str

(* Generate a descriptive sentence for a project *)
let project_description (pr : Project.t) =
  let end_str = match pr.Project.finish with
    | Some year -> string_of_int year
    | None -> "present"
  in
  Printf.sprintf "Project (%d–%s)" pr.Project.start end_str
