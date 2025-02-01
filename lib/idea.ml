type level =
  | Any
  | PartII
  | MPhil
  | PhD
  | Postdoc

let level_of_yaml = function
  | `String ("Any" | "any") -> Ok Any
  | `String ("PartII" | "partii") -> Ok PartII
  | `String ("MPhil" | "mphil") -> Ok MPhil
  | `String ("PhD" | "phd") -> Ok PhD
  | `String ("postdoc" | "Postdoc") -> Ok Postdoc
  | _ -> Error (`Msg "level_of_yaml")
;;

let level_to_string = function
  | Any -> "Any"
  | PartII -> "PartII"
  | MPhil -> "MPhil"
  | PhD -> "PhD"
  | Postdoc -> "postdoctoral"
;;

let level_to_tag = function
  | Any -> "idea-beginner"
  | PartII -> "idea-medium"
  | MPhil -> "idea-hard"
  | PhD -> "idea-phd"
  | Postdoc -> "idea-postdoc"
;;

let level_to_yaml s = `String (level_to_string s)

type status =
  | Available
  | Discussion
  | Ongoing
  | Completed

let status_of_yaml = function
  | `String ("Available" | "available") -> Ok Available
  | `String ("Discussion" | "discussion") -> Ok Discussion
  | `String ("Ongoing" | "ongoing") -> Ok Ongoing
  | `String ("Completed" | "completed") -> Ok Completed
  | _ -> Error (`Msg "status_of_yaml")
;;

let status_to_string = function
  | Available -> "Available"
  | Discussion -> "Discussion"
  | Ongoing -> "Ongoing"
  | Completed -> "Completed"
;;

let status_to_tag = function
  | Available -> "idea-available"
  | Discussion -> "idea-discuss"
  | Ongoing -> "idea-ongoing"
  | Completed -> "idea-done"
;;

let status_to_yaml s = `String (status_to_string s)

type t =
  { slug : string
  ; title : string
  ; level : level
  ; project : string
  ; status : status
  ; year : int
  ; supervisors : string list
  ; students : string list
  ; reading : string
  ; body : string
  ; url : string option
  ; tags : string list
  }

type ts = t list

let title i = i.title
let supervisors i = i.supervisors
let students i = i.students
let reading i = i.reading
let status i = i.status
let level i = i.level
let year i = i.year
let body i = i.body
let project i = i.project

let compare a b =
  match compare a.status b.status with
  | 0 ->
    (match a.status with
     | Completed -> compare b.year a.year
     | _ ->
       (match compare a.level b.level with
        | 0 -> compare b.year a.year
        | n -> n))
  | n -> n
;;

let of_md fname =
  match Jekyll_post.of_string ~fname:(Filename.basename fname) (Util.read_file fname) with
  | Error _ -> failwith "TODO"
  | Ok jp ->
    let fields = jp.Jekyll_post.fields in
    let y = Jekyll_format.fields_to_yaml fields in
    let year, _, _ = jp.Jekyll_post.date |> Ptime.to_date in
    let body = jp.Jekyll_post.body in
    let string f = Yaml.Util.(find_exn f y |> Option.get |> to_string |> Result.get_ok) in
    let string' f d =
      try Yaml.Util.(find_exn f y |> Option.get |> to_string |> Result.get_ok) with
      | _ -> d
    in
    let to_list = function
      | `A l -> Ok l
      | _ -> Error (`Msg "to_list")
    in
    let strings f =
      try
        Yaml.Util.(
          find_exn f y
          |> Option.get
          |> to_list
          |> Result.get_ok
          |> List.map (fun x -> to_string x |> Result.get_ok))
      with
      | _exn -> []
    in
    let level =
      Yaml.Util.(find_exn "level" y |> Option.get |> level_of_yaml |> Result.get_ok)
    in
    let status =
      Yaml.Util.(find_exn "status" y |> Option.get |> status_of_yaml |> Result.get_ok)
    in
    let slug = jp.Jekyll_post.slug in
    { slug
    ; title = string "title"
    ; level
    ; project = string "project"
    ; status
    ; supervisors = strings "supervisors"
    ; students = strings "students"
    ; tags = strings "tags"
    ; reading = string' "reading" ""
    ; year
    ; body
    ; url = None (* TODO *)
    }
;;

let lookup ideas slug = List.find_opt (fun i -> i.slug = slug) ideas
