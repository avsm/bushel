type t =
  { slug : string
  ; title : string
  ; published_date : Ptime.t
  ; uuid : string
  ; description : string
  ; url : string
  ; talk : bool
  ; paper : string option
  ; project : string option
  ; tags : string list
  }

type ts = t list

let get_shadow fs k =
  match List.assoc_opt k fs with
  | Some v -> Some v
  | None -> List.assoc_opt ("_" ^ k) fs
;;

let get_shadow_string fs k =
  match get_shadow fs k with
  | Some (`String v) -> v
  | _ -> failwith "invalid yaml"
;;

let get_shadow_bool fs k =
  match get_shadow fs k with
  | Some (`Bool v) -> v
  | _ -> failwith "invalid yaml"
;;

let compare a b = Ptime.compare b.published_date a.published_date
let url v = v.url
let body { description; _ } = description
let title { title; _ } = title
let uuid { uuid; _ } = uuid
let paper { paper; _ } = paper
let project { project; _ } = project
let slug { slug; _ } = slug
let date { published_date; _ } = published_date |> Ptime.to_date
let datetime { published_date; _ } = published_date
let talk { talk; _ } = talk

let t_of_yaml ~description = function
  | `O fields ->
    let slug = get_shadow_string fields "uuid" in
    let title = get_shadow_string fields "title" in
    let published_date =
      get_shadow_string fields "published_date"
      |> Ptime.of_rfc3339
      |> Result.get_ok
      |> fun (a, _, _) -> a
    in
    let uuid = get_shadow_string fields "uuid" in
    let url = get_shadow_string fields "url" in
    let talk =
      try get_shadow_bool fields "talk" with
      | _ -> false
    in
    let tags =
      match List.assoc_opt "tags" fields with
      | Some l -> Ezjsonm.get_list Ezjsonm.get_string l
      | _ -> []
    in
    let paper =
      try Some (get_shadow_string fields "paper") with
      | _ -> None
    in
    let project =
      try Some (get_shadow_string fields "project") with
      | _ -> None
    in
    { slug; title; tags; published_date; uuid; description; talk; paper; project; url }
  | _ -> failwith "invalid yaml"
;;

let read_file file = In_channel.(with_open_bin file input_all)

let of_md fname =
  (* TODO fix Jekyll_post to not error on no date *)
  let fname' = "2000-01-01-" ^ Filename.basename fname in
  match Jekyll_post.of_string ~fname:fname' (read_file fname) with
  | Error (`Msg m) -> failwith ("paper_of_md: " ^ m)
  | Ok jp ->
    let fields = jp.Jekyll_post.fields |> Jekyll_format.fields_to_yaml in
    let { Jekyll_post.body; _ } = jp in
    t_of_yaml ~description:body fields
;;
