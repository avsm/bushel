type t =
  { slug : string
  ; title : string
  ; start : int (* year *)
  ; finish : int option
  ; tags : string list
  ; ideas : string
  ; body : string
  }

type ts = t list

let tags p = p.tags

let compare a b =
  match compare a.start b.start with
  | 0 -> compare b.finish a.finish
  | n -> n
;;

let title { title; _ } = title
let body { body; _ } = body
let ideas { ideas; _ } = ideas

let of_md fname =
  match Jekyll_post.of_string ~fname (Util.read_file fname) with
  | Error (`Msg m) -> failwith ("Project.of_file: " ^ m)
  | Ok jp ->
    let fields = jp.Jekyll_post.fields in
    let { Jekyll_post.title; date; slug; body; _ } = jp in
    let (start, _, _), _ = Ptime.to_date_time date in
    let finish =
      match Jekyll_format.find "finish" fields with
      | Some (`String date) ->
        let date = Jekyll_format.parse_date_exn date in
        let (finish, _, _), _ = Ptime.to_date_time date in
        Some finish
      | _ -> None
    in
    let ideas =
      match Jekyll_format.find "ideas" fields with
      | Some (`String e) -> e
      | _ -> failwith ("no ideas key in " ^ fname)
    in
    let tags =
      match Jekyll_format.find "tags" fields with
      | Some (`A tags) -> List.map Yaml.Util.to_string_exn tags
      | _ -> []
    in
    { slug; title; start; finish; ideas; tags; body }
;;

let lookup projects slug = List.find_opt (fun p -> p.slug = slug) projects

(* TODO:claude *)
let typesense_schema =
  let open Ezjsonm in
  dict [
    ("name", string "projects");
    ("fields", list (fun d -> dict d) [
      [("name", string "id"); ("type", string "string")];
      [("name", string "title"); ("type", string "string")];
      [("name", string "description"); ("type", string "string")];
      [("name", string "start_year"); ("type", string "int32")];
      [("name", string "finish_year"); ("type", string "int32"); ("optional", bool true)];
      [("name", string "date"); ("type", string "string")];
      [("name", string "date_timestamp"); ("type", string "int64")];
      [("name", string "tags"); ("type", string "string[]"); ("facet", bool true)];
      [("name", string "repository_url"); ("type", string "string"); ("optional", bool true)];
      [("name", string "homepage_url"); ("type", string "string"); ("optional", bool true)];
      [("name", string "languages"); ("type", string "string[]"); ("facet", bool true); ("optional", bool true)];
      [("name", string "license"); ("type", string "string"); ("facet", bool true); ("optional", bool true)];
      [("name", string "status"); ("type", string "string"); ("facet", bool true); ("optional", bool true)];
      [("name", string "related_papers"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "related_talks"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "body"); ("type", string "string"); ("optional", bool true)];
      [("name", string "ideas"); ("type", string "string"); ("optional", bool true)];
    ]);
    ("default_sorting_field", string "date_timestamp");
  ]
