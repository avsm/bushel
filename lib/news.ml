let date_of_yaml t =
  match t with
  | `String date -> Scanf.sscanf date "%d-%d-%d" (fun y m d -> y, m, d)
  | _ -> failwith "not a date"
;;

type t =
  { slug : string
  ; slug_ent : string
  ; date : Ptime.date
  ; tags : string list
  ; title : string
  ; body : string
  }

type ts = t list

let slug t = t.slug
let slug_ent t = t.slug_ent
let tags t = t.tags
let title t = t.title
let date t = t.date
let body t = t.body
let read_file file = In_channel.(with_open_bin file input_all)
let site_url n = "/news/" ^ n.slug

let of_md fname =
  (* TODO fix Jekyll_post to basename the fname all the time *)
  match Jekyll_post.of_string ~fname:(Filename.basename fname) (read_file fname) with
  | Error (`Msg m) -> failwith ("news_of_file: " ^ m)
  | Ok jp ->
    let fields = jp.Jekyll_post.fields in
    let { Jekyll_post.title; date; slug; body; _ } = jp in
    let date, _ = Ptime.to_date_time date in
    let tags =
      match Jekyll_format.find "tags" fields with
      | Some (`A l) ->
        List.filter_map
          (function
            | `String s -> Some s
            | _ -> None)
          l
      | _ -> []
    in
    let slug_ent =
      match Jekyll_format.find "slug_ent" fields with
      | Some (`String s) -> s
      | _ -> failwith "slug_ent field needed for news"
    in
    { title; date; slug; slug_ent; body; tags }
;;

let datetime v = Option.get @@ Ptime.of_date @@ v.date
let compare a b = Ptime.compare (datetime a) (datetime b)
