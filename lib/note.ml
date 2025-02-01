type t =
  { title : string
  ; date : Ptime.date
  ; slug : string
  ; body : string
  ; tags : string list
  ; updated : Ptime.date option
  ; sidebar : string option
  ; index_page : bool
  ; via : (string * string) option
  }

type ts = t list

let link { body; via; slug; _ } =
  match body, via with
  | "", Some (l, u) -> `Ext (l, u)
  | "", None -> failwith (slug ^ ": note external without via, via-url")
  | _, _ -> `Local slug
;;

let origdate { date; _ } = Option.get @@ Ptime.of_date date

let date { date; updated; _ } =
  match updated with
  | None -> date
  | Some v -> v
;;

let datetime v = Option.get @@ Ptime.of_date @@ date v
let compare a b = Ptime.compare (datetime b) (datetime a)
let slug { slug; _ } = slug
let body { body; _ } = body
let title { title; _ } = title
let tags { tags; _ } = tags
let sidebar { sidebar; _ } = sidebar
let lookup slug notes = List.find (fun n -> n.slug = slug) notes
let read_file file = In_channel.(with_open_bin file input_all)
let words { body; _ } = Util.count_words body

let of_md fname =
  (* TODO fix Jekyll_post to basename the fname all the time *)
  match Jekyll_post.of_string ~fname:(Filename.basename fname) (read_file fname) with
  | Error (`Msg m) -> failwith ("note_of_md: " ^ m)
  | Ok jp ->
    let fields = jp.Jekyll_post.fields in
    let { Jekyll_post.title; date; slug; body; _ } = jp in
    let date, _ = Ptime.to_date_time date in
    let index_page =
      match Jekyll_format.find "index_page" fields with
      | Some (`Bool v) -> v
      | _ -> false
    in
    let updated =
      match Jekyll_format.find "updated" fields with
      | Some (`String v) -> Some (Jekyll_format.parse_date_exn v |> Ptime.to_date)
      | _ -> None
    in
    let sidebar =
      try Some (read_file ("data/sidebar/" ^ Filename.basename fname)) with
      | _ -> None
    in
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
    let via =
      match Jekyll_format.find "via" fields, Jekyll_format.find "via-url" fields with
      | Some (`String a), Some (`String b) -> Some (a, b)
      | None, Some (`String b) -> Some ("", b)
      | _ -> None
    in
    { title; date; slug; index_page; body; via; updated; tags; sidebar }
;;
