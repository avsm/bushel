type t =
  { title : string
  ; date : Ptime.date
  ; slug : string
  ; body : string
  ; tags : string list
  ; draft : bool
  ; updated : Ptime.date option
  ; sidebar : string option
  ; index_page : bool
  ; synopsis: string option
  ; titleimage: string option
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
let synopsis { synopsis; _ } = synopsis
let draft { draft; _ } = draft
let titleimage { titleimage; _ } = titleimage
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
    let draft =
      match Jekyll_format.find "draft" fields with
      | Some (`Bool v) -> v
      | _ -> false
    in
    let titleimage =
      match Jekyll_format.find "titleimage" fields with
      | Some (`String v) -> Some v
      | _ -> None
    in
    let synopsis =
      match Jekyll_format.find "synopsis" fields with
      | Some (`String v) -> Some v
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
    { title; draft; date; slug; synopsis; titleimage; index_page; body; via; updated; tags; sidebar }

(* TODO:claude *)
let typesense_schema =
  let open Ezjsonm in
  dict [
    ("name", string "notes");
    ("fields", list (fun d -> dict d) [
      [("name", string "id"); ("type", string "string")];
      [("name", string "title"); ("type", string "string")];
      [("name", string "content"); ("type", string "string")];
      [("name", string "date"); ("type", string "string")];
      [("name", string "date_timestamp"); ("type", string "int64")];
      [("name", string "tags"); ("type", string "string[]"); ("facet", bool true)];
      [("name", string "body"); ("type", string "string"); ("optional", bool true)];
      [("name", string "draft"); ("type", string "bool")];
      [("name", string "synopsis"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "thumbnail_url"); ("type", string "string"); ("optional", bool true)];
      [("name", string "type"); ("type", string "string"); ("facet", bool true); ("optional", bool true)];
      [("name", string "status"); ("type", string "string"); ("facet", bool true); ("optional", bool true)];
      [("name", string "related_papers"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "related_projects"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "related_contacts"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "attachments"); ("type", string "string[]"); ("optional", bool true)];
    ]);
    ("default_sorting_field", string "date_timestamp");
  ]

(** TODO:claude Pretty-print a note with ANSI formatting *)
let pp ppf n =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %a@," (styled `Bold string) "Type" (styled `Cyan string) "Note";
  pf ppf "%a: %a@," (styled `Bold string) "Slug" string (slug n);
  pf ppf "%a: %a@," (styled `Bold string) "Title" string (title n);
  let (year, month, day) = date n in
  pf ppf "%a: %04d-%02d-%02d@," (styled `Bold string) "Date" year month day;
  (match n.updated with
   | Some (y, m, d) -> pf ppf "%a: %04d-%02d-%02d@," (styled `Bold string) "Updated" y m d
   | None -> ());
  pf ppf "%a: %b@," (styled `Bold string) "Draft" (draft n);
  pf ppf "%a: %b@," (styled `Bold string) "Index Page" n.index_page;
  (match synopsis n with
   | Some syn -> pf ppf "%a: %a@," (styled `Bold string) "Synopsis" string syn
   | None -> ());
  (match titleimage n with
   | Some img -> pf ppf "%a: %a@," (styled `Bold string) "Title Image" string img
   | None -> ());
  (match n.via with
   | Some (label, url) ->
     if label <> "" then
       pf ppf "%a: %a (%a)@," (styled `Bold string) "Via" string label string url
     else
       pf ppf "%a: %a@," (styled `Bold string) "Via" string url
   | None -> ());
  let t = tags n in
  if t <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Tags" (list ~sep:comma string) t;
  (match sidebar n with
   | Some sb ->
     pf ppf "@,";
     pf ppf "%a:@," (styled `Bold string) "Sidebar";
     pf ppf "%a@," string sb
   | None -> ());
  let bd = body n in
  if bd <> "" then begin
    pf ppf "@,";
    pf ppf "%a:@," (styled `Bold string) "Body";
    pf ppf "%a@," string bd;
  end;
  pf ppf "@]"
