type t =
  { names : string list
  ; handle : string
  ; email : string option
  ; icon : string option
  ; github : string option
  ; twitter : string option
  ; bluesky : string option
  ; mastodon : string option
  ; orcid : string option
  ; url : string option
  ; atom : string list option
  }

type ts = t list

let v ?email ?github ?twitter ?bluesky ?mastodon ?orcid ?icon ?url ?atom handle names =
  { names; handle; email; github; twitter; bluesky; mastodon; orcid; url; icon; atom }
;;

let make names email icon github twitter bluesky mastodon orcid url atom =
  v ?email ?github ?twitter ?bluesky ?mastodon ?orcid ?icon ?url ?atom "" names
;;

let names { names; _ } = names
let name { names; _ } = List.hd names
let handle { handle; _ } = handle
let email { email; _ } = email
let icon { icon; _ } = icon
let github { github; _ } = github
let twitter { twitter; _ } = twitter
let bluesky { bluesky; _ } = bluesky
let mastodon { mastodon; _ } = mastodon
let orcid { orcid; _ } = orcid
let url { url; _ } = url
let atom { atom; _ } = atom

let json_t =
  let open Jsont in
  let open Jsont.Object in
  let mem_opt f v ~enc = mem f v ~dec_absent:None ~enc_omit:Option.is_none ~enc in
  map ~kind:"Contact" make
  |> mem "names" (list string) ~dec_absent:[] ~enc:names
  |> mem_opt "email" (some string) ~enc:email
  |> mem_opt "icon" (some string) ~enc:icon
  |> mem_opt "github" (some string) ~enc:github
  |> mem_opt "twitter" (some string) ~enc:twitter
  |> mem_opt "bluesky" (some string) ~enc:bluesky
  |> mem_opt "mastodon" (some string) ~enc:mastodon
  |> mem_opt "orcid" (some string) ~enc:orcid
  |> mem_opt "url" (some string) ~enc:url
  |> mem_opt "atom" (some (list string)) ~enc:atom
  |> finish
;;

let v = Jsont_bytesrw.decode_string (Jsont.list json_t)
let compare a b = String.compare a.handle b.handle
let find_by_handle ts h = List.find_opt (fun { handle; _ } -> handle = h) ts

let best_url c =
  match c.url with
  | Some v -> Some v
  | None ->
    (match c.github with
     | Some v -> Some ("https://github.com/" ^ v)
     | None ->
       (match c.email with
        | Some v -> Some ("mailto:" ^ v)
        | None -> None))
;;

let of_md fname =
  (* TODO fix Jekyll_post to not error on no date *)
  let fname' = "2000-01-01-" ^ Filename.basename fname in
  let handle = Filename.basename fname |> Filename.chop_extension in
  match Jekyll_post.of_string ~fname:fname' (Util.read_file fname) with
  | Error (`Msg m) -> failwith ("contact_of_md: " ^ m)
  | Ok jp ->
    let fields = jp.Jekyll_post.fields |> Jekyll_format.fields_to_yaml in
    let c = Jsont_bytesrw.decode_string json_t (Ezjsonm.value_to_string fields) in
    (match c with
     | Error e -> failwith e
     | Ok c -> { c with handle })
;;

(* Given a name, turn it lowercase and return the concatenation of the
initials of all the words in the name and the full last name. *)
let handle_of_name name =
  let name = String.lowercase_ascii name in
  let words = String.split_on_char ' ' name in
  let initials = String.concat "" (List.map (fun w -> String.sub w 0 1) words) in
  initials ^ List.hd (List.rev words)
;;

(* fuzzy lookup for an author. Strip out any non alpha numeric characters while
   searching for the name *)
let lookup_by_name ts a =
  let a = String.lowercase_ascii a in
  let rec aux acc = function
    | [] -> acc
    | t :: ts ->
      if List.exists (fun n -> String.lowercase_ascii n = a) t.names
      then aux (t :: acc) ts
      else aux acc ts
  in
  match aux [] ts with
  | [ a ] -> a
  | [] -> raise (Failure ("contact.ml: author not found: " ^ a))
  | _ -> raise (Failure ("ambiguous author: " ^ a))
;;

(* TODO:claude *)
let typesense_schema =
  let open Ezjsonm in
  dict [
    ("name", string "contacts");
    ("fields", list (fun d -> dict d) [
      [("name", string "id"); ("type", string "string")];
      [("name", string "handle"); ("type", string "string")];
      [("name", string "name"); ("type", string "string")];
      [("name", string "names"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "email"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "icon"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "github"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "twitter"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "bluesky"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "mastodon"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "orcid"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "url"); ("type", string "string[]"); ("optional", bool true)];
      [("name", string "atom"); ("type", string "string[]"); ("optional", bool true)];
    ]);
  ]

(** TODO:claude Pretty-print a contact with ANSI formatting *)
let pp ppf c =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %a@," (styled `Bold string) "Type" (styled `Cyan string) "Contact";
  pf ppf "%a: @%a@," (styled `Bold string) "Handle" string (handle c);
  pf ppf "%a: %a@," (styled `Bold string) "Name" string (name c);
  let ns = names c in
  if List.length ns > 1 then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Aliases" (list ~sep:comma string) (List.tl ns);
  (match email c with
   | Some e -> pf ppf "%a: %a@," (styled `Bold string) "Email" string e
   | None -> ());
  (match github c with
   | Some g -> pf ppf "%a: https://github.com/%a@," (styled `Bold string) "GitHub" string g
   | None -> ());
  (match twitter c with
   | Some t -> pf ppf "%a: https://twitter.com/%a@," (styled `Bold string) "Twitter" string t
   | None -> ());
  (match bluesky c with
   | Some b -> pf ppf "%a: %a@," (styled `Bold string) "Bluesky" string b
   | None -> ());
  (match mastodon c with
   | Some m -> pf ppf "%a: %a@," (styled `Bold string) "Mastodon" string m
   | None -> ());
  (match orcid c with
   | Some o -> pf ppf "%a: https://orcid.org/%a@," (styled `Bold string) "ORCID" string o
   | None -> ());
  (match url c with
   | Some u -> pf ppf "%a: %a@," (styled `Bold string) "URL" string u
   | None -> ());
  (match icon c with
   | Some i -> pf ppf "%a: %a@," (styled `Bold string) "Icon" string i
   | None -> ());
  (match atom c with
   | Some atoms when atoms <> [] ->
     pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Atom Feeds" (list ~sep:comma string) atoms
   | _ -> ());
  pf ppf "@]"
