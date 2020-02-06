open Lwt.Infix

type link = {
  key : string list;
  target_type : [`Post];
}
[@@deriving irmin]

type t = link list
[@@deriving irmin]

let v ~key ~target_type =
  { key; target_type }

module Tree (S : Irmin.S with type key = string list and type step = string and type contents = string) = struct
  let links_folder =
    [".bushel"; "links"]

  let links_key target =
    links_folder @ target

  let find_or_empty tree from_key =
    let key = links_key from_key in
    S.Tree.find tree key >>= function
    | None -> Lwt.return []
    | Some value ->
        match Irmin.Type.of_string t value with
        | Ok t -> Lwt.return t
        | Error (`Msg msg) -> Lwt.fail_with msg

  let store tree ~key ~link =
    find_or_empty tree key >>= fun links ->
    let serialized = Irmin.Type.to_string t (link::links) in
    S.Tree.add tree (links_key key) serialized

  let delete_all tree =
    S.Tree.remove tree links_folder
end
