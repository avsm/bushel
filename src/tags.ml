open Lwt.Infix

module Make(S : Irmin.S with type key = string list and type step = string and type contents = Contents.t) = struct
  let key_prefix = ["tags"; "keys"]
  let index_prefix = ["tags"; "index"]

  let tag_key key =
    key_prefix @ key

  let index_key tag =
    index_prefix @ [tag]

  let tags tree ~key =
    S.Tree.find tree (tag_key key) >|= function
    | None -> []
    | Some (Tags ts) -> ts
    | Some (Repository _)
    | Some (Post _)
    | Some (Link _)
    | Some (TaggedKeys _) -> assert false

  let tagged_keys tree ~tag =
    S.Tree.find tree (index_key tag) >|= function
    | None -> []
    | Some (TaggedKeys keys) -> keys
    | Some (Repository _)
    | Some (Post _)
    | Some (Link _)
    | Some (Tags _) -> assert false

  let store tree ~key ~tags =
    S.Tree.add tree (tag_key key) (Tags tags)

  let check_key_exists tree ~key =
    S.Tree.mem tree key >>= function
    | false -> Lwt.fail (Failure "Key doesn't exist")
    | true -> Lwt.return ()

  let add_tag tree ~key ~tag =
    check_key_exists tree ~key >>= fun () ->
    tags tree ~key >>= fun tags ->
    S.Tree.add tree (tag_key key) (Tags tags) >>= fun tree ->
    tagged_keys tree ~tag >>= fun keys ->
    S.Tree.add tree (index_key tag) (TaggedKeys (key::keys))

  let remove_tag tree ~key ~tag =
    tags tree ~key >>= fun tags ->
    let tags' = List.filter (fun tag' -> String.compare tag tag' <> 0) tags in
    S.Tree.add tree (tag_key key) (Tags tags') >>= fun tree ->
    tagged_keys tree ~tag >>= fun keys ->
    let keys' = List.filter (fun key' -> Irmin.Type.compare (S.Key.t) key key' <> 0) keys in
    S.Tree.add tree (index_key tag) (TaggedKeys keys')
end
