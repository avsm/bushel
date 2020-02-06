open Lwt.Infix

module Make(S : Irmin.S with type key = string list and type step = string and type contents = string) = struct
  let key_prefix = [".bushel"; "tags"; "keys"]
  let index_prefix = [".bushel"; "tags"; "index"]

  let tags_type = Irmin.Type.(list string)
  let tagged_keys_type = Irmin.Type.(list (list string))

  let find_exn typ tree key =
    S.Tree.find tree key >>= function
    | None -> Lwt.fail Not_found
    | Some value ->
        match Irmin.Type.of_string typ value with
        | Ok t -> Lwt.return t
        | Error (`Msg msg) -> Lwt.fail_with msg

  let add typ tree key value =
    let serialized = Irmin.Type.to_string typ value in
    S.Tree.add tree key serialized

  let tags_key key =
    key_prefix @ key

  let index_key tag =
    index_prefix @ [tag]

  let tags tree ~key =
    find_exn tags_type tree (tags_key key)

  let tagged_keys tree ~tag =
    find_exn tagged_keys_type tree (index_key tag)

  let store tree ~key ~tags =
    let serialized = Irmin.Type.to_string tags_type tags in
    S.Tree.add tree (tags_key key) serialized

  let check_key_exists tree ~key =
    S.Tree.mem tree key >>= function
    | false -> Lwt.fail (Failure "Key doesn't exist")
    | true -> Lwt.return ()

  let add_tag tree ~key ~tag =
    check_key_exists tree ~key >>= fun () ->
    tags tree ~key >>= fun tags ->
    add tags_type tree (tags_key key) tags >>= fun tree ->
    tagged_keys tree ~tag >>= fun keys ->
    add tagged_keys_type tree (index_key tag) (key::keys)

  let remove_tag tree ~key ~tag =
    tags tree ~key >>= fun tags ->
    let tags' = List.filter (fun tag' -> String.compare tag tag' <> 0) tags in
    add tags_type tree (tags_key key) tags' >>= fun tree ->
    tagged_keys tree ~tag >>= fun keys ->
    let keys' = List.filter (fun key' -> Irmin.Type.compare (S.Key.t) key key' <> 0) keys in
    add tagged_keys_type tree (index_key tag) keys'
end
