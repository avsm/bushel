open Lwt.Infix

module Make(S : Irmin.S with type key = string list and type step = string and type contents = Contents.t) = struct
  module Store = S

  let extract_links contents =
    match contents with
    | Contents.Post p ->
        let pattern = Re.Posix.(compile (re "bushel://([a-zA-Z_-0-9%./]*)")) in
        Re.all pattern p.contents
        |> List.map (fun group -> Re.Group.get group 1)
        |> List.map (fun s ->
          String.split_on_char '/' s
          |> List.filter ((<>) "")
        )
    | Link _
    | Repository _
    | TaggedKeys _
    | Tags _ -> []

  let rec concat_key k k' =
    match S.Key.decons k' with
    | None -> k
    | Some (step, rest) -> concat_key (S.Key.rcons k step) rest

  let store tree ~source ~target =
    S.Tree.find tree target >>= fun contents ->
    let links = match contents with
      | None -> []
      | Some (Link l) -> l
      | Some (Repository _)
      | Some (Post _)
      | Some (TaggedKeys _)
      | Some (Tags _) -> assert false
    in
    S.Tree.add tree target (Link (source::links))

  let sync tree ~links_key ~contents_key =
    S.Tree.remove tree links_key >>= fun tree' ->
    S.Tree.find_tree tree contents_key >>= function
    | None -> Lwt.return tree'
    | Some contents_tree ->
      S.Tree.fold (fun key contents links_tree ->
        let links = extract_links contents in
        Lwt_list.fold_left_s (fun memo link ->
          let target = concat_key links_key link in
          store memo ~source:key ~target
        ) links_tree links
      ) tree' contents_tree
end
