open Lwt.Infix

module Make(S : Irmin.S with type key = string list and type step = string and type contents = string) = struct
  module Tree = struct
    module Site = Site.Tree (S)
    module Post = Post.Tree (S)
    module Link = Link.Tree (S)
  end

  let extract_keys post =
    let pattern = Re.Posix.(compile (re "bushel://([a-zA-Z_-0-9%./]*)")) in
    Re.all pattern post.Post.contents
    |> List.map (fun group -> Re.Group.get group 1)
    |> List.map (fun s ->
      String.split_on_char '/' s
      |> List.filter ((<>) "")
    )

  let fold_posts tree ~f =
    Tree.Site.fold tree ~f:(fun tree' site ->
      Tree.Post.list tree' site >>= fun posts ->
      Lwt_list.fold_left_s (fun tree'' post ->
        f tree'' post
      ) tree posts
    )
  ;;

  let sync tree =
    Tree.Link.delete_all tree >>= fun tree' ->
    fold_posts tree' ~f:(fun tree'' post ->
      let keys = extract_keys post in
      let link = Link.v ~key:post.key ~target_type:`Post in
      Lwt_list.fold_left_s (fun tree''' key ->
        Tree.Link.store tree''' ~key ~link
      ) tree'' keys
    )
end
