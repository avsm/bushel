open Lwt.Infix

type t = {
  key : string list;
  contents : string;
}

module Tree (S : Irmin.S with type key = string list and type step = string and type contents = string) = struct
  let posts_key site =
    site.Site.key @ site.Site.posts_key

  let post_key site post_name =
    (posts_key site) @ [post_name]

  let find tree key =
    S.Tree.find tree key >>= function
    | None -> Lwt.fail Not_found
    | Some contents -> Lwt.return { key; contents }

  let find_by_site_and_name tree site name =
    let key = post_key site name in
    find tree key

  let list tree site =
    S.Tree.list tree (posts_key site) >>= fun entries ->
    Lwt_list.filter_map_p (fun (file_name, kind) ->
      match kind with
      | `Node -> Lwt.return None
      | `Contents ->
          find_by_site_and_name tree site file_name >|= fun post ->
          Some post
    ) entries

  let fold tree site ~f =
    list tree site >>= fun posts ->
    Lwt_list.fold_left_s (fun tree' post ->
      f tree' post
    ) tree posts
end
