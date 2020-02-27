open Lwt.Infix

module Make(S : Irmin.S with type key = string list and type step = string and type contents = string) = struct
  module L = Backlinks.Make(S)
  module GH = Github.Make(S)
  module T = Tags.Make(S)
  module SG = Summary_generator.Make(S)(T)(struct
    let summarise pull_requests =
      pull_requests
      |> List.map (fun pr -> Format.sprintf "%s\n===\n\n%s" pr.Repository.Pull_request.title pr.body)
      |> String.concat "\n\n---\n\n"
  end)
  module GQL = Graphql.Make(S)

  type t = {
    repo  : S.repo;
    store : S.t;
    jar   : Github_cookie_jar.t;
  }

  let data_key = [".bushel"; "data"; "github"]

  let v path =
    let cfg = Irmin_git.config path in
    S.Repo.v cfg >>= fun repo ->
    S.master repo >>= fun store ->
    Github_cookie_jar.init () >|= fun jar ->
    { repo; store; jar }

  let info s =
    let date = Unix.gettimeofday () |> Int64.of_float in
    let author = "Bushel" in
    Fmt.kstrf (fun msg ->
      fun () ->
        Irmin.Info.v ~date ~author msg
    ) s

  let load_file file =
    let ic = open_in file in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s

  let import_post t ~site ~file =
    let contents = load_file file in
    let key = ["sites"; site; "posts"; Filename.basename file] in
    let info = info "import post %s" file in
    S.set_exn t.store ~info key contents

  let add_repo t ~cookie ~owner ~name =
    Github_cookie_jar.get t.jar ~name:cookie >>= fun auth ->
    let info = info "add repo %s/%s" owner name in
    S.with_tree_exn t.store S.Key.empty ~info (function
      | None -> Lwt.return None
      | Some tree ->
          GH.sync_one tree ~data_key ~token:auth.token ~owner ~name >|= fun tree ->
          Some tree
    )

  let add_tag t ~key ~tag =
    let info = info "add tag %s to %a" tag (Irmin.Type.pp S.Key.t) key in
    S.with_tree_exn t.store S.Key.empty ~info (function
      | None -> Lwt.return None
      | Some tree ->
          T.add_tag tree ~key ~tag >|= fun tree ->
          Some tree
    )

  let remove_tag t ~key ~tag =
    let info = info "remove tag %s from %a" tag (Irmin.Type.pp S.Key.t) key in
    S.with_tree_exn t.store S.Key.empty ~info (function
      | None -> Lwt.return None
      | Some tree ->
          T.remove_tag tree ~key ~tag >|= fun tree ->
          Some tree
    )

  let list_tags t ~key =
    S.find_tree t.store S.Key.empty >>= function
    | None -> Lwt.return []
    | Some tree -> T.tags tree ~key

  let list_tagged_keys t ~tag =
    S.find_tree t.store S.Key.empty >>= function
    | None -> Lwt.return []
    | Some tree -> T.tagged_keys tree ~tag

  let sync t ~cookie =
    Github_cookie_jar.get t.jar ~name:cookie >>= fun auth ->
    let info = info "sync" in
    S.with_tree_exn t.store S.Key.empty ~info (function
      | None -> Lwt.return None
      | Some tree ->
          L.sync tree >>= fun tree ->
          GH.sync tree ~token:auth.token ~data_key >|= fun tree ->
          Some tree
    )

  let summarise_tag t ~tag ~start_time ~end_time =
    S.find_tree t.store S.Key.empty >>= function
      | None -> Lwt.return ""
      | Some tree -> SG.summarise tree ~tag ~start_time ~end_time

  module Graphql_cohttp_lwt =
    Graphql_cohttp.Make (Graphql_lwt.Schema) (Cohttp_lwt_unix.IO)
      (Cohttp_lwt.Body)

  let server t =
    let callback = Graphql_cohttp_lwt.make_callback (fun _req ->
      GQL.ctx ~store:t.store
    ) GQL.schema in
    Cohttp_lwt_unix.Server.make_response_action ~callback ()
end

module Ptime_ext = Ptime_ext
module Store = Irmin_unix.Git.FS.Ref(Irmin.Contents.String)
include Make(Store)
