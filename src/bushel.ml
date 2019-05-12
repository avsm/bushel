open Lwt.Infix

module Make(S : Irmin.S with type key = string list and type step = string and type contents = Contents.t) = struct
  module L = Backlinks.Make(S)
  module GH = Github.Make(S)

  type t = {
    repo  : S.repo;
    store : S.t;
    jar   : Github_cookie_jar.t;
  }

  let v path =
    let cfg = Irmin_git.config path in
    S.Repo.v cfg >>= fun repo ->
    S.master repo >>= fun store ->
    Github_cookie_jar.init () >|= fun jar ->
    { repo; store; jar }

  module Server = struct
    include Irmin_unix.Graphql.Server.Make
      (S)
      (struct
        let remote = None
      end)

    let v t =
      v t.repo
  end

  let info s =
    let date = Unix.gettimeofday () |> Int64.of_float in
    let author = "Bushel" in
    Format.ksprintf (fun msg ->
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
    S.set_exn t.store ~info key (Post { contents })

  let add_repo t ~cookie ~owner ~name =
    Github_cookie_jar.get t.jar ~name:cookie >>= fun auth ->
    let info = info "add repo %s/%s" owner name in
    S.with_tree_exn t.store S.Key.empty ~info (function
      | None -> Lwt.return None
      | Some tree ->
          GH.sync_one tree ~token:auth.token ~owner ~name >|= fun tree ->
          Some tree
    )

  let sync t ~cookie =
    Github_cookie_jar.get t.jar ~name:cookie >>= fun auth ->
    let info = info "sync" in
    S.with_tree_exn t.store S.Key.empty ~info (function
      | None -> Lwt.return None
      | Some tree ->
          L.sync tree ~links_key:["links"] ~contents_key:["sites"] >>= fun tree ->
          GH.sync tree ~token:auth.token ~data_key:["data"; "github"] >|= fun tree ->
          Some tree
    )
end

module Store = Irmin_unix.Git.FS.Ref(Contents)
include Make(Store)
