open Lwt.Infix

module Args = struct
  let conv_of_type ty =
    let parse = Irmin.Type.of_string ty in
    let print = Irmin.Type.pp ty in
    Cmdliner.Arg.conv (parse, print)

  let key = conv_of_type Bushel.Store.Key.t

  let bushel =
    let open Cmdliner in
    let path =
      let doc = Arg.info ["path"] ~doc:"Path to the bushel store" in
      Arg.(value & opt string "." & doc)
    in
    let create path =
      Bushel.v path
    in
    Term.(const create $ path)
end

let run_cmd cmd bushel =
  Lwt_main.run begin
    Lwt.catch
      (fun () ->
        bushel >>= cmd >|= fun v -> `Ok v
      )
      (fun exn ->
        let error =
          Format.printf "ERROR: %s\n%s\n"
            (Printexc.to_string exn)
            (Printexc.get_backtrace ())
        in
        Lwt.return (`Error (true, error))
      )
  end

let default =
  let cmd () =
    `Help (`Pager, None)
  in
  Cmdliner.Term.(
    const cmd $ const (),
    info "bushel" ~doc:"A data store for blogging collectives" ~exits:default_exits
  )

let graphql =
  let open Cmdliner in
  let port =
    let doc = "Port for HTTP server" in
    Arg.(value & opt int 8888 & info ["p"; "port"] ~docv:"PORT" ~doc)
  in
  let cmd port =
    run_cmd (fun bushel ->
      let server = Bushel.server bushel in
      Conduit_lwt_unix.init ~src:"127.0.0.1" () >>= fun ctx ->
      let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
      let on_exn exn =
        Logs.err (fun l -> l "on_exn: %s" (Printexc.to_string exn))
      in
      Cohttp_lwt_unix.Server.create ~on_exn ~ctx
        ~mode:(`TCP (`Port port))
        server
    )
  in
  Term.(const cmd $ port $ Args.bushel),
  Term.info "graphql" ~doc:"Start graphql server" ~exits:Term.default_exits

let add_repo =
  let open Cmdliner in
  let cookie =
    let doc = "Name of the cookie to use from Github jar" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"GITHUB_COOKIE" ~doc)
  in
  let owner =
    let doc = "Owner of Github repo" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"REPO_OWNER" ~doc)
  in
  let name_ =
    let doc = "Name of Github repo" in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"REPO_NAME" ~doc)
  in
  let cmd cookie owner name =
    run_cmd (fun bushel ->
      Bushel.add_repo bushel ~cookie ~owner ~name
    )
  in
  Term.(const cmd $ cookie $ owner $ name_ $ Args.bushel),
  Term.info "add_repo" ~doc:"Add Github repository to bushel" ~exits:Term.default_exits

let import_post =
  let open Cmdliner in
  let site =
    let doc = "The site to import the blog post to" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SITE" ~doc)
  in
  let file =
    let doc = "The file to import as a blog post" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE" ~doc)
  in
  let cmd site file =
    run_cmd (fun bushel ->
      Bushel.import_post bushel ~site ~file
    )
  in
  Term.(const cmd $ site $ file $ Args.bushel),
  Term.info "import_post" ~doc:"Import blog post into bushel" ~exits:Term.default_exits

let add_tag =
  let open Cmdliner in
  let key =
    let doc = "The key to tag" in
    Arg.(required & pos 0 (some Args.key) None & info [] ~docv:"TAG" ~doc)
  in
  let tag =
    let doc = "The tag to add" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"KEY" ~doc)
  in
  let cmd key tag =
    run_cmd (fun bushel ->
      Bushel.add_tag bushel ~key ~tag
    )
  in
  Term.(const cmd $ key $ tag $ Args.bushel),
  Term.info "add_tag" ~doc:"Add a tag to a key" ~exits:Term.default_exits

let remove_tag =
  let open Cmdliner in
  let key =
    let doc = "The key remove tag from" in
    Arg.(required & pos 0 (some Args.key) None & info [] ~docv:"TAG" ~doc)
  in
  let tag =
    let doc = "The tag to remove" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"KEY" ~doc)
  in
  let cmd key tag =
    run_cmd (fun bushel ->
      Bushel.remove_tag bushel ~key ~tag
    )
  in
  Term.(const cmd $ key $ tag $ Args.bushel),
  Term.info "remove_tag" ~doc:"Remove tag from a key" ~exits:Term.default_exits

let list_tags =
  let open Cmdliner in
  let key =
    let doc = "The key to list tags for" in
    Arg.(required & pos 0 (some Args.key) None & info [] ~docv:"TAG" ~doc)
  in
  let cmd key =
    run_cmd (fun bushel ->
      Bushel.list_tags bushel ~key >|= fun tags ->
      print_string (String.concat ", " tags)
    )
  in
  Term.(const cmd $ key $ Args.bushel),
  Term.info "list_tags" ~doc:"List tags for a key" ~exits:Term.default_exits

let list_tagged_keys =
  let open Cmdliner in
  let tag =
    let doc = "The tag to list keys for" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TAG" ~doc)
  in
  let cmd tag =
    run_cmd (fun bushel ->
      Bushel.list_tagged_keys bushel ~tag >|= fun keys ->
      Fmt.(pr "%a" (list (Irmin.Type.pp Bushel.Store.Key.t)) keys)
    )
  in
  Term.(const cmd $ tag $ Args.bushel),
  Term.info "list_tagged_keys" ~doc:"List all keys with the given tag" ~exits:Term.default_exits

let summarise_tag =
  let open Cmdliner in
  let ptime_conv = Args.conv_of_type Bushel.Ptime_ext.t in
  let tag =
    let doc = "The tag to summarise" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TAG" ~doc)
  in
  let start_time =
    let doc = "The start time of activity to be summarised" in
    Arg.(required & pos 1 (some ptime_conv) None & info [] ~docv:"START_TIME" ~doc)
  in
  let end_time =
    let doc = "The end time of activity to be summarised" in
    Arg.(required & pos 2 (some ptime_conv) None & info [] ~docv:"END_TIME" ~doc)
  in
  let cmd tag start_time end_time =
    run_cmd (fun bushel ->
      Bushel.summarise_tag bushel ~tag ~start_time ~end_time >|= fun summary ->
      print_string summary
    )
  in
  Term.(const cmd $ tag $ start_time $ end_time $ Args.bushel),
  Term.info "summarise_tag" ~doc:"Summarise the activity of all content with a particular tag" ~exits:Term.default_exits

let sync =
  let open Cmdliner in
  let cookie =
    let doc = "The name of the cookie to use from Github jar" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"GITHUB_COOKIE" ~doc)
  in
  let cmd cookie =
    run_cmd (fun bushel ->
      Bushel.sync bushel ~cookie
    )
  in
  Term.(const cmd $ cookie $ Args.bushel),
  Term.info "sync" ~doc:"Synchronises the local data store with remote sources" ~exits:Term.default_exits

let commands = [
  graphql;
  add_repo;
  import_post;
  add_tag;
  remove_tag;
  list_tags;
  list_tagged_keys;
  summarise_tag;
  sync
]

let () =
  Cmdliner.Term.(exit @@ eval_choice default commands)
