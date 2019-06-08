open Lwt.Infix

let bushel =
  let open Cmdliner in
  let path =
    let doc =
      Arg.info ~doc:"Bushel path" ~docs:"foo"
        [ "p"; "path" ]
    in
    Arg.(value & opt string "." & doc)
  in
  let create path =
    Bushel.v path
  in
  Term.(const create $ path)

let run_cmd cmd bushel =
  Lwt_main.run begin
    Lwt.catch
      (fun () ->
        bushel >>= cmd
      )
      (fun exn ->
        Format.printf "ERROR: %s\n%s\n" (Printexc.to_string exn) (Printexc.get_backtrace ());
        Lwt.return_unit
      )
  end

let default =
  let open Cmdliner in
  let cmd () =
    print_string "bushel\n"
  in
  Term.(const cmd $ const (), info "usage")

let graphql =
  let open Cmdliner in
  let cmd =
    run_cmd (fun bushel ->
      let server = Bushel.server bushel in
      Conduit_lwt_unix.init ~src:"127.0.0.1" () >>= fun ctx ->
      let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
      let on_exn exn =
        Logs.err (fun l -> l "on_exn: %s" (Printexc.to_string exn))
      in
      Cohttp_lwt_unix.Server.create ~on_exn ~ctx
        ~mode:(`TCP (`Port 8888))
        server
    )
  in
  Term.(const cmd $ bushel, info "graphql")

let add_repo =
  let open Cmdliner in
  let cookie =
    let env = Arg.env_var "GITHUB_COOKIE" in
    Arg.(required & pos 0 (some string) None & info [] ~env)
  in
  let owner =
    let doc = "Owner of Github repo" in
    Arg.(required & pos 1 (some string) None & info [] ~doc)
  in
  let name_ =
    let doc = "Name of Github repo" in
    Arg.(required & pos 2 (some string) None & info [] ~doc)
  in
  let cmd cookie owner name =
    run_cmd (fun bushel ->
      Bushel.add_repo bushel ~cookie ~owner ~name
    )
  in
  Term.(const cmd $ cookie $ owner $ name_ $ bushel, info "add_repo")

let import_post =
  let open Cmdliner in
  let site =
    let doc = "The site to import the post to" in
    Arg.(required & pos 0 (some string) None & info [] ~doc)
  in
  let file =
    let doc = "The file to import as a post" in
    Arg.(required & pos 1 (some string) None & info [] ~doc)
  in
  let cmd site file =
    run_cmd (fun bushel ->
      Bushel.import_post bushel ~site ~file
    )
  in
  Term.(const cmd $ site $ file $ bushel, info "import_post")

let sync =
  let open Cmdliner in
  let cookie =
    let env = Arg.env_var "GITHUB_COOKIE" in
    Arg.(required & pos 0 (some string) None & info [] ~env)
  in
  let cmd cookie =
    run_cmd (fun bushel ->
      Bushel.sync bushel ~cookie
    )
  in
  Term.(const cmd $ cookie $ bushel, info "sync")

let commands = [
  graphql;
  add_repo;
  import_post;
  sync
]

let () =
  Cmdliner.Term.(exit @@ eval_choice default commands)
