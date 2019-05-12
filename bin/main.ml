open Lwt.Infix

let path = "/tmp/bushel-test"

let run_cmd cmd =
  Lwt_main.run begin
    Lwt.catch
      cmd
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
  let cmd () =
    run_cmd (fun () ->
      Bushel.v path >>= fun bushel ->
      let server = Bushel.Server.v bushel in
      Conduit_lwt_unix.init ~src:"localhost" () >>= fun ctx ->
      let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
      let on_exn exn =
        Logs.debug (fun l -> l "on_exn: %s" (Printexc.to_string exn))
      in
      Cohttp_lwt_unix.Server.create ~on_exn ~ctx
        ~mode:(`TCP (`Port 8888))
        server
    )
  in
  Term.(const cmd $ const (), info "graphql")

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
    run_cmd (fun () ->
      Bushel.v path >>= fun bushel ->
      Bushel.add_repo bushel ~cookie ~owner ~name
    )
  in
  Term.(const cmd $ cookie $ owner $ name_, info "add_repo")

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
    run_cmd (fun () ->
      Bushel.v path >>= fun bushel ->
      Bushel.import_post bushel ~site ~file
    )
  in
  Term.(const cmd $ site $ file, info "import_post")

let sync =
  let open Cmdliner in
  let cookie =
    let env = Arg.env_var "GITHUB_COOKIE" in
    Arg.(required & pos 0 (some string) None & info [] ~env)
  in
  let cmd cookie =
    run_cmd (fun () ->
      Bushel.v path >>= fun bushel ->
      Bushel.sync bushel ~cookie
    )
  in
  Term.(const cmd $ cookie, info "sync")

let commands = [
  graphql;
  add_repo;
  import_post;
  sync
]

let () =
  Cmdliner.Term.(exit @@ eval_choice default commands)
