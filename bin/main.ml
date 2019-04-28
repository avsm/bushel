open Lwt.Infix

let default =
  let open Cmdliner in
  let usage () =
    print_string "bushel\n"
  in
  Term.(const usage $ const ()),
  Term.(info "bushel")

let add =
  let open Cmdliner in
  let token =
    let env = Arg.env_var "GITHUB_TOKEN" in
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
  let cmd token owner name =
    Lwt_main.run begin
      let cfg = Irmin_git.config "/tmp/bushel-test" in
      Bushel.Store.Repo.v cfg >>= fun repo ->
      Bushel.Store.master repo >>= fun store ->
      Bushel.sync_one store ~token ~owner ~name
    end
  in
  Term.(const cmd $ token $ owner $ name_, info "add")
  
let sync =
  let open Cmdliner in
  let token =
    let env = Arg.env_var "GITHUB_TOKEN" in
    Arg.(required & pos 0 (some string) None & info [] ~env)
  in
  let cmd token =
    Lwt_main.run begin
      let cfg = Irmin_git.config "/tmp/bushel-test" in
      Bushel.Store.Repo.v cfg >>= fun repo ->
      Bushel.Store.master repo >>= fun store ->
      Bushel.sync_all store ~token
    end
  in
  Term.(const cmd $ token, info "sync")

let commands = [
  add;
  sync
]

let () =
  Cmdliner.Term.(exit @@ eval_choice default commands)
