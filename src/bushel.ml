open Lwt.Infix

let failwithf = Format.ksprintf failwith

let execute_query q ~token =
  let uri = Uri.of_string "https://api.github.com/graphql" in
  let body =
    Yojson.Basic.to_string (`Assoc [
      "query", `String q#query;
      "variables", q#variables;
    ]) |> Cohttp_lwt.Body.of_string
  in
  let headers = Cohttp.Header.of_list [
    "Content-type", "application/json";
    "Authorization", "Bearer " ^ token
  ] in
  Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (_rsp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body ->
  let json_body = Yojson.Basic.from_string body in
  q#parse (Yojson.Basic.Util.member "data" json_body)

let fail_if_err = function
  | Ok x -> Lwt.return x
  | Error msg -> Lwt.fail (Failure msg)

module Make(S : Irmin.S with type key = string list and type step = string and type contents = Repository.t) = struct
  let sync_one store ~token ~owner ~name =
    let owner_and_name = Format.sprintf "%s/%s" owner name in
    let key = ["data"; "github"; owner_and_name] in
    let q = Queries.Repo.make ~owner ~name () in
    execute_query q ~token >>= fun rsp ->
    Queries.Repo.to_repository rsp |> fail_if_err >>= fun repo ->
    S.set_exn store ~info:Irmin.Info.none key repo

  let sync_all store ~token =
    let key = ["data"; "github"] in
    S.tree store >>= fun tree ->
    S.Tree.list tree key >>= fun nodes ->
    Lwt_list.iter_p (fun (owner_and_name, _) ->
      match String.split_on_char '/' owner_and_name with
      | [owner; name] ->
        sync_one store ~token ~owner ~name
      | _ -> failwithf "Malformed key in data/github: `%s`" owner_and_name
    ) nodes
end

module Store = Irmin_unix.Git.FS.Ref(Repository)
include Make(Store)
