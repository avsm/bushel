open Lwt.Infix

module Queries = struct
  module Repo = struct
    include [%graphql {|
      query PR($owner: String!, $name: String!) {
        repository(owner: $owner, name: $name) {
          updatedAt @bsDecoder(fn: "Yojson.Basic.Util.to_string")
          pullRequests(first: 100, orderBy: {field: UPDATED_AT, direction: DESC}) {
            nodes {
              number
              title
              state
              updatedAt
              headRef {
                id
              }
              baseRef {
                id
              }
              author {
                login
              }
              body
              comments(first: 100) {
                nodes {
                  author {
                    login
                  }
                  body
                  updatedAt
                }
                pageInfo {
                  endCursor
                  hasNextPage
                }
                totalCount
              }
            }
            pageInfo {
              endCursor
              hasNextPage
            }
            totalCount
          }
        }
      }
    |}]

    let to_repository rsp =
      match rsp#repository with
      | Some repo ->
          Ok {
            Repository.updated_at = repo#updatedAt
          }
      | None -> Error "Repository not found"
  end
end

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

module Make(S : Irmin.S with type key = string list and type step = string and type contents = Contents.t) = struct
  module Store = S

  let sync_one tree ~token ~owner ~name =
    let owner_and_name = Format.sprintf "%s/%s" owner name in
    let key = ["data"; "github"; owner_and_name] in
    let q = Queries.Repo.make ~owner ~name () in
    execute_query q ~token >>= fun rsp ->
    Queries.Repo.to_repository rsp |> fail_if_err >>= fun repo ->
    S.Tree.add tree key (Repository repo)

  let sync tree ~token ~data_key =
    S.Tree.list tree data_key >>= fun owners ->
    Lwt_list.fold_left_s (fun tree (owner, _) ->
      let owner_key = S.Key.rcons data_key owner in
      S.Tree.list tree owner_key >>= fun repos ->
      Lwt_list.fold_left_s (fun tree (name, _) ->
        sync_one tree ~token ~owner ~name
      ) tree repos
    ) tree owners
end
