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
