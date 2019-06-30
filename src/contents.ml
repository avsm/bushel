type t =
  | Repository of Repository.t
  | Post of Post.t
  | Link of Link.t

let t =
  let open Irmin.Type in
  variant "contents" (fun repository post link -> function
    | Repository r -> repository r
    | Post p -> post p
    | Link l -> link l
  )
  |~ case1 "Repository" Repository.t (fun r -> Repository r)
  |~ case1 "Post" Post.t (fun p -> Post p)
  |~ case1 "Link" Link.t (fun l -> Link l)
  |> sealv

let merge = Irmin.Merge.(option (idempotent t))
