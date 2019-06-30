type t =
  | Repository of Repository.t
  | Post of Post.t
  | Link of Link.t
  | Tags of string list
  | TaggedKeys of string list list

let t =
  let open Irmin.Type in
  variant "contents" (fun repository post link tags tagged_keys -> function
    | Repository r -> repository r
    | Post p -> post p
    | Link l -> link l
    | Tags t -> tags t
    | TaggedKeys keys -> tagged_keys keys
  )
  |~ case1 "Repository" Repository.t (fun r -> Repository r)
  |~ case1 "Post" Post.t (fun p -> Post p)
  |~ case1 "Link" Link.t (fun l -> Link l)
  |~ case1 "Tags" (list string) (fun t -> Tags t)
  |~ case1 "TaggedKeys" (list (list string)) (fun keys -> TaggedKeys keys)
  |> sealv

let merge = Irmin.Merge.(option (idempotent t))
