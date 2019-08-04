open Lwt.Infix

module type TAGS = sig
  type tree
  type key

  val tagged_keys : tree -> tag:string -> key list Lwt.t
end

module type SUMMARISER = sig
  val summarise : Repository.pull_request list -> string
end

module Make
    (S : Irmin.S with type key = string list and type step = string and type contents = Contents.t)
    (Tags : TAGS with type tree := S.tree and type key := S.key)
    (Summariser : SUMMARISER)
  = struct
    let summarise tree ~start_time:_ ~end_time:_ ~tag =
      Tags.tagged_keys tree ~tag >>= fun keys ->
      Format.printf "Found %d keys tagged with %s\n%!" (List.length keys) tag;
      Lwt_list.filter_map_p (fun key ->
        S.Tree.find tree key >|= function
        | Some (Repository r) -> Some r
        | Some (TaggedKeys _)
        | Some (Post _)
        | Some (Link _)
        | Some (Tags _)
        | None -> None
      ) keys
      >|= fun repositories ->
      repositories
      |> List.map (fun repository -> repository.Repository.pull_requests)
      |> List.flatten
      |> List.filter (fun _pull_request -> true)
      |> Summariser.summarise
end
