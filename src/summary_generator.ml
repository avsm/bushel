open Lwt.Infix

module type TAGS = sig
  type tree
  type key

  val tagged_keys : tree -> tag:string -> key list Lwt.t
end

module type SUMMARISER = sig
  val summarise : Repository.Pull_request.t list -> string
end

module Make
    (S : Irmin.S with type key = string list and type step = string and type contents = string)
    (Tags : TAGS with type tree := S.tree and type key := S.key)
    (Summariser : SUMMARISER)
  = struct
    module Tree = struct
      module Repository = Repository.Tree (S)
    end

    let summarise tree ~start_time:_ ~end_time:_ ~tag =
      Tags.tagged_keys tree ~tag >>= fun keys ->
      Format.printf "Found %d keys tagged with %s\n%!" (List.length keys) tag;
      Lwt_list.filter_map_p (function
        [".bushel"; "data"; "github"; _] as key ->
          Tree.Repository.find_exn tree key >|= fun repo ->
          Some repo
        | _ -> Lwt.return_none
      ) keys
      >|= fun repositories ->
      repositories
      |> List.map (fun repository -> repository.Repository.pull_requests)
      |> List.flatten
      |> List.filter (fun _pull_request -> true)
      |> Summariser.summarise
end
