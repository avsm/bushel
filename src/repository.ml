open Lwt.Infix

module Pull_request = struct
  type state =
    | Closed
    | Merged
    | Open
  [@@deriving irmin]

  type t = {
    number : int;
    title : string;
    state : state;
    created_at : Ptime_ext.t;
    updated_at : Ptime_ext.t;
    merged_at : Ptime_ext.t option;
    body : string;
  }
  [@@deriving irmin]
end

type t = {
  updated_at : Ptime_ext.t;
  pull_requests : Pull_request.t list;
}
[@@deriving irmin]


module Tree (S : Irmin.S with type key = string list and type step = string and type contents = string) = struct
  let find_exn tree key =
    S.Tree.find tree key >>= function
    | None -> Lwt.fail Not_found
    | Some value ->
        match Irmin.Type.of_string t value with
        | Ok t -> Lwt.return t
        | Error (`Msg msg) -> Lwt.fail_with msg
end
