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
