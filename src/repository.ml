(* ppx_irmin does not support types qualified by structure name, so this code has to
 * work around that limitation (https://github.com/CraigFe/ppx_irmin/issues/1) *)
type ptime = Ptime_ext.t
let ptime_t = Ptime_ext.t

type pull_request_state =
  | Closed
  | Merged
  | Open
[@@deriving irmin]

type pull_request = {
  number : int;
  title : string;
  state : pull_request_state;
  created_at : ptime;
  updated_at : ptime;
  merged_at : ptime option;
  body : string;
}
[@@deriving irmin]

type t = {
  updated_at : ptime;
  pull_requests : pull_request list;
}
[@@deriving irmin]
