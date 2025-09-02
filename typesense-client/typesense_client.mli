(** Standalone Typesense client for OCaml *)

(** Configuration for Typesense client *)
type config = {
  endpoint : string;
  api_key : string;
}

(** Error types for Typesense operations *)
type error = 
  | Http_error of int * string
  | Json_error of string
  | Connection_error of string

val pp_error : Format.formatter -> error -> unit

(** Search result types *)
type search_result = {
  id: string;
  title: string;
  content: string;
  score: float;
  collection: string;
  highlights: (string * string list) list;
  document: Ezjsonm.value;  (* Store raw document for flexible field access *)
}

type search_response = {
  hits: search_result list;
  total: int;
  query_time: float;
}

(** Multisearch result types *)
type multisearch_response = {
  results: search_response list;
}

(** Search a single collection *)
val search_collection : config -> string -> string -> ?limit:int -> ?offset:int -> unit -> (search_response, error) result Lwt.t

(** Perform multisearch across all collections *)
val multisearch : config -> string -> ?limit:int -> unit -> (multisearch_response, error) result Lwt.t

(** Combine multisearch results into single result set *)
val combine_multisearch_results : multisearch_response -> ?limit:int -> ?offset:int -> unit -> search_response

(** List all collections *)
val list_collections : config -> ((string * int) list, error) result Lwt.t

(** Pretty printer utilities *)
val extract_field_string : Ezjsonm.value -> string -> string
val extract_field_string_list : Ezjsonm.value -> string -> string list
val extract_field_bool : Ezjsonm.value -> string -> bool
val format_authors : string list -> string
val format_date : string -> string
val format_tags : string list -> string

(** One-line pretty printer for search results *)
val pp_search_result_oneline : search_result -> string