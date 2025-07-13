(** TODO:claude Typesense API client for Bushel *)

type config = {
  endpoint : string;
  api_key : string;
}

type error = 
  | Http_error of int * string
  | Json_error of string
  | Connection_error of string

val pp_error : Format.formatter -> error -> unit

(** TODO:claude Create a collection with the given schema *)
val create_collection : config -> Ezjsonm.value -> (string, error) result Lwt.t

(** TODO:claude Check if a collection exists *)
val collection_exists : config -> string -> bool Lwt.t

(** TODO:claude Delete a collection *)
val delete_collection : config -> string -> (string, error) result Lwt.t

(** TODO:claude Upload documents to a collection in batch *)
val upload_documents : config -> string -> Ezjsonm.value list -> (string, error) result Lwt.t

(** TODO:claude Upload all bushel objects from a data directory to Typesense *)
val upload_all : config -> string -> unit Lwt.t