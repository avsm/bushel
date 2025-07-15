(** Typesense API client for Bushel
    
    This module provides an OCaml client for the Typesense search engine API.
    It handles collection management and document indexing for all Bushel object
    types including contacts, papers, projects, news, videos, notes, and ideas.
    
    Example usage:
    {[
      let config = { endpoint = "https://search.example.com"; api_key = "xyz123" } in
      Lwt_main.run (Typesense.upload_all config "/path/to/bushel/data")
    ]}
    
    TODO:claude *)

(** Configuration for connecting to a Typesense server *)
type config = {
  endpoint : string;  (** Typesense server URL (e.g., "https://search.example.com") *)
  api_key : string;   (** API key for authentication *)
}

(** Possible errors that can occur during Typesense operations *)
type error = 
  | Http_error of int * string        (** HTTP error with status code and message *)
  | Json_error of string              (** JSON parsing or encoding error *)
  | Connection_error of string        (** Network connection error *)

(** Pretty-printer for error types *)
val pp_error : Format.formatter -> error -> unit

(** Create a collection with the given schema. 
    The schema should follow Typesense's collection schema format.
    TODO:claude *)
val create_collection : config -> Ezjsonm.value -> (string, error) result Lwt.t

(** Check if a collection exists by name. 
    Returns true if the collection exists, false otherwise.
    TODO:claude *)
val collection_exists : config -> string -> bool Lwt.t

(** Delete a collection by name.
    TODO:claude *)
val delete_collection : config -> string -> (string, error) result Lwt.t

(** Upload documents to a collection in batch using JSONL format.
    More efficient than uploading documents one by one.
    TODO:claude *)
val upload_documents : config -> string -> Ezjsonm.value list -> (string, error) result Lwt.t

(** Upload all bushel objects from a data directory to Typesense.
    This function will:
    - Load all bushel data types from the specified directory
    - Create or recreate collections for each type
    - Upload all documents in batches
    - Report progress to stdout
    TODO:claude *)
val upload_all : config -> string -> unit Lwt.t