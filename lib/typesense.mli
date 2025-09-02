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
  endpoint : string;    (** Typesense server URL (e.g., "https://search.example.com") *)
  api_key : string;     (** API key for authentication *)
  openai_key : string;  (** OpenAI API key for embeddings *)
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

(** Search result structure containing document information and relevance score *)
type search_result = {
  id: string;                               (** Document ID *)
  title: string;                            (** Document title *)
  content: string;                          (** Document content/description *)
  score: float;                             (** Relevance score *)
  collection: string;                       (** Collection name *)
  highlights: (string * string list) list; (** Highlighted search terms by field *)
  document: Ezjsonm.value;                  (** Raw document for flexible field access *)
}

(** Search response containing results and metadata *)
type search_response = {
  hits: search_result list;                 (** List of matching documents *)
  total: int;                               (** Total number of matches *)
  query_time: float;                        (** Query execution time in milliseconds *)
}

(** Search a specific collection.
    TODO:claude *)
val search_collection : config -> string -> string -> ?limit:int -> ?offset:int -> unit -> (search_response, error) result Lwt.t

(** Search across all bushel collections.
    Results are sorted by relevance score and paginated.
    TODO:claude *)
val search_all : config -> string -> ?limit:int -> ?offset:int -> unit -> (search_response, error) result Lwt.t

(** Multisearch response containing results from multiple collections *)
type multisearch_response = {
  results: search_response list;                (** Results from each collection *)
}

(** Perform multisearch across all collections using Typesense's multi_search endpoint.
    More efficient than individual searches as it's done in a single request.
    TODO:claude *)
val multisearch : config -> string -> ?limit:int -> unit -> (multisearch_response, error) result Lwt.t

(** Combine multisearch results into a single result set.
    Results are sorted by relevance score and paginated.
    TODO:claude *)
val combine_multisearch_results : multisearch_response -> ?limit:int -> ?offset:int -> unit -> search_response

(** List all collections with document counts.
    Returns a list of (collection_name, document_count) pairs.
    TODO:claude *)
val list_collections : config -> ((string * int) list, error) result Lwt.t

(** Load configuration from .typesense-url and .typesense-api files.
    Falls back to environment variables and defaults.
    TODO:claude *)
val load_config_from_files : unit -> config

(** Pretty-print a search result in a one-line format with relevant information.
    Shows different fields based on the collection type (papers, videos, etc.).
    TODO:claude *)
val pp_search_result_oneline : search_result -> string