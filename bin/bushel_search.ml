open Cmdliner
open Lwt.Syntax

(** TODO:claude Bushel search command for integration with main CLI *)

let endpoint =
  let doc = "Typesense server endpoint URL" in
  Arg.(value & opt string "" & info ["endpoint"; "e"] ~doc)

let api_key =
  let doc = "Typesense API key for authentication" in
  Arg.(value & opt string "" & info ["api-key"; "k"] ~doc)


let limit =
  let doc = "Maximum number of results to return" in
  Arg.(value & opt int 50 & info ["limit"; "l"] ~doc)

let offset =
  let doc = "Number of results to skip (for pagination)" in
  Arg.(value & opt int 0 & info ["offset"; "o"] ~doc)

let query_text =
  let doc = "Search query text" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

(** TODO:claude Search function using multisearch *)
let search endpoint api_key query_text limit offset =
  let base_config = Bushel.Typesense.load_config_from_files () in
  let config = { 
    Bushel.Typesense.endpoint = if endpoint = "" then base_config.endpoint else endpoint;
    api_key = if api_key = "" then base_config.api_key else api_key;
    openai_key = base_config.openai_key;
  } in
  
  if config.api_key = "" then (
    Printf.eprintf "Error: API key is required. Use --api-key, set TYPESENSE_API_KEY environment variable, or create .typesense-key file.\n";
    exit 1
  );
  
  Printf.printf "Searching Typesense at %s\n" config.endpoint;
  Printf.printf "Query: \"%s\"\n" query_text;
  Printf.printf "Limit: %d, Offset: %d\n" limit offset;
  Printf.printf "\n";
  
  Lwt_main.run (
    Lwt.catch (fun () ->
      let* result = Bushel.Typesense.multisearch config query_text ~limit:50 () in
      match result with
      | Ok multisearch_resp ->
        let combined_response = Bushel.Typesense.combine_multisearch_results multisearch_resp ~limit ~offset () in
        Printf.printf "Found %d results (%.2fms)\n\n" combined_response.total combined_response.query_time;
        
        List.iteri (fun i (hit : Bushel.Typesense.search_result) ->
          Printf.printf "%d. %s (score: %.2f)\n" (i + 1) (Bushel.Typesense.pp_search_result_oneline hit) hit.Bushel.Typesense.score
        ) combined_response.hits;
        Lwt.return_unit
      | Error err ->
        Format.eprintf "Search error: %a\n" Bushel.Typesense.pp_error err;
        exit 1
    ) (fun exn ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      exit 1
    )
  );
  0

(** TODO:claude Command line term *)
let term = Term.(const search $ endpoint $ api_key $ query_text $ limit $ offset)