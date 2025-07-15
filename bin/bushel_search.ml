open Cmdliner
open Lwt.Syntax

(** TODO:claude Bushel search command for integration with main CLI *)

let endpoint =
  let doc = "Typesense server endpoint URL" in
  Arg.(value & opt string "" & info ["endpoint"; "e"] ~doc)

let api_key =
  let doc = "Typesense API key for authentication" in
  Arg.(value & opt string "" & info ["api-key"; "k"] ~doc)

let collection =
  let doc = "Specific collection to search (contacts, papers, projects, news, videos, notes, ideas)" in
  Arg.(value & opt string "" & info ["collection"; "c"] ~doc)

let limit =
  let doc = "Maximum number of results to return" in
  Arg.(value & opt int 10 & info ["limit"; "l"] ~doc)

let offset =
  let doc = "Number of results to skip (for pagination)" in
  Arg.(value & opt int 0 & info ["offset"; "o"] ~doc)

let query_text =
  let doc = "Search query text" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

(** TODO:claude Search function *)
let search endpoint api_key query_text collection limit offset =
  let base_config = Bushel.Typesense.load_config_from_files () in
  let config = { 
    Bushel.Typesense.endpoint = if endpoint = "" then base_config.endpoint else endpoint;
    api_key = if api_key = "" then base_config.api_key else api_key;
    openai_key = base_config.openai_key;
  } in
  
  if config.api_key = "" then (
    Printf.eprintf "Error: API key is required. Use --api-key, set TYPESENSE_API_KEY environment variable, or create .typesense-api file.\n";
    exit 1
  );
  
  Printf.printf "Searching Typesense at %s\n" config.endpoint;
  Printf.printf "Query: \"%s\"\n" query_text;
  if collection <> "" then Printf.printf "Collection: %s\n" collection;
  Printf.printf "Limit: %d, Offset: %d\n" limit offset;
  Printf.printf "\n";
  
  Lwt_main.run (
    Lwt.catch (fun () ->
      let search_fn = if collection = "" then 
        Bushel.Typesense.search_all config query_text ~limit ~offset
      else
        Bushel.Typesense.search_collection config collection query_text ~limit ~offset
      in
      let* result = search_fn () in
      match result with
      | Ok response ->
        Printf.printf "Found %d results (%.2fms)\n\n" response.total response.query_time;
        List.iteri (fun i (hit : Bushel.Typesense.search_result) ->
          Printf.printf "%d. [%s] %s (score: %.2f)\n" (i + 1) hit.collection hit.title hit.score;
          if hit.content <> "" then Printf.printf "   %s\n" hit.content;
          if hit.highlights <> [] then (
            Printf.printf "   Highlights:\n";
            List.iter (fun (field, snippets) ->
              List.iter (fun snippet ->
                Printf.printf "     %s: %s\n" field snippet
              ) snippets
            ) hit.highlights
          );
          Printf.printf "\n"
        ) response.hits;
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
let term = Term.(const search $ endpoint $ api_key $ query_text $ collection $ limit $ offset)