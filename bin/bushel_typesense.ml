open Cmdliner
open Lwt.Syntax

(** TODO:claude Bushel Typesense binary with upload and query functionality *)

let endpoint =
  let doc = "Typesense server endpoint URL" in
  Arg.(value & opt string "http://localhost:8108" & info ["endpoint"; "e"] ~doc)

let api_key =
  let doc = "Typesense API key for authentication" in
  Arg.(value & opt string "" & info ["api-key"; "k"] ~doc)

let openai_key =
  let doc = "OpenAI API key for embeddings" in
  Arg.(value & opt string "" & info ["openai-key"; "oa"] ~doc)

let data_dir =
  let doc = "Directory containing bushel data files" in
  Arg.(value & opt string "." & info ["data-dir"; "d"] ~doc)

(** TODO:claude Main upload function *)
let upload endpoint api_key openai_key data_dir =
  if api_key = "" then (
    Printf.eprintf "Error: API key is required. Use --api-key or set TYPESENSE_API_KEY environment variable.\n";
    exit 1
  );

  if openai_key = "" then (
    Printf.eprintf "Error: OpenAI API key is required for embeddings. Use --openai-key or set OPENAI_API_KEY environment variable.\n";
    exit 1
  );

  let config = Bushel.Typesense.{ endpoint; api_key; openai_key } in

  Printf.printf "Loading bushel data from %s\n" data_dir;
  let entries = Bushel.load data_dir in

  Printf.printf "Uploading bushel data to Typesense at %s\n" endpoint;

  Lwt_main.run (
    Lwt.catch (fun () ->
      Bushel.Typesense.upload_all config entries
    ) (fun exn ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      exit 1
    )
  )


(** TODO:claude Query function *)
let query endpoint api_key query_text collection limit offset =
  let base_config = Bushel.Typesense.load_config_from_files () in
  let config = { 
    Bushel.Typesense.endpoint = if endpoint = "" then base_config.endpoint else endpoint;
    api_key = if api_key = "" then base_config.api_key else api_key;
    openai_key = base_config.openai_key;
  } in
  
  if config.api_key = "" then (
    Printf.eprintf "Error: API key is required. Use --api-key or set TYPESENSE_API_KEY environment variable.\n";
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
  )

(** TODO:claude List collections function *)
let list endpoint api_key =
  let base_config = Bushel.Typesense.load_config_from_files () in
  let config = { 
    Bushel.Typesense.endpoint = if endpoint = "" then base_config.endpoint else endpoint;
    api_key = if api_key = "" then base_config.api_key else api_key;
    openai_key = base_config.openai_key;
  } in
  
  if config.api_key = "" then (
    Printf.eprintf "Error: API key is required. Use --api-key or set TYPESENSE_API_KEY environment variable.\n";
    exit 1
  );
  
  Printf.printf "Listing collections at %s\n\n" config.endpoint;
  
  Lwt_main.run (
    Lwt.catch (fun () ->
      let* result = Bushel.Typesense.list_collections config in
      match result with
      | Ok collections ->
        Printf.printf "Collections:\n";
        List.iter (fun (name, count) ->
          Printf.printf "  %s (%d documents)\n" name count
        ) collections;
        Lwt.return_unit
      | Error err ->
        Format.eprintf "List error: %a\n" Bushel.Typesense.pp_error err;
        exit 1
    ) (fun exn ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      exit 1
    )
  )

(** TODO:claude Command line arguments for query *)
let query_text =
  let doc = "Search query text" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

let collection =
  let doc = "Specific collection to search (contacts, papers, projects, news, videos, notes, ideas)" in
  Arg.(value & opt string "" & info ["collection"; "c"] ~doc)

let limit =
  let doc = "Maximum number of results to return" in
  Arg.(value & opt int 10 & info ["limit"; "l"] ~doc)

let offset =
  let doc = "Number of results to skip (for pagination)" in
  Arg.(value & opt int 0 & info ["offset"; "o"] ~doc)

(** TODO:claude Query command *)
let query_cmd =
  let doc = "Search bushel collections in Typesense" in
  let man = [
    `S Manpage.s_description;
    `P "Search across all or specific bushel collections in Typesense.";
    `P "The API key can be provided via --api-key flag or TYPESENSE_API_KEY environment variable.";
    `P "If .typesense-url and .typesense-api files exist, they will be used for configuration.";
    `S Manpage.s_examples;
    `P "Search all collections:";
    `Pre "  bushel-typesense query \"machine learning\"";
    `P "Search specific collection:";
    `Pre "  bushel-typesense query \"OCaml\" --collection papers";
    `P "Search with pagination:";
    `Pre "  bushel-typesense query \"AI\" --limit 5 --offset 10";
  ] in
  let info = Cmd.info "query" ~doc ~man in
  Cmd.v info Term.(const query $ endpoint $ api_key $ query_text $ collection $ limit $ offset)

(** TODO:claude List command *)
let list_cmd =
  let doc = "List all collections in Typesense" in
  let man = [
    `S Manpage.s_description;
    `P "List all available collections and their document counts.";
  ] in
  let info = Cmd.info "list" ~doc ~man in
  Cmd.v info Term.(const list $ endpoint $ api_key)

(** TODO:claude Updated upload command *)
let upload_cmd =
  let doc = "Upload bushel collections to Typesense search engine" in
  let man = [
    `S Manpage.s_description;
    `P "Upload all bushel object types (contacts, papers, projects, news, videos, notes, ideas) to a Typesense search engine instance.";
    `P "The API key can be provided via --api-key flag or TYPESENSE_API_KEY environment variable.";
    `S Manpage.s_examples;
    `P "Upload to local Typesense instance:";
    `Pre "  bushel-typesense upload --api-key xyz123 --openai-key sk-abc... --data-dir /path/to/data";
    `P "Upload to remote Typesense instance:";
    `Pre "  bushel-typesense upload --endpoint https://search.example.com --api-key xyz123 --openai-key sk-abc...";
  ] in
  let info = Cmd.info "upload" ~doc ~man in
  Cmd.v info Term.(const upload $ endpoint $ api_key $ openai_key $ data_dir)

(** TODO:claude Main command group *)
let main_cmd =
  let doc = "Bushel Typesense client" in
  let man = [
    `S Manpage.s_description;
    `P "Client for uploading to and querying Bushel collections in Typesense search engine.";
    `S Manpage.s_commands;
    `S Manpage.s_common_options;
  ] in
  let info = Cmd.info "bushel-typesense" ~doc ~man in
  Cmd.group info [upload_cmd; query_cmd; list_cmd]

let () =
  (* Check for API keys in environment if not provided *)
  let api_key_env = try Some (Sys.getenv "TYPESENSE_API_KEY") with Not_found -> None in
  let openai_key_env = try Some (Sys.getenv "OPENAI_API_KEY") with Not_found -> None in
  match api_key_env with
  | Some key when key <> "" ->
    (* Override the api_key argument with environment variable *)
    let api_key = Arg.(value & opt string key & info ["api-key"; "k"] ~doc:"Typesense API key") in
    let openai_key = match openai_key_env with
      | Some oa_key when oa_key <> "" -> Arg.(value & opt string oa_key & info ["openai-key"; "oa"] ~doc:"OpenAI API key")
      | _ -> openai_key
    in
    let upload_cmd =
      let doc = "Upload bushel collections to Typesense search engine" in
      let info = Cmd.info "upload" ~doc in
      Cmd.v info Term.(const upload $ endpoint $ api_key $ openai_key $ data_dir)
    in
    let query_cmd =
      let doc = "Search bushel collections in Typesense" in
      let info = Cmd.info "query" ~doc in
      Cmd.v info Term.(const query $ endpoint $ api_key $ query_text $ collection $ limit $ offset)
    in
    let list_cmd =
      let doc = "List all collections in Typesense" in
      let info = Cmd.info "list" ~doc in
      Cmd.v info Term.(const list $ endpoint $ api_key)
    in
    let main_cmd =
      let doc = "Bushel Typesense client" in
      let info = Cmd.info "bushel-typesense" ~doc in
      Cmd.group info [upload_cmd; query_cmd; list_cmd]
    in
    exit (Cmd.eval main_cmd)
  | _ ->
    exit (Cmd.eval main_cmd)