open Cmdliner

(** TODO:claude Get default base directory from BUSHEL_DATA env variable or current directory *)
let get_default_base_dir () =
  match Sys.getenv_opt "BUSHEL_DATA" with
  | Some dir -> dir
  | None -> "."

(** TODO:claude Optional base directory term with BUSHEL_DATA env variable support *)
let base_dir =
  let doc = "Base directory containing Bushel data (defaults to BUSHEL_DATA env var or current directory)" in
  Arg.(value & opt dir (get_default_base_dir ()) & info ["d"; "dir"] ~docv:"DIR" ~doc)

(** TODO:claude Output directory as option *)
let output_dir ~default =
  let doc = "Output directory for generated files" in
  Arg.(value & opt string default & info ["o"; "output"] ~docv:"DIR" ~doc)

(** TODO:claude URL term with custom default *)
let url_term ~default ~doc =
  Arg.(value & opt string default & info ["u"; "url"] ~docv:"URL" ~doc)

(** TODO:claude API key file term *)
let api_key_file ~default =
  let doc = "File containing API key" in
  Arg.(value & opt string default & info ["k"; "key-file"] ~docv:"FILE" ~doc)

(** TODO:claude API key term *)
let api_key =
  let doc = "API key for authentication" in
  Arg.(value & opt (some string) None & info ["api-key"] ~docv:"KEY" ~doc)

(** TODO:claude Overwrite flag *)
let overwrite =
  let doc = "Overwrite existing files" in
  Arg.(value & flag & info ["overwrite"] ~doc)

(** TODO:claude Verbose flag *)
let verbose =
  let doc = "Enable verbose output" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

(** TODO:claude File path term *)
let file_term ~default ~doc =
  Arg.(value & opt string default & info ["f"; "file"] ~docv:"FILE" ~doc)

(** TODO:claude Channel/handle term *)
let channel ~default =
  let doc = "Channel or handle name" in
  Arg.(value & opt string default & info ["c"; "channel"] ~docv:"CHANNEL" ~doc)

(** TODO:claude Optional handle term *)
let handle_opt =
  let doc = "Process specific handle" in
  Arg.(value & opt (some string) None & info ["h"; "handle"] ~docv:"HANDLE" ~doc)

(** TODO:claude Tag term for filtering *)
let tag =
  let doc = "Tag to filter or apply" in
  Arg.(value & opt (some string) None & info ["t"; "tag"] ~docv:"TAG" ~doc)

(** TODO:claude Limit term *)
let limit =
  let doc = "Limit number of items to process" in
  Arg.(value & opt (some int) None & info ["l"; "limit"] ~docv:"N" ~doc)

(** TODO:claude Setup logging with standard options *)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

(** TODO:claude Common setup term combining logs setup *)
let setup_term =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())