open Cmdliner

let version = "0.1.0"

(* Import actual command implementations from submodules *)

(* Faces command *)
let faces_cmd =
  let doc = "Retrieve face thumbnails from Immich photo service" in
  let info = Cmd.info "faces" ~version ~doc in
  Cmd.v info Bushel_faces.term

(* Links command - uses group structure *)
let links_cmd = Bushel_links.cmd

(* Obsidian command *)
let obsidian_cmd =
  let doc = "Convert Bushel entries to Obsidian format" in
  let info = Cmd.info "obsidian" ~version ~doc in
  Cmd.v info Bushel_obsidian.term

(* Paper command *)
let paper_cmd =
  let doc = "Fetch paper metadata from DOI" in
  let info = Cmd.info "paper" ~version ~doc in
  Cmd.v info Bushel_paper.term

(* Thumbs command *)
let thumbs_cmd =
  let doc = "Generate thumbnails from paper PDFs" in
  let info = Cmd.info "thumbs" ~version ~doc in
  Cmd.v info Bushel_thumbs.term

(* Video command *)
let video_cmd =
  let doc = "Fetch videos from PeerTube instances" in
  let info = Cmd.info "video" ~version ~doc in
  Cmd.v info Bushel_video.term

(* Search command *)
let search_cmd =
  let doc = "Search Bushel collections in Typesense" in
  let info = Cmd.info "search" ~version ~doc in
  Cmd.v info Bushel_search.term

(* Main command *)
let bushel_cmd =
  let doc = "Bushel content management toolkit" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) is a unified command-line tool for managing various types of \
        content in the Bushel system, including papers, videos, links, and more.";
    `P "$(tname) provides unified access to all Bushel functionality through \
        integrated subcommands.";
    `S Manpage.s_commands;
    `S Manpage.s_common_options;
    `S "ENVIRONMENT";
    `P "BUSHEL_CONFIG - Path to configuration file with default settings";
    `S Manpage.s_authors;
    `P "Anil Madhavapeddy";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/avsm/bushel/issues";
  ] in
  let info = Cmd.info "bushel" ~version ~doc ~sdocs ~man in
  Cmd.group info [
    faces_cmd;
    links_cmd;
    obsidian_cmd;
    paper_cmd;
    search_cmd;
    thumbs_cmd;
    video_cmd;
  ]

let () = exit (Cmd.eval' bushel_cmd)