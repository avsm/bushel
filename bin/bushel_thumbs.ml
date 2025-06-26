open Printf
open Cmdliner

(** TODO:claude
    Helper module for ImageMagick operations *)
module Imagemagick = struct
  (* Generate thumbnail from PDF *)
  let generate_thumbnail ~pdf_path ~size ~output_path =
    let cmd = 
      sprintf "magick -density 600 -quality 100 %s[0] -gravity North -crop 100%%x50%%+0+0 -resize %s %s" 
        pdf_path size output_path
    in
    eprintf "Running: %s\n%!" cmd;
    Sys.command cmd
end

(** TODO:claude
    Process a single paper to generate its thumbnail *)
let process_paper base_dir output_dir paper =
  let slug = Bushel.Paper.slug paper in
  let pdf_path = sprintf "%s/static/papers/%s.pdf" base_dir slug in
  let thumbnail_path = sprintf "%s/%s.png" output_dir slug in
  
  (* Skip if thumbnail already exists *)
  if Sys.file_exists thumbnail_path then (
    printf "Thumbnail already exists for %s, skipping\n%!" slug
  ) else if Sys.file_exists pdf_path then (
    try
      let size = sprintf "2048x" in
      printf "Generating high-res thumbnail for %s (size: %s)\n%!" slug size;
      match Imagemagick.generate_thumbnail ~pdf_path ~size ~output_path:thumbnail_path with
      | 0 -> printf "Successfully generated thumbnail for %s\n%!" slug
      | n -> eprintf "Error generating thumbnail for %s (exit code: %d)\n%!" slug n
    with
    | e -> eprintf "Error processing paper %s: %s\n%!" slug (Printexc.to_string e)
  ) else (
    eprintf "PDF file not found for paper: %s\n%!" slug
  )

(** TODO:claude
    Main function to process all papers in a directory *)
let process_papers base_dir output_dir =
  (* Create output directory if it doesn't exist *)
  if not (Sys.file_exists output_dir) then (
    printf "Creating output directory: %s\n%!" output_dir;
    Unix.mkdir output_dir 0o755
  );
  
  (* Load Bushel entries and get papers *)
  printf "Loading papers from %s\n%!" base_dir;
  let e = Bushel.load base_dir in
  let papers = Bushel.Entry.papers e in
  
  (* Process each paper *)
  printf "Found %d papers\n%!" (List.length papers);
  List.iter (process_paper base_dir output_dir) papers

(* Command line arguments are now imported from Bushel_common *)

(* Export the term for use in main bushel.ml *)
let term =
  Term.(const (fun base_dir output_dir -> process_papers base_dir output_dir; 0) $ 
    Bushel_common.base_dir $ Bushel_common.output_dir ~default:".")

let cmd =
  let doc = "Generate thumbnails for paper PDFs" in
  let info = Cmd.info "thumbs" ~doc in
  Cmd.v info term

(* Main entry point removed - accessed through bushel_main.ml *)
