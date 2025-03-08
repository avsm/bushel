[@@@warning "-26-27-32"]

open Lwt.Infix
open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let process_videos output_dir overwrite =
  let base_url = "https://crank.recoil.org" in
  let channel = "anil" in
  Peertube.fetch_channel_videos base_url channel >>= fun response ->
  Logs.info (fun f -> f "Total videos: %d" response.total);
  
  (* Process each video, fetching full details for complete descriptions *)
  Lwt_list.map_s (fun video ->
    (* Fetch complete video details to get full description *)
    Peertube.fetch_video_details base_url video.Peertube.uuid >>= fun full_video ->
    let (description, published_date, title, url, uuid, slug) = 
      Peertube.to_bushel_video full_video 
    in
    Logs.info (fun f -> f "Title: %s, URL: %s" title url);
    Lwt.return {Bushel.Video.description; published_date; title; url; uuid; slug; 
                talk=false; paper=None; project=None; tags=full_video.tags}
  ) response.data >>= fun vids ->
  
  (* Write video files - fixed overwrite flag logic *)
  Lwt_list.iter_s (fun video ->
    let file_path = Filename.concat output_dir (video.Bushel.Video.uuid ^ ".md") in
    let file_exists = Sys.file_exists file_path in
    
    (* Only write file if:
       - overwrite flag is true, OR
       - file doesn't exist yet *)
    if overwrite || not file_exists then
      match Bushel.Video.to_file output_dir video with
      | Ok () -> 
          Logs.info (fun f -> f "Wrote video %s to %s" video.Bushel.Video.title file_path);
          Lwt.return_unit
      | Error (`Msg e) -> 
          Logs.err (fun f -> f "Failed to write video %s: %s" video.Bushel.Video.title e);
          Lwt.return_unit
    else begin
      (* Skip existing files when overwrite is false *)
      Logs.info (fun f -> f "Skipping existing video %s (use --overwrite to replace)" video.Bushel.Video.title);
      Lwt.return_unit
    end
  ) vids

let output_dir =
  let doc = "Output directory" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT_DIR" ~doc)

let overwrite =
  let doc = "Overwrite existing files" in
  Arg.(value & flag & info ["overwrite"] ~doc)

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd =
  let doc = "Fetch and process videos" in
  let info = Cmd.info "bushel_video" ~doc in
  Cmd.v info 
    Term.(const (fun output_dir overwrite () -> 
      Lwt_main.run (process_videos output_dir overwrite)) 
      $ output_dir $ overwrite $ setup_log)

let () =
  exit (Cmd.eval cmd)
