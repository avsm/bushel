[@@@warning "-26-27-32"]

open Lwt.Infix
open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let process_video_thumbs videos_dir thumbs_dir base_url =
  (* Ensure thumbnail directory exists *)
  (if not (Sys.file_exists thumbs_dir) then
    Unix.mkdir thumbs_dir 0o755);

  (* Read all video markdown files *)
  let video_files = Sys.readdir videos_dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun f -> Filename.concat videos_dir f)
  in

  Logs.info (fun f -> f "Found %d video files to process" (List.length video_files));

  (* Process each video file *)
  Lwt_list.iter_s (fun video_file ->
    try
      (* Load existing video *)
      let video = Bushel.Video.of_md video_file in
      let uuid = video.Bushel.Video.uuid in

      Logs.info (fun f -> f "Processing video: %s (UUID: %s)" video.title uuid);

      (* Fetch video details from PeerTube to get thumbnail info *)
      Peertube.fetch_video_details base_url uuid >>= fun peertube_video ->

      (* Download thumbnail *)
      let thumb_path = Filename.concat thumbs_dir (uuid ^ ".jpg") in
      Peertube.download_thumbnail base_url peertube_video thumb_path >>= fun result ->

      match result with
      | Ok () ->
          Logs.info (fun f -> f "Downloaded thumbnail for %s to %s" video.title thumb_path);

          (* Update video file with thumbnail_url field *)
          (match Peertube.thumbnail_url base_url peertube_video with
           | Some url ->
               Logs.info (fun f -> f "Thumbnail URL: %s" url);
               Lwt.return_unit
           | None ->
               Logs.warn (fun f -> f "No thumbnail URL for video %s" video.title);
               Lwt.return_unit)
      | Error (`Msg e) ->
          Logs.err (fun f -> f "Failed to download thumbnail for %s: %s" video.title e);
          Lwt.return_unit
    with exn ->
      Logs.err (fun f -> f "Error processing %s: %s" video_file (Printexc.to_string exn));
      Lwt.return_unit
  ) video_files

let term =
  let videos_dir =
    let doc = "Directory containing video markdown files" in
    Arg.(value & opt string "data/videos" & info ["videos-dir"; "d"] ~docv:"DIR" ~doc)
  in
  let thumbs_dir =
    let doc = "Directory to save thumbnails" in
    Arg.(value & opt string "images/videos" & info ["thumbs-dir"; "t"] ~docv:"DIR" ~doc)
  in
  Term.(const (fun videos_dir thumbs_dir base_url () ->
    Lwt_main.run (process_video_thumbs videos_dir thumbs_dir base_url); 0)
    $ videos_dir $
    thumbs_dir $
    Bushel_common.url_term ~default:"https://crank.recoil.org" ~doc:"PeerTube base URL" $
    Bushel_common.setup_term)

let cmd =
  let doc = "Download thumbnails for existing videos and update metadata" in
  let info = Cmd.info "video-thumbs" ~doc in
  Cmd.v info term
