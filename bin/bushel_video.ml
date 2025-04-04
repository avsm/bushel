[@@@warning "-26-27-32"]

open Lwt.Infix
open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let process_videos output_dir overwrite base_url channel =
  Peertube.fetch_all_channel_videos base_url channel >>= fun all_videos ->
  Logs.info (fun f -> f "Total videos: %d" (List.length all_videos));
  
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
  ) all_videos >>= fun vids ->
  
  (* Write video files *)
  Lwt_list.iter_s (fun video ->
    let file_path = Filename.concat output_dir (video.Bushel.Video.uuid ^ ".md") in
    let file_exists = Sys.file_exists file_path in
    
    if file_exists then
      try
        (* If file exists, load it to preserve specific fields *)
        let existing_video = Bushel.Video.of_md file_path in
        (* Create merged video with preserved fields *)
        let merged_video = {
          video with
          tags = existing_video.tags;  (* Preserve existing tags *)
          paper = existing_video.paper;  (* Preserve paper field *)
          project = existing_video.project;  (* Preserve project field *)
          talk = existing_video.talk;  (* Preserve talk field *)
        } in
        
        (* Write the merged video data *)
        if overwrite then
          match Bushel.Video.to_file output_dir merged_video with
          | Ok () -> 
              Logs.info (fun f -> f "Updated video %s with preserved fields in %s" 
                merged_video.Bushel.Video.title file_path);
              Lwt.return_unit
          | Error (`Msg e) -> 
              Logs.err (fun f -> f "Failed to update video %s: %s" 
                merged_video.Bushel.Video.title e);
              Lwt.return_unit
        else begin
          Logs.info (fun f -> f "Skipping existing video %s (use --overwrite to replace)" 
            video.Bushel.Video.title);
          Lwt.return_unit
        end
      with _ ->
        (* If reading existing file fails, proceed with new data *)
        if overwrite then
          match Bushel.Video.to_file output_dir video with
          | Ok () -> 
              Logs.info (fun f -> f "Wrote video %s to %s (existing file could not be read)" 
                video.Bushel.Video.title file_path);
              Lwt.return_unit
          | Error (`Msg e) -> 
              Logs.err (fun f -> f "Failed to write video %s: %s" 
                video.Bushel.Video.title e);
              Lwt.return_unit
        else begin
          Logs.info (fun f -> f "Skipping existing video %s (use --overwrite to replace)" 
            video.Bushel.Video.title);
          Lwt.return_unit
        end
    else
      (* If file doesn't exist, just write new data *)
      match Bushel.Video.to_file output_dir video with
      | Ok () -> 
          Logs.info (fun f -> f "Wrote new video %s to %s" 
            video.Bushel.Video.title file_path);
          Lwt.return_unit
      | Error (`Msg e) -> 
          Logs.err (fun f -> f "Failed to write video %s: %s" 
            video.Bushel.Video.title e);
          Lwt.return_unit
  ) vids

let output_dir =
  let doc = "Output directory" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT_DIR" ~doc)

let overwrite =
  let doc = "Overwrite existing files" in
  Arg.(value & flag & info ["overwrite"] ~doc)

let base_url =
  let doc = "PeerTube base URL" in
  let default = "https://crank.recoil.org" in
  Arg.(value & opt string default & info ["url"] ~docv:"URL" ~doc)

let channel =
  let doc = "PeerTube channel name" in
  let default = "anil" in
  Arg.(value & opt string default & info ["channel"] ~docv:"CHANNEL" ~doc)

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd =
  let doc = "Fetch and process videos" in
  let info = Cmd.info "bushel_video" ~doc in
  Cmd.v info 
    Term.(const (fun output_dir overwrite base_url channel () -> 
      Lwt_main.run (process_videos output_dir overwrite base_url channel)) 
      $ output_dir $ overwrite $ base_url $ channel $ setup_log)

let () =
  exit (Cmd.eval cmd)
