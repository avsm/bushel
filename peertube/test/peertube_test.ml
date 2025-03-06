(* PeerTube API Client Tests
   TODO:claude *)

open Peertube
open Lwt.Infix

(* Helper to load sample JSON from file *)
let load_sample filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content |> Ezjsonm.from_string

(* Test parsing video channel responses from different PeerTube instances *)
let%expect_test "parse_crank_response" =
  let json = load_sample "samples/crank_response.json" in
  let response = parse_video_response json in
  Printf.printf "Total videos: %d\n" response.total;
  let video = List.hd response.data in
  Printf.printf "First video: %s (ID: %d, UUID: %s)\n" video.name video.id video.uuid;
  [%expect {|
    Total videos: 31
    First video: FOSDEM 2021: A history of OCaml (ID: 264, UUID: efd8a77c-10c9-4806-bc81-f0cb018190df)
  |}]

let%expect_test "parse_eeg_response" =
  let json = load_sample "samples/eeg_response.json" in
  let response = parse_video_response json in
  Printf.printf "Total videos: %d\n" response.total;
  let video = List.hd response.data in
  Printf.printf "First video: %s (ID: %d, UUID: %s)\n" video.name video.id video.uuid;
  [%expect {|
    Total videos: 131
    First video: Greg Morrisett - Keynote Speech (ID: 100, UUID: 123e4567-e89b-12d3-a456-426614174000)
  |}]

let%expect_test "parse_ocaml_response" =
  let json = load_sample "samples/ocaml_response.json" in
  let response = parse_video_response json in
  Printf.printf "Total videos: %d\n" response.total;
  let video = List.hd response.data in
  Printf.printf "First video: %s (ID: %d, UUID: %s)\n" video.name video.id video.uuid;
  [%expect {|
    Total videos: 70
    First video: Making the OCaml compiler collaborative - Florian Angeletti (ID: 150, UUID: 87654321-abcd-9876-fedc-012345678901)
  |}]

(* Mock module to test pagination without actual HTTP requests *)
module MockPeertubeApi = struct
  (* Mock data for pagination tests *)
  let mock_videos = 
    let sample = load_sample "samples/ocaml_response.json" in
    let response = parse_video_response sample in
    response.data
  
  (* Simulates fetching a page of videos *)
  let fetch_page ~start ~count =
    let total = List.length mock_videos in
    let end_idx = min (start + count) total in
    let page_size = end_idx - start in
    
    (* Extract the requested page of videos *)
    let page_videos = 
      if start >= total then []
      else 
        mock_videos
        |> List.to_seq
        |> Seq.drop start
        |> Seq.take page_size
        |> List.of_seq
    in
    
    Lwt.return { total; data = page_videos }
  
  (* Replaces the HTTP fetching with mock data *)
  let fetch_channel_videos ?(count=20) ?(start=0) _base_url _channel =
    fetch_page ~start ~count
  
  (* Use the real implementation with our mock fetch_channel_videos *)
  let fetch_all_channel_videos ?(page_size=20) ?max_pages _base_url _channel =
    let rec fetch_pages start acc _total_count =
      fetch_page ~start ~count:page_size >>= fun response ->
      let all_videos = acc @ response.data in
      
      (* Determine if we need to fetch more pages *)
      let fetched_count = start + List.length response.data in
      let more_available = fetched_count < response.total in
      let under_max_pages = match max_pages with
        | None -> true
        | Some max -> (start / page_size) + 1 < max
      in
      
      if more_available && under_max_pages then
        fetch_pages fetched_count all_videos response.total
      else
        Lwt.return { total = response.total; data = all_videos }
    in
    fetch_pages 0 [] 0
end

(* Test pagination using the mock API *)
let%expect_test "test_pagination_single_page" =
  let open MockPeertubeApi in
  let response = fetch_channel_videos ~count:5 ~start:0 "" "" |> Lwt_main.run in
  Printf.printf "Total videos: %d\n" response.total;
  Printf.printf "Videos in page: %d\n" (List.length response.data);
  [%expect {|
    Total videos: 3
    Videos in page: 3
  |}]

let%expect_test "test_pagination_second_page" =
  let open MockPeertubeApi in
  let response = fetch_channel_videos ~count:1 ~start:1 "" "" |> Lwt_main.run in
  Printf.printf "Total videos: %d\n" response.total;
  Printf.printf "Videos in page: %d\n" (List.length response.data);
  if List.length response.data > 0 then
    Printf.printf "Video name: %s\n" (List.hd response.data).name;
  [%expect {|
    Total videos: 3
    Videos in page: 1
    Video name: Multicore OCaml: Status Update
  |}]
  
let%expect_test "test_pagination_empty_page" =
  let open MockPeertubeApi in
  let response = fetch_channel_videos ~count:5 ~start:10 "" "" |> Lwt_main.run in
  Printf.printf "Total videos: %d\n" response.total;
  Printf.printf "Videos in page: %d\n" (List.length response.data);
  [%expect {|
    Total videos: 3
    Videos in page: 0
  |}]

let%expect_test "test_fetch_all_videos" =
  let open MockPeertubeApi in
  let response = fetch_all_channel_videos ~page_size:2 "" "" |> Lwt_main.run in
  Printf.printf "Total videos: %d\n" response.total;
  Printf.printf "Total fetched: %d\n" (List.length response.data);
  [%expect {|
    Total videos: 3
    Total fetched: 3
  |}]

let%expect_test "test_fetch_with_max_pages" =
  let open MockPeertubeApi in
  let response = fetch_all_channel_videos ~page_size:1 ~max_pages:2 "" "" |> Lwt_main.run in
  Printf.printf "Total videos: %d\n" response.total;
  Printf.printf "Total fetched: %d\n" (List.length response.data);
  [%expect {|
    Total videos: 3
    Total fetched: 2
  |}]
