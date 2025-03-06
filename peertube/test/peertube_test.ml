(* PeerTube API Client Tests
   TODO:claude *)

open Peertube

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
