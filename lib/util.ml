let first_hunk s =
  let lines = String.split_on_char '\n' s in
  let rec aux acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | "" :: "" :: _ -> String.concat "\n" (List.rev acc)
    | line :: rest -> aux (line :: acc) rest
  in
  aux [] lines
;;

let first_and_last_hunks s =
  let lines = String.split_on_char '\n' s in
  let rec aux acc = function
    | [] -> String.concat "\n" (List.rev acc), ""
    | "" :: "" :: rest ->
      String.concat "\n" (List.rev acc), String.concat "\n" (List.rev rest)
    | line :: rest -> aux (line :: acc) rest
  in
  aux [] lines
;;

(* Find all footnote definition lines in text *)
let find_footnote_lines s =
  let lines = String.split_on_char '\n' s in
  let is_footnote_def line =
    String.length line > 3 &&
    line.[0] = '[' &&
    line.[1] = '^' &&
    String.contains line ':' &&
    let colon_pos = String.index line ':' in
    colon_pos > 2 && line.[colon_pos - 1] = ']'
  in
  let is_continuation line =
    String.length line > 0 && (line.[0] = ' ' || line.[0] = '\t')
  in
  let rec collect_footnotes acc in_footnote = function
    | [] -> List.rev acc
    | line :: rest ->
      if is_footnote_def line then
        collect_footnotes (line :: acc) true rest
      else if in_footnote && is_continuation line then
        collect_footnotes (line :: acc) true rest
      else
        collect_footnotes acc false rest
  in
  collect_footnotes [] false lines
;;

(* Augment first hunk with footnote definitions from last hunk *)
let first_hunk_with_footnotes s =
  let first, last = first_and_last_hunks s in
  let footnote_lines = find_footnote_lines last in
  if footnote_lines = [] then first
  else first ^ "\n\n" ^ String.concat "\n" footnote_lines
;;

let count_words (text : string) : int =
  let len = String.length text in
  let rec count_words_helper (index : int) (in_word : bool) (count : int) : int =
    if index >= len
    then if in_word then count + 1 else count
    else (
      let char = String.get text index in
      let is_whitespace =
        Char.equal char ' '
        || Char.equal char '\t'
        || Char.equal char '\n'
        || Char.equal char '\r'
      in
      if is_whitespace
      then
        if in_word
        then count_words_helper (index + 1) false (count + 1)
        else count_words_helper (index + 1) false count
      else count_words_helper (index + 1) true count)
  in
  count_words_helper 0 false 0
;;

let read_file file = In_channel.(with_open_bin file input_all)
