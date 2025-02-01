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
