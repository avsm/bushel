open Printf
open Cmdliner

(** TODO:claude Format author name for LaTeX with initials and full last name *)
let format_author_name author =
  (* Split author name and convert to "F.M.~Lastname" format *)
  let parts = String.split_on_char ' ' author |> List.filter (fun s -> s <> "") in
  match List.rev parts with
  | [] -> ""
  | lastname :: rest_rev ->
    let firstname_parts = List.rev rest_rev in
    let initials = List.map (fun name -> 
      if String.length name > 0 then String.sub name 0 1 ^ "." else ""
    ) firstname_parts in
    let initials_str = String.concat "" initials in
    if initials_str = "" then lastname
    else initials_str ^ "~" ^ lastname

(** TODO:claude Format author name for LaTeX with underline for target author *)
let format_author target_name author =
  let formatted = format_author_name author in
  (* Check if author contains target name substring for underlining *)
  if String.lowercase_ascii author |> fun s -> 
     Re.execp (Re.Perl.compile_pat ~opts:[`Caseless] target_name) s
  then sprintf "\\underline{%s}" formatted
  else formatted

(** TODO:claude Format authors list for LaTeX *)
let format_authors target_name authors =
  match authors with
  | [] -> ""
  | [single] -> format_author target_name single
  | _ -> 
    let formatted = List.map (format_author target_name) authors in
    String.concat ", " formatted

(** TODO:claude Escape special LaTeX characters *)
let escape_latex str =
  let replacements = [
    ("&", "\\&");
    ("%", "\\%");
    ("$", "\\$");
    ("#", "\\#");
    ("_", "\\_");
    ("{", "\\{");
    ("}", "\\}");
    ("~", "\\textasciitilde{}");
    ("^", "\\textasciicircum{}");
  ] in
  List.fold_left (fun s (from, to_) ->
    Re.replace_string (Re.compile (Re.str from)) ~by:to_ s
  ) str replacements

(** TODO:claude Clean venue name by removing common prefixes and handling arXiv *)
let clean_venue_name venue =
  (* Special handling for arXiv to avoid redundancy like "arXiv (arXiv:ID)" *)
  let venue_lower = String.lowercase_ascii venue in
  if Re.execp (Re.Perl.compile_pat ~opts:[`Caseless] "arxiv") venue_lower then
    if String.contains venue ':' then
      (* If it contains arXiv:ID format, just return the ID part *)
      let parts = String.split_on_char ':' venue in
      match parts with
      | _ :: id :: _ -> String.trim id
      | _ -> venue
    else venue
  else
    let prefixes = [
      "in proceedings of the ";
      "proceedings of the ";
      "in proceedings of ";
      "proceedings of ";
      "in the ";
      "the ";
    ] in
    let rec remove_prefixes v = function
      | [] -> v
      | prefix :: rest ->
        if String.length v >= String.length prefix && 
           String.sub (String.lowercase_ascii v) 0 (String.length prefix) = prefix
        then String.sub v (String.length prefix) (String.length v - String.length prefix)
        else remove_prefixes v rest
    in
    let cleaned = remove_prefixes venue prefixes in
    (* Capitalize first letter *)
    if String.length cleaned > 0 then
      String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) cleaned
    else cleaned

(** TODO:claude Format venue for LaTeX with volume/number details for full papers *)
let format_venue paper =
  let open Bushel.Paper in
  let classification = classification paper in
  match bibtype paper with
  | "article" -> 
    let journal_name = try journal paper |> clean_venue_name |> escape_latex with _ -> "Journal" in
    if classification = Full then (
      let vol_info = 
        let vol = volume paper in
        let num = issue paper in
        match vol, num with
        | Some v, Some n -> sprintf ", %s(%s)" v n
        | Some v, None -> sprintf ", vol. %s" v
        | None, Some n -> sprintf ", no. %s" n
        | None, None -> ""
      in
      sprintf "\\textit{%s%s}" journal_name vol_info
    ) else 
      sprintf "\\textit{%s}" journal_name
  | "inproceedings" ->
    let conf_name = try booktitle paper |> clean_venue_name |> escape_latex with _ -> "Conference" in
    sprintf "\\textit{%s}" conf_name
  | "techreport" ->
    let inst = try institution paper |> escape_latex with _ -> "Institution" in
    sprintf "\\textit{Technical Report, %s}" inst
  | "phdthesis" ->
    let school = try institution paper |> escape_latex with _ -> "University" in
    sprintf "\\textit{PhD thesis, %s}" school
  | "mastersthesis" ->
    let school = try institution paper |> escape_latex with _ -> "University" in
    sprintf "\\textit{Master's thesis, %s}" school
  | "book" ->
    let publisher_str = try Bushel.Paper.publisher paper |> escape_latex with _ -> "" in
    let edition_str = try 
      let json = Bushel.Paper.raw_json paper in
      let keys = Ezjsonm.get_dict json in
      List.assoc "edition" keys |> Ezjsonm.get_string |> escape_latex 
    with _ -> "" in
    let isbn_str = try Bushel.Paper.isbn paper |> escape_latex with _ -> "" in
    let venue_info = 
      let base = match publisher_str, edition_str with
        | pub, ed when pub <> "" && ed <> "" -> sprintf "%s, %s edition" pub ed
        | pub, _ when pub <> "" -> pub
        | _, ed when ed <> "" -> sprintf "%s edition" ed
        | _, _ -> "Book"
      in
      if isbn_str <> "" then
        sprintf "%s, ISBN %s" base isbn_str
      else
        base
    in
    sprintf "\\textit{%s}" venue_info
  | "misc" ->
    (* Try to get meaningful venue info for misc entries *)
    let journal_str = try Bushel.Paper.journal paper |> clean_venue_name |> escape_latex with _ -> "" in
    let booktitle_str = try Bushel.Paper.booktitle paper |> clean_venue_name |> escape_latex with _ -> "" in
    let publisher_str = try Bushel.Paper.publisher paper |> escape_latex with _ -> "" in
    if journal_str <> "" then
      sprintf "\\textit{%s}" journal_str
    else if booktitle_str <> "" then
      sprintf "\\textit{%s}" booktitle_str
    else if publisher_str <> "" then
      sprintf "\\textit{%s}" publisher_str  
    else
      sprintf "\\textit{Preprint}"
  | "abstract" ->
    (* Handle conference abstracts *)
    let conf_name = try Bushel.Paper.booktitle paper |> clean_venue_name |> escape_latex with _ -> "" in
    let journal_str = try Bushel.Paper.journal paper |> clean_venue_name |> escape_latex with _ -> "" in
    if conf_name <> "" then
      sprintf "\\textit{%s (Abstract)}" conf_name
    else if journal_str <> "" then
      sprintf "\\textit{%s (Abstract)}" journal_str
    else
      sprintf "\\textit{Conference Abstract}"
  | _ -> 
    (* Fallback for other types with special arXiv handling *)
    let journal_str = try Bushel.Paper.journal paper with _ -> "" in
    let publisher_str = try Bushel.Paper.publisher paper |> escape_latex with _ -> "" in
    
    (* Special handling for arXiv papers - skip venue, let note handle it *)
    if String.lowercase_ascii journal_str = "arxiv" then
      ""
    else if journal_str <> "" then
      sprintf "\\textit{%s}" (journal_str |> clean_venue_name |> escape_latex)
    else if publisher_str <> "" then
      sprintf "\\textit{%s}" publisher_str  
    else
      sprintf "\\textit{Preprint}"

(** TODO:claude Generate LaTeX PubItem for a paper *)
let generate_latex_entry target_name paper =
  let open Bushel.Paper in
  let slug_str = slug paper in
  let title_str = title paper |> escape_latex in
  let authors_str = format_authors target_name (authors paper) in
  let venue_str = format_venue paper in
  let year_str = year paper |> string_of_int in
  let month_str = 
    let (_, m, _) = date paper in
    sprintf "%02d" m
  in
  
  (* Check if paper is in the future *)
  let is_in_press =
    let paper_time = datetime paper in
    let now = Ptime_clock.now () in
    Ptime.compare paper_time now > 0
  in
  
  (* Add DOI or PDF link if available, but not for in-press papers unless they have explicit URL *)
  let title_with_link = 
    if is_in_press then
      (* For in-press papers, only add link if there's an explicit URL field *)
      match Bushel.Paper.url paper with
      | Some u -> sprintf "\\href{%s}{%s}" u title_str
      | None -> title_str  (* No link for in-press papers without explicit URL *)
    else
      (* For published papers, use DOI or URL or default PDF link *)
      match Bushel.Paper.doi paper with
      | Some doi -> sprintf "\\href{https://doi.org/%s}{%s}" doi title_str
      | None -> 
        (* Check if there's a URL, otherwise default to PDF link *)
        let url = match Bushel.Paper.url paper with
          | Some u -> u
          | None -> sprintf "https://anil.recoil.org/papers/%s.pdf" slug_str
        in
        sprintf "\\href{%s}{%s}" url title_str
  in
  
  (* Add "(in press)" if paper is in the future *)
  let in_press_str = if is_in_press then " \\textit{(in press)}" else "" in
  
  (* Add note if present *)
  let note_str = match Bushel.Paper.note paper with
    | Some n -> sprintf " \\textit{(%s)}" (escape_latex n)
    | None -> ""
  in
  
  sprintf "\\BigGap\n\\PubItemLabeled{%s}\n{``%s,''\n%s,\n%s%s%s,\n\\DatestampYM{%s}{%s}.}\n"
    slug_str title_with_link authors_str venue_str in_press_str note_str year_str month_str

(** TODO:claude Generate LaTeX output files for papers *)
let generate_tex base_dir output_dir target_name =
  try
    let papers = Bushel.load_papers base_dir in
    let latest_papers = List.filter (fun p -> p.Bushel.Paper.latest) papers in
    
    (* Extract selected papers first *)
    let selected_papers = List.filter Bushel.Paper.selected latest_papers in
    
    (* Group remaining papers by classification, excluding selected ones *)
    let non_selected_papers = List.filter (fun p -> not (Bushel.Paper.selected p)) latest_papers in
    let full_papers = List.filter (fun p -> 
      Bushel.Paper.classification p = Bushel.Paper.Full) non_selected_papers in
    let short_papers = List.filter (fun p -> 
      Bushel.Paper.classification p = Bushel.Paper.Short) non_selected_papers in
    let preprint_papers = List.filter (fun p -> 
      Bushel.Paper.classification p = Bushel.Paper.Preprint) non_selected_papers in
    
    (* Sort each group by date, newest first *)
    let sorted_full = List.sort Bushel.Paper.compare full_papers in
    let sorted_short = List.sort Bushel.Paper.compare short_papers in
    let sorted_preprint = List.sort Bushel.Paper.compare preprint_papers in
    let sorted_selected = List.sort Bushel.Paper.compare selected_papers in
    
    (* Ensure output directory exists *)
    (try Unix.mkdir output_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    
    (* Write papers_full.tex *)
    let oc_full = open_out (Filename.concat output_dir "papers_full.tex") in
    List.iter (fun paper ->
      let latex = generate_latex_entry target_name paper in
      output_string oc_full latex;
      output_char oc_full '\n'
    ) sorted_full;
    close_out oc_full;
    Printf.printf "Generated %s/papers_full.tex with %d entries\n" output_dir (List.length sorted_full);
    
    (* Write papers_short.tex *)
    let oc_short = open_out (Filename.concat output_dir "papers_short.tex") in
    List.iter (fun paper ->
      let latex = generate_latex_entry target_name paper in
      output_string oc_short latex;
      output_char oc_short '\n'
    ) sorted_short;
    close_out oc_short;
    Printf.printf "Generated %s/papers_short.tex with %d entries\n" output_dir (List.length sorted_short);
    
    (* Write papers_preprint.tex *)
    let oc_preprint = open_out (Filename.concat output_dir "papers_preprint.tex") in
    List.iter (fun paper ->
      let latex = generate_latex_entry target_name paper in
      output_string oc_preprint latex;
      output_char oc_preprint '\n'
    ) sorted_preprint;
    close_out oc_preprint;
    Printf.printf "Generated %s/papers_preprint.tex with %d entries\n" output_dir (List.length sorted_preprint);
    
    (* Write papers_selected.tex *)
    let oc_selected = open_out (Filename.concat output_dir "papers_selected.tex") in
    List.iter (fun paper ->
      let latex = generate_latex_entry target_name paper in
      output_string oc_selected latex;
      output_char oc_selected '\n'
    ) sorted_selected;
    close_out oc_selected;
    Printf.printf "Generated %s/papers_selected.tex with %d entries\n" output_dir (List.length sorted_selected);
    
    (* Write paper_count.tex *)
    let total_count = List.length latest_papers in
    let oc_count = open_out (Filename.concat output_dir "paper_count.tex") in
    output_string oc_count (sprintf "\\setcounter{pubcounter}{%d}\n" total_count);
    close_out oc_count;
    Printf.printf "Generated %s/paper_count.tex with total count: %d\n" output_dir total_count;
    
    0
  with e ->
    Printf.eprintf "Error loading papers: %s\n" (Printexc.to_string e);
    1

let output_dir_arg =
  let doc = "Output directory for generated LaTeX files" in
  Arg.(value & opt string "." & info ["output"; "o"] ~docv:"DIR" ~doc)

let target_name_arg =
  let doc = "Name to underline in author list (e.g., 'Madhavapeddy')" in
  Arg.(value & opt string "Madhavapeddy" & info ["target"; "t"] ~docv:"NAME" ~doc)

let term =
  Term.(const generate_tex $ Bushel_common.base_dir $ output_dir_arg $ target_name_arg)

let cmd =
  let doc = "Generate LaTeX publication entries" in
  let info = Cmd.info "paper-tex" ~doc in
  Cmd.v info term
