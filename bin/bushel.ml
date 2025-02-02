open Bushel

let obsidian_links =
  let inline c = function
    | Md.Obsidian_link l ->
      Cmarkit_renderer.Context.string c l;
      true
    | _ -> false
  in
  Cmarkit_renderer.make ~inline ()
;;

let obsidian_of_doc doc =
  let default = Cmarkit_commonmark.renderer () in
  let r = Cmarkit_renderer.compose default obsidian_links in
  Cmarkit_renderer.doc_to_string r doc
;;

let md_to_obsidian entries md =
  let open Cmarkit in
  Doc.of_string ~strict:false ~resolver:Md.with_bushel_links md
  |> Mapper.map_doc (Mapper.make ~inline:(Md.bushel_inline_mapper_to_obsidian entries) ())
  |> obsidian_of_doc
;;

let obsidian_output base output_dir =
  let e = load base in
  let all = Entry.all_entries e @ Entry.all_papers e in
  List.iter
    (fun ent ->
      let slug =
        match ent with
        | `Paper { Paper.latest; slug; ver; _ } when not latest ->
          Printf.sprintf "%s-%s" slug ver
        | _ -> Entry.slug ent
      in
      prerr_endline slug;
      let fname = Filename.concat output_dir (slug ^ ".md") in
      let tags =
        Tags.tags_of_ent e ent
        |> List.filter_map (fun tag ->
          match tag with
          | `Slug _ -> None
          | `Set s -> Some (Printf.sprintf "\"#%s\"" s)
          | `Text s -> Some (Printf.sprintf "%s" s)
          | `Contact _ -> None
          | `Year y -> Some (Printf.sprintf "\"#y%d\"" y))
        |> List.map (fun s -> "- " ^ s)
        |> String.concat "\n"
      in
      let links =
        Tags.tags_of_ent e ent
        |> List.filter_map (fun tag ->
          match tag with
          | `Slug s when s <> slug -> Some (Printf.sprintf "- \"[[%s]]\"" s)
          | `Contact c -> Some (Printf.sprintf "- \"[[@%s]]\"" c)
          | _ -> None)
        |> String.concat "\n"
        |> function
        | "" -> ""
        | s -> "linklist:\n" ^ s ^ "\n"
      in
      let body = Entry.body ent |> md_to_obsidian e in
      let buf = Printf.sprintf "---\ntags:\n%s\n%s---\n\n%s" tags links body in
      Out_channel.with_open_bin fname (fun oc -> output_string oc buf))
    all;
  List.iter
    (fun contact ->
      let slug = Contact.handle contact in
      let fname = Filename.concat output_dir ("@" ^ slug ^ ".md") in
      let buf = String.concat "\n" (Contact.names contact) in
      Out_channel.with_open_bin fname (fun oc -> output_string oc buf))
    (Entry.contacts e)
;;

let () = obsidian_output "." "obsidian"
