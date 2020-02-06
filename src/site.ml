open Lwt.Infix

module Re_ext = struct
  include Re

  let re_t =
    Irmin.Type.(map string)
      (fun s ->
        Re.compile (Re.Posix.re s)
      )
      (fun re ->
        let buf = Buffer.create 32 in
        let fmt = Format.formatter_of_buffer buf in
        Re.pp_re fmt re;
        Format.pp_print_flush fmt ();
        Buffer.contents buf
      )
end

type t = {
  key : string list;
  folder_name : string;
  domain : string;
  post_path_pattern : Re_ext.re;
  posts_key : string list;
}
[@@deriving irmin]

type sites = t list [@@deriving irmin]

module Tree (S : Irmin.S with type key = string list and type step = string and type contents = string) = struct
  let sites_key =
    [".bushes"; "sites"]

  let default_site =
    {
      key = ["tarides.com"];
      folder_name = "tarides.com";
      domain = "tarides.com";
      post_path_pattern = Re_ext.compile (Re.Posix.re "tarides\\.com");
      posts_key = ["data"; "blog"; "content"];
    }

  let list tree =
    S.Tree.find tree sites_key >>= function
    | None -> Lwt.return [default_site]
    | Some value ->
        match Irmin.Type.of_string sites_t value with
        | Ok t -> Lwt.return t
        | Error (`Msg msg) -> Lwt.fail_with msg

  let fold tree ~f =
    list tree >>= fun sites ->
    Lwt_list.fold_left_s (fun tree' site ->
      f tree' site
    ) tree sites
end
