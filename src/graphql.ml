open Lwt.Infix

module Make (S : Irmin.S with type key = string list and type step = string and type contents = string) : sig
  type ctx
  val ctx : store:S.t -> ctx

  val schema : ctx Graphql_lwt.Schema.schema
end = struct
  open Graphql_lwt
  
  module Stores = Stores.Make (S)

  type ctx = {
    store : S.t;
    (* FIXME: this is a hacky workaround because Graphql_cohttp_lwt.make_callback
     * does not allow returning a Lwt.t value *)
    mutable tree : S.tree Lazy.t;
  }

  let ctx ~store = { store; tree = lazy (assert false) }

  let datetime_typ = Schema.scalar "DateTime"
    ~coerce:(fun ptime -> `String (Ptime.to_rfc3339 ptime))

  let _repository = Schema.(obj "Repository"
    ~fields:(fun _ -> [
      field "updatedAt"
        ~typ:(non_null datetime_typ)
        ~args:[]
        ~resolve:(fun _ repo -> repo.Repository.updated_at)
    ])
  )

  let post = Schema.(obj "Post"
    ~fields:(fun post -> [
      field "contents"
        ~typ:(non_null string)
        ~args:[]
        ~resolve:(fun _ post -> post.Post.contents)
      ;
      io_field "backlinks"
        ~typ:(non_null (list (non_null post)))
        ~args:[]
        ~resolve:(fun info post ->
          Stores.Link.find_or_empty Lazy.(force info.ctx.tree) post.Post.key >>= fun links ->
          Lwt_list.map_p (fun link ->
            Stores.Post.find Lazy.(force info.ctx.tree) link.Link.key
          ) links >|= fun posts ->
          Ok posts
        )
    ])
  )

  let site = Schema.(obj "Site"
    ~fields:(fun _ -> [
      field "name"
        ~typ:(non_null string)
        ~args:[]
        ~resolve:(fun _info site ->
          site.Site.folder_name
        )
      ;
      io_field "posts"
        ~typ:(non_null (list (non_null post)))
        ~args:[]
        ~resolve:(fun info site ->
          Stores.Post.list Lazy.(force info.ctx.tree) site >|= fun posts ->
          Ok posts
        )
      ; 
    ])
  )

  let schema : ctx Schema.schema = Schema.(schema [
    io_field "sites"
      ~typ:(non_null (list (non_null site)))
      ~args:[]
      ~resolve:(fun info () ->
        S.tree info.ctx.store >>= fun tree ->
        info.ctx.tree <- lazy tree;
        Stores.Site.list tree >|= fun sites ->
        Ok sites
      )
  ])
end
