open Lwt.Infix

module Make (S : Irmin.S with type key = string list and type step = string and type contents = string) : sig
  type ctx
  val ctx : tree:S.tree -> ctx

  val schema : ctx Graphql_lwt.Schema.schema
end = struct
  open Graphql_lwt
  
  module Tree = struct
    module Site = Site.Tree (S)
    module Post = Post.Tree (S)
    module Link = Link.Tree (S)
  end

  type ctx = {
    tree : S.tree
  }

  let ctx ~tree = { tree }

  let datetime_typ = Schema.scalar "DateTime"
    ~coerce:(fun ptime -> `String (Ptime.to_rfc3339 ptime))

  let repository = Schema.(obj "Repository"
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
          Tree.Link.find_or_empty info.ctx.tree post.Post.key >>= fun links ->
          Lwt_list.map_p (fun link ->
            Tree.Post.find info.ctx.tree link.Link.key
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
          Tree.Post.list info.ctx.tree site >|= fun posts ->
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
        Tree.Site.list info.ctx.tree >|= fun sites ->
        Ok sites
      )
  ])
end
