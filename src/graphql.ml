open Lwt.Infix

module Make(S : Irmin.S with type key = string list and type step = string and type contents = Contents.t) = struct
  module Presentation = struct
    module Defaults = Irmin_graphql.Server.Default_presentation(S)

    module Contents = struct
      module Schema = Irmin_graphql.Server.Schema

      type src = (unit, [`Contents]) Schema.abstract_value

      let schema_typ = Schema.union "ContentValue"

      let repository = Schema.(obj "Repository"
        ~fields:(fun _ -> [
          field "updatedAt"
            ~typ:(non_null string)
            ~args:[]
            ~resolve:(fun _ repo -> repo.Repository.updated_at)
        ])
      )

      let link = Schema.(obj "Link"
        ~fields:(fun _ -> [
          field "key"
            ~typ:(non_null (list (non_null string)))
            ~args:[]
            ~resolve:(fun _ link -> link)
        ])
      )

      let post = Schema.(obj "Post"
        ~fields:(fun _ -> [
          field "contents"
            ~typ:(non_null string)
            ~args:[]
            ~resolve:(fun _ (_tree, _key, post) -> post.Post.contents)
          ;
          io_field "backlinks"
            ~typ:(non_null (list (non_null link)))
            ~args:[]
            ~resolve:(fun _ (tree, key, _post) ->
              S.Tree.find tree ("links"::key) >|= function
              | Some (Link l) -> Ok l
              | Some _
              | None -> assert false
            )
        ])
      )

      let repository_as_contents = Schema.add_type schema_typ repository
      let post_as_contents = Schema.add_type schema_typ post
      let link_as_contents = Schema.add_type schema_typ link

      let to_src (tree : S.tree) (key : S.key) (contents : S.contents) : src =
        match contents with
        | Contents.Repository r -> repository_as_contents r
        | Post p -> post_as_contents (tree, key, p)
        | Link _l -> link_as_contents []
    end

    module Metadata = Defaults.Metadata
  end

  include Irmin_unix.Graphql.Server.Make_ext
    (S)
    (struct
      let remote = None
    end)
    (Presentation)
end
