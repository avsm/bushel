module Make(S : Irmin.S with type key = string list and type step = string and type contents = string) = struct
  module Site = Site.Store (S)
  module Post = Post.Store (S)
  module Link = Link.Store (S)
  module Repository = Repository.Store (S)
end
