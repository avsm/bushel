(lang dune 3.17)
(name bushel)

(source (github avsm/bushel))
(license ISC)
(authors "Anil Madhavapeddy")
(maintainers "anil@recoil.org")

(generate_opam_files true)

(package
 (name bushel)
 (synopsis "A webring but not as oldskool")
 (description "This is all still a work in progress")
 (depends
  (ocaml (>= "5.2.0"))
  uri
  cmarkit
  ezjsonm
  ptime
  jsont
  bytesrw
  jekyll-format
  yaml
  lwt
  cohttp-lwt-unix
  peertube
  karakeep
  cmdliner))

(package
 (name peertube)
 (synopsis "PeerTube API client")
 (description "Client for interacting with PeerTube instances")
 (depends
  (ocaml (>= "5.2.0"))
  ezjsonm
  lwt
  cohttp-lwt-unix
  ptime
  fmt))

(package
 (name karakeep)
 (synopsis "Karakeep API client for Bushel")
 (description "Karakeep API client to retrieve bookmarks from Karakeep instances")
 (depends
  (ocaml (>= "5.2.0"))
  ezjsonm
  lwt
  cohttp-lwt-unix
  ptime
  fmt))
