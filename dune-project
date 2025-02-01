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
  cmarkit
  ezjsonm
  ptime
  jsont
  bytesrw
  jekyll-format
  yaml))
