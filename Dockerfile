FROM ocaml/opam2:4.08 as base

WORKDIR /home/bushel

RUN opam depext -iy dune uri lwt cohttp-lwt-unix yojson irmin-unix re ptime graphql-cohttp
RUN opam install dune uri lwt cohttp-lwt-unix yojson irmin-unix re ptime graphql-cohttp

RUN opam pin add graphql_ppx https://github.com/reasonml-community/graphql_ppx.git
RUN opam pin add ppx_irmin https://github.com/mirage/irmin.git

ADD --chown=opam . /home/bushel

RUN opam config exec -- dune build
