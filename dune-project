(lang dune 2.8)
(using menhir 2.0)

(name twmc)
(version 0.0.1)

(generate_opam_files true)

(source
 (github adrieng/twmc))

(authors "Adrien Guatto <adrien@guatto.org>")
(maintainers "Adrien Guatto <adrien@guatto.org>")

(license GPL-3.0-or-later)

(package
 (name twmc)
 (synopsis "A model-checker for time warps")
 (description "")
 (depends ocaml dune z3 zarith alcotest menhir pprint ppx_deriving ppx_hash))
