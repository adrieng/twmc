#!/usr/bin/env bash

DEPS="dune z3 zarith alcotest menhir pprint ppx_deriving ppx_hash"

opam >/dev/null 2>&1
if [ $? -ne 0 ]
then
    echo "Please install the OCaml Package Manager http://opam.ocaml.org" >2
    exit 1
fi
opam install $DEPS
