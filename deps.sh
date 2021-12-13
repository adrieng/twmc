#!/usr/bin/env bash

opam >/dev/null 2>&1
if [ $? -ne 0 ]
then
    echo "Please install the OCaml Package Manager http://opam.ocaml.org" >2
    exit 1
fi
opam install dune z3 zarith alcotest menhir pprint
