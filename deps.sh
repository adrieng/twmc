#!/usr/bin/env bash

set -e

opam >/dev/null 2>&1
if [ $? -ne 0 ]
then
    echo "Please install the OCaml Package Manager http://opam.ocaml.org" >2
    exit 1
fi

dune --version >/dev/null 2>&1
if [ $? -ne 0 ]
then
    echo 'Please install the Dune build system (opam install dune)' >2
    exit 1
fi

dune build twmc.opam
opam install --deps-only .
