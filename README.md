# Time Warps

## What

This is a work-in-progress implementation of the decision procedure described in
the paper [Time Warps, from Algebra to
Algorithms](https://arxiv.org/abs/2106.06205).

## How

To build this project, you need a working OCaml development environment with
OPAM.

```shell
$ ./deps.sh     # Install dependencies
$ dune build    # Compile tw.exe
```

The generated binary is in `_build/default` and can be run using `dune exec`.

```shell
$ dune exec ./twmc.exe -- -help
$ dune exec ./twmc.exe -- "x * y <= y * x"
```
