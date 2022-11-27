type mode =
  | Dump of string
  | Z3

let mode = ref Z3

let debug = ref false

let verbosity = ref 0
