type mode =
  | Dump of string
  | Z3

let mode = ref Z3

let debug = ref false

let simplify = ref true

let statistics = ref false

let verbosity = ref 0

let verbosity_above k = !verbosity > k
