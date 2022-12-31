open Twmclib
open Options

let on_initial_positive_term t =
  Print.PPrint.print
    PPrint.(prefix 2 1 (!^ "Problem:") (!^ "id <=" ^/^ Term.pp t))

let on_residual_simple_term t =
  if verbosity_above 0
  then
    Print.PPrint.print PPrint.(prefix 2 1
                                 (!^ "Residual-simple:")
                                 (!^ "id <=" ^/^ Term.pp t))

let on_canonical_term t =
  if verbosity_above 0
  then
    Print.PPrint.print PPrint.(prefix 2 1
                                 (!^ "Canonical:")
                                 (!^ "id <=" ^/^ Term.pp t))

let on_basic_positive_terms ts =
  if verbosity_above 0
  then Print.PPrint.print PPrint.(prefix 2 1
                                    (!^ "Subproblem:")
                                    (!^ "id <=" ^/^
                                       separate_map (!^ " \\/ ") Basic.pp ts))

let on_simplified_basic_positive_terms ts =
  if verbosity_above 0
  then Print.PPrint.print PPrint.(prefix 2 1
                                    (!^ "Simplified subproblem:")
                                    (!^ "id <=" ^/^
                                       separate_map (!^ " \\/ ") Basic.pp ts))

let on_saturated_sample_set ss =
  if verbosity_above 1
  then Print.PPrint.print PPrint.(prefix 2 1 (!^ "SSS:") (Sampleset.pp ss))

let on_existence_query ~pp query =
  if verbosity_above 4
  then Print.PPrint.print PPrint.(!^ " Existence query:" ^/^ group (pp query))

let on_diagram d =
  if verbosity_above 1
  then Print.PPrint.print PPrint.(prefix 2 1 (!^ "Diagram:") (Diagram.pp d))

let parse_problem s =
  try Parser.problem Lexer.token (Lexing.from_string s)
  with Parser.Error ->
    Printf.eprintf "%s: syntax error\n" s;
    exit 1

let output_fn = ref ""

let smtlib s output_fn =
  let pref, ext = Filename.(remove_extension output_fn, extension output_fn) in
  match ext with
  | ".smt" ->
     let pb = parse_problem s in
     let stms =
       Problem.existence_statements
         (module Backends.SMTLIB)
         ~on_initial_positive_term
         ~on_residual_simple_term
         ~on_canonical_term
         ~on_basic_positive_terms
         ~on_simplified_basic_positive_terms
         ~on_saturated_sample_set
         pb
     in
     List.iteri
       (fun i stm ->
         let oc = open_out (Printf.sprintf "%s.%d.smt" pref i) in
         Backends.SMTLIB.to_channel oc
         @@ Diagram.query_of_existence_statement stm;
         close_out oc) stms
  | _ ->
     Printf.eprintf "Unknown file extension %s (should be .smt)@." ext;
     exit 1

let z3 s =
  let p = parse_problem s in
  let stms =
    Problem.existence_statements
      (module Backends.Z3)
      ~on_initial_positive_term
      ~on_residual_simple_term
      ~on_canonical_term
      ~on_basic_positive_terms
      ~on_simplified_basic_positive_terms
      ~on_saturated_sample_set
      p
  in
  List.iter
    (fun stm -> on_existence_query ~pp:Backends.Z3.pp
                  (Diagram.query_of_existence_statement stm))
    stms;
  let sol = Problem.solve_with_z3 ~on_diagram stms in
  Format.printf "%a@." (Print.PPrint.to_fmt Problem.Solution.pp) sol

let dump = ref ""

let process s =
  if !dump = "" then z3 s else smtlib s !dump

let _ =
  Arg.parse
    Arg.(align
           [
             "-dump",
             String (fun fn -> dump := fn),
             "<FILE> dump to <FILE> in SMTLIB2 format";
             "-v",
             Unit (fun () -> incr verbosity),
             " Be verbose";
             "-d",
             Set debug,
             " Print debug messages";
             "-n",
             Unit (fun () -> simplify := false),
             " Do not simplify basic terms";
             "-stat",
             Unit (fun () -> statistics := true),
             " Print statistics";
           ])
    process
    (Printf.sprintf
       "Usage: %s [OPTIONS] ineq1 ... ineqN\nOptions:"
       Sys.argv.(0))
