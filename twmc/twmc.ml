open Twmclib

let on_initial_positive_term t =
  Print.PPrint.print
    PPrint.(prefix 2 1 (!^ "Problem:") (!^ "id <=" ^/^ Term.pp t))

let verbosity_above k = !Options.verbosity > k

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

let on_saturated_sample_set ss =
  if verbosity_above 1
  then Print.PPrint.print PPrint.(prefix 2 1 (!^ "SSS:") (Sampleset.pp ss))

let on_logic_query ~pp query =
  if verbosity_above 2
  then Print.PPrint.print PPrint.(!^ " Logic query:" ^/^ group (pp query))

let parse_problem s =
  try Parser.problem Lexer.token (Lexing.from_string s)
  with Parser.Error ->
    Printf.eprintf "%s: syntax error\n" s;
    exit 1

let process s =
  let p = parse_problem s in
  match !Options.mode with
  | Z3 ->
     let sol =
       Problem.to_solution
         ~on_initial_positive_term
         ~on_residual_simple_term
         ~on_canonical_term
         ~on_basic_positive_terms
         ~on_saturated_sample_set
         ~on_logic_query
         p
     in
     begin match sol with
     | `Valid ->
        Printf.printf "VALID\n"
     | `Invalid cm ->
        Print.PPrint.print
          PPrint.(prefix 2 1 (!^ "INVALID at:") (Counterexample.pp cm))
     end
  | Dump file ->
     let oc = open_out file in
     Problem.to_logic (module Backends.SMTLIB) p
     |> List.iter (Backends.SMTLIB.to_channel oc);
     close_out oc

let _ =
  Arg.parse
    Arg.(align
           [
             "-dump",
             String (fun out -> Options.mode := Dump out),
             "<FILE> Write generated problem to FILE.smt";
             "-z3",
             Unit (fun () -> Options.mode := Z3),
             " Solve directly using Z3 (default)";
             "-v",
             Unit (fun () -> incr Options.verbosity),
             " Be verbose";
             "-d",
             Set Options.debug,
             " Print debug messages";
           ])
    process
    (Printf.sprintf
       "Usage: %s [OPTIONS] ineq1 ... ineqN\nOptions:"
       Sys.argv.(0))
