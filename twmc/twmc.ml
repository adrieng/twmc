open Twmclib

let on_initial_positive_term t =
  Print.print PPrint.(prefix 2 1 (!^ "Problem:") (!^ "id <=" ^/^ Term.pp t))

let on_residual_simple_term t =
  if !Options.debug
  then
    Print.print PPrint.(prefix 2 1
                          (!^ "Residual-simple:")
                          (!^ "id <=" ^/^ Term.pp t))

let on_canonical_term t =
  if !Options.debug
  then
    Print.print PPrint.(prefix 2 1
                          (!^ "Canonical:")
                          (!^ "id <=" ^/^ Term.pp t))

let on_basic_positive_terms ts =
  if !Options.verbose
  then Print.print PPrint.(prefix 2 1
                             (!^ "Subproblem:")
                             (!^ "id <=" ^/^
                                separate_map
                                  (!^ " \\/ ")
                                  Basic.pp
                                  ts))

let on_saturated_sample_set ss =
  if !Options.verbose
  then Print.print PPrint.(prefix 2 1 (!^ "SSS:") (Sampleset.pp ss))

let on_logic_query ~pp query =
  ignore pp; ignore query;
  ()
  (* if !verbose
   * then Print.print PPrint.(!^ " Logic query:" ^/^ group (pp query)) *)

let parse_problem s =
  try Parser.problem Lexer.token (Lexing.from_string s)
  with Parser.Error ->
    Printf.eprintf "%s: syntax error\n" s;
    exit 1

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
             Set Options.verbose,
             " Be verbose";
             "-d",
             Set Options.debug,
             " Print debug messages";
           ])
    (fun s ->
      let sol =
        parse_problem s
        |> Problem.to_solution
             ~on_initial_positive_term
             ~on_residual_simple_term
             ~on_canonical_term
             ~on_basic_positive_terms
             ~on_saturated_sample_set
             ~on_logic_query
      in
      match sol with
      | `Valid ->
         Printf.printf "VALID\n"
      | `Invalid cm ->
         Print.print
           PPrint.(prefix 2 1 (!^ "INVALID at:") (Counterexample.print cm))
    )
    (Printf.sprintf
       "Usage: %s [OPTIONS] ineq1 ... ineqN\nOptions:"
       Sys.argv.(0))
