open Twmclib
open Hashcons

module T_SMT = Translation.Make(Backends.SMTLIB)
module T_Z3 = Translation.Make(Backends.Z3)

type mode =
  | Dump of string
  | Z3

let mode = ref Z3

let parse_relation s =
  try Parser.problem Lexer.token (Lexing.from_string s)
  with Parser.Error ->
    Printf.eprintf "%s: syntax error\n" s;
    exit 1

let decide_relation r =
  match !mode with
  | Dump file ->
     let oc = open_out file in
     Backends.SMTLIB.to_channel oc (T_SMT.translate r).T_SMT.problem;
     close_out oc
  | Z3 ->
     Print.print (Warp.Print.rel r);
     Print.print Result.(print @@ solve r);;

let _ =
  Arg.parse
    Arg.(align
           [
             "-dump",
             String (fun out -> mode := Dump out),
             "<FILE> Write generated problem to FILE.smt";
             "-z3",
             Unit (fun () -> mode := Z3),
             " Solve directly using Z3 (default)";
           ])
    (fun s -> decide_relation @@ parse_relation s)
    (Printf.sprintf
       "Usage: %s [OPTIONS] ineq1 ... ineqN\nOptions:"
       Sys.argv.(0))
