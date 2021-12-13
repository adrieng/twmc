type t =
  | Valid
  | Invalid of Counterexample.t
  | Internal_error of string

let print res =
  let open PPrint in
  match res with
  | Valid ->
     !^ "valid"
  | Invalid cex ->
     prefix 2 1 (!^ "invalid:") (Counterexample.print cex)
  | Internal_error msg ->
     prefix 2 1
       (!^ "error (please report to https://github.com/adrieng/twmc/issues):")
       (!^ msg)

let equal ?(consider_counterexamples = false) r1 r2 =
  match r1, r2 with
  | Valid, Valid ->
     true
  | Invalid cex1, Invalid cex2 ->
     not consider_counterexamples || Counterexample.equal cex1 cex2
  | Internal_error msg1, Internal_error msg2 ->
     msg1 = msg2
  | _ ->
     false

let solve rel =
  let module T_Z3 = Translation.Make(Backends.Z3) in
  let instance = T_Z3.translate rel in
  match Backends.Z3.solve instance.T_Z3.problem with
  | Z3.Solver.UNSATISFIABLE ->
     Valid
  | Z3.Solver.UNKNOWN ->
     Internal_error "unknown"
  | Z3.Solver.SATISFIABLE ->
     match Backends.Z3.model instance.T_Z3.problem with
     | None ->
        Internal_error "could not get model"
     | Some model ->
        Invalid (T_Z3.counterexample instance model)
