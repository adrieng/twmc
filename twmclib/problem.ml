type t =
  | Le of Term.t * Term.t
  | Eq of Term.t * Term.t

let pp pb =
  let open PPrint in
  let bt t = group @@ Term.pp t in
  match pb with
  | Le (t, u) ->
     infix 2 1 (!^ "<=") (bt t) (bt u)
  | Eq (t, u) ->
     infix 2 1 (!^ "=") (bt t) (bt u)

let existence_statements
      (type a b)
      (module L : Logic.S with type query = a and type V.t = b)
      ?(on_initial_positive_term = fun _ -> ())
      ?(on_residual_simple_term = fun _ -> ())
      ?(on_canonical_term = fun _ -> ())
      ?(on_basic_positive_terms = fun _ -> ())
      ?(on_simplified_basic_positive_terms = fun _ -> ())
      ?(on_saturated_sample_set = fun _ -> ())
      pb : (a, b) Diagram.existence_statement list =
  (* First, we translate the problem [pb] into a term [s] such that [pb] is
     valid iff [id <= s] is valid. *)
  let s =
    match pb with
    | Le (t, u) ->
       Term.Under (t, u)
    | Eq (t, u) ->
       Term.(Meet (Under (t, u), Under (u, t)))
  in
  on_initial_positive_term s;
  (* Then, we put [s] in conjunctive normal form, with basic terms as atoms. *)
  let s = Normalize.eliminate_binary_residuals s in
  on_residual_simple_term s;
  let s = Normalize.canonicalize `CNF s in
  on_canonical_term s;
  let tss = Normalize.to_cnf s in
  List.iter on_basic_positive_terms tss;
  let tss =
    if !Options.simplify
    then List.map (List.map Normalize.simplify) tss
    else tss
  in
  List.iter on_simplified_basic_positive_terms tss;
  (* Now, for each disjunction of basic terms, we build a saturated sample
     set and a sample variable. *)
  let sss =
    List.map
      (fun ts ->
        let kv = Sample.V.fresh () in
        let k = Sample.var kv in
        kv,
        ts,
        List.fold_left
          (fun s t -> Sampleset.saturate (Sample.eval t k) s)
          Sampleset.empty
          ts
      )
      tss
  in
  List.iter (fun (_, _, ss) -> on_saturated_sample_set ss) sss;
  List.map
    (fun (root_variable, basic_terms_under_test, sample_set) ->
      Diagram.statement_of_basic_conjunctive_problem
        (module L)
        ~sample_set
        ~root_variable
        ~basic_terms_under_test)
    sss

module Solution = struct
  type t = Valid
         | Invalid of Counterexample.t

  let valid = Valid

  let invalid = Invalid Counterexample.dummy

  let pp =
    let open PPrint in
    function
    | Valid ->
       !^ "VALID\n"
     | Invalid cm ->
        group @@ prefix 2 1 (!^ "INVALID at:") (Counterexample.pp cm)

  let equal sol1 sol2 =
    match sol1, sol2 with
    | Valid, Valid | Invalid _, Invalid _ -> true
    | _ -> false
end

let solve_with_z3 ?(on_diagram = (fun _ -> ()))
  =
  let rec loop = function
    | [] ->
       Solution.Valid
    | stm :: stms ->
       let err () =
         Printf.eprintf
           "unknown result, please send the problem to <guatto@irif.fr>\n";
         exit 1
       in
       let solve q =
         match Backends.Z3.solve q with
         | Z3.Solver.UNKNOWN ->
            err ()
         | Z3.Solver.UNSATISFIABLE ->
            `Unsat
         | Z3.Solver.SATISFIABLE ->
            begin match Backends.Z3.model q with
            | None ->
               err ()
            | Some f ->
               `Sat f
            end
       in
       begin match Diagram.exists ~solve stm with
       | None ->
          loop stms
       | Some d ->
          on_diagram d;
          Solution.Invalid (Diagram.counterexample d)
       end
  in
  loop
