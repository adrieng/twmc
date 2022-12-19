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

type ('logic_query, 'result) solver =
  ?on_initial_positive_term:(Term.t -> unit) ->
  ?on_residual_simple_term:(Term.t -> unit) ->
  ?on_canonical_term:(Term.t -> unit) ->
  ?on_basic_positive_terms:(Basic.t list -> unit) ->
  ?on_simplified_basic_positive_terms:(Basic.t list -> unit) ->
  ?on_saturated_sample_set:(Sampleset.t -> unit) ->
  ?on_logic_query:(pp:('logic_query -> PPrint.document)
                   -> 'logic_query -> unit) ->
  t ->
  'result

let to_logic
      (type a b)
      (module L : Logic.S with type query = a and type V.t = b)
      ?(on_initial_positive_term = fun _ -> ())
      ?(on_residual_simple_term = fun _ -> ())
      ?(on_canonical_term = fun _ -> ())
      ?(on_basic_positive_terms = fun _ -> ())
      ?(on_simplified_basic_positive_terms = fun _ -> ())
      ?(on_saturated_sample_set = fun _ -> ())
      ?(on_logic_query = fun ~pp _ -> ignore pp; ())
      pb : (a * (b Logic.valuation -> Counterexample.t)) list =
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
  let queries =
    List.map
      (fun (k, ts, ss) -> Diagram.translate (module L) ss k ts)
      sss
  in
  List.iter (fun (q, _) -> on_logic_query ~pp:L.pp q) queries;
  queries

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
        prefix 2 1 (!^ "INVALID at:") (Counterexample.pp cm)

  let equal sol1 sol2 =
    match sol1, sol2 with
    | Valid, Valid | Invalid _, Invalid _ -> true
    | _ -> false
end

let to_solution
      ?(on_initial_positive_term = fun _ -> ())
      ?(on_residual_simple_term = fun _ -> ())
      ?(on_canonical_term = fun _ -> ())
      ?(on_basic_positive_terms = fun _ -> ())
      ?(on_simplified_basic_positive_terms = fun _ -> ())
      ?(on_saturated_sample_set = fun _ -> ())
      ?(on_logic_query = fun ~pp _ -> ignore pp; ())
      pb =
  let solve query builder =
    match Backends.Z3.solve query with
    | Z3.Solver.UNKNOWN ->
       let r_s = Print.PPrint.to_string (pp pb) in
       Printf.eprintf
         "%s: unknown result, please send to <guatto@irif.fr>\n"
         r_s;
       exit 1
    | Z3.Solver.UNSATISFIABLE ->
       Solution.Valid
    | Z3.Solver.SATISFIABLE ->
       begin match Backends.Z3.model query with
       | None ->
          let r_s = Print.PPrint.to_string (pp pb) in
          Printf.eprintf
            "%s: no model, please send to <guatto@irif.fr>\n"
            r_s;
          exit 1
       | Some countermodel ->
          Solution.Invalid
            (builder (fun x -> Enat.raw_of_int @@ countermodel x))
       end
  in

  let rec solve_all queries =
    match queries with
    | [] ->
       Solution.Valid
    | (query, builder) :: queries ->
       begin match solve query builder with
       | Solution.Valid ->
          solve_all queries
       | invalid ->
          invalid
       end
  in

  solve_all @@
    to_logic
      (module Backends.Z3)
      ~on_initial_positive_term
      ~on_residual_simple_term
      ~on_canonical_term
      ~on_basic_positive_terms
      ~on_simplified_basic_positive_terms
      ~on_saturated_sample_set
      ~on_logic_query
      pb
