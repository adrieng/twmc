(* {1 From Saturated Sample Sets to Logical Formulas}

   This module translates a saturated sample set into a logical query in the
   quantifier-free first-order theory of natural numbers with ordering,
   successor, and zero. This query describes the set of possible countermodels
   to the time warp query from which the sample set was generated.

   This is the last key part of the algorithm, since this query can then be sent
   to an SMT solver and checked for satisfiability. *)

let translate
      (type a b)
      (module L : Logic.S with type query = a and type V.t = b)
      set k ts : a * (b Logic.valuation -> Counterexample.t) =
  let q = L.make () in

  let v =
    let t = Sample.Table.create 1000 in
    fun a ->
    try Sample.Table.find t a
    with Not_found ->
      let v =
        L.fresh
          ~comment:(if !Options.debug
                    then Print.PPrint.to_string (Sample.pp a)
                    else "") q
      in
      Sample.Table.add t a v;
      v
  in

  let holds ?comment p =
    Option.iter (L.comment q) comment;
    L.assert_ q p
  in

  (* [d] is the delta map of the paper. *)
  let d a = L.var (v a) in

  (* Encoding of difference logic over omega+ into IDL over omega.  *)

  let open L in

  let comment = L.comment q in

  let c0 = lit 0 and c1 = lit 1 in

  let ( <==> ) p q = p ==> q && q ==> p in

  let is_omega x = x = c0 in
  let is_zero x = x = c1 in
  let is_finite x = not_ (is_omega x) in
  let is_positive x = not_ (is_zero x) in
  let is_finite_positive x = is_finite x && is_positive x in
  (* [is_succ ~suc ~pre] means that [suc] is [pre + 1]. *)
  let is_succ ~suc ~pre =
    (is_omega suc && is_omega pre)
    || (is_finite suc && is_finite pre && suc = pre + c1)
  in
  let ( <= ) x y = is_omega y || (is_finite x && x <= y) in
  let ( < ) x y = x <= y && not_ (x = y) in

  (* Diagram conditions. *)

  let deval t a = d (Sample.eval t a) in
  let dlast t = d (Sample.last t) in
  let devallast t = deval t (Sample.last t) in

  (* Condition 3.1-3.2 and 3.4-3.5. *)
  Sampleset.iter_evals
    (fun t samples ->
      comment (Format.sprintf "Conditions for %s[-]"
                 (Print.PPrint.to_string (Basic.pp t)));
      let for_all p = Sample.Set.iter (fun a -> p a) samples in
      let holds_for_all ?comment p = for_all (fun a -> holds ?comment @@ p a) in
      (* Condition 3.1. *)
      for_all
        (fun a1 ->
          for_all
            (fun a2 ->
              if not (Sample.equal a1 a2)
              then holds
                     ~comment:"3.1: a1 <= a2 => t[a1] <= t[a2]"
                     ((d a1 <= d a2) ==> ((deval t a1) <= (deval t a2)))));
      for_all
        (fun a ->
          (* Condition 3.2. *)
          holds
            ~comment:"3.2: a = 0 => t[a] = 0"
            (is_zero (d a) ==> is_zero (deval t a));
          (* Condition 3.4. *)
          holds
            ~comment:"3.4: last t <= a => t[last t] = t[a]"
            (dlast t <= d a <==> (devallast t = deval t a));
          (* Condition 3.5. *)
          begin match Sample.view a with
          | Sample.SLast t' when Basic.equal t t' ->
             holds
               ~comment:"3.5: last t = omega => t[last t] = omega"
               (is_omega (d a) ==> is_omega (devallast t))
          | _ ->
             ()
          end);

      (* Condition 3.3. *)
      comment "* Successor conditions *";
      Sampleset.iter_all
        (fun a ->
          match Sample.view a with
          | Sample.SSucc a' ->
             holds
               ~comment:"3.3: succ a' = S a"
               (is_succ ~pre:(d a') ~suc:(d a))

          | _ ->
             ())
        set;

      (* Conditions 3.6-3.12. *)
      begin match t with
      | Id ->
         comment "* Identity conditions *";
         (* Condition 3.6. *)
         holds_for_all
           ~comment:"3.6: id[a] = a"
           (fun a -> deval t a = d a);
         (* Condition 3.7 *)
         holds
           ~comment:"3.7: last id = omega"
         @@ is_omega (dlast t)

      | Comp (u, v) ->
         comment "* Composition conditions *";
         (* Condition 3.8. *)
         holds_for_all ~comment:"3.8: u[v[a]] = uv[a]"
           (fun a -> deval u (Sample.eval v a) = deval t a);
         (* Condition 3.9. *)
         holds
           ~comment:"3.9: last uv = omega => last u = omega && last v = omega"
           (is_omega (dlast t) ==> (is_omega (dlast u) && is_omega (dlast v)))

      | Neg u ->
         comment "* Negation conditions *";
         (* Condition 3.10. *)
         holds_for_all
           ~comment:"3.10: u[u'[a]] < a"
           (fun a -> is_finite_positive (d a) ==>
                       (deval u (Sample.eval t a) < d a));
         (* Condition 3.11. *)
         holds_for_all
           ~comment:"3.11: u'[a] < omega => a <= u[S[u'[a]]"
           (fun a -> is_finite (deval t a) ==>
                       (d a <= deval u Sample.(succ (eval t a))));
         (* Condition 3.12. *)
         holds
           ~comment:"3.12: last u' = omega => last u = omega"
         @@ (is_omega (dlast t) ==> is_omega (dlast u))

      | Var _ ->
         ()
      end
    )
    set;

  (* These are the top-level conditions to be falsified, expressing that we are
     looking for counter-examples. *)
  comment "Root constraints";
  List.iter (fun t -> holds Sample.(deval t (var k) < d (var k))) ts;

  (* Build a counter-example from an SMT-level counter-model. *)
  let build_countermodel (c : b Logic.valuation) =
    let s a = c @@ v a in
    Sampleset.fold_evals
      (fun t samples ce ->
        begin match t with
        | Basic.Var x ->
           (* FIXME conversion *)
           let last = s Sample.(eval t @@ last t) in
           let points = Sample.Set.to_seq samples
                        |> Seq.map (fun a -> s a, s Sample.(eval t a))
                        |> List.of_seq
                        |> List.sort_uniq
                             (fun (a, _) (b, _) -> Enat.compare a b) in
           if Options.verbosity_above 3
           then
             begin
               Format.eprintf "POINTS for %s:@." (Term.V.to_string x);
               List.iter (fun (x, y) ->
                   Format.eprintf "  (%s, %s)@."
                     (Enat.to_string x)
                     (Enat.to_string y)) points
             end;
           Counterexample.add ce x (EvLinear.of_points ~last points)
        | _ ->
           ce
        end)
      set
      Counterexample.{ valuation = [];
                       point = Enat.to_int @@ c @@ v @@ Sample.var k; }
  in

  (* We return the final query together with the countermodel builder. *)
  q, build_countermodel
