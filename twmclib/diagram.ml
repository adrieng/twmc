(* {1 From Saturated Sample Sets to Logical Formulas}

   This module translates a saturated sample set into a logical query in the
   quantifier-free first-order theory of natural numbers with ordering,
   successor, and zero. This query describes the set of possible countermodels
   to the time warp query from which the sample set was generated.

   This is the last key part of the algorithm, since this query can then be sent
   to an SMT solver and checked for satisfiability. *)

let translate (type a) (module L : Logic.S with type query = a) set k ts : a =
  let q = L.make () in

  let v =
    let t = Sample.Table.create 1000 in
    fun a ->
    try Sample.Table.find t a
    with Not_found ->
      let v = L.fresh q in
      Sample.Table.add t a v;
      v
  in

  let holds p = L.assert_ q p in

  (* [d] is the delta map of the paper. *)
  let d a = L.var (v a) in

  (* Encoding of LIA over omega+ into LIA over omega.  *)

  let open L in

  let c0 = lit 0 and c1 = lit 1 in
  let is_omega x = x = c0 in
  let is_zero x = x = c1 in
  let is_finite x = not_ (is_omega x) in
  let is_positive x = not_ (is_zero x) in
  let is_finite_positive x = is_finite x && is_positive x in
  let is_succ x y = (is_omega x && is_omega y) || (is_finite x && x = y + c1) in
  let ( <= ) x y = is_omega y || (is_finite x && x <= y) in
  let ( < ) x y = x <= y && not_ (x = y) in
  let ( <==> ) p q = p ==> q && q ==> p in

  (* Diagram conditions. *)

  let deval t a = d (Sample.eval t a) in
  let dlast t = d (Sample.last t) in
  let devalast t = deval t (Sample.last t) in

  (* Condition 1, for all pairs. *)
  let cond_mon t a1 a2 = (d a1 <= d a2) ==> ((deval t a1) <= (deval t a2)) in
  Sampleset.iter_evals
    (fun t samples ->
      Sample.Set.iter
        (fun a1 ->
          Sample.Set.iter
            (fun a2 -> if not (Sample.equal a1 a2)
                       then holds (cond_mon t a1 a2))
            samples)
        samples)
    set;

  (* Conditions 2-23. *)
  Sampleset.iter_all
    (fun a ->
      match Sample.view a with
      (* Conditions for structural soundness (minus condition 1). *)
      | Sample.SPred a' ->
         (* Condition 3 *)
         holds @@ is_succ (d a) (d a')
      | Sample.SSucc a' ->
         (* Condition 4 *)
         holds @@ is_succ (d a') (d a)
      | Sample.SLast (Basic.RedO _) ->
         (* Condition 13 *)
         holds @@ is_finite (d a)
      | Sample.SEval (t, a') ->
         (* Condition 2 *)
         holds @@ (is_zero (d a') ==> is_zero (d a));
         (* Condition 5 *)
         holds @@ ((dlast t <= d a') <==> (d a = devalast t));
         (* Condition 6 *)
         holds @@ (is_omega (dlast t) ==> is_omega (devalast t));
         (* The remaining conditions are specific to the shape of t. *)
         begin match t with
         | Basic.Var _ ->
            ();

         (* Conditions for logical soundness. *)
         | Basic.Id ->
            (* Condition 7 *)
            holds (d a = d a')
         | Basic.Bot ->
            (* Condition 8 *)
            holds @@ is_zero (dlast t)
         | Basic.Top ->
            (* These conditions does not appear in the paper since Top is not
               primitive there. *)
            holds (is_positive (d a') ==> is_omega (d a));
            holds (dlast t = c1);
         | Basic.Comp (u1, u2) ->
            (* Condition 9 *)
            holds (d a = deval u1 (Sample.eval u2 a'));
            (* Condition 10 *)
            holds (is_omega (dlast t)
                   ==> (is_omega (dlast u1) && is_omega (dlast u2)))

         (* Conditions for o-soundness. *)
         | Basic.RedO t' ->
            (* Condition 11 *)
            holds (is_zero (d a) || is_omega (d a));
            (* Condition 12 *)
            holds (is_finite (d a')
                   ==> (is_omega (d a) <==> is_omega (deval t' a')));
            (* Condition 13 has been handled above. *)
            ();
            (* Condition 14 *)
            holds ((is_finite (devalast t) && is_finite (d a'))
                   ==> is_finite (deval t' a'))

         (* Conditions for r-soundness. *)
         | Basic.RedR t' ->
            (* Condition 15 *)
            holds (deval t' Sample.(eval t a') <= d a');
            (* Condition 16 *)
            holds ((is_finite_positive (d a') && is_finite (d a))
                   ==> (d a' < deval t' Sample.(succ a)));
            (* Condition 17 *)
            holds (is_omega (dlast t) ==> (is_omega (dlast t')));
            (* Condition 18 *)
            holds (is_finite (devalast t)
                   ==> is_omega (deval t' Sample.(succ (eval t (last t)))));

         (* Conditions for l-soundness. *)
         | Basic.RedL t' ->
            (* Condition 19 *)
            holds (is_finite (d a) ==> (d a' <= deval t' a));
            (* Condition 20 *)
            holds ((is_finite_positive (d a') && is_finite (d a))
                   ==> (deval t' Sample.(pred a) < d a'));
            (* Condition 21 *)
            holds ((is_finite (d a') && is_omega (d a))
                   ==> (deval t' a < d a'));
            (* Condition 22 *)
            holds (is_omega (dlast t) ==> is_omega (dlast t'));
            (* Condition 23 *)
            holds (is_finite (devalast t)
                   ==> is_omega (deval t' Sample.(eval t (last t))))

         end
      | Sample.SVar _ | Sample.SLast _ ->
         ()
    )
    set;

  (* These are the top-level conditions to be falsified, expressing that we are
     looking for counter-examples. *)
  List.iter (fun t -> holds Sample.(deval t (var k) < d (var k))) ts;

  (* We return the final query. *)
  q
