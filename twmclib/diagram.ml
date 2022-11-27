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

  let holds ?comment p =
    Option.iter (L.comment q) comment;
    L.assert_ q p
  in

  (* [d] is the delta map of the paper. *)
  let d a = L.var (v a) in

  (* Encoding of IDL over omega+ into IDL over omega.  *)

  let open L in

  let c0 = lit 0 and c1 = lit 1 in
  let is_omega x = x = c0 in
  let is_zero x = x = c1 in
  let is_finite x = not_ (is_omega x) in
  let is_positive x = not_ (is_zero x) in
  let is_finite_positive x = is_finite x && is_positive x in
  let is_succ ~suc ~pre =
    (is_omega suc && is_omega pre)
    || (is_finite suc && is_finite pre && suc = pre + c1)
  in
  let ( <= ) x y = is_omega y || (is_finite x && x <= y) in
  let ( < ) x y = x <= y && not_ (x = y) in
  let ( <==> ) p q = p ==> q && q ==> p in

  (* Diagram conditions. *)

  let deval t a = d (Sample.eval t a) in
  let dlast t = d (Sample.last t) in
  let devallast t = deval t (Sample.last t) in

  (* Condition 3.1-3.2 and 3.4-3.5. *)
  Sampleset.iter_evals
    (fun t samples ->
      let for_all p = Sample.Set.iter (fun a -> p a) samples in
      let holds_for_all ?comment p = for_all (fun a -> holds ?comment @@ p a) in
      (* Condition 3.1. *)
      for_all
        (fun a1 ->
          for_all
            (fun a2 ->
              if not (Sample.equal a1 a2)
              then holds
                     ~comment:"3.1"
                     ((d a1 <= d a2) ==> ((deval t a1) <= (deval t a2)))));
      for_all
        (fun a ->
          (* Condition 3.2. *)
          holds
            ~comment:"3.2"
            (is_zero (d a) ==> is_zero (deval t a));
          (* Condition 3.4. *)
          holds
            ~comment:"3.3"
            (dlast t <= d a <==> (devallast t = deval t a));
          (* Condition 3.5. *)
          begin match Sample.view a with
          | Sample.SLast t' when Basic.equal t t' ->
             holds
                ~comment:"3.5"
               (is_omega (d a) ==> is_omega (devallast t))
          | _ ->
             ()
          end);

      (* Condition 3.3. *)
      Sampleset.iter_all
        (fun a ->
          match Sample.view a with
          | Sample.SSucc a' ->
             holds ~comment:"3.3" @@ is_succ ~pre:(d a') ~suc:(d a)

          | _ ->
             ())
        set;

      (* Conditions 3.6-3.12. *)
      begin match t with
      | Id ->
         (* Condition 3.6. *)
         holds_for_all ~comment:"3.6" (fun a -> deval t a = d a);
         (* Condition 3.7 *)
         holds ~comment:"3.7" @@ is_omega (dlast Basic.Id)
      | Comp (u, v) ->
         (* Condition 3.8. *)
         holds_for_all ~comment:"3.8"
           (fun a -> deval u (Sample.eval v a) = deval t a);
         (* Condition 3.9. *)
         holds ~comment:"3.9" @@ (is_omega (dlast t) ==>
                                    (is_omega (dlast u) && is_omega (dlast v)))
      | Neg u ->
         (* Condition 3.10. *)
         holds_for_all ~comment:"3.10"
           (fun a -> is_finite_positive (d a) ==>
                       (deval u (Sample.eval t a) < d a));
         (* Condition 3.11. *)
         holds_for_all ~comment:"3.11"
           (fun a -> is_finite (deval u a) ==>
                       (d a <= deval u Sample.(succ (eval t a))));
         (* Condition 3.12. *)
         holds ~comment:"3.12" @@ (is_omega (dlast t) ==> is_omega (dlast u))
      | _ ->
         ()
      end
    )
    set;

  (* These are the top-level conditions to be falsified, expressing that we are
     looking for counter-examples. *)
  List.iter (fun t -> holds Sample.(deval t (var k) < d (var k))) ts;

  (* We return the final query. *)
  q
