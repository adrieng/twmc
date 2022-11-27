module Make(L : Logic.S) = struct
  module S = Sampler.Make(Term.V)(L)

  type instance =
    {
      problem : L.query;
      zero : L.var;
      omega : L.var;
      test : L.var;
      samples : S.t;
    }

  let translate rel =
    let prb = L.make () in
    let s = S.make prb in
    let z = L.fresh ~name:"z" prb in
    let o = L.fresh ~name:"o" prb in

    let rec translate_atom t p =
      match t with
      | Warp.Var x ->
         S.sample s x p
      | Warp.Bot ->
         z
      | Warp.Top ->
         (* FIXME *)
         o
      | Warp.Star (t, u) ->
         translate_atom t (translate_atom u p)
    in

    let translate_rel rel p =
      match rel with
      | Warp.Le (t, u) ->
         let t_p = translate_atom t p in
         let u_p = translate_atom u p in
         L.(var u_p < var t_p)
    in

    let ass = L.assert_ prb in
    let com = L.comment prb in

    let assert_bounds p =
      ass L.(var z <= var p && var p <= var o)
    in

    let assert_monotonicity _ p1 q1 p2 q2 =
      ass L.((var p1 <= var p2) ==> (var q1 <= var q2))
    in

    let assert_cocont_z _ p q =
      ass L.((var p <= var z) ==> (var q <= var z))
    in

    let assert_cocont_o _ p q =
      (* FIXME *)
      ass L.((var o <= var p) ==> (var o <= var q))
    in

    let p = S.fresh_point ~name:"i" s in

    com "Relation";
    ass @@ translate_rel rel p;
    com "Bounds";
    ass @@ L.(var z = lit 0);
    S.iter_points assert_bounds s;
    com "Monotonicity";
    S.iter_sample_pairs assert_monotonicity s;
    com "Cocontinuity (zero)";
    S.iter_samples assert_cocont_z s;
    com "Cocontinuity (omega)";
    S.iter_samples assert_cocont_o s;
    {
      problem = prb;
      zero = z;
      omega = o;
      test = p;
      samples = s;
    }

  let counterexample inst model =
    let module HT = Hashtbl.Make(Term.V) in
    let omega = model inst.omega in
    let ht = HT.create 100 in
    let add_sample x p q =
      let a =
        try HT.find ht x
        with Not_found ->
          let a = Array.make (omega + 1) 0 in HT.add ht x a; a
      in
      let p = model p in
      let q = model q in
      a.(p) <- q
    in
    let complete a =
      for i = 1 to Array.length a - 1 do
        if a.(i) < a.(i - 1) then a.(i) <- a.(i - 1)
      done
    in
    S.iter_samples add_sample inst.samples;
    HT.iter (fun _ a -> complete a) ht;
    {
      Counterexample.valuation =
        HT.to_seq ht
        |> Seq.map (fun (x, a) -> x, Compact.(make a (Fin 0)))
        |> List.of_seq;
      Counterexample.point = model inst.test;
    }
end
