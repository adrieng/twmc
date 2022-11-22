type t =
  {
    all : Sample.Set.t;
    evals : Sample.Set.t Basic.Map.t;
  }

let pp set =
  let open PPrint in
  let all = List.of_seq @@ Seq.map Sample.pp @@ Sample.Set.to_seq set.all in
  braces (space ^^ align (flow (!^ ";" ^^ break 1) all) ^^ space)

let empty =
  {
    all = Sample.Set.empty;
    evals = Basic.Map.empty;
  }

let extend a set =
  {
    all = Sample.Set.add a set.all;
    evals =
      match Sample.view a with
      | SEval (t, a) ->
         let evals_of_t =
           try Basic.Map.find t set.evals
           with Not_found -> Sample.Set.empty
         in
         Basic.Map.add t (Sample.Set.add a evals_of_t) set.evals
      | _ ->
         set.evals;
  }

let rec saturate a set =
  if Sample.Set.mem a set.all then set
  else
    let set = extend a set in
    match Sample.view a with
    | Sample.SVar _ | Sample.SLast _ ->
       set
    | Sample.SEval (t, a) ->
       (* Rewrite rule 1 from paper. *)
       let set = extend a set in
       (* Rewrite rule 3 from paper. *)
       let set = saturate Sample.(eval t (last t)) set in
       begin match t with
       | Basic.Var _ | Basic.Id ->
          set
       | Basic.Comp (t, u) ->
          (* Rewrite rule 4 from paper. *)
          set
          |> saturate Sample.(eval t (eval u a))
       | Basic.Neg t ->
          (* Rewrite rule 5 from paper. *)
          set
          |> saturate Sample.(eval t @@ eval (Neg t) a)
          |> saturate Sample.(eval t @@ succ @@ eval (Neg t) a)
       end
    | Sample.SSucc a ->
       (* Rewrite rule 2 from paper. *)
       extend a set

let fold_all f set ini =
  Sample.Set.fold f set.all ini

let iter_all f set =
  Sample.Set.iter f set.all

let fold_evals f set ini =
  Basic.Map.fold f set.evals ini

let iter_evals f set =
  Basic.Map.iter f set.evals
