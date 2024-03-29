type t =
  {
    all : Sample.Set.t;
    (** All the samples in the sample set. *)
    evals : Sample.Set.t Basic.Map.t;
    (** Samples [a] such that [eval t a] belongs to [all], indexed by [t]. *)
    lasts : Basic.Set.t;
    (** Basic terms [t] such that [last t] belongs to [all]. *)
  }

let pp set =
  let open PPrint in
  let all = List.of_seq @@ Seq.map Sample.pp @@ Sample.Set.to_seq set.all in
  braces @@ group @@ space ^^ align (flow (!^ ";" ^^ break 1) all) ^^ space

let empty =
  {
    all = Sample.Set.empty;
    evals = Basic.Map.empty;
    lasts = Basic.Set.empty;
  }

let extend a set =
  {
    all =
      Sample.Set.add a set.all;
    evals =
      begin match Sample.view a with
      | SEval (t, a) ->
         let evals_of_t =
           try Basic.Map.find t set.evals
           with Not_found -> Sample.Set.empty
         in
         Basic.Map.add t (Sample.Set.add a evals_of_t) set.evals
      | _ ->
         set.evals
      end;
    lasts =
      begin match Sample.view a with
      | SLast t ->
         Basic.Set.add t set.lasts
      | _ ->
         set.lasts
      end;
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
       let set = saturate a set in
       (* Rewrite rule 3 from paper. *)
       let set = saturate Sample.(eval t (last t)) set in
       begin match t with
       | Basic.Var _ | Basic.Id ->
          set
       | Basic.Comp (t, u) ->
          (* Rewrite rule 4 from paper. *)
          set
          |> saturate Sample.(eval t (eval u a))
       | Basic.Neg u ->
          (* Rewrite rule 5 from paper. *)
          set
          |> saturate Sample.(eval u @@ eval t a)
          |> saturate Sample.(eval u @@ succ @@ eval t a)
       end
    | Sample.SSucc a ->
       (* Rewrite rule 2 from paper. *)
       saturate a set

let fold_all f set ini =
  Sample.Set.fold f set.all ini

let iter_all f set =
  Sample.Set.iter f set.all

let fold_evals f set ini =
  Basic.Map.fold f set.evals ini

let iter_evals f set =
  Basic.Map.iter f set.evals

let fold_lasts f set ini =
  Basic.Set.fold f set.lasts ini

let iter_lasts f set =
  Basic.Set.iter f set.lasts
