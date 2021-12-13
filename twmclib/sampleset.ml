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
       let set = extend a set in
       let set = saturate Sample.(last t) set in
       begin match t with
       | Basic.Var _ | Basic.Id | Basic.Top | Basic.Bot ->
          set
       | Basic.Comp (t, u) ->
          set
          |> saturate Sample.(eval t (eval u a))
       | Basic.RedO t ->
          set
          |> saturate Sample.(eval t a)
       | Basic.RedR t ->
          set
          |> saturate Sample.(eval t @@ eval (Basic.RedR t) a)
          |> saturate Sample.(eval t @@ succ @@ eval (Basic.RedR t) a)
       | Basic.RedL t ->
          set
          |> saturate Sample.(eval t @@ eval (Basic.RedL t) a)
          |> saturate Sample.(eval t @@ pred @@ eval (Basic.RedL t) a)
       end
    | Sample.SPred a | Sample.SSucc a ->
       extend a set

let fold_all f set ini =
  Sample.Set.fold f set.all ini

let iter_all f set =
  Sample.Set.iter f set.all

let fold_evals f set ini =
  Basic.Map.fold f set.evals ini

let iter_evals f set =
  Basic.Map.iter f set.evals
