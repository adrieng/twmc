open Term

let rec eliminate_binary_residuals t =
  match t with
  | Var _ | Id | Top | Bot ->
     t
  | Comp (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Comp (t, u)
  | Under (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Join (Comp (RedR t, u), Join (RedR (Comp (Top, t)), RedO u))
  | Over (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Join (Comp (t, RedL u), RedO (RedL t))
  | RedO t ->
     RedO (eliminate_binary_residuals t)
  | RedL t ->
     RedL (eliminate_binary_residuals t)
  | RedR t ->
     RedR (eliminate_binary_residuals t)
  | Meet (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Meet (t, u)
  | Join (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Join (t, u)

let rec comp t u =
  match t, u with
  | Meet (t1, t2), _ ->
     Meet (comp t1 u, comp t2 u)
  | _, Meet (u1, u2) ->
     Meet (comp t u1, comp t u2)
  | Join (t1, t2), _ ->
     Join (comp t1 u, comp t2 u)
  | _, Join (u1, u2) ->
     Join (comp t u1, comp t u2)
  | _ ->
     Comp (t, u)

let rec meet t u =
  match t, u with
  | Top, v | v, Top ->
     v
  | Bot, _ | _, Bot ->
     Bot
  | Join (t1, t2), u ->
     Join (meet t1 u, meet t2 u)
  | t, Join (u1, u2) ->
     Join (meet t u1, meet t u2)
  | _ ->
     Meet (t, u)

let join t u =
  match t, u with
  | Bot, v | v, Bot ->
     v
  | Top, _ | _, Top ->
     Top
  | _ ->
     Join (t, u)

let rec over t u =
  match t, u with
  | Meet (t1, t2), _ ->
     Meet (over t1 u, over t2 u)
  | _, Meet (u1, u2) ->
     Join (over t u1, over t u2)
  | Join (t1, t2), _ ->
     Join (over t1 u, over t2 u)
  | _, Join (u1, u2) ->
     Join (over t u1, over t u2)
  | _ ->
     Over (t, u)

let rec under t u =
  match t, u with
  | Meet (t1, t2), _ ->
     Meet (under t1 u, under t2 u)
  | _, Meet (u1, u2) ->
     Join (under t u1, under t u2)
  | Join (t1, t2), _ ->
     Join (under t1 u, under t2 u)
  | _, Join (u1, u2) ->
     Join (under t u1, under t u2)
  | _ ->
     Under (t, u)

let rec redo t =
  match t with
  | Meet (t1, t2) ->
     Meet (redo t1, redo t2)
  | Join (t1, t2) ->
     Join (redo t1, redo t2)
  | _ ->
     RedO t

let rec redl t =
  match t with
  | Meet (t1, t2) ->
     Join (redl t1, redl t2)
  | Join (t1, t2) ->
     Meet (redl t1, redl t2)
  | _ ->
     RedL t

let rec redr t =
  match t with
  | Meet (t1, t2) ->
     Join (redr t1, redr t2)
  | Join (t1, t2) ->
     Meet (redr t1, redr t2)
  | _ ->
     RedR t

let rec canonicalize t =
  match t with
  | Var _ | Id | Top | Bot ->
     t
  | Comp (t, u) ->
     comp (canonicalize t) (canonicalize u)
  | Over (t, u) ->
     over (canonicalize t) (canonicalize u)
  | Under (t, u) ->
     under (canonicalize t) (canonicalize u)
  | RedO t ->
     redo (canonicalize t)
  | RedR t ->
     redr (canonicalize t)
  | RedL t ->
     redl (canonicalize t)
  | Meet (t, u) ->
     meet (canonicalize t) (canonicalize u)
  | Join (t, u) ->
     join (canonicalize t) (canonicalize u)

exception Not_canonical

let rec to_basic t =
  let get bss =
    match bss with
    | [[b]] -> b
    | _ -> raise Not_canonical
  in
  match t with
  | Var s ->
     [[Basic.Var s]]
  | Id ->
     [[Basic.Id]]
  | Top ->
     [[Basic.Top]]
  | Bot ->
     [[Basic.Bot]]
  | Comp (t, u) ->
     let t = get @@ to_basic t in
     let u = get @@ to_basic u in
     [[Basic.Comp (t, u)]]
  | RedO t ->
     let t = get @@ to_basic t in
     [[Basic.RedO t]]
  | RedL t ->
     let t = get @@ to_basic t in
     [[Basic.RedL t]]
  | RedR t ->
     let t = get @@ to_basic t in
     [[Basic.RedR t]]
  | Under _ | Over _ ->
     raise Not_canonical
  | Meet (t, u) ->
     to_basic t @ to_basic u
  | Join (t, u) ->
     let tss = to_basic t in
     let uss = to_basic u in
     let rec distr tss =
       match tss with
       | [] ->
          uss
       | ts :: tss ->
          List.map (fun us -> ts @ us) (distr tss)
     in
     distr tss
