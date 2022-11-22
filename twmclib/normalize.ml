open Term

let rec eliminate_binary_residuals t =
  match t with
  | Var _ | Id ->
     t
  | Comp (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Comp (t, u)
  | Under (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Neg (Comp (Neg u, t))
  | Over (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Neg (Comp (t, Neg u))
  | Meet (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Meet (t, u)
  | Join (t, u) ->
     let t = eliminate_binary_residuals t in
     let u = eliminate_binary_residuals u in
     Join (t, u)
  | Neg t ->
     Neg (eliminate_binary_residuals t)

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

let meet t u =
  Meet (t, u)
  (* match t, u with *)
  (* | Join (t1, t2), u -> *)
  (*    Join (meet t1 u, meet t2 u) *)
  (* | t, Join (u1, u2) -> *)
  (*    Join (meet t u1, meet t u2) *)
  (* | _ -> *)
  (*    Meet (t, u) *)

let rec join t u =
  match t, u with
  | Meet (t1, t2), u ->
     Meet (join t1 u, join t2 u)
  | t, Meet (u1, u2) ->
     Meet (join t u1, join t u2)
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

let rec neg t =
  match t with
  | Meet (t, u) ->
     Join (neg t, neg u)
  | Join (t, u) ->
     Meet (neg t, neg u)
  | Neg t ->
     t
  | _ ->
     Neg t

let rec canonicalize t =
  match t with
  | Var _ | Id ->
     t
  | Comp (t, u) ->
     comp (canonicalize t) (canonicalize u)
  | Over (t, u) ->
     over (canonicalize t) (canonicalize u)
  | Under (t, u) ->
     under (canonicalize t) (canonicalize u)
  | Meet (t, u) ->
     meet (canonicalize t) (canonicalize u)
  | Join (t, u) ->
     join (canonicalize t) (canonicalize u)
  | Neg t ->
     neg (canonicalize t)

exception Not_canonical

let rec to_cnf t =
  let get_atomic = function
    | [[b]] -> b
    | _ -> raise Not_canonical
  in
  let get_join = function
    | [bs] -> bs
    | _ -> raise Not_canonical
  in
  match t with
  | Var s ->
     [[Basic.Var s]]
  | Id ->
     [[Basic.Id]]
  | Comp (t, u) ->
     let t = get_atomic @@ to_cnf t in
     let u = get_atomic @@ to_cnf u in
     [[Basic.Comp (t, u)]]
  | Under _ | Over _ ->
     raise Not_canonical
  | Meet (t, u) ->
     to_cnf t @ to_cnf u
  | Join (t, u) ->
     let ts = get_join @@ to_cnf t in
     let us = get_join @@ to_cnf u in
     [ts @ us]
  | Neg t ->
     let t = get_atomic @@ to_cnf t in
     [[Basic.Neg t]]
