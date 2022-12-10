open Term

(** {2 Elimination of binary residuals} *)

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
     Neg (Comp (u, Neg t))
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

(** {2 Conversion to basic form} *)

let invert_pol = function
  | `CNF -> `DNF
  | `DNF -> `CNF


let rec comp pol t u =
  match t, u with
  | Meet (t1, t2), _ ->
     meet pol (comp pol t1 u) (comp pol t2 u)
  | _, Meet (u1, u2) ->
     meet pol (comp pol t u1) (comp pol t u2)
  | Join (t1, t2), _ ->
     join pol (comp pol t1 u) (comp pol t2 u)
  | _, Join (u1, u2) ->
     join pol (comp pol t u1) (comp pol t u2)
  | _ ->
     Comp (t, u)

and meet pol t u =
  match pol with
  | `CNF ->
     Meet (t, u)
  | `DNF ->
     begin match t, u with
     | Join (t1, t2), u ->
        Join (meet pol t1 u, meet pol t2 u)
     | t, Join (u1, u2) ->
        Join (meet pol t u1, meet pol t u2)
     | _ ->
        Meet (t, u)
     end

and join pol t u =
  match pol with
  | `DNF ->
     Join (t, u)
  | `CNF ->
     begin match t, u with
     | Meet (t1, t2), u ->
        Meet (join pol t1 u, join pol t2 u)
     | t, Meet (u1, u2) ->
        Meet (join pol t u1, join pol t u2)
     | _ ->
        Join (t, u)
     end

(* [neg pol t] assumes that [t] is canonical for the polarity [pol]. *)
and neg pol t =
  match t with
  | Meet (t, u) ->
     join pol (neg pol t) (neg pol u)
  | Join (t, u) ->
     meet pol (neg pol t) (neg pol u)
  | Neg t ->
     t
  | _ ->
     Neg t
;;

exception Not_residual_simple

let rec canonicalize pol t =
  match t with
  | Var _ | Id ->
     t
  | Comp (t, u) ->
     comp pol (canonicalize pol t) (canonicalize pol u)
  | Meet (t, u) ->
     meet pol (canonicalize pol t) (canonicalize pol u)
  | Join (t, u) ->
     join pol (canonicalize pol t) (canonicalize pol u)
  | Neg t ->
     neg pol (canonicalize (invert_pol pol) t)
  | Over _ | Under _ ->
     raise Not_residual_simple

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
