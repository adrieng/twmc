module V = Symbol.Make()

type var = V.t

(** General time-warp terms. *)
type t =
  | Var of (V.t [@compare V.compare] [@equal V.equal])
  | Id
  | Comp of t * t
  | Over of t * t
  | Under of t * t
  | Meet of t * t
  | Join of t * t
  | Neg of t
  [@@deriving eq, ord, hash]

let pp_var v =
  PPrint.string @@ V.to_string v

let rec pp t =
  let open PPrint in
  match t with
  | Var _ | Id | Neg _ ->
     pp_simple t
  | Comp (t, u) ->
     group @@ pp_simple t ^//^ pp u
  | Over (t, u) ->
     parens @@ infix 2 1 (!^ "/") (pp t) (pp u)
  | Under (t, u) ->
     parens @@ infix 2 1 (!^ "\\") (pp t) (pp u)
  | Meet (t, u) ->
     parens @@ infix 2 1 (!^ "/\\") (pp t) (pp u)
  | Join (t, u) ->
     parens @@ infix 2 1 (!^ "\\/") (pp t) (pp u)

and pp_simple t =
  let open PPrint in
  match t with
  | Var x ->
     pp_var x
  | Id ->
     !^ "id"
  | Neg t ->
     pp_simple t ^^ !^ "'"
  | t ->
     parens (pp t)
