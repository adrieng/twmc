module V = Symbol.Make()

type var = V.t

(** General time-warp terms. *)
type t =
  | Var of (V.t [@compare V.compare] [@equal V.equal])
  | Id
  | Top
  | Bot
  | Comp of t * t
  | Over of t * t
  | Under of t * t
  | RedO of t
  | RedR of t
  | RedL of t
  | Meet of t * t
  | Join of t * t
  [@@deriving eq, ord, hash]

let pp_var v =
  PPrint.string @@ V.to_string v

let rec pp t =
  let open PPrint in
  let suff t k = pp t ^^ !^ ("^" ^ k) in
  match t with
  | Var x ->
     pp_var x
  | Id ->
     !^ "id"
  | Top ->
     !^ "top"
  | Bot ->
     !^ "bot"
  | Comp (t, u) ->
     parens @@ group @@ pp t ^//^ pp u
  | Over (t, u) ->
     parens @@ infix 2 1 (!^ "/") (pp t) (pp u)
  | Under (t, u) ->
     parens @@ infix 2 1 (!^ "\\") (pp t) (pp u)
  | RedO t ->
     suff t "o"
  | RedR t ->
     suff t "r"
  | RedL t ->
     suff t "l"
  | Meet (t, u) ->
     parens @@ infix 2 1 (!^ "/\\") (pp t) (pp u)
  | Join (t, u) ->
     parens @@ infix 2 1 (!^ "\\/") (pp t) (pp u)
