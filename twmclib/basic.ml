type t =
  | Var of (Term.V.t [@compare Term.V.compare] [@equal Term.V.equal])
  | Id
  | Top
  | Bot
  | Comp of t * t
  | RedO of t
  | RedR of t
  | RedL of t
  [@@deriving eq, ord, hash]

let rec pp t =
  let open PPrint in
  let suff t k = pp t ^^ !^ ("^" ^ k) in
  match t with
  | Var x ->
     Term.pp_var x
  | Id ->
     !^ "id"
  | Top ->
     !^ "top"
  | Bot ->
     !^ "bot"
  | Comp (t, u) ->
     parens @@ group @@ pp t ^//^ pp u
  | RedO t ->
     suff t "o"
  | RedR t ->
     suff t "r"
  | RedL t ->
     suff t "l"

module Infix = struct
  let v name = Var (Term.V.fresh ~name ())
  let ( * ) t u = Comp (t, u)
end

type _elt = t
module M = struct type t = _elt let compare = compare end
module Set = Set.Make(M)
module Map = Map.Make(M)
