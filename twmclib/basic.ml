type t =
  | Var of (Term.V.t [@compare Term.V.compare] [@equal Term.V.equal])
  | Id
  | Comp of t * t
  | Neg of t
  [@@deriving eq, ord, hash]

let rec pp t =
  let open PPrint in
  match t with
  | Var x ->
     Term.pp_var x
  | Id ->
     !^ "id"
  | Comp (t, u) ->
     group @@ pp t ^//^ pp u
  | Neg t ->
     begin match t with
     | Comp _ -> parens (pp t)
     | _ -> pp t
     end ^^ !^ "'"

module Infix = struct
  let v name = Var (Term.V.fresh ~name ())
  let ( * ) t u = Comp (t, u)
end

type _elt = t
module M = struct type t = _elt let compare = compare end
module Set = Set.Make(M)
module Map = Map.Make(M)
