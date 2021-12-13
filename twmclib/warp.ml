module V = Symbol.Make()

type id = V.t

type atom =
  | Var of id
  | Top
  | Bot
  | Star of atom * atom

type rel =
  | Le of atom * atom

module Print = struct
  open PPrint

  let id x =
    !^ (V.to_string x)

  let rec atom t =
    let open PPrint in
    match t with
    | Var x ->
       id x
    | Top ->
       !^ "top"
    | Bot ->
       !^ "bot"
    | Star (t, u) ->
       infix 2 1 (!^ "*") (atom t) (atom u)

  let rel r =
    let batom t = group @@ atom t in
    match r with
    | Le (t, u) ->
       infix 2 1 (!^ "<=") (batom t) (batom u)
end

module Infix = struct
  let v name = Var (V.fresh ~name ())
  let ( * ) t u = Star (t, u)
  let ( <= ) t u = Le (t, u)
end
