module V = Symbol.Make()

module Sig = struct
  type 'a t =
    | SVar of (V.t [@compare V.compare] [@equal V.equal])
    | SEval of Basic.t * 'a
    | SSucc of 'a
    | SLast of Basic.t
    [@@deriving ord, hash]

  let hash_fold = hash_fold_t

  let map f a =
    match a with
    | SVar v ->
       SVar v
    | SEval (t, a) ->
       SEval (t, f a)
    | SSucc a ->
       SSucc (f a)
    | SLast t ->
       SLast t

  let pp pp_term a =
    let open PPrint in
    match a with
    | SVar v ->
       !^ ("'k" ^ V.to_string v)
    | SEval (t, a) ->
       group (Basic.pp t ^//^ brackets (pp_term a))
    | SSucc a ->
       group (!^ "succ" ^//^ pp_term a)
    | SLast t ->
       group (!^ "last" ^//^ Basic.pp t)
end

type 'a sign = 'a Sig.t =
  | SVar of V.t
  | SEval of Basic.t * 'a
  | SSucc of 'a
  | SLast of Basic.t

module T = Hashcons.Default(Sig)

include T

let var v = make (SVar v)
let eval t a = make (SEval (t, a))
let succ a = make (SSucc a)
let last t = make (SLast t)

module Set = Set.Make(T)
module Map = Map.Make(T)
module Table = Hashtbl.Make(T)
