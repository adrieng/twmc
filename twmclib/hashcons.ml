module type TermBuilder =
  functor (S : Sigs.Signature) -> Sigs.Term with module S = S

module Share(S : Sigs.Signature) = struct
  module M = struct
    (** The implementation below is inspired by Filliâtre and Conchon's
       "Type-Safe Modular Hash-Consing" (ML'06). *)
    type t =
      {
        body : t S.t;
        (** Body of the term, i.e., the term with its subterms. *)
        id : int;
        (** Unique identifier. *)
        hash : int;
        (** Hash of the term. *)
      }

    let rec compare t1 t2 =
      if t1 == t2 then 0
      else S.compare compare t1.body t2.body

    let equal t1 t2 =
      0 = compare t1 t2

    let hash t =
      t.hash

    let hash_fold state t =
      Ppx_hash_lib.Std.Hash.fold_int state t.hash
  end

  module S = S

  include M

  module HT = Hashtbl.Make(M)

  let gensym =
    let r = ref 0 in
    fun () -> let id = !r in incr r; id

  let ht = HT.create 100

  let make body =
    let hash =
      S.hash_fold hash_fold (Ppx_hash_lib.Std.Hash.alloc ()) body
      |> Ppx_hash_lib.Std.Hash.get_hash_value
    in
    let t = { body; hash; id = 0; } in
    try HT.find ht t
    with Not_found ->
      let t = { t with id = gensym (); } in
      HT.add ht t t;
      t

  let view t =
    t.body

  let rec pp ?(debug = false) term =
    let loop { body; id; hash; } =
      let open PPrint in
      let d = S.pp pp body in
      if debug
      then
        OCaml.record
          ""
          [
            "body", S.pp pp body;
            "id", !^ (string_of_int id);
            "hash", !^ (string_of_int hash);
          ]
      else
        d
    in
    loop term

  let equal t1 t2 =
    t1 == t2 || 0 = Stdlib.compare t1.id t2.id

  let compare t1 t2 =
    Stdlib.compare t1.id t2.id
end

module NoShare(S : Sigs.Signature) = struct
  module S = S

  type t = C of t S.t

  let rec compare (C x) (C y) =
    S.compare compare x y

  let equal t1 t2 =
    0 = compare t1 t2

  let rec pp ?debug (C x) = ignore debug; S.pp pp x

  let rec hash_fold state (C x) =
    S.hash_fold hash_fold state x

  let hash =
    let open Ppx_hash_lib.Std in
    let state = Hash.alloc () in
    fun x -> Hash.get_hash_value @@ hash_fold state x

  let make x = C x

  let view (C x) = x
end

module Default = Share
