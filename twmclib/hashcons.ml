module Term(S : Sigs.Signature) = struct
  module M = struct
    type t =
      {
        body : t S.t;
        (** Body of the term, i.e., the term with its subterms. *)
        id : int;
        (** Unique identifier. *)
        hash : int;
        (** Hash of the term. *)
      }

    let equal t1 t2 =
      t1 == t2

    let compare t1 t2 =
      if t1 == t2 then 0 else Stdlib.compare t1.id t2.id

    let hash t =
      t.hash
  end

  include M

  module HT = Hashtbl.Make(M)

  let gensym =
    let r = ref 0 in
    fun () -> let id = !r in incr r; id

  let ht = HT.create 100

  let make body =
    let t = { body; hash = S.hash hash body; id = 0; } in
    try HT.find ht t
    with Not_found ->
      let t = { t with id = gensym (); } in
      HT.add ht t t;
      t

  let view t =
    t.body
end
