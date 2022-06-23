module type HashedOrderedType = sig
  type t
  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t
end

module String = struct
  type t = string
  let hash (x : t) =
    Hashtbl.hash x
  let hash_fold_t state s =
    Ppx_hash_lib.Std.Hash.fold_string state s
  let compare (x : t) (y : t) =
    Stdlib.compare x y
  let equal (x : t) (y : t) =
    x = y
end

type 'a hasher = 'a -> int

type 'a hashfolder =
  Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state

type 'a cmp = 'a -> 'a -> int

module Compare = struct
  let andthen : 'a cmp -> int Lazy.t -> 'a cmp =
    fun cmp res x y -> let r = cmp x y in
                       if r = 0 then Lazy.force res else r

  let int : int cmp = Stdlib.compare

  let pair : 'a cmp -> 'b cmp -> ('a * 'b) cmp =
    fun cmp1 cmp2 (x1, y1) (x2, y2) ->
    let r = cmp1 x1 x2 in
    if r <> 0 then r else cmp2 y1 y2
end

module type Signature = sig
  type 'a t
  val pp : ('a -> PPrint.document) -> 'a t -> PPrint.document
  val map : ('a -> 'b) -> 'a t -> 'b t
  val compare : 'a cmp -> 'a t cmp
  val hash_fold : 'a hashfolder -> 'a t hashfolder
end

module Int = struct
  let mod_b1 x y = succ (pred x mod y)

  let div_b1 x y = pred x / y

  let rec gcd a b = if b = 0 then a else gcd b (a mod b)

  let lcm a b = (a * b) / gcd a b
end
