module type HashedOrderedType = sig
  type t
  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t
end

module String = struct
  type t = string
  let hash (x : t) = Hashtbl.hash x
  let compare (x : t) (y : t) = Stdlib.compare x y
  let equal (x : t) (y : t) = x = y
end

type 'a cmp = 'a -> 'a -> int

type 'a hash = 'a -> int

module type Signature = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val compare : 'a cmp -> 'a t cmp
  val hash : 'a hash -> 'a t hash
end
