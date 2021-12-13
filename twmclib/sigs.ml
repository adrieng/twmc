module type HashedOrderedType = sig
  type t
  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t
end

type 'a printer = 'a -> PPrint.document

module type PrintableType = sig
  type t
  val pp : t printer
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

type 'a cmp = 'a -> 'a -> int

type 'a hasher = 'a -> int

type 'a hashfolder =
  Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state

module type Signature = sig
  type 'a t
  val pp : ('a -> PPrint.document) -> 'a t -> PPrint.document
  val map : ('a -> 'b) -> 'a t -> 'b t
  val compare : 'a cmp -> 'a t cmp
  val hash_fold : 'a hashfolder -> 'a t hashfolder
end
