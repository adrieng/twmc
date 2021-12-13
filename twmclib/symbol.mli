module type S = sig
  (** An abstract type of symbols. *)
  type t

  (** Symbols support constant-time comparison. *)
  val compare : t -> t -> int

  (** Symbols support constant-time equality testing. *)
  val equal : t -> t -> bool

  (** Symbols support constant-time hashing. *)
  val hash : t Sigs.hasher

  (** For compatibility with ppx_hash. *)
  val hash_fold_t : t Sigs.hashfolder

  (** Symbols can be printed to strings in an injective (w.r.t. [equal])
     way. *)
  val to_string : t -> string

  (** [fresh ?name] generates a fresh new symbol. This symbol is guaranteed to
     be distinct from all the previous ones, and [name] is only used for
     printing purposes. *)
  val fresh : ?name:string -> unit -> t
end

(** Create a new symbol module. *)
module Make() : S
