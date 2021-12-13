(** This module implements first-order terms with hashconsing, i.e., provides a
   representation of first-order terms guaranteeing that structurally equal
   terms are physically equal.

   Deviating from universal algebra, we consider that variables are part of the
   signature itself. This simplifies the implementation since we do not deal
   with substitution at the moment. *)
module Term(S : Sigs.Signature) : sig
  (** The type of terms is abstract, making sure that a client can only build
     values of this type using functions from this module. *)
  type t

  (** The comparison functions ([compare], [equal], [hash]) pertaining to {!
     Sigs.HashedOrderedType} run in constant time. *)
  include Sigs.HashedOrderedType with type t := t

  (** [make t] turns an operator applied to a bunch of term into a term. *)
  val make : t S.t -> t

  (** [view t] exposes the body of a term, i.e., its internal representation. *)
  val view : t -> t S.t
end
