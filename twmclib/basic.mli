(** {1 Basic Terms} *)

(** Basic terms are time warp terms in a normal form. Mainly, residuals have
   been specialized to single-sided cases. *)
type t =
  | Var of Term.V.t
  (** Time warp variables. *)
  | Id
  (** Identity time warp. *)
  | Top
  (** Highest time warp. *)
  | Bot
  (** Lowest time warp. *)
  | Comp of t * t
  (** Composition, i.e., [Star (t, u)] maps [n] to [t (u n)]. *)
  | RedO of t
  (** O-residual, i.e., [RedO t] is top \ [t]. *)
  | RedR of t
  (** R-residual, i.e., [RedR t] is [t] \ id. *)
  | RedL of t
  (** L-residual, i.e., [RedL t] is id / [t]. *)

include Sigs.HashedOrderedType with type t := t

val hash_fold_t : t Sigs.hashfolder

(** Printing. *)

include Print.PrintableType with type t := t

module Infix : sig
  (** Creates a fresh variable with the given name. *)
  val v : string -> t

  (** Synonymous to [Star]. *)
  val ( * ) : t -> t -> t
end

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
