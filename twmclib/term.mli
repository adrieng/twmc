module V : Symbol.S

(** General time-warp terms. *)
type t =
  | Var of V.t
  (** Time warp variables. *)
  | Id
  (** Identity time warp. *)
  | Comp of t * t
  (** Composition, i.e., [Comp (t, u)] maps [n] to [t (u n)]. *)
  | Over of t * t
  (** First residual, i.e., [Over (t, u)] is [t]/[u]. *)
  | Under of t * t
  (** Second residual, i.e., [Under (t, u)] is [t]\[u]. *)
  | Meet of t * t
  (** Lattice-theoretical operator (lower bound). *)
  | Join of t * t
  (** Lattice-theoretical operator (upper bound). *)
  | Neg of t
  (** Negation of t is [t]\predecessor. *)

(** {2 Infrastructure} *)

include Sigs.HashedOrderedType with type t := t

val hash_fold_t : t Sigs.hashfolder

(** {2 Printing} *)

include Print.PrintableType with type t := t
val pp_var : V.t Print.printer
