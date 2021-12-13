(** {1 Result} *)

(** This module provides a high-level interface to the decision procedure. In
   particular, it takes care of calling the SMT solver for the user, and
   building counterexamples. *)

(** The type [t] represents the result of a call to our decision procedure on a
   problem, that is, on a relation of the form [u <= v] where. *)
type t =
  | Valid (** The relation holds in all cases. *)
  | Invalid of Counterexample.t
  (** The relation does not hold in some cases, including the specified one. *)
  | Internal_error of string (** Something went wrong. *)

(** Display a result. *)
val print : t -> PPrint.document

(** Test two results for equality, taking counterexamples into account or
   not depending on the optional argument. *)
val equal : ?consider_counterexamples:bool -> t -> t -> bool

(** Decide a relation. *)
val solve : Warp.rel -> t
