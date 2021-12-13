(** {1 Counterexamples} *)

(** A counterexample to a relation [r]. *)
type t =
  {
    valuation : (Warp.id * Compact.t) list;
    (** The valuation assigns to every variable in [r] a compact term.  *)
    point : int;
    (** The time step at which the relation [r] fails when its variables are
       specified as per the valuation. *)
  }

(** Display a counterexample. *)
val print : t -> PPrint.document

(** Test counterexamples for equality. *)
val equal : t -> t -> bool

(** A trivial counterexample. *)
val dummy : t
