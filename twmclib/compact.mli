(** {1 Compact time warps} *)

(** A time warp is compact when it is eventually constant. *)
type t

(** [make u] creates a compact time warp whose first values are [u]. *)
val make : int array -> t

(** [eval p i] returns [p(i)]. *)
val eval : t -> int -> int

(** Pretty-print a compact time warp. *)
val print : t -> PPrint.document

(** Test compact time warps for equality. *)
val equal : t -> t -> bool

(** A counterexample to a relation [r]. *)
type counterexample =
  {
    valuation : (Warp.id * t) list;
    (** The valuation assigns to every variable in [r] a compact term.  *)
    point : int;
    (** The time step at which the relation [r] fails when its variables are
       specified as per the valuation. *)
  }

(** Display a counterexample. *)
val print_counterexample : counterexample -> PPrint.document

(** Test counterexamples for equality. *)
val equal_counterexample : counterexample -> counterexample -> bool
