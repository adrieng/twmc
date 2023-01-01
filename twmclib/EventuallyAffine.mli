(** {1 Eventually-linear Time Warps} *)

(** The type of eventually linear time warps. *)
type t

(** [extend points] computes an eventually linear time warp which strongly
    extends the list of points [points]. This list should contain at least one
    pair of the form [(Enat.omega, y)] for some [y], otherwise [of_points] will
    raise {! Invalid_argument}. *)
val extend : (Enat.t * Enat.t) list -> t

(** [eval p i] returns [p(i)]. *)
val eval : t -> Enat.t -> Enat.t

(** Pretty-print a compact time warp. *)
val print : t -> PPrint.document

(** Test compact time warps for equality. *)
val equal : t -> t -> bool
