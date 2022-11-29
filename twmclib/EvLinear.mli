(** {1 Eventually-linear time warps} *)

(** The type of eventually linear time warps. *)
type t

(** [make u s] creates a compact time warp whose first values grow by [u] and
    which grows by increments of [s] after [u]. *)
val make : int array -> Enat.t -> t

(** [of_points ~last points] creates a compact time warp whose values are
    determined by [points] and whose last value. *)
val of_points : last:Enat.t -> (Enat.t * Enat.t) list -> t

(** [eval p i] returns [p(i)]. *)
val eval : t -> Enat.t -> Enat.t

(** Pretty-print a compact time warp. *)
val pp : t -> PPrint.document

(** Test compact time warps for equality. *)
val equal : t -> t -> bool

(** Test compact time warps for the pointwise ordering. *)
val ( <= ) : t -> t -> bool
