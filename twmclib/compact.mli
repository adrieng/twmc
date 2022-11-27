(** {1 Compact time warps} *)

(** Elements of the second infinite ordinal. *)
type enat =
  | Fin of int
  | Omega

(** A time warp is compact when it is eventually constant. *)
type t

(** [make u] creates a compact time warp whose first values are [u]. *)
val make : int array -> enat -> t

(** [of_points ~last points] creates a compact time warp whose values are
    determined by [points] and whose last value. *)
val of_points : last:enat -> (int * int) list -> t

(** [eval p i] returns [p(i)]. *)
val eval : t -> enat -> enat

(** Pretty-print a compact time warp. *)
val pp : t -> PPrint.document

(** Test compact time warps for equality. *)
val equal : t -> t -> bool

(** Test compact time warps for the pointwise ordering. *)
val ( <= ) : t -> t -> bool
