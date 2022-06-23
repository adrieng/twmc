(** {1 Compact time warps} *)

(** A time warp is compact when it is eventually constant. *)
type t

(** [make u] creates a compact time warp whose first values are [u]. *)
val make : int array -> t

(** [eval p i] returns [p(i)]. *)
val eval : t -> int -> int

(** Pretty-print a compact time warp. *)
val pp : t -> PPrint.document

(** Test compact time warps for equality. *)
val equal : t -> t -> bool

(** Test compact time warps for the pointwise ordering. *)
val ( <= ) : t -> t -> bool
