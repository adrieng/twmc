module type S =
  sig
    (** Warps *)
    type t

    include Map.OrderedType with type t := t
    include Print.FormattableType with type t := t

    val equal : t -> t -> bool

    (** Evaluation of a warp at a point. *)
    val eval : t -> Enat.t -> Enat.t

    (** Warp composition *)
    val on : t -> t -> t

    (** Warp division *)
    val div : t -> t -> t

    (** Precedence test *)
    val ( <= ) : t -> t -> bool

    (** The warp represented by (0) *)
    val zero : t

    (** The warp represented by (1) *)
    val one : t

    (** The warp represented by (\omega) *)
    val omega : t

    (** The warp represented by 0(1) *)
    val zero_one : t

    (** Assuming [p <= q], [size p q] returns the buffer capacity needed to
        delay [q] to [p]. We assume that instantenous communication takes no
        space, e.g. [size one one] returns zero. *)
    val size : t -> t -> Enat.t
  end
