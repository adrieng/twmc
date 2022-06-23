(** {1 Rational time warps} *)

(** A time warp [p] is rational when it can be defined as the running sum of
    some eventually periodic sequence of extended numbers. *)

type extremal =
  | Zero
  | Omega

type period =
  | Ext of extremal
  | Pat of Word.t

type t =
  private
    {
      u : Word.t;
      v : period;
    }

include Warp_sig.S with type t := t

val extremal : ?prefix:Word.t -> extremal -> t

val pattern : ?prefix:Word.t -> ppattern:Word.t -> unit -> t

val weight : t -> Enat.t
