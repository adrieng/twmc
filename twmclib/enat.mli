(** {1 Extended natural numbers} *)

(** An "extended" natural number is an element of ω+1. *)
type t =
  | Inf
  | Fin of int

include Sigs.HashedOrderedType with type t := t
include Print.FormattableType with type t := t

val zero : t

val one : t

val succ : t -> t

val ( <= ) : t -> t -> bool

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( * ) : t -> t -> t

val ( / ) : t -> t -> t

val min : t -> t -> t

val max : t -> t -> t

val of_int : int -> t

exception Too_big

val to_int : t -> int

val print_utf8 : bool ref
