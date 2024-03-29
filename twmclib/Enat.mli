(** An extended number can be either a natural number up to 2^{W-2}-1, where W
    denotes the size of a machine word, or omega, a number larger than all the
    other ones. *)
type t

val view : t -> [`Omega | `Fin of int]

(** Display an extended number as a string. *)
val to_string : t -> string

(** Pretty-print an extended number. *)
val pp : t Print.fmt

val print : t Print.printer

(** [of_int n] raises {! Invalid_argument} if [n] is negative. *)
val of_int : int -> t

(** [to_int n] raises {! Invalid_argument} if [n] is not finite. *)
val to_int : t -> int

(** A number larger than all naturals. *)
val omega : t

(** [is_omega x] is equivalent to [view x = `Omega] *)
val is_omega : t -> bool

(** Comparison operator. *)
val ( <= ) : t -> t -> bool

(** Strict comparison operator. *)
val ( < ) : t -> t -> bool

(** Comparison function in the sense of {! Stdlib.compare}, consistent with
    the ordering defined above. *)
val compare : t -> t -> int

(** The smallest natural. *)
val zero : t

(** Least upper bound. *)
val max : t -> t -> t

(** Greatest lower bound. *)
val min : t -> t -> t

val succ : t -> t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( * ) : t -> t -> t

(** Use at your own peril.  *)
val raw_of_int : int -> t
