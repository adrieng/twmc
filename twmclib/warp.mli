module V : Symbol.S

(** Time-warp variables. *)
type id = V.t

(** Atomic time-warp terms. *)
type atom =
  | Var of id
  (** Time warp variables. *)
  | Top
  (** Highest time warp. *)
  | Bot
  (** Lowest time warp. *)
  | Star of atom * atom
  (** Composition, i.e., [Star (t, u)] maps [n] to [t (u n)]. *)

(** Relation on time-warps *)
type rel =
  | Le of atom * atom
  (** Lower or equal constraint. *)

(** Printing. *)
module Print : sig
  val id : id -> PPrint.document
  val atom : atom -> PPrint.document
  val rel : rel -> PPrint.document
end

module Infix : sig
  (** Creates a fresh variable with the given name. *)
  val v : string -> atom

  (** Synonymous to [Star]. *)
  val ( * ) : atom -> atom -> atom

  (** Synonymous to [Le]. *)
  val ( <= ) : atom -> atom -> rel
end
