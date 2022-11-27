module V : Symbol.S

type t

(** {2 Infrastructure} *)

include Sigs.HashedOrderedType with type t := t

val equal : t -> t -> bool

val pp : ?debug:bool -> t -> PPrint.document

(** {2 Construction/destruction} *)

type 'a sign =
  | SVar of V.t
  | SEval of Basic.t * 'a
  | SSucc of 'a
  | SLast of Basic.t

val view : t -> t sign

val make : t sign -> t

(** {2 Smart constructors} *)

(** All the following functions can be derived from [make]. *)

val var : V.t -> t

val eval : Basic.t -> t -> t

val succ : t -> t

val last : Basic.t -> t

(** {2 Sets and Maps} *)

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
module Table : Hashtbl.S with type key = t
