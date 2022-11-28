(** A module for the quantifier-free theory of integers with addition, plus
    added administrative functions to interact with e.g. solvers. *)
module type S = sig
  module V : Sigs.HashedOrderedType

  (** Variables. *)
  type var = V.t

  (** First-order terms. *)
  type term

  val lit : int -> term

  val var : var -> term

  val add : term -> term -> term

  val ( + ) : term -> term -> term

  (** First-order formulae. *)
  type prop

  val ( = ) : term -> term -> prop

  val ( <= ) : term -> term -> prop

  val ( < ) : term -> term -> prop

  val ( && ) : prop -> prop -> prop

  val ( || ) : prop -> prop -> prop

  val ( ==> ) : prop -> prop -> prop

  val true_ : prop

  val false_ : prop

  val not_ : prop -> prop

  val and_ : prop list -> prop

  val or_ : prop list -> prop

  val entails : prop list -> prop -> prop

  (** Queries are top-level objects in which formulae are asserted and variables
      are created. *)
  type query

  (** Pretty-print a query. *)
  val pp : query -> PPrint.document

  (** Create a new query. *)
  val make : unit -> query

  (** Create a fresh variable in the given query. *)
  val fresh : ?comment:string -> ?name:string -> query -> var

  (** Assert a formula in a logical query, i.e., assume that it ought to be
      satisfiable. *)
  val assert_ : query -> prop -> unit

  (** Append a comment to a logical query. *)
  val comment : query -> string -> unit
end

type 'a valuation = 'a -> Enat.t
