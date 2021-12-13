module type S = sig
  module V : Sigs.HashedOrderedType

  (** Variables. *)
  type var = V.t

  (** First-order terms. *)
  type term

  val int : int -> term

  val var : var -> term

  (** Propositions. *)
  type prop

  val ( = ) : term -> term -> prop

  val ( <= ) : term -> term -> prop

  val ( < ) : term -> term -> prop

  val ( && ) : prop -> prop -> prop

  val ( || ) : prop -> prop -> prop

  val ( ==> ) : prop -> prop -> prop

  val and_ : prop list -> prop

  val or_ : prop list -> prop

  val entails : prop list -> prop -> prop

  (** Problems are top-level objects in which formulae are asserted and
     variables are created. *)
  type problem

  (** Create a new problem. *)
  val make : unit -> problem

  (** Create a fresh variable in the given problem. *)
  val fresh : ?name:string -> problem -> var

  (** Assert a formula in a logical problem, i.e., assume that it ought to be
     satisfiable. *)
  val assert_ : problem -> prop -> unit

  (** Append a comment to a logical problem. *)
  val comment : problem -> string -> unit
end
