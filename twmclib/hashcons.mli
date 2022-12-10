(** This module implements first-order terms with hash-consing, i.e., provides a
    representation of first-order terms guaranteeing that structurally equal
    terms are physically equal.

    Deviating from universal algebra, we consider that variables are part of the
    signature itself. This simplifies the implementation since we do not deal
    with substitution at the moment. *)

module type TermBuilder =
  functor (S : Sigs.Signature) -> Sigs.Term with module S = S

module Share : TermBuilder
module NoShare : TermBuilder
module Default : TermBuilder
