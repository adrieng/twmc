(** {1 Normalization of time warp terms} *)

(** [eliminate_binary_residuals t] returns a term equivalent to [t] that no
   longer contains the binary residuals [Over] and [Under]. *)
val eliminate_binary_residuals : Term.t -> Term.t

(** [canonicalize pol t] puts the input term in canonical form, with [pol]
    indicating whether this canonical form should be DNF or CNF. *)
val canonicalize : [`DNF | `CNF] -> Term.t -> Term.t

exception Not_canonical

(** [to_cnf t] returns a finite meet of finite join of basic terms. May raise {!
    Not_canonical} if [t] is not in canonical form. *)
val to_cnf : Term.t -> Basic.t list list

(** [simplify t] rewrites [t] to an equivalent term [u] using basic algebraic
    laws (neutrality of Id, idempotence of neg). *)
val simplify : Basic.t -> Basic.t
