(** {1 Normalization of time warp terms} *)

(** [eliminate_binary_residuals t] returns a term equivalent to [t] that no
   longer contains the binary residuals [Over] and [Under]. *)
val eliminate_binary_residuals : Term.t -> Term.t

(** [canonicalize t] puts the input term in canonical form. *)
val canonicalize : Term.t -> Term.t

exception Not_canonical

(** [to_basic t] returns a finite meet of finite join of basic terms. May raise
   {! Not_canonical} if [t] is not in canonical form. *)
val to_basic : Term.t -> Basic.t list list
