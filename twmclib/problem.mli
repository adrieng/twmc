(** {1 Problems} *)

type t =
  | Le of Term.t * Term.t
  (** Ordering problems. *)
  | Eq of Term.t * Term.t
  (** Equality problems. *)

(** {2 Pretty-printing} *)

val pp : t Print.printer

(** {2 Core Decision Procedure} *)

val existence_statements :
  (module Logic.S with type query = 'a and type V.t = 'b) ->
  ?on_initial_positive_term:(Term.t -> unit) ->
  ?on_residual_simple_term:(Term.t -> unit) ->
  ?on_canonical_term:(Term.t -> unit) ->
  ?on_basic_positive_terms:(Basic.t list -> unit) ->
  ?on_simplified_basic_positive_terms:(Basic.t list -> unit) ->
  ?on_saturated_sample_set:(Sampleset.t -> unit) ->
  t ->
  ('a, 'b) Diagram.existence_statement list

module Solution : sig
  type t
  val valid : t
  val invalid : t
  val pp : t -> PPrint.document
  val equal : t -> t -> bool
end

val solve_with_z3 :
  ?on_diagram:(Diagram.t -> unit) ->
  (Backends.Z3.query, Backends.Z3.var) Diagram.existence_statement list ->
  Solution.t
