(** {1 Problems} *)

type t =
  | Le of Term.t * Term.t
  (** Ordering problems. *)
  | Eq of Term.t * Term.t
  (** Equality problems. *)

(** {2 Pretty-printing} *)

val pp : t Print.printer

(** {2 Solvers} *)

type ('logic_query, 'result) solver =
  ?on_initial_positive_term:(Term.t -> unit) ->
  ?on_residual_simple_term:(Term.t -> unit) ->
  ?on_canonical_term:(Term.t -> unit) ->
  ?on_basic_positive_terms:(Basic.t list -> unit) ->
  ?on_saturated_sample_set:(Sampleset.t -> unit) ->
  ?on_logic_query:(pp:('logic_query -> PPrint.document)
                   -> 'logic_query -> unit) ->
  t ->
  'result

val to_logic :
  (module Logic.S with type query = 'a and type V.t = 'b) ->
  ('a, (('a * ('b Logic.valuation -> Counterexample.t)) list)) solver

val to_solution :
  (Backends.Z3.query, [`Valid | `Invalid of Counterexample.t]) solver
