(** The type of diagrams, i.e., finite counterexamples to the validity of an
    (in)equality over time warps. *)
type t

(** Pretty-printing with {! PPrint}. *)
val pp : t -> PPrint.document

(** Returns the sample set over which the diagram is defined. *)
val support : t -> Sampleset.t

(** [eval d s] evaluates the diagram [d] at a sample [s]. Raises {!
    Invalid_argument} if this sample does not belong to [support d]. *)
val eval : t -> Sample.t -> Enat.t

(** Extract a counterexample expressed in time warp terms from a diagram. *)
val counterexample : t -> Counterexample.t

(** The type of existence statements for diagrams, which are essentially SMT
    formulas whose satisfiability entail the existence of a diagram for the
    initial problem. The type parameters ['query] and ['var] stand for the
    corresponding types in the SMT backend. *)
type ('query, 'var) existence_statement

(** [query_of_existence_statement stm] extracts the logical query underlying the
    existence statement [stm]. *)
val query_of_existence_statement : ('query, 'var) existence_statement -> 'query

(** [statement_of_problem (module L) ~sample_set ~root_variable
    ~basic_terms_under_test] generates a logic formula expressing the existence
    of a diagram over [sample_set] sending [Sample.eval t root_variable]
    strictly below [root_variable] for each basic term [t] in
    [basic_terms_under_test]. *)
val statement_of_basic_conjunctive_problem :
  (module Logic.S with type query = 'query and type V.t = 'var) ->
  sample_set:Sampleset.t ->
  root_variable:Sample.V.t ->
  basic_terms_under_test:Basic.t list ->
  ('query, 'var) existence_statement

(** [exists ~solve stm] checks whether the statement [stm] is satisfiable using
    the oracle [solve], and returns some corresponding diagram when it is the
    case. *)
val exists :
  solve:('query -> [`Sat of 'var -> int | `Unsat]) ->
  ('query, 'var) existence_statement ->
  t option
