module Make(L : Logic.S) : sig
  module S : module type of Sampler.Make(Warp.V)(L)

  type instance =
    {
      problem : L.query;
      (** Logical problem equivalent to some time-warp inequality. *)
      zero : L.var;
      (** First-order variable representing 0. *)
      omega : L.var;
      (** First-order variable representing omega. *)
      test : L.var;
      (** First-order variable at which the time warps are tested. *)
      samples : S.t;
      (** First-order variables at which warp variables have been evaluated. *)
    }

  (** [translate r] computes a SAT problem which is satisfiable iff the time
     warp relation [r] is {i not} valid. *)
  val translate : Warp.rel -> instance

  (** [counterexample inst model] returns a counterexample from a model [model]
     for [inst.problem]. *)
  val counterexample : instance -> (L.var -> int) -> Counterexample.t
end
