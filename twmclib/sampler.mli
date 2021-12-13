module Make(X : Sigs.HashedOrderedType)(L : Logic.S) : sig
  type t

  val make : L.problem -> t

  val fresh_point : ?name:string -> t -> L.V.t

  val sample : t -> X.t -> L.V.t -> L.V.t

  val iter_samples : (X.t -> L.V.t -> L.V.t -> unit) -> t -> unit

  val iter_sample_pairs :
    (X.t -> L.V.t -> L.V.t -> L.V.t -> L.V.t -> unit) -> t -> unit

  val iter_points : (L.V.t -> unit) -> t -> unit

  val fold_samples : (X.t -> L.V.t -> L.V.t -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_points : (L.V.t -> 'a -> 'a) -> t -> 'a -> 'a
end
