val translate :
  (module Logic.S with type query = 'a and type V.t = 'b) ->
  Sampleset.t ->
  Sample.V.t ->
  Basic.t list ->
  'a * ('b Logic.valuation -> Counterexample.t)
