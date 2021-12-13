val translate :
  (module Logic.S with type query = 'a) ->
  Sampleset.t ->
  Sample.V.t ->
  Basic.t list ->
  'a
