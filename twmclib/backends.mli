module SMTLIB : sig
  include Logic.S

  val to_channel : out_channel -> query -> unit
end

module Z3 : sig
  include Logic.S

  val flush : unit -> unit

  val solve : query -> Z3.Solver.status

  val model : query -> (var -> int) option
end
