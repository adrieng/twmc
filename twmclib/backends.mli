module SMTLIB : sig
  include Logic.S

  val to_channel : out_channel -> problem -> unit
end

module Z3 : sig
  include Logic.S

  val flush : unit -> unit

  val solve : problem -> Z3.Solver.status

  val model : problem -> (var -> int) option
end
