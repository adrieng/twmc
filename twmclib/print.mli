(** The global variable [utf8_output] controls whether UTF-8 characters should
    be emitted by some of the functions below. *)
val utf8_output : bool ref

(** The type of pretty-printers. *)
type 'a fmt = Format.formatter -> 'a -> unit

module Fmt : sig
  val string_of : 'a fmt -> 'a -> string

  val pp_nothing : 'a fmt

  val pp_space : unit fmt

  val pp_break : unit fmt

  val pp_breakable_space : unit fmt

  val pp_strong_break : unit fmt

  val pp_comma : unit fmt

  val pp_semicolon : unit fmt

  val pp_times : unit fmt

  val pp_arrow : unit fmt

  val pp_thick_arrow : unit fmt

  val pp_circledast : unit fmt

  val pp_lambda : unit fmt

  val pp_omega : unit fmt

  val pp_bool : bool fmt

  val pp_int : int fmt

  val pp_string : string fmt

  val pp_pair :
    ?pp_sep:unit fmt ->
    'a fmt ->
    'b fmt ->
    ('a * 'b) fmt

  val pp_opt :
    ?pp_left:unit fmt ->
    ?pp_right:unit fmt ->
    'a fmt ->
    'a option fmt

  val pp_list :
    ?pp_left:unit fmt ->
    ?pp_right:unit fmt ->
    ?pp_sep:unit fmt ->
    'a fmt ->
    'a list fmt

  val pp_array :
    ?pp_left:unit fmt ->
    ?pp_right:unit fmt ->
    ?pp_sep:unit fmt ->
    'a fmt ->
    'a array fmt

  val pp_thunk :
    unit fmt fmt
end

module type FormattableType = sig
  type t
  val fmt : t fmt
end

type 'a printer = 'a -> PPrint.document

module PPrint : sig
  type doc = PPrint.document
  val print : ?oc:out_channel -> doc -> unit
  val to_string : doc -> string
  val fmt : doc fmt
  val to_fmt : 'a printer -> 'a fmt
end

module type PrintableType = sig
  type t
  val pp : t printer
end
