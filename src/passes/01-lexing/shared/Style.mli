(* Checking style based on the lexical context *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Unit   = LexerLib.Unit

(* LIGO dependencies *)

module type TOKEN = Token.S

(* Functor *)

module Make (Token : TOKEN) :
 sig
   type error =
     Odd_lengthed_bytes
   | Missing_break

   type units = Token.t Unit.t list

   type message = string Region.reg

   type result = (units, units * message) Stdlib.result

   val filter :
     ?print_passes:Std.t ->
     add_warning:(Main_warnings.all -> unit) ->
     units ->
     result
 end
