(* Checking style based on the lexical context *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module LexUnit = LexerLib.LexUnit

(* LIGO dependencies *)

module type TOKEN = Token.S

(* Functor *)

module Make (Token : TOKEN) :
 sig
   type error =
     Odd_lengthed_bytes
   | Missing_break

   type units = Token.t LexUnit.t list

   type message = string Region.reg

   type nonrec result = (units, units * message) result

   val filter :
     ?print_passes:Std.t ->
     add_warning:(Main_warnings.all -> unit) ->
     units ->
     result
 end
