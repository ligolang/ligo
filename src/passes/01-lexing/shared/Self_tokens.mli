(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Trace  = Simple_utils.Trace

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter : add_warning:(Main_warnings.all -> unit) ->
      (lex_unit list, message) result -> (token list, message) result
  end
