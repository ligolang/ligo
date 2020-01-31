(* Functor to build a standalone LIGO lexer *)

module Region = Simple_utils.Region

module type IO =
  sig
    val ext : string              (* LIGO file extension *)
    val options : EvalOpt.options (* CLI options *)
  end

module Make (IO: IO) (Lexer: Lexer.S) :
  sig
    val scan  : unit -> (Lexer.token list, string Region.reg) Stdlib.result
    val trace : unit -> (unit, string Region.reg) Stdlib.result
  end
