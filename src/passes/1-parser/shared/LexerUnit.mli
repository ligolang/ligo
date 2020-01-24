(* Functor to build a standalone LIGO lexer *)

module type IO =
  sig
    val ext : string              (* LIGO file extension *)
    val options : EvalOpt.options (* CLI options *)
  end

module Make (IO: IO) (Lexer: Lexer.S) :
  sig
    val scan  : unit -> (Lexer.token list, string) Stdlib.result
    val trace : unit -> (unit, string) Stdlib.result
  end
