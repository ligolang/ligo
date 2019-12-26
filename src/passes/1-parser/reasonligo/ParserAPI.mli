(** Generic parser API for LIGO *)

(* Errors *)

val format_error :
  ?offsets:bool -> [`Byte | `Point] ->
  string Region.reg -> file:bool -> string

(* Main functor *)

module Make (Lexer: Lexer.S with module Token := LexToken)
            (Parser: module type of Parser)
            (ParErr: sig val message: int -> string end) :
  sig
    type message = string
    type valid   = Lexer.token
    type invalid = Lexer.token

    exception Point of message * valid option * invalid

    val mono_contract : (Lexing.lexbuf -> Lexer.token) -> Lexing.lexbuf -> AST.t
    val incr_contract : Lexer.instance -> AST.t
  end
