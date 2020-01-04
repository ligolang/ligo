(** Generic parser API for LIGO *)

module Make (Lexer: Lexer.S with module Token := LexToken)
            (Parser: module type of Parser)
            (ParErr: sig val message: int -> string end) :
  sig
    (* Monolithic and incremental APIs of Menhir for parsing *)

    val mono_contract : (Lexing.lexbuf -> Lexer.token) -> Lexing.lexbuf -> AST.t
    val incr_contract : Lexer.instance -> AST.t

    (* Error handling *)

    type message = string
    type valid   = Lexer.token
    type invalid = Lexer.token
    type error = message * valid option * invalid

    exception Point of error

    val format_error : ?offsets:bool -> [`Byte | `Point] -> error -> string
  end
