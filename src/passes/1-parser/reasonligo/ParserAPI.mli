(** Generic parser API for LIGO *)

module type PARSER =
  sig
    (* The type of tokens *)

    type token

    (* This exception is raised by the monolithic API functions *)

    exception Error

    (* The monolithic API *)

    val contract : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> AST.t

    (* The incremental API *)

    module MenhirInterpreter :
      sig
        include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
                with type token = token
      end

    module Incremental :
      sig
        val contract : Lexing.position -> AST.t MenhirInterpreter.checkpoint
      end

  end

(* Main functor *)

module Make (Lexer: Lexer.S)
            (Parser: PARSER with type token = Lexer.Token.token) :
  sig
    val mono_contract : (Lexing.lexbuf -> Lexer.token) -> Lexing.lexbuf -> AST.t
    val incr_contract : Lexer.instance -> AST.t
  end
