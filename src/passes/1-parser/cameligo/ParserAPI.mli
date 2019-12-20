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

(* Errors *)

module type PAR_ERR =
  sig
    val message : int -> string   (* From error states to messages *)
  end

val format_error :
  ?offsets:bool -> [`Byte | `Point] ->
  string Region.reg -> file:bool -> string

(* Main functor *)

module Make (Lexer: Lexer.S)
            (Parser: PARSER with type token = Lexer.Token.token)
            (ParErr: PAR_ERR) :
  sig
    type message = string
    type valid   = Lexer.token
    type invalid = Lexer.token

    exception Point of message * valid option * invalid

    val mono_contract : (Lexing.lexbuf -> Lexer.token) -> Lexing.lexbuf -> AST.t
    val incr_contract : Lexer.instance -> AST.t
  end
