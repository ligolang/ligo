(* Generic parser API for LIGO *)

module Region = Simple_utils.Region

module type PARSER =
  sig
    (* The type of tokens. *)

    type token
    type ast
    type expr

    (* This exception is raised by the monolithic API functions. *)

    exception Error

    (* The monolithic API. *)

    val interactive_expr :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> expr
    val contract :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ast

    (* The incremental API. *)

    module MenhirInterpreter :
      sig
        include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
                with type token = token
      end

    (* The entry point(s) to the incremental API. *)

    module Incremental :
      sig
        val interactive_expr :
          Lexing.position -> expr MenhirInterpreter.checkpoint
        val contract :
          Lexing.position -> ast MenhirInterpreter.checkpoint
      end
  end

module Make (Lexer: Lexer.S)
            (Parser: PARSER with type token = Lexer.Token.token)
            (ParErr: sig val message : int -> string end) :
  sig
    (* Monolithic and incremental APIs of Menhir for parsing *)

    val mono_contract :
      (Lexing.lexbuf -> Lexer.token) -> Lexing.lexbuf -> Parser.ast
    val incr_contract :
      Lexer.instance -> Parser.ast

    val mono_expr :
      (Lexing.lexbuf -> Lexer.token) -> Lexing.lexbuf -> Parser.expr
    val incr_expr :
      Lexer.instance -> Parser.expr

    (* Error handling *)

    type message = string
    type valid   = Parser.token
    type invalid = Parser.token
    type error   = message * valid option * invalid

    exception Point of error

    val format_error :
      ?offsets:bool -> [`Byte | `Point] -> error -> string

    val short_error :
      ?offsets:bool -> [`Byte | `Point] -> message -> Region.t -> string
  end
