(** Generic parser for LIGO *)

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
            (Parser: PARSER with type token = Lexer.Token.token) =
  struct

    module I = Parser.MenhirInterpreter

    (* The parser has successfully produced a semantic value. *)

    let success v = v

    (* The parser has suspended itself because of a syntax error. Stop. *)

    let fail _checkpoint = raise Parser.Error

    (* The generic parsing function *)

    let incr_contract Lexer.{read; buffer; close; _} : AST.t =
      let supplier = I.lexer_lexbuf_to_supplier read buffer in
      let parser = Parser.Incremental.contract buffer.Lexing.lex_curr_p in
      let ast = I.loop_handle success fail supplier parser
      in close (); ast

    let mono_contract = Parser.contract

  end
