(* Generic parser for LIGO *)

module Region = Simple_utils.Region

module type PARSER =
  sig
    (* The type of tokens, abstract syntax trees and expressions *)

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

    module MenhirInterpreter :
      sig
        (* The incremental API. *)

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

(* Main functor *)

module Make (Lexer: Lexer.S)
            (Parser: PARSER with type token = Lexer.Token.token)
            (ParErr: sig val message : int -> string end) =
  struct
    module I = Parser.MenhirInterpreter
    module S = MenhirLib.General (* Streams *)

    (* The call [stack checkpoint] extracts the parser's stack out of
       a checkpoint. *)

    let stack = function
      I.HandlingError env -> I.stack env
    |                   _ -> assert false

    (* The call [state checkpoint] extracts the number of the current
       state out of a parser checkpoint. *)

    let state checkpoint : int =
      match Lazy.force (stack checkpoint) with
        S.Nil -> 0 (* WARNING: Hack. The first state should be 0. *)
      | S.Cons (I.Element (s,_,_,_),_) -> I.number s

    (* The parser has successfully produced a semantic value. *)

    let success v = v

    (* The parser has suspended itself because of a syntax error. Stop. *)

    type message = string
    type valid   = Parser.token
    type invalid = Parser.token
    type error   = message * valid option * invalid

    exception Point of error

    let failure get_win checkpoint =
      let message = ParErr.message (state checkpoint) in
      match get_win () with
        Lexer.Nil -> assert false
      | Lexer.One invalid ->
          raise (Point (message, None, invalid))
      | Lexer.Two (invalid, valid) ->
          raise (Point (message, Some valid, invalid))

    (* The two Menhir APIs are called from the following functions. *)

    module Incr = Parser.Incremental

    let incr_contract memo Lexer.{read; buffer; get_win; close; _} =
      let supplier = I.lexer_lexbuf_to_supplier read buffer
      and failure  = failure get_win in
      let parser   = Incr.contract buffer.Lexing.lex_curr_p in
      let ast      =
        try I.loop_handle success failure supplier parser with
          Point (message, valid_opt, invalid) ->
          let error = Memo. (* TODO *)
          in Stdlib.Error ()

      in close (); ast

    let mono_contract = Parser.contract

    let incr_expr  Lexer.{read; buffer; get_win; close; _} : Parser.expr =
      let supplier = I.lexer_lexbuf_to_supplier read buffer
      and failure  = failure get_win in
      let parser   = Incr.interactive_expr buffer.Lexing.lex_curr_p in
      let expr     = I.loop_handle success failure supplier parser
      in close (); expr

    let mono_expr = Parser.interactive_expr

    (* Errors *)

    let format_error ?(offsets=true) mode (msg, valid_opt, invalid) =
      let invalid_region = Lexer.Token.to_region invalid in
      let header =
        "Parse error " ^ invalid_region#to_string ~offsets mode in
      let trailer =
        match valid_opt with
          None ->
            if Lexer.Token.is_eof invalid then ""
            else let invalid_lexeme = Lexer.Token.to_lexeme invalid in
                 Printf.sprintf ", before \"%s\"" invalid_lexeme
        | Some valid ->
            let valid_lexeme = Lexer.Token.to_lexeme valid in
            let s = Printf.sprintf ", after \"%s\"" valid_lexeme in
            if Lexer.Token.is_eof invalid then s
            else
              let invalid_lexeme = Lexer.Token.to_lexeme invalid in
              Printf.sprintf "%s and before \"%s\"" s invalid_lexeme in
      let header = header ^ trailer in
      header ^ (if msg = "" then ".\n" else ":\n" ^ msg)

  end
