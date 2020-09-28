(* Generic parser for LIGO *)

(* Dependencies *)

module Region   = Simple_utils.Region
module EvalOpt  = Lexer_shared.EvalOpt
module Lexer    = Lexer_shared.Lexer
module LexerLib = Lexer_shared.LexerLib
module LexerLog = Lexer_shared.LexerLog

(* Input/Output *)

type options = <
  offsets : bool;
  mode    : [`Byte | `Point];
  cmd     : EvalOpt.command
>

module type IO =
  sig
    val options : options
  end

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

module Make (IO: IO)
            (Lexer: Lexer.S)
            (Parser: PARSER with type token = Lexer.token)
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
       state out of a parser checkpoint. The case [None] denotes the
       case of an error state with an empty LR stack: Menhir does not
       know how to determine that state. Until this is fixed, we
       return [None] and a generic error message (see function
       [message] below.) *)

    let state checkpoint : int option =
      match Lazy.force (stack checkpoint) with
        S.Nil -> None
      | S.Cons (I.Element (s,_,_,_),_) -> Some (I.number s)

    (* The parser has successfully produced a semantic value. *)

    let success v = v

    (* The parser has suspended itself because of a syntax error. Stop. *)

    type message = string
    type valid   = Parser.token
    type invalid = Parser.token
    type error   = message * valid option * invalid

    exception Point of error

    let format_error ?(offsets=true) _mode (msg, _valid_opt, invalid) =
      ignore(offsets);
      let invalid_region = Lexer.Token.to_region invalid in
      Region.{value=msg; region=invalid_region}

    (* From error states to error messages *)

    let message checkpoint =
      match state checkpoint with
        None -> "Syntax error." (* Menhir bug. See [state] above. *)
      | Some state ->
         match ParErr.message state with
           (* Default error message *)
           "<YOUR SYNTAX ERROR MESSAGE HERE>\n" ->
             string_of_int state ^ ": Syntax error."
         | msg -> msg
           (* A build error but we work around it: *)
         | exception Not_found -> "Syntax error."

    let failure get_win checkpoint =
      let message = message checkpoint in
      match get_win () with
        LexerLib.Nil -> assert false
      | LexerLib.One invalid ->
          raise (Point (message, None, invalid))
      | LexerLib.Two (invalid, valid) ->
          raise (Point (message, Some valid, invalid))

    (* The monolithic API of Menhir *)

    let mono_contract = Parser.contract

    let mono_expr = Parser.interactive_expr

    (* Incremental API of Menhir *)

    module Incr = Parser.Incremental

    module Log = LexerLog.Make (Lexer)
    let log    = Log.output_token
                   ~offsets:IO.options#offsets
                   IO.options#mode IO.options#cmd stdout

    let incr_contract LexerLib.{read; buffer; get_win; close; _} =
      let supplier  = I.lexer_lexbuf_to_supplier (read ~log) buffer
      and failure   = failure get_win in
      let parser    = Incr.contract buffer.Lexing.lex_curr_p in
      let ast       = I.loop_handle success failure supplier parser
      in flush_all (); close (); ast

    let incr_expr LexerLib.{read; buffer; get_win; close; _} =
      let supplier   = I.lexer_lexbuf_to_supplier (read ~log) buffer
      and failure    = failure get_win in
      let parser     = Incr.interactive_expr buffer.Lexing.lex_curr_p in
      let expr       = I.loop_handle success failure supplier parser
      in flush_all (); close (); expr
  end
