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

(* Errors *)

module type PAR_ERR =
  sig
    val message : int -> string   (* From error states to messages *)
  end

let format_error ?(offsets=true) mode Region.{region; value} ~file =
  let reg = region#to_string ~file ~offsets mode in
  Printf.sprintf "\027[31mParse error %s:\n%s\027[0m%!" reg value

(* Main functor *)

module Make (Lexer: Lexer.S)
            (Parser: PARSER with type token = Lexer.Token.token)
            (ParErr: PAR_ERR) =
  struct
    type message = string
    type valid   = Lexer.token
    type invalid = Lexer.token

    exception Point of message * valid option * invalid

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

    (* let fail _checkpoint = raise Parser.Error *)

    let failure get_win checkpoint =
      let message = ParErr.message (state checkpoint) in
      match get_win () with
        Lexer.Nil -> assert false
      | Lexer.One invalid ->
          raise (Point (message, None, invalid))
      | Lexer.Two (invalid, valid) ->
          raise (Point (message, Some valid, invalid))

    (* The generic parsing function *)

    let incr_contract Lexer.{read; buffer; get_win; close; _} : AST.t =
      let supplier = I.lexer_lexbuf_to_supplier read buffer
      and failure  = failure get_win in
      let parser   = Parser.Incremental.contract buffer.Lexing.lex_curr_p in
      let ast = I.loop_handle success failure supplier parser
      in close (); ast

    let mono_contract = Parser.contract

  end
