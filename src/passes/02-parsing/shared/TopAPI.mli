(* This module is a wrapper for running the LIGO parsers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region          = Simple_utils.Region
module Utils           = Simple_utils.Utils
module Std             = Simple_utils.Std
module Lexbuf          = Simple_utils.Lexbuf
module Unit            = LexerLib.Unit
module type LEXER      = ParserLib.LowAPI.LEXER
module type PARSER     = ParserLib.LowAPI.PARSER
module type PARAMETERS = ParserLib.CLI.PARAMETERS

(* Internal dependencies *)

module Token       = Lexing_shared.Token
module LexerAPI    = Lexing_shared.TopAPI
module type PASSES = Lexing_shared.Pipeline.PASSES

(* The functor *)

module type PRINTER =
  sig
    type tree
    type state

    val mk_state :
      ?buffer:Buffer.t -> offsets:bool -> [`Point | `Byte] -> state

    type ('src, 'dst) printer = state -> 'src -> 'dst

    val to_buffer : (tree, Buffer.t) printer
    val to_string : (tree, string) printer
  end

module type PRETTY =
  sig
    type state
    val default_state : state

    type tree
    val print : state -> tree -> PPrint.document
  end

module type WARNING =
  sig
    val add : Main_warnings.all -> unit
  end

module Make
         (Lexer       : LexerAPI.S)
         (Parameters  : PARAMETERS)
         (ParErr      : sig val message : int -> string end)
         (Warning     : WARNING)
         (UnitPasses  : PASSES with type item = Lexer.Token.t Unit.t)
         (TokenPasses : PASSES with type item = Lexer.Token.t)
         (CST         : sig type t end)
         (Parser      : PARSER with type token = Lexer.Token.t
                                and type tree = CST.t)
         (Print       : PRINTER with type tree = CST.t)
         (Pretty      : PRETTY with type tree = CST.t) :
  sig
    (* Re-exporting the lexer *)

    module Lexer4LowAPI : LEXER

    (* Checking the CLI

       Note that the function [check_cli] is pure and therefore leaves
       to its callers to perform the side-effect of printing any error
       and information, nor not. *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    val check_cli : unit -> cli_status

    (* PARSING ERRORS *)

    type message = string Region.reg

    type single_error = {
      preprocessed : Buffer.t option;
      used_tokens  : Lexer.Token.t list;
      message      : message
    }

    type error =
      Single   of single_error
    | Multiple of message Utils.nseq

    (* The parser (which scans its tokens into a tree of type
       [parser.tree]) *)

    val parse :
      no_colour:bool ->
      Lexbuf.input -> Std.t * (Parser.tree * message list, error) result
  end
