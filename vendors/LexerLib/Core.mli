(* A library for writing UTF8-aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Lexbuf = Simple_utils.Lexbuf
module Config = Preprocessor.Config

(* The functor return signature *)

module type S =
  sig
    (* Lexical units *)

    type lex_unit

    (* Utility types *)

    type file_path = string
    type message   = string Region.reg

    type units = lex_unit list

    (* LEXER INSTANCE (see README.md) *)

    (* Errors *)

    type error = {
      used_units : units;
      message    : message
    }

    (* Instances *)

    type instance = {
      input      : Lexbuf.input;
      read_units : Lexing.lexbuf -> (units, error) result;
      lexbuf     : Lexing.lexbuf;
      close      : Lexbuf.close
    }

    val open_stream : Lexbuf.input -> (instance, message) result
  end

(* THE FUNCTOR *)

module Make (Config : Config.S) (Client : Client.S)
       : S with type lex_unit = Client.token Unit.t
