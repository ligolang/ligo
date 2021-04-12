(* Interfacing the JsLIGO lexer. *)

(* Vendors dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Token  = Lexing_jsligo.Token
module Errors = Lexing_shared.Errors

type file_path = string

(* Results *)

type result = (Token.t list, Errors.t) Trace.result

(* Lexing various sources *)

val from_file    : file_path -> result
val from_string  : string -> result
val from_buffer  : Buffer.t -> result
val from_channel : in_channel -> result

(* Aliases *)

val lex_file    : file_path -> result
val lex_string  : string -> result
val lex_buffer  : Buffer.t -> result
val lex_channel : in_channel -> result
