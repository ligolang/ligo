(* This module is used by the files [pascaligo.ml], [cameligo.ml],
   [jsligo.ml] etc. in the directory above. This means that this
   module is specifically used to interface the lexers with the
   compiler. *)

(* Vendors dependencies *)

module Trace  = Simple_utils.Trace
module Config = Preprocessor.Config

(* Signature *)

module type S =
  sig
    module Token : Token.S
    module Warnings = Warnings

    (* Some inputs *)

    type file_path = string
    type dirs = file_path list

    (* Results *)

    module Errors = Errors

    type raise = (Errors.t, Warnings.t) Trace.raise

    type tokens = Token.t list

    (* Lexing various sources *)

    type 'src lexer = raise:raise -> dirs -> 'src -> tokens

    val from_file    : file_path lexer
    val from_string  : ?file:string -> string       lexer
    val from_buffer  : ?file:string -> Buffer.t     lexer
    val from_channel : ?file:string -> In_channel.t lexer

    (* Aliases *)

    val lex_file     : file_path lexer
    val lex_string   : ?file:string -> string       lexer
    val lex_buffer   : ?file:string -> Buffer.t     lexer
    val lex_channel  : ?file:string -> In_channel.t lexer
  end

(* Making lexers *)

module Make (Config : Config.S) (Token : Token.S)
       : S with module Token = Token
