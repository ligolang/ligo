(* Internal dependencies *)

module type COMMENTS = Preprocessing_shared.Comments.S

(* Making lexers *)

module Make (Comments : COMMENTS) (Token : Token.S) :
  sig
    module Trace = Simple_utils.Trace
    module Errors = Errors

    type file_path = string
    type result = (Token.t list, Errors.t) Trace.result

    val from_file    : file_path  -> result
    val from_string  : string     -> result
    val from_buffer  : Buffer.t   -> result
    val from_channel : in_channel -> result

    (* Aliases *)

    val lex_file    : file_path  -> result
    val lex_string  : string     -> result
    val lex_buffer  : Buffer.t   -> result
    val lex_channel : in_channel -> result
  end
