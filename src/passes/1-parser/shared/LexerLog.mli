module Region = Simple_utils.Region

module type S =
  sig
    module Lexer : Lexer.S
    type token = Lexer.token

    val output_token :
      ?offsets:bool ->
      [`Byte | `Point] ->
      EvalOpt.command ->
      out_channel ->
      Markup.t list ->
      token ->
      unit

    type file_path = string

    val trace :
      ?offsets:bool ->
      [`Byte | `Point] ->
      ?block:EvalOpt.block_comment ->
      ?line:EvalOpt.line_comment ->
      token_to_region:(token -> Region.t) ->
      style:(token ->
             (Lexing.lexbuf -> (Markup.t list * token) option) ->
             Lexing.lexbuf ->
             unit) ->
      LexerLib.input ->
      EvalOpt.command ->
      (unit, string Region.reg) Stdlib.result
  end

module Make (Lexer: Lexer.S) : S with module Lexer = Lexer
