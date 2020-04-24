module Region = Simple_utils.Region

module type S =
  sig
    module Lexer : LexerLib.S

    val output_token :
      ?offsets:bool ->
      [`Byte | `Point] ->
      EvalOpt.command ->
      out_channel ->
      Markup.t list ->
      Lexer.token ->
      unit

    type file_path = string

    val trace :
      ?offsets:bool ->
      [`Byte | `Point] ->
      ?block:EvalOpt.block_comment ->
      ?line:EvalOpt.line_comment ->
      Lexer.input ->
      EvalOpt.command ->
      (unit, string Region.reg) Stdlib.result
  end

module Make (Lexer: LexerLib.S) : S with module Lexer = Lexer
