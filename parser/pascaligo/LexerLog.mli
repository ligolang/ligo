module type S =
  sig
    module Lexer : Lexer.S

    val output_token :
      ?offsets:bool -> [`Byte | `Point] ->
      EvalOpt.command -> out_channel ->
      Markup.t list -> Lexer.token -> unit

    type file_path = string

    val trace :
      ?offsets:bool -> [`Byte | `Point] ->
      file_path option -> EvalOpt.command -> unit
  end

module Make (Lexer: Lexer.S) : S with module Lexer = Lexer
