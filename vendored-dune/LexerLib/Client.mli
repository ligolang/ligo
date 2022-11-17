module Region = Simple_utils.Region

module type S =
  sig
    type token

    type message = string Region.reg

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) Stdlib.result

    val mk_string : Thread.t -> token
    val mk_eof    : Region.t -> token
    val callback  : lexer
  end
