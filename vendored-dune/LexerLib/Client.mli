module Region = Simple_utils.Region

module type S =
  sig
    type token

    type message = string Region.reg

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) Stdlib.result

    val mk_verbatim : Thread.t -> token (* FOR FUTURE USE *)
    val mk_string   : Thread.t -> token
    val mk_eof      : Region.t -> token

    val callback : lexer

    (* For JsLIGO only. The accumulator is the list of previous tokens
       in reverse order. The first argument is the comment-as-a-token. *)

    val line_comment_attr :
      token -> (* The comment-token *)
      token list ->
      Lexing.lexbuf ->
      (token list, message) Stdlib.result

    val block_comment_attr :
      token -> (* The comment-token *)
      token list ->
      Lexing.lexbuf ->
      (token list, message) Stdlib.result
  end
