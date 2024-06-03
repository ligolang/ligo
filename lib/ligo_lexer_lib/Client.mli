module Region = Simple_utils.Region

module type S =
  sig
    type token

    type message = string Region.reg

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) result

    val mk_verbatim : LexThread.t -> token (* FOR FUTURE USE *)
    val mk_string   : LexThread.t -> token
    val mk_eof      : Region.t -> token

    val callback : lexer

    (* For JsLIGO only. The accumulator is the list of previous tokens
       in reverse order. The first argument is the comment-as-a-token. *)

    val line_comment_attr :
      token -> (* The comment-token *)
      token list ->
      Lexing.lexbuf ->
      (token list, message) result

    val block_comment_attr :
      token -> (* The comment-token *)
      token list ->
      Lexing.lexbuf ->
      (token list, message) result
  end
