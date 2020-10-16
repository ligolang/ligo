(* Using Core to make UTF-8 aware lexers that scan for one token or
   all of them in a variety of inputs. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Generic signature of input lexers *)

module type LEXER =
  sig
    type token

    val scan : token Core.scanner

    (* The function [check_right_context] is used for checking
       stylistic conventions, e.g. the need for at least a space
       between a string and an identifier. *)

    val check_right_context : token Core.style_checker
  end

(* The functor itself *)

module type S =
  sig
    type token
    type file_path = string
    type message   = string Region.reg

    type ('src,'dst) lexer =
      token Core.config ->
      'src ->
      ('dst, message) Stdlib.result

    val from_lexbuf  : (Lexing.lexbuf, token Core.instance) lexer
    val from_channel : (in_channel,    token Core.instance) lexer
    val from_string  : (string,        token Core.instance) lexer
    val from_file    : (file_path,     token Core.instance) lexer

    val all_from_lexbuf  : (Lexing.lexbuf, token list) lexer
    val all_from_channel : (in_channel,    token list) lexer
    val all_from_string  : (string,        token list) lexer
    val all_from_file    : (file_path,     token list) lexer
  end

module Make (Lexer : LEXER) : S with type token = Lexer.token
