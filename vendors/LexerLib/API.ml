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

module Make (Lexer: LEXER) =
  struct
    type token = Lexer.token
    type file_path = string
    type message   = string Region.reg

    type ('src,'dst) lexer =
      token Core.config ->
      'src ->
      ('dst, message) Stdlib.result

    (* Generic lexer for all kinds of inputs *)

    let generic lexbuf_of config source =
      let lexbuf = lexbuf_of source in
      Core.open_token_stream
        config
        ~scan:Lexer.scan
        ~style:Lexer.check_right_context
        (Core.Buffer lexbuf)

    (* Lexing the input to recognise one token *)

    let from_lexbuf  config = generic (fun x -> x) config
    let from_channel config = generic Lexing.from_channel config
    let from_string  config = generic Lexing.from_string config

    let from_file config path =
      Core.open_token_stream
        config
        ~scan:Lexer.scan
        ~style:Lexer.check_right_context
        (Core.File path)

    (* Lexing the entire input *)

    let scan_all_tokens (config: 'token Core.config) = function
      Stdlib.Error _ as err -> flush_all (); err
    | Ok Core.{read; buffer; close; _} ->
        let close_all () = flush_all (); close () in
        let rec read_tokens tokens =
          match read buffer with
            Stdlib.Ok token ->
              if   config#is_eof token
              then Stdlib.Ok (List.rev tokens)
              else read_tokens (token::tokens)
          | Stdlib.Error _ as err -> err in
        let result = read_tokens []
        in close_all (); result

    let all_from_lexbuf config lexbuf =
      from_lexbuf config lexbuf |> scan_all_tokens config

    let all_from_channel config src =
      generic Lexing.from_channel config src |> scan_all_tokens config

    let all_from_string config src =
      generic Lexing.from_string config src |> scan_all_tokens config

    let all_from_file config src =
      from_file config src |> scan_all_tokens config
  end
