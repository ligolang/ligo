open Trace
open Parser_pascaligo
module Parser = Parser_pascaligo.Parser
module AST = Parser_pascaligo.AST
module ParserLog = Parser_pascaligo.ParserLog

let parse_file (source: string) : AST.t result =
  let pp_input =
    let prefix = Filename.(source |> basename |> remove_extension)
    and suffix = ".pp.ligo"
    in prefix ^ suffix in

  let cpp_cmd = Printf.sprintf "cpp -traditional-cpp %s > %s"
                               source pp_input in
  let%bind () = sys_command cpp_cmd in

  let%bind channel =
    generic_try (simple_error "error opening file") @@
    (fun () -> open_in pp_input) in
  let lexbuf = Lexing.from_channel channel in
  let module Lexer = Lexer.Make(LexToken) in
  let Lexer.{read ; close ; _} =
    Lexer.open_token_stream None in
  specific_try (function
      | Parser.Error -> (
          let start = Lexing.lexeme_start_p lexbuf in
          let end_ = Lexing.lexeme_end_p lexbuf in
          let str = Format.sprintf
              "Parse error at \"%s\" from (%d, %d) to (%d, %d). In file \"%s|%s\"\n"
              (Lexing.lexeme lexbuf)
              start.pos_lnum (start.pos_cnum - start.pos_bol)
              end_.pos_lnum (end_.pos_cnum - end_.pos_bol)
              start.pos_fname source
          in
          simple_error str
        )
      | exn ->
          let start = Lexing.lexeme_start_p lexbuf in
          let end_ = Lexing.lexeme_end_p lexbuf in
          let str = Format.sprintf
              "Unrecognized error (%s) at \"%s\" from (%d, %d) to (%d, %d). In file \"%s|%s\"\n"
              (Printexc.to_string exn)
              (Lexing.lexeme lexbuf)
              start.pos_lnum (start.pos_cnum - start.pos_bol)
              end_.pos_lnum (end_.pos_cnum - end_.pos_bol)
              start.pos_fname source
          in
          simple_error str
    ) @@ (fun () ->
      let raw = Parser.contract read lexbuf in
      close () ;
      raw
    ) >>? fun raw ->
  ok raw

let parse_string (s:string) : AST.t result =
  
  let lexbuf = Lexing.from_string s in
  let module Lexer = Lexer.Make(LexToken) in
  let Lexer.{read ; close ; _} =
    Lexer.open_token_stream None in
  specific_try (function
      | Parser.Error -> (
          let start = Lexing.lexeme_start_p lexbuf in
          let end_ = Lexing.lexeme_end_p lexbuf in
          let str = Format.sprintf
              "Parse error at \"%s\" from (%d, %d) to (%d, %d)\n"
              (Lexing.lexeme lexbuf)
              start.pos_lnum (start.pos_cnum - start.pos_bol)
              end_.pos_lnum (end_.pos_cnum - end_.pos_bol) in
          simple_error str
        )
      | _ -> simple_error "unrecognized parse_ error"
    ) @@ (fun () ->
      let raw = Parser.contract read lexbuf in
      close () ;
      raw
    ) >>? fun raw ->
  ok raw

let parse_expression (s:string) : AST.expr result =
  let lexbuf = Lexing.from_string s in
  let module Lexer = Lexer.Make(LexToken) in
  let Lexer.{read ; close; _} =
    Lexer.open_token_stream None in
  specific_try (function
      | Parser.Error -> (
          let start = Lexing.lexeme_start_p lexbuf in
          let end_ = Lexing.lexeme_end_p lexbuf in
          let str = Format.sprintf
              "Parse error at \"%s\" from (%d, %d) to (%d, %d)\n"
              (Lexing.lexeme lexbuf)
              start.pos_lnum (start.pos_cnum - start.pos_bol)
              end_.pos_lnum (end_.pos_cnum - end_.pos_bol) in
          simple_error str
        )
      | exn ->
          let start = Lexing.lexeme_start_p lexbuf in
          let end_ = Lexing.lexeme_end_p lexbuf in
          let str = Format.sprintf
              "Unrecognized error (%s) at \"%s\" from (%d, %d) to (%d, %d). In expression \"%s|%s\"\n"
              (Printexc.to_string exn)
              (Lexing.lexeme lexbuf)
              start.pos_lnum (start.pos_cnum - start.pos_bol)
              end_.pos_lnum (end_.pos_cnum - end_.pos_bol)
              start.pos_fname s
          in
          simple_error str
    ) @@ (fun () ->
      let raw = Parser.interactive_expr read lexbuf in
      close () ;
      raw
    ) >>? fun raw ->
  ok raw
