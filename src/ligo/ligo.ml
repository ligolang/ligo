open Ligo_parser

module Parser = Parser
module Lexer = Lexer
module CST = AST
module AST = AST2
module Typed = Typed
module Mini_c = Mini_c

open Ligo_helpers.Trace
let parse_file (source:string) : CST.t result =
  let channel = open_in source in
  let lexbuf = Lexing.from_channel channel in
  let Lexer.{read ; _} =
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
    ) @@ (fun () -> Parser.program read lexbuf) >>? fun program_cst ->
  ok program_cst

let parse (s:string) : CST.t result =
  let lexbuf = Lexing.from_string s in
  let Lexer.{read ; _} =
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
    ) @@ (fun () -> Parser.program read lexbuf) >>? fun program_cst ->
  ok program_cst


let abstract (cst:CST.t) : AST.O.ast result = ok @@ AST.s_ast cst

let annotate_types (ast:AST.O.ast) = ok @@ Typed.annotate ast
