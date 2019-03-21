open Ligo_parser

module Parser = Parser
module Lexer = Lexer
module AST_Raw = AST
module AST_Simplified = Ast_simplified
module AST_Typed = Ast_typed
module Mini_c = Mini_c

open Ligo_helpers.Trace
let parse_file (source:string) : AST_Raw.t result =
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
    ) @@ (fun () -> Parser.contract read lexbuf) >>? fun program_cst ->
  ok program_cst

let parse (s:string) : AST_Raw.t result =
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
    ) @@ (fun () -> Parser.contract read lexbuf) >>? fun program_cst ->
  ok program_cst


let simplify (p:AST_Raw.t) : Ast_simplified.program result = AST_Simplified.Simplify.simpl_program p

let type_ (p:AST_Simplified.program) : AST_Typed.program result = Typer.type_program p

let transpile (p:AST_Typed.program) : Mini_c.program result = Transpiler.translate_program p
