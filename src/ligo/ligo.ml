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

let parse_expression (s:string) : AST_Raw.expr result =
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
    ) @@ (fun () -> Parser.interactive_expr read lexbuf) >>? fun expr ->
  ok expr

let simplify (p:AST_Raw.t) : Ast_simplified.program result = AST_Simplified.Simplify.simpl_program p
let simplify_expr (e:AST_Raw.expr) : Ast_simplified.annotated_expression result = AST_Simplified.Simplify.simpl_expression e
let unparse_simplified_expr (e:AST_Simplified.annotated_expression) : string result =
  ok @@ Format.asprintf "%a" AST_Simplified.PP.annotated_expression e

let type_ (p:AST_Simplified.program) : AST_Typed.program result = Typer.type_program p
let type_expression ?(env:Typer.Environment.t = Typer.Environment.empty)
    (e:AST_Simplified.annotated_expression) : AST_Typed.annotated_expression result =
  Typer.type_annotated_expression env e
let untype_expression (e:AST_Typed.annotated_expression) : AST_Simplified.annotated_expression = Typer.untype_annotated_expression e

let transpile (p:AST_Typed.program) : Mini_c.program result = Transpiler.translate_program p
let transpile_expression ?(env:Mini_c.Environment.t = Mini_c.Environment.empty)
    (e:AST_Typed.annotated_expression) : Mini_c.expression result = Transpiler.translate_annotated_expression env e
let transpile_value ?(env:Mini_c.Environment.t = Mini_c.Environment.empty)
    (e:AST_Typed.annotated_expression) : Mini_c.expression result =
  let%bind e = Transpiler.translate_annotated_expression env e in
  Mini_c.expression_to_value e
