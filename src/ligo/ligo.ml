open Ligo_parser

module Parser = Parser
module Lexer = Lexer
module AST_Raw = AST
module AST_Simplified = Ast_simplified
module AST_Typed = Ast_typed
module Mini_c = Mini_c
module Typer = Typer
module Transpiler = Transpiler

open Ligo_helpers.Trace
let parse_file (source:string) : AST_Raw.t result =
  let%bind channel =
    generic_try (simple_error "error opening file") @@
    (fun () -> open_in source) in
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
let untype_expression (e:AST_Typed.annotated_expression) : AST_Simplified.annotated_expression result = Typer.untype_annotated_expression e

let transpile (p:AST_Typed.program) : Mini_c.program result = Transpiler.translate_program p
let transpile_entry (p:AST_Typed.program) (name:string) : Mini_c.anon_function result = Transpiler.translate_entry p name
let transpile_expression ?(env:Mini_c.Environment.t = Mini_c.Environment.empty)
    (e:AST_Typed.annotated_expression) : Mini_c.expression result = Transpiler.translate_annotated_expression env e
let transpile_value
    (e:AST_Typed.annotated_expression) : Mini_c.value result =
  let%bind f =
    let open Transpiler in
    let (f, t) = functionalize e in
    let%bind main = translate_main f t in
    ok main
  in

  let input = Mini_c.Combinators.d_unit in
  let%bind r = Mini_c.Run.run_entry f input in
  ok r

let untranspile_value (v : Mini_c.value) (e:AST_Typed.type_value) : AST_Typed.annotated_expression result =
  Transpiler.untranspile v e

let type_file ?(debug_simplify = false) ?(debug_typed = false)
    (path:string) : AST_Typed.program result =
  let%bind raw = parse_file path in
  let%bind simpl =
    trace (simple_error "simplifying") @@
    simplify raw in
  (if debug_simplify then
     Format.(printf "Simplified : %a\n%!" AST_Simplified.PP.program simpl)
  ) ;
  let%bind typed =
    trace (simple_error "typing") @@
    type_ simpl in
  (if debug_typed then (
      Format.(printf "Typed : %a\n%!" AST_Typed.PP.program typed)
    )) ;
  ok typed


let easy_evaluate_typed (entry:string) (program:AST_Typed.program) : AST_Typed.annotated_expression result =
  let%bind result =
    let%bind mini_c_main =
      transpile_entry program entry in
    Mini_c.Run.run_entry mini_c_main (Mini_c.Combinators.d_unit) in
  let%bind typed_result =
    let%bind typed_main = Ast_typed.get_entry program entry in
    untranspile_value result typed_main.type_annotation in
  ok typed_result

let easy_evaluate_typed = trace_f_2_ez easy_evaluate_typed "easy evaluate typed"

let easy_run_typed
    ?(debug_mini_c = false) (entry:string)
    (program:AST_Typed.program) (input:AST_Typed.annotated_expression) : AST_Typed.annotated_expression result =
  let%bind mini_c_main =
    trace (simple_error "transpile mini_c entry") @@
    transpile_entry program entry in
  (if debug_mini_c then
     Format.(printf "Mini_c : %a\n%!" Mini_c.PP.function_ mini_c_main.content)
  ) ;

  let%bind mini_c_value = transpile_value input in

  let%bind mini_c_result =
    trace (simple_error "run mini_c") @@
    Mini_c.Run.run_entry mini_c_main mini_c_value in
  let%bind typed_result =
    let%bind main_result_type =
      let%bind typed_main = Ast_typed.get_functional_entry program entry in
      match (snd typed_main).type_value with
      | Type_function (_, result) -> ok result
      | _ -> simple_fail "main doesn't have fun type" in
    untranspile_value mini_c_result main_result_type in
  ok typed_result

let easy_run_main_typed
    ?(debug_mini_c = false)
    (program:AST_Typed.program) (input:AST_Typed.annotated_expression) : AST_Typed.annotated_expression result =
  easy_run_typed ~debug_mini_c "main" program input

let easy_run_main (path:string) (input:string) : AST_Typed.annotated_expression result =
  let%bind typed = type_file path in

  let%bind raw_expr = parse_expression input in
  let%bind simpl_expr = simplify_expr raw_expr in
  let%bind typed_expr = type_expression simpl_expr in
  easy_run_main_typed typed typed_expr
