open Trace
open Ast_typed

let compile : Ast_typed.program -> Mini_c.program result = fun p ->
  Transpiler.transpile_program p

let compile_expression : annotated_expression -> Mini_c.expression result = fun e ->
  Transpiler.transpile_annotated_expression e