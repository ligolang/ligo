open Trace
open Ast_core
open Desugaring
open Main_errors

let decompile (program : program) : (Ast_sugar.program , _) result =
  trace sugaring_tracer @@ decompile_program program

let decompile_expression (e : expression) : (Ast_sugar.expression , _) result =
  trace sugaring_tracer @@ decompile_expression e
