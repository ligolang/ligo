open Trace
open Ast_sugar
open Purification
open Main_errors

let decompile (program : program) : (Ast_imperative.program , _) result =
  trace depurification_tracer @@ decompile_program program

let decompile_expression (e : expression) : (Ast_imperative.expression , _) result =
  trace depurification_tracer @@ decompile_expression e
