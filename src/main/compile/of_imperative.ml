open Main_errors
open Simple_utils.Trace
open Ast_imperative
open Purification

let compile ~raise (p : program) : Ast_sugar.program =
  trace ~raise purification_tracer @@ compile_program p

let compile_expression ~raise (e : expression) : Ast_sugar.expression =
  trace ~raise (Fn.compose purification_tracer List.return) @@ compile_expression ~last:true e
