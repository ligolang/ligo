open Main_errors
open Simple_utils.Trace
open Ast_imperative
open Purification

let compile ~raise (m : module_) : Ast_sugar.module_ =
  trace ~raise purification_tracer @@ compile_module m

let compile_expression ~raise (e : expression) : Ast_sugar.expression =
  trace ~raise purification_tracer @@ compile_expression ~last:true e
