open Trace
open Ast_sugar
open Purification
open Main_errors

let decompile (m : module_) : (Ast_imperative.module_ , _) result =
  trace depurification_tracer @@ decompile_module m

let decompile_expression (e : expression) : (Ast_imperative.expression , _) result =
  trace depurification_tracer @@ decompile_expression e
