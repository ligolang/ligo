open Trace
open Ast_core
open Desugaring
open Main_errors

let decompile (m : module_) : (Ast_sugar.module_ , _) result =
  trace sugaring_tracer @@ decompile_module m

let decompile_expression (e : expression) : (Ast_sugar.expression , _) result =
  trace sugaring_tracer @@ decompile_expression e
