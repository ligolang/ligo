open Trace
open Main_errors

let decompile (m : Ast_typed.module_fully_typed) : (Ast_core.module_ , _) result =
  trace checking_tracer @@ Checking.untype_module_fully_typed m
