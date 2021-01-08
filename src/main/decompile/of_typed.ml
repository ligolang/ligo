open Trace
open Main_errors

let decompile (typer_switch : Ast_typed.typer_switch)  (m : Ast_typed.module_fully_typed) : (Ast_core.module_ , _) result =
  trace typer_tracer @@ Typer.untype_module typer_switch m
