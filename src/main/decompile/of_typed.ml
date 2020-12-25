open Trace
open Main_errors

let decompile (typer_switch : Ast_typed.typer_switch)  (program : Ast_typed.program_fully_typed) : (Ast_core.program , _) result =
  trace typer_tracer @@ Typer.untype_program typer_switch program
