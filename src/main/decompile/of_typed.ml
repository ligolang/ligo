open Trace
open Main_errors

let decompile (program : Ast_typed.program_fully_typed) : (Ast_core.program , _) result =
  trace typer_tracer @@ Typer.untype_program program
