open Ast_simplified
open Trace
open Tezos_utils

let compile_function_entry (program : program) entry_point : Compiler.Program.compiled_program result =
  let%bind typed_program = Typer.type_program program in
  Of_typed.compile_function_entry typed_program entry_point

let compile_expression_entry (program : program) entry_point : Compiler.Program.compiled_program result =
  let%bind typed_program = Typer.type_program program in
  Of_typed.compile_expression_entry typed_program entry_point

let compile_expression ae : Michelson.t result =
  let%bind typed = Typer.type_expression Ast_typed.Environment.full_empty ae in
  Of_typed.compile_expression typed
