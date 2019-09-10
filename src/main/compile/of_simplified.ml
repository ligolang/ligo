open Ast_simplified
open Trace

let compile_entry (program : program) entry_point =
  let%bind typed_program = Typer.type_program program in
  Of_typed.compile_entry typed_program entry_point
