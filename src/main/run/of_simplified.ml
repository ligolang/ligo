open Trace
open Ast_simplified

let compile_expression ?(value = false) ?env ~state expr = (* TODO: state optional *)
  if value
  then (
    Compile.Of_simplified.compile_expression_as_value ?env ~state expr
  )
  else (
    let%bind code = Compile.Of_simplified.compile_expression_as_function ?env ~state expr in
    Of_michelson.evaluate_michelson code
  )

let run_typed_program           (* TODO: this runs an *untyped* program, not a typed one. *)
    ?options ?input_to_value
    (program : Ast_typed.program) (state : Typer.Solver.state) (entry : string)
    (input : expression) : expression result =
  let%bind code = Compile.Of_typed.compile_function_entry program entry in
  let%bind input =
    let env = Ast_typed.program_environment program in
    compile_expression ?value:input_to_value ~env ~state input
  in
  let%bind ex_ty_value = Of_michelson.run ?options code input in
  Compile.Of_simplified.uncompile_typed_program_entry_function_result program entry ex_ty_value

let evaluate_typed_program_entry
    ?options
    (program : Ast_typed.program) (entry : string)
  : Ast_simplified.expression result =
  let%bind code = Compile.Of_typed.compile_expression_as_function_entry program entry in
  let%bind ex_ty_value = Of_michelson.evaluate ?options code in
  Compile.Of_simplified.uncompile_typed_program_entry_expression_result program entry ex_ty_value
