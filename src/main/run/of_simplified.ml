open Trace
open Ast_simplified

let compile_expression ?env ~state expr = (* TODO: state optional *)
  let%bind code = Compile.Of_simplified.compile_expression_as_function ?env ~state expr in
  Of_michelson.evaluate_michelson code

let run_typed_program           (* TODO: this runs an *untyped* program, not a typed one. *)
    ?options
    (program : Ast_typed.program) (state : Typer.Solver.state) (entry : string)
    (input : expression) : expression result =
  let%bind code = Compile.Of_typed.compile_function_entry program entry in
  let%bind input =
    let env = Ast_typed.program_environment program in
    compile_expression ~env ~state input
  in
  let%bind ex_ty_value = Of_michelson.run ?options code input in
  Compile.Of_simplified.uncompile_typed_program_entry_function_result program entry ex_ty_value

let run_failwith_program
    ?options
    (program : Ast_typed.program) (state : Typer.Solver.state) (entry : string)
    (input : expression) : Of_michelson.failwith_res result =
  let%bind code = Compile.Of_typed.compile_function_entry program entry in
  let%bind input =
    let env = Ast_typed.program_environment program in
    compile_expression ~env ~state input
  in
  Of_michelson.get_exec_error ?options code input

let evaluate_typed_program_entry
    ?options
    (program : Ast_typed.program) (entry : string)
  : Ast_simplified.expression result =
  let%bind code = Compile.Of_typed.compile_expression_as_function_entry program entry in
  let%bind ex_ty_value = Of_michelson.evaluate ?options code in
  Compile.Of_simplified.uncompile_typed_program_entry_expression_result program entry ex_ty_value

let compile_program
    ?options
    (program : Ast_typed.program) (entry : string)
  : unit result =
  let%bind code = Compile.Of_typed.compile_expression_as_function_entry program entry in
  let%bind _ex_ty_value = Of_michelson.evaluate ?options code in
  ok ()