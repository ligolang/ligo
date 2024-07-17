module Errors = Errors
module Helpers = Helpers

let all_program
    ~(raise : (Errors.self_ast_typed_error, _) Simple_utils.Trace.raise)
    program
  =
  View_passes.program ~raise program;
  program
  |> Union_to_sum.program ~raise
  |> Deprecated_module.program ~raise
  |> Dynamic_entrypoints.program ~raise
  |> Make_entry_point.program ~raise


let all_program_just_remove_unions
    ~(raise : (Errors.self_ast_typed_error, _) Simple_utils.Trace.raise)
    program
  =
  program |> Union_to_sum.program ~raise


let all_expression
    ~(raise : (Errors.self_ast_typed_error, _) Simple_utils.Trace.raise)
    expr
  =
  expr |> Union_to_sum.expression ~raise
