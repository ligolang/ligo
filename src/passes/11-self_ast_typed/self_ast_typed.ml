module Errors = Errors
module Helpers = Helpers

let all_program
    ~(raise : (Errors.self_ast_typed_error, _) Simple_utils.Trace.raise)
    program
  =
  View_passes.program ~raise program;
  program |> Dynamic_entrypoints.program ~raise |> Make_entry_point.program ~raise
