type interpreter_error = Errors.interpreter_error

val eval_expression
  :  raise:(interpreter_error, Main_warnings.all) Simple_utils.Trace.raise
  -> steps:int
  -> options:Compiler_options.t
  -> Ast_typed.program
  -> Ast_typed.expression
  -> (bool * Ligo_interpreter.Types.value) Lwt.t

val eval_test
  :  raise:(interpreter_error, Main_warnings.all) Simple_utils.Trace.raise
  -> steps:int
  -> options:Compiler_options.t
  -> Ast_typed.program
  -> (bool * Ligo_interpreter.Types.toplevel_env) Lwt.t
