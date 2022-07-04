type interpreter_error = Errors.interpreter_error

val eval_test : raise:interpreter_error Simple_utils.Trace.raise
    -> add_warning:(Main_warnings.all -> unit)
    -> steps:int
    -> options:Compiler_options.t
    -> Ast_typed.program
    -> (string * Ligo_interpreter.Types.value) list
