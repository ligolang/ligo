type interpreter_error = Errors.interpreter_error

val eval_test : raise:(interpreter_error, Main_warnings.all) Simple_utils.Trace.raise
    -> steps:int
    -> options:Compiler_options.t
    -> ?source_file:string
    -> Ast_typed.program
    -> (string * Ligo_interpreter.Types.value) list
