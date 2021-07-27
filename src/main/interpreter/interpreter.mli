type interpreter_error = Errors.interpreter_error

val eval      : raise:interpreter_error Trace.raise -> Ast_typed.module_fully_typed -> Ligo_interpreter.Types.env * Tezos_state.context
val eval_test : raise:interpreter_error Trace.raise -> Ast_typed.module_fully_typed -> (string * Ligo_interpreter.Types.value) list
