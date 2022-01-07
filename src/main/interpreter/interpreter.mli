type interpreter_error = Errors.interpreter_error

val eval_test   : raise:interpreter_error Simple_utils.Trace.raise -> steps:int -> protocol_version:Environment.Protocols.t -> Ast_typed.program -> (Ast_aggregated.module_variable * Ligo_interpreter.Types.value) list
