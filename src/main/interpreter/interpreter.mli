type interpreter_error = Errors.interpreter_error

val eval_program : raise:interpreter_error Simple_utils.Trace.raise -> steps:int -> protocol_version:Environment.Protocols.t -> Ast_typed.program * Tezos_state.context * Ligo_interpreter.Types.env -> Ligo_interpreter.Types.env * Tezos_state.context
val eval_test   : raise:interpreter_error Simple_utils.Trace.raise -> steps:int -> protocol_version:Environment.Protocols.t -> Ast_typed.program -> Ligo_interpreter.Types.env * ((string * Ligo_interpreter.Types.value) list)
