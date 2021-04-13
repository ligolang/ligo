open Trace

type interpreter_error = Errors.interpreter_error

val eval : Ast_typed.module_fully_typed -> (Ligo_interpreter.Types.env , interpreter_error) result
val eval_test : Ast_typed.module_fully_typed -> string -> (Ligo_interpreter.Types.value , interpreter_error) result
