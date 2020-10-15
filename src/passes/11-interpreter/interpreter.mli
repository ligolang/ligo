open Trace

type interpreter_error = []
val eval : Ast_typed.program_fully_typed -> (string, interpreter_error) result
