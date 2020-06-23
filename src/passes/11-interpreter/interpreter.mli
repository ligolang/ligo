open Trace

type interpreter_error = []
val eval : Ast_typed.program -> (string, interpreter_error) result