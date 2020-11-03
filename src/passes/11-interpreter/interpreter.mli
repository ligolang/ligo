open Trace

module Errors = Errors
type interpreter_error = Errors.interpreter_error

open Proto_alpha_utils.Memory_proto_alpha
val eval : ?options:options -> Ast_typed.program_fully_typed -> (Ligo_interpreter.Types.env , interpreter_error) result
val eval_test : ?options:options -> Ast_typed.program_fully_typed -> string -> (bool , interpreter_error) result
