open Main_errors
open Simple_utils.Trace
open Ast_imperative
open Desugaring

let compile ~raise (p : program) : Ast_core.program =
  trace ~raise desugaring_tracer @@ compile_program p


let compile_expression
    ~(raise : (Main_errors.all, Main_warnings.all) raise)
    (e : expression)
    : Ast_core.expression
  =
  trace ~raise desugaring_tracer @@ compile_expression e
