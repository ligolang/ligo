open Ast_sugar
open Desugaring

let compile (p : program) : Ast_core.program  =
  let p = compile_program p in
  Self_ast_core.all_program ~init:p

let compile_expression ~raise (e : expression) : Ast_core.expression  =
  let e = compile_expression e in
  Self_ast_core.all_expression ~raise e
