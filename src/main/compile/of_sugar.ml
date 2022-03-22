open Ast_sugar
open Desugaring

let compile (p : module_) : Ast_core.module_  =
  let p = compile_module p in
  Self_ast_core.all_module ~init:p

let compile_expression ~raise (e : expression) : Ast_core.expression  =
  let e = compile_expression e in
  Self_ast_core.all_expression ~raise e
