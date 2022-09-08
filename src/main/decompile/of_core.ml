open Ast_core
open Desugaring

let decompile (m : program) : Ast_sugar.program =
  decompile_program m

let decompile_expression (e : expression) : Ast_sugar.expression  =
  decompile_expression e
