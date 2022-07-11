open Ast_sugar
open Purification

let decompile (m : program) : Ast_imperative.program  =
  decompile_program m

let decompile_expression (e : expression) : Ast_imperative.expression  =
  decompile_expression e
