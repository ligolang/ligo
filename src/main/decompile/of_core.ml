open Ast_core
open Nanopasses

let decompile ~raise ~syntax (m : program) : Ast_unified.program =
  decompile_program ~raise ~syntax m


let decompile_expression ~raise ~syntax (e : expression) : Ast_unified.expr =
  decompile_expression ~raise ~syntax e


let decompile_ty_expr ~raise ~syntax (t : type_expression) : Ast_unified.ty_expr =
  decompile_ty_expr ~raise ~syntax t
