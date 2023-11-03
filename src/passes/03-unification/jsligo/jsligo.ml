module CST = Cst.Jsligo
module AST = Ast_unified
open Unification_shared.Helpers.Make_unification (Compile)

let compile_expression = compile_expression
let compile_program = compile_program
let decompile_program = Decompile.decompile_program
let decompile_pattern = Decompile.decompile_pattern
let decompile_expression = Decompile.decompile_expression
let decompile_ty_expr = Decompile.decompile_type_expression
let decompile_sig_expr = Decompile.decompile_sig_expr
