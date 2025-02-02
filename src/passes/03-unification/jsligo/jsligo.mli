module CST = Cst.Jsligo
module Stripped = Typescript_stripper.Ast_stripped
module AST = Ast_unified

val compile_expression : Stripped.expr -> AST.expr
val compile_type_expression : Stripped.type_expr -> AST.ty_expr
val compile_program : Stripped.t -> AST.program
val decompile_expression : AST.expr -> CST.expr
val decompile_program : AST.program -> CST.t
val decompile_pattern : AST.pattern -> CST.pattern
val decompile_ty_expr : AST.ty_expr -> CST.type_expr
val decompile_sig_expr : AST.sig_expr -> CST.intf_expr
