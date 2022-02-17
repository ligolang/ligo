
let decompile (m : Ast_typed.program) : Ast_core.module_  =
  Checking.untype_program m

let decompile_expression (e : Ast_typed.expression) : Ast_core.expression =
  Checking.untype_expression e

let decompile_type_expression (e : Ast_typed.type_expression) : Ast_core.type_expression =
  Checking.untype_type_expression e
