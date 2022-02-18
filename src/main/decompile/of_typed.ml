
let decompile (m : Ast_typed.program) : Ast_core.module_  =
  Checking.untype_program m

let decompile_expression (e : Ast_typed.expression) : Ast_core.expression =
  Checking.untype_expression e
