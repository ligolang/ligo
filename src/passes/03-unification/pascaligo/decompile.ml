module CST = Cst.Pascaligo
module AST = Ast_unified

let rec folder =
  let todo _ = failwith ("TODO" ^ __LOC__) in
  AST.Catamorphism.
    { expr = todo
    ; ty_expr = todo
    ; pattern = todo
    ; statement = todo
    ; block = todo
    ; mod_expr = todo
    ; instruction = todo
    ; declaration = todo
    ; program_entry = todo
    ; program = todo
    }


and decompile_program p = AST.Catamorphism.cata_program ~f:folder p
and decompile_expression e = AST.Catamorphism.cata_expr ~f:folder e
and decompile_pattern p = AST.Catamorphism.cata_pattern ~f:folder p
