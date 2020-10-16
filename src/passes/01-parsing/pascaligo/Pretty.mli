(* A pretty printer for PascaLIGO *)

type cst        = Cst.Pascaligo.t
type expr       = Cst.Pascaligo.expr
type type_expr  = Cst.Pascaligo.type_expr
type pattern    = Cst.Pascaligo.pattern

val print           : cst -> PPrint.document
val print_expr      : expr -> PPrint.document
val print_type_expr : type_expr -> PPrint.document
val print_pattern   : pattern -> PPrint.document
