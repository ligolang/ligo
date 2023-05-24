(* A pretty printer for PascaLIGO *)

module CST = Cst_pascaligo.CST

type state
val default_state : state

type cst           = CST.t
type expr          = CST.expr
type type_expr     = CST.type_expr
type pattern       = CST.pattern
type declaration   = CST.declaration

val print             : state -> cst -> PPrint.document
val print_expr        : state -> expr -> PPrint.document
val print_type_expr   : state -> type_expr -> PPrint.document
val print_pattern     : state -> pattern -> PPrint.document
val print_declaration : state -> declaration -> PPrint.document
