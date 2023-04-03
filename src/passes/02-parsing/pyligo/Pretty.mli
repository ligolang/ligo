(* A pretty printer for PyLIGO *)

module CST = Cst_pyligo.CST

type environment
val default_environment : environment

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern

val print           : environment -> cst -> PPrint.document
val print_expr      : environment -> expr -> PPrint.document
val print_type_expr : environment -> type_expr -> PPrint.document
val print_pattern   : environment -> pattern -> PPrint.document
