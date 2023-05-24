(* A pretty printer for PyLIGO *)

module CST = Cst_pyligo.CST

type state
val default_state : state

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern

val print           : state -> cst -> PPrint.document
val print_expr      : state -> expr -> PPrint.document
val print_type_expr : state -> type_expr -> PPrint.document
val print_pattern   : state -> pattern -> PPrint.document
