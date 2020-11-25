(* A pretty printer for CameLIGO *)

type cst        = Cst.Cameligo.t
type expr       = Cst.Cameligo.expr
type type_expr  = Cst.Cameligo.type_expr
type pattern    = Cst.Cameligo.pattern

val print           : cst -> PPrint.document
val print_expr      : expr -> PPrint.document
val print_type_expr : type_expr -> PPrint.document
val print_pattern   : pattern -> PPrint.document
