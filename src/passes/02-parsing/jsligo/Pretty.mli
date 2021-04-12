(* A pretty printer for JsLIGO *)

type cst        = Cst.Jsligo.t
type expr       = Cst.Jsligo.expr
type type_expr  = Cst.Jsligo.type_expr
type pattern    = Cst.Jsligo.pattern

val print           : cst -> PPrint.document
val print_expr      : expr -> PPrint.document
val print_type_expr : type_expr -> PPrint.document
val print_pattern   : pattern -> PPrint.document
