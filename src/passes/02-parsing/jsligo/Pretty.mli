(* A pretty printer for JsLIGO *)

type leading_bar =
    Always
  | Only_on_new_line
  | Avoid

type environment =
  { indent : int
  ; leading_vbar : leading_bar
  }

val default_environment : environment

type cst       = Cst_jsligo.CST.t
type expr      = Cst_jsligo.CST.expr
type type_expr = Cst_jsligo.CST.type_expr
type pattern   = Cst_jsligo.CST.pattern

val print           : environment -> cst -> PPrint.document
val print_expr      : environment -> expr -> PPrint.document
val print_type_expr : environment -> type_expr -> PPrint.document
val print_pattern   : environment -> pattern -> PPrint.document
