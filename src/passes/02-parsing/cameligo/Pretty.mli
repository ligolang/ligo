(* A pretty printer for CameLIGO *)

module CST = Cst_cameligo.CST
module PrettyComb = Parsing_shared.PrettyComb

(* Placement *)

type state = PrettyComb.state

val default_state : state

type cst         = CST.t
type expr        = CST.expr
type type_expr   = CST.type_expr
type pattern     = CST.pattern
type declaration = CST.declaration
type signature_expr = CST.signature_expr

val print             : state -> cst -> PPrint.document
val print_expr        : state -> expr -> PPrint.document
val print_type_expr   : state -> type_expr -> PPrint.document
val print_pattern     : state -> pattern -> PPrint.document
val print_declaration : state -> declaration -> PPrint.document
val print_signature_expr : state -> signature_expr -> PPrint.document