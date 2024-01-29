(* A pretty printer for JsLIGO *)

module CST = Cst_jsligo.CST
module PrettyComb = Parsing_shared.PrettyComb

(* Placement *)

type state = PrettyComb.state

val default_state : state

type 'a reg = 'a CST.reg

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern
type statement = CST.statement
type 'a legacy_variant = 'a CST.legacy_variant
type signature_expr    = CST.intf_expr

val print           : state -> cst -> PPrint.document
val print_expr      : state -> expr -> PPrint.document
val print_type_expr : state -> type_expr -> PPrint.document
val print_pattern   : state -> pattern -> PPrint.document
val print_statement : state -> statement -> PPrint.document
val print_legacy_variant : 'a. (state -> 'a -> PPrint.document) -> state -> 'a legacy_variant reg -> PPrint.document
val print_signature_expr : state -> signature_expr -> PPrint.document