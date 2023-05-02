open Types
open Format

val value : formatter -> value -> unit
val type_expression : formatter -> type_expression -> unit
val expression : formatter -> expression -> unit
val constant : formatter -> Ligo_prim.Constant.constant' -> unit
