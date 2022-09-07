open Types
open Format

val type_variable : formatter -> type_expression -> unit
val environment_element : formatter -> environment_element -> unit
val environment : formatter -> environment -> unit
val value : formatter -> value -> unit
val type_expression : formatter -> type_expression -> unit
val type_content    : formatter -> type_content -> unit

val expression_content : formatter -> expression_content -> unit
val type_constant : formatter -> type_base -> unit

val expression : formatter -> expression -> unit
val expression_with_type : formatter -> expression -> unit
val function_ : formatter -> anon_function -> unit

val constant : formatter -> Ligo_prim.Constant.constant' -> unit
