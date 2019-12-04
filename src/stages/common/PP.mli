open Types
open Format

val name             : formatter -> expression_variable -> unit 
val type_variable    : formatter -> type_variable ->  unit
val constructor      : formatter -> constructor ->  unit
val label            : formatter -> label -> unit
val constant         : formatter -> constant -> unit 
val cmap_sep         : (formatter -> 'a -> unit) -> (formatter -> unit -> unit) -> formatter -> 'a CMap.t  -> unit
val lmap_sep         : (formatter -> 'a -> unit) -> (formatter -> unit -> unit) -> formatter -> 'a LMap.t  -> unit
val type_expression' : (formatter -> 'a -> unit) -> formatter -> 'a type_expression' -> unit
val type_operator    : (formatter -> 'a -> unit) -> formatter -> 'a type_operator -> unit
val type_constant    : formatter -> type_constant -> unit
val literal          : formatter -> literal -> unit
