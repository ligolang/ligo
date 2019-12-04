open Types
open Format

(*
val list_sep_d : ( formatter -> 'a -> unit ) -> formatter -> 'a list -> unit
val space_sep : formatter -> unit -> unit
val lr : formatter -> [< `Left ] -> unit
val type_base : formatter -> type_base -> unit 
*)

val type_variable : formatter -> type_value -> unit
val environment_element : formatter -> environment_element -> unit
val environment : formatter -> environment -> unit
val value : formatter -> value -> unit

(*
val value_assoc : formatter -> (value * value) -> unit
*)
val expression' : formatter -> expression' -> unit

val expression : formatter -> expression -> unit
val expression_with_type : formatter -> expression -> unit
val function_ : formatter -> anon_function -> unit

(*
val assignment : formatter -> assignment -> unit
*)
val declaration : formatter -> assignment -> unit
(*
val tl_statement : formatter -> assignment * 'a -> unit
*)
val program : formatter -> program -> unit
