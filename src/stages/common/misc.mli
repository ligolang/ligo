open Types

val map_type_operator : ('a -> 'b) -> 'a type_operator -> 'b type_operator
val bind_map_type_operator : ('a -> ('b * 'c list, 'd) Pervasives.result) -> 'a type_operator -> ('b type_operator * 'c list, 'd) Pervasives.result
val type_operator_name : 'a type_operator -> string
val type_expression'_of_string : string * 'a list -> ('a type_expression' * 'b list, 'c) Pervasives.result
val string_of_type_operator : 'a type_operator -> string * 'a list
val string_of_type_constant : type_constant -> string * 'a list
val string_of_type_expression' : 'a type_expression' -> string * 'a list
