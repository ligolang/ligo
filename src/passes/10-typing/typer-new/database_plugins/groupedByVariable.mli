open Ast_typed.Types
include Plugin
val get_constraints_by_lhs : 'type_variable -> 'type_variable t -> constraints
val bindings : 'type_variable t -> ('type_variable * constraints) list