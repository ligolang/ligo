open Ast_typed.Types
include Plugin
val find_opt : 'type_variable -> 'type_variable t -> constructor_or_row option
val bindings : 'type_variable t -> ('type_variable * constructor_or_row) list
