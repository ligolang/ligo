open Ast_typed.Types
include Plugin
val find_opt : 'type_variable -> 'type_variable t -> c_constructor_simpl option
