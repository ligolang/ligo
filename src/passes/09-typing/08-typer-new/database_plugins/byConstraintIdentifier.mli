open Ast_typed.Types
include Plugin
val find_opt : constraint_identifier -> 'type_variable t -> c_typeclass_simpl option
