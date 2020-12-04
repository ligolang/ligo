open Ast_typed.Types
include Plugin
val get_typeclasses_constraining : 'typeVariable -> 'typeVariable t -> Ast_typed.Types.constraint_identifier_set
