open Ast_typed.Types
include Plugin
val get_typeclasses_constraining : 'typeVariable -> 'typeVariable t -> c_typeclass_simpl PolySet.t
val get_typeclasses_constraining_list : 'typeVariable -> 'typeVariable t -> c_typeclass_simpl list
val get_refined_typeclasses_constraining_list :
  'type_variable ->
  < refined_typeclasses : 'type_variable RefinedTypeclasses.t;
    typeclasses_constraining : 'type_variable t; .. > ->
  refined_typeclass list
