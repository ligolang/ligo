open Ast_typed.Types
include Plugin
val find_opt : constraint_identifier -> 'typeVariable t -> refined_typeclass option
val find : c_typeclass_simpl -> 'typeVariable t -> refined_typeclass
(* TODO: this shouldn't be exposed, just a WIP while refactoring. *)
val values : 'typeVariable t -> refined_typeclass list
