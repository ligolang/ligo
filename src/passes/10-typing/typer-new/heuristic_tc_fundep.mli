open Trace
open Typer_common.Errors
open Database_plugins.All_plugins
open Ast_typed.Types
open Solver_types

val heuristic : <
    assignments              : type_variable Assignments.t ;
    grouped_by_variable      : type_variable Grouped_by_variable.t ;
    typeclasses_constraining : type_variable Typeclasses_constraining.t ;
    by_constraint_identifier : type_variable By_constraint_identifier.t ;
  ..
> ex_heuristic_plugin

val restrict : (type_variable -> type_variable) -> constructor_or_row -> c_typeclass_simpl -> c_typeclass_simpl

type deduce_and_clean_result = {
  deduced : c_constructor_simpl list ;
  cleaned : c_typeclass_simpl ;
}
val deduce_and_clean : (type_variable -> type_variable) -> c_typeclass_simpl -> (deduce_and_clean_result, typer_error) result

val pp_deduce_and_clean_result : Format.formatter -> deduce_and_clean_result -> unit
