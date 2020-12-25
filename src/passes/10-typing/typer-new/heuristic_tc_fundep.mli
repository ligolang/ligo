open Trace
open Typer_common.Errors
open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

val heuristic : <
    assignments              : type_variable Assignments.t ;
    grouped_by_variable      : type_variable GroupedByVariable.t ;
    refined_typeclasses      : type_variable RefinedTypeclasses.t ;
    typeclasses_constraining : type_variable TypeclassesConstraining.t ;
    by_constraint_identifier : type_variable ByConstraintIdentifier.t ;
  ..
> ex_heuristic_plugin

val restrict : constructor_or_row -> c_typeclass_simpl -> c_typeclass_simpl
val deduce_and_clean : c_typeclass_simpl -> (deduce_and_clean_result, typer_error) result
