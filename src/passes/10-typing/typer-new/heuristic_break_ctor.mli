open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

val heuristic : <
  grouped_by_variable : type_variable GroupedByVariable.t ;
  ..
> ex_heuristic_plugin

val selector_ : type_constraint_simpl -> type_variable GroupedByVariable.t -> output_break_ctor list