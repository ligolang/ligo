open Database_plugins.All_plugins
open Ast_typed.Types
open Typesystem.Solver_types

type selector_output = {
  a_k_var : c_row_simpl ;
  a_var_l : c_access_label_simpl ;
}

val heuristic : <
  grouped_by_variable : type_variable GroupedByVariable.t ;
  ..
> ex_heuristic_plugin
