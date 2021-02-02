open Typesystem.Solver_types
open Ast_typed.Types

module Indexers = Database_plugins
let heuristics : Indexers.PluginFields(PerPluginState).flds heuristic_plugins = [
  Heuristic_break_ctor.heuristic ;
  Heuristic_access_label.heuristic ;
  Heuristic_specialize1.heuristic ;
  Heuristic_tc_fundep.heuristic ;
]
