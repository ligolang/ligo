open Test_helpers
(* open Trace *)

(* module Core = Typesystem.Core *)
(* open Ast_typed.Types *)
(* open Ast_typed.Reasons *)
(* open Ast_typed.Combinators *)
(* open Database_plugins.All_plugins *)
(* module Assignments                   = Assignments
module GroupedByVariable             = GroupedByVariable
module CycleDetectionTopologicalSort = CycleDetectionTopologicalSort
module ByConstraintIdentifier        = ByConstraintIdentifier
module RefinedTypeclasses            = RefinedTypeclasses
module TypeclassesConstraining       = TypeclassesConstraining *)

(* open Db_index_tests_common *)
open Db_index_assignment_tests
open Db_index_grouped_by_variable_tests
open Db_index_cycle_detection_topological_sort_tests

let main =
  test_suite "Indexers" @@
    [
      test "assignments" assignments ;
      test "grouped by variable" grouped_by_variable ;
      test "cycle detection topological sort" cycle_detection_topological_sort ;
      (* test "cycle detection topological sort" cycle_detection_topological_sort ; *)
      test "invariant" invariant ; (* TODO : do this for all this plugins (?) *)
    ]
