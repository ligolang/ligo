open Solver_types

(* data Indexers_plugins_fields (Ppt :: PerPluginType) = Indexers_plugins_fields {
     assignments       :: Ppt Assignments,
     groupedByVariable :: Ppt GroupedByVariable,
     …
   }
*)

module Assignments = Assignments.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module GroupedByVariable = GroupedByVariable.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module CycleDetectionTopologicalSort = CycleDetectionTopologicalSort.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module ByConstraintIdentifier = ByConstraintIdentifier.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
module TypeclassesConstraining = TypeclassesConstraining.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)

(* TODO: this is probably hidden by its signature somewhere? *)
module Indexers_plugins_fields = functor (Ppt : PerPluginType) -> struct
  type flds = <
    assignments                      : Ppt(Assignments).t ;
    grouped_by_variable              : Ppt(GroupedByVariable).t ;
    cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t ;
    by_constraint_identifier         : Ppt(ByConstraintIdentifier).t ;
    typeclasses_constraining         : Ppt(TypeclassesConstraining).t ;
  >

  module Assignments = Assignments
  let assignments flds = (flds :> <assignments:_>)
end

(* TODO: try removing this _ workaround *)
module Indexers_plugins_fields_ = Indexers_plugins_fields

(* mapPlugins :: (F : MappedFunction) → (Indexers_plugins_fields F.MakeIn) → (Indexers_plugins_fields F.MakeOut) *)
module Map_indexer_plugins = functor (F : Mapped_function) -> struct
  let f :
    F.extra_args ->
    Indexers_plugins_fields(F.MakeInType).flds ->
    Indexers_plugins_fields(F.MakeOutType).flds F.Monad.t
    = fun extra_args fieldsIn ->
      let module Let_syntax = F.Monad in
      let%bind assignments                      = (let module F = F.F(Assignments)                   in F.f "assign" extra_args fieldsIn#assignments)                      in
      let%bind grouped_by_variable              = (let module F = F.F(GroupedByVariable)             in F.f "g by v" extra_args fieldsIn#grouped_by_variable)              in
      let%bind cycle_detection_topological_sort = (let module F = F.F(CycleDetectionTopologicalSort) in F.f "c topo" extra_args fieldsIn#cycle_detection_topological_sort) in
      let%bind by_constraint_identifier         = (let module F = F.F(ByConstraintIdentifier)        in F.f "by  id" extra_args fieldsIn#by_constraint_identifier)         in
      let%bind typeclasses_constraining         = (let module F = F.F(TypeclassesConstraining)       in F.f "tc con" extra_args fieldsIn#typeclasses_constraining) ;       in
      F.Monad.return (object
        method assignments                      = assignments
        method grouped_by_variable              = grouped_by_variable
        method cycle_detection_topological_sort = cycle_detection_topological_sort
        method by_constraint_identifier         = by_constraint_identifier
        method typeclasses_constraining         = typeclasses_constraining
      end)
end

(* A value containing an empty (dummy) unit associated each plugin
   name This allows us to use `map' to discard this `unit' and
   e.g. initialize each plugin. *)
type indexers_plugins_fields_unit = Indexers_plugins_fields(PerPluginUnit).flds
let indexers_plugins_fields_unit : indexers_plugins_fields_unit = object
  method assignments                      = () ;
  method grouped_by_variable              = () ;
  method cycle_detection_topological_sort = () ;
  method by_constraint_identifier         = () ;
  method typeclasses_constraining         = () ;
end

module All_plugins = All_plugins
