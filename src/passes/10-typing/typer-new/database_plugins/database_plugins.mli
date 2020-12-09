open Ast_typed.Types

module PluginFields_ (Ppt : PerPluginType) : sig
  type flds = <
    assignments                      : Ppt(Assignments).t ;
    grouped_by_variable              : Ppt(GroupedByVariable).t ;
    cycle_detection_topological_sort : Ppt(CycleDetectionTopologicalSort).t ;
    by_constraint_identifier         : Ppt(ByConstraintIdentifier).t ;
    refined_typeclasses              : Ppt(RefinedTypeclasses).t ;
    typeclasses_constraining         : Ppt(TypeclassesConstraining).t ;
  >

  module Assignments : sig
    type 'typeVariable t
    val find_opt : 'type_variable -> 'type_variable t -> constructor_or_row option
    val bindings : 'type_variable t -> ('type_variable * constructor_or_row) list
  end
  val assignments : flds -> < assignments : Ppt(Assignments).t >
end

include Ast_typed.Types.IndexerPlugins
  (* TODO: do we need this & the definition above? *)
  with module PluginFields = PluginFields_


(* OCaml/dune hide the contents of a folder unless they are
   re-exported… this is just to be able to access the modules from
   outside. This has nothing to do with the plugin architecture. *)
module All_plugins = All_plugins
