open Trace
open Typer_common.Errors
open Worklist_and_pending

module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_typed.Types
open Typesystem.Solver_types

module M(Plugins : Plugins)(Solver : sig
    type plugin_states = Plugins.Indexers.PluginFields(PerPluginState).flds
    type nonrec typer_state = (typer_error, plugin_states) Typesystem.Solver_types.typer_state
  end) = struct

  open Solver

  let filter_already_added ((state : typer_state), type_constraint) =
    if PolySet.mem type_constraint state.added_constraints
    then ok (state, Worklist.empty)
    else ok ({ state with added_constraints = PolySet.add type_constraint state.added_constraints},
             { Worklist.empty with pending_filtered_not_already_added_constraints = Pending.singleton type_constraint })

  let simplify_constraint (state, new_constraint) =
    ok (state, { Worklist.empty with pending_type_constraint_simpl = Pending.of_list (Simplifier.type_constraint_simpl new_constraint) })

  let split_aliases (state, new_constraint) =
    match new_constraint with
      Ast_typed.Types.SC_Alias c_alias ->
      ok (state, { Worklist.empty with pending_c_alias = Pending.singleton c_alias })
    | non_alias ->
      ok (state, { Worklist.empty with pending_non_alias = Pending.singleton non_alias })

end
