open Trace
open Typer_common.Errors
module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_typed.Types
open Typesystem.Solver_types
open UnionFind

(* Function which merges all aliases withing a single plugin's state *)
module MergeAliases = struct
  type extra_args = type_variable UnionFind.Poly2.changed_reprs
  module MakeInType = PerPluginState
  module MakeOutType = PerPluginState
  module Monad = NoMonad
  module F(Plugin : Plugin) = struct
    let f UnionFind.Poly2.{ demoted_repr ; new_repr } state =
      let merge_keys = {
        map = (fun m -> ReprMap.alias ~demoted_repr ~new_repr m);
        set = (fun s -> ReprSet.alias ~demoted_repr ~new_repr s);
        (* var = (fun a -> if Var.compare a demoted_repr = 0 then new_repr else a) *)
      }
      in Plugin.merge_aliases merge_keys state
  end
end

(* Function which adds a constraint to single indexer plugin's state *)
module AddConstraint = struct
  type extra_args = (type_variable -> type_variable) * type_constraint_simpl
  module MakeInType = PerPluginState
  module MakeOutType = PerPluginState
  module Monad = NoMonad
  module F(Plugin : Plugin) = struct
    let f (repr, constraint_) state =
      Plugin.add_constraint repr state constraint_
  end
end

module Typer_error_trace_monad = struct
  type 'a t = ('a, typer_error) Trace.result
  let bind x ~f = Trace.bind f x
  let return x = Trace.ok x
end

(* Function which merges all aliases withing a single plugin's state *)
module RemoveConstraint = struct
  (* TODO: check this API to see if we're giving too much flexibility to the plugin *)
  type extra_args = ((type_variable -> type_variable) * type_constraint_simpl)
  module MakeInType = PerPluginState
  module MakeOutType = PerPluginState
  module Monad = Typer_error_trace_monad
  module F(Plugin : Plugin) = struct
    let f (repr, to_remove) state =
      Plugin.remove_constraint repr state to_remove
  end
end

(* Function which creates a plugin's initial state *)
module CreateState = struct
  type extra_args = unit
  module MakeInType = PerPluginUnit
  module MakeOutType = PerPluginState
  module Monad = NoMonad
  module F(Plugin : Plugin) = struct
    let f () (() as _state) = Plugin.create_state ~cmp:Ast_typed.Compare.type_variable
  end
end

type nonrec 'a result = ('a, typer_error) Simple_utils.Trace.result

let simplify_constraint : type_constraint -> type_constraint_simpl list =
  fun new_constraint ->
  Simplifier.type_constraint_simpl new_constraint

let rec until predicate f state = if predicate state then ok state else let%bind state = f state in until predicate f state

(* library function *)
let rec worklist : ('state -> 'element -> ('state * 'element list) result) -> 'state -> 'element list -> 'state result =
  fun f state elements ->
  match elements with
    [] -> ok state
  | element :: rest ->
    let%bind new_state, new_elements = f state element in
    worklist f new_state (new_elements @ rest)

let init_propagator_heuristic (Heuristic_plugin plugin) =
  Heuristic_state { plugin; already_selected = Set.create ~cmp:plugin.comparator }
