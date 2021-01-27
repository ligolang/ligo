open Trace
open Ast_typed.Types
module Set = RedBlackTrees.PolySet

type 'old_constraint_type selector_input = 'old_constraint_type (* some info about the constraint just added, so that we know what to look for *)
type 'selector_output selector_outputs = 'selector_output list
(* type ('old_contraint_type, 'selector_output) selector = 'old_constraint_type selector_input -> structured_dbs -> 'selector_output selector_outputs *)
type ('selector_output , 'errors) propagator = 'selector_output -> (type_variable -> type_variable) -> (updates, 'errors) result

type ('selector_output, -'flds) selector = type_constraint_simpl -> 'flds -> 'selector_output list

type ('selector_output, -'flds) heuristic_plugin = {
  (* Finds in the databases tuples of constraints which are
     interesting for this plugin and include the given
     type_constraint_simpl. *)
  heuristic_name : string ;
  selector     : ('selector_output, 'flds) selector ;
  (* Select in the databases tuples of constraints which are
     interesting and involve the given two type_viables, knowing that
     these are about to be merged. This function is called before the
     database's merge_aliases functions are called (i.e. the database
     does not reflect the effects of the merge yet). *)
  alias_selector : type_variable -> type_variable -> 'flds -> 'selector_output list ;
  propagator   : ('selector_output , Ast_typed.Typer_errors.typer_error) propagator ;
  (* called when two 'data are associated with the same type_constraint *)
  printer      : Format.formatter -> 'selector_output -> unit ;
  printer_json : 'selector_output -> Yojson.Safe.t ;
  comparator   : 'selector_output -> 'selector_output -> int ;
}

type ('selector_output, -'flds) heuristic_state = {
  plugin : ('selector_output, 'flds) heuristic_plugin ;
  already_selected : 'selector_output Set.t ;
}

type -'flds ex_heuristic_plugin =
    Heuristic_plugin : ('selector_output, 'flds) heuristic_plugin -> 'flds ex_heuristic_plugin

type -'flds ex_heuristic_state =
    Heuristic_state : ('selector_output, 'flds) heuristic_state -> 'flds ex_heuristic_state

type -'flds ex_heuristic_selector =
    Heuristic_selector: ('selector_output, 'flds) heuristic_state * 'selector_output selector_outputs -> 'flds ex_heuristic_selector

type 'flds heuristic_plugins = 'flds ex_heuristic_plugin list

type 'flds heuristic_states = 'flds ex_heuristic_state list

module type Plugins = sig
  module Indexers : IndexerPlugins
  val heuristics : Indexers.PluginFields(PerPluginState).flds heuristic_plugins
end

type ('errors, 'plugin_states) typer_state = {
  all_constraints                  : type_constraint_simpl PolySet.t ;
  added_constraints                : type_constraint PolySet.t ;
  aliases                          : type_variable UnionFind.Poly2.t ;
  plugin_states                    : 'plugin_states ;
  already_selected_and_propagators : 'plugin_states ex_heuristic_state list ;
}

open Format
open PP_helpers

let pp_already_selected = fun printer ppf set ->
  let lst = (RedBlackTrees.PolySet.elements set) in
    Format.fprintf ppf "Set [@,@[<hv 2> %a @]@,]" (list_sep printer (fun ppf () -> fprintf ppf " ;@ ")) lst

let pp_ex_propagator_state = fun ppf (Heuristic_state { plugin = { selector ; propagator ; printer ; printer_json=_ } ; already_selected }) ->
  ignore ( selector, propagator );
  Format.fprintf ppf "{ selector = (* OCaml function *); propagator = (* OCaml function *); already_selected = %a }"
  (pp_already_selected printer) already_selected

let json_already_selected = fun printer_json set : Yojson.Safe.t ->
  let lst = (RedBlackTrees.PolySet.elements set) in
let list f lst = `List (List.map f lst) in
    `List [`String "Set"; (list printer_json lst)]

let json_ex_propagator_state = fun (Heuristic_state { plugin = { selector; propagator; printer=_ ; printer_json } ; already_selected }) : Yojson.Safe.t ->
  ignore (selector,propagator);
  `Assoc[ ("selector", `String "OCaml function"); ("propagator", `String "OCaml function"); ("already_selected" ,          (json_already_selected printer_json) already_selected)]

(* TODO: remove lift_state_list_monad and lift: not needed after moving to plugin system *)
(* state+list monad *)
type ('state, 'elt) state_list_monad = { state: 'state ; list : 'elt list }
let lift_state_list_monad ~state ~list = { state ; list }
let lift f =
  fun { state ; list } ->
    let (new_state , new_lists) = List.fold_map_acc f state list in
    { state = new_state ; list = List.flatten new_lists }
