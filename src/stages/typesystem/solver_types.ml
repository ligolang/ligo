open Ast_typed.Types
module Set = RedBlackTrees.PolySet

type 'old_constraint_type selector_input = 'old_constraint_type (* some info about the constraint just added, so that we know what to look for *)
type 'selector_output selector_outputs =
    WasSelected of 'selector_output list
  | WasNotSelected
type new_constraints = type_constraint list
type new_assignments = c_constructor_simpl list
type ('old_constraint_type, 'selector_output) selector = 'old_constraint_type selector_input -> structured_dbs -> 'selector_output selector_outputs
type 'selector_output propagator = 'selector_output -> structured_dbs -> new_constraints * new_assignments
type ('old_constraint_type , 'selector_output ) propagator_heuristic = {
  selector               : ('old_constraint_type, 'selector_output) selector ;
  propagator             : 'selector_output propagator ;
  empty_already_selected : 'selector_output Set.t;
}

type ex_propagator_heuristic =
  (* For now only support a single type of input, make this polymorphic as needed. *)
  | Propagator_heuristic : (type_constraint_simpl, 'selector_output) propagator_heuristic -> ex_propagator_heuristic

type typer_state = {
  structured_dbs                   : structured_dbs   ;
  already_selected_and_propagators : ex_propagator_heuristic list ;
}

(* state+list monad *)
type ('state, 'elt) state_list_monad = { state: 'state ; list : 'elt list }
let lift_state_list_monad ~state ~list = { state ; list }
let lift f =
  fun { state ; list } ->
    let (new_state , new_lists) = List.fold_map_acc f state list in
    { state = new_state ; list = List.flatten new_lists }

