open Trace
module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_typed.Types
open Typesystem.Solver_types

(* TODO: move the propagator_heuristics list to a separate module which calls the solver with a bunch of heuristics *)
let propagator_heuristics =
  [
    Heuristic_break_ctor.heuristic ;
    Heuristic_specialize1.heuristic ;
  ]

let init_propagator_heuristic (Propagator_heuristic { selector ; propagator ; printer ; comparator ; initial_private_storage }) =
  Propagator_state { selector ; propagator ; printer ; already_selected = Set.create ~cmp:comparator ; private_storage = initial_private_storage }

let initial_state : typer_state = {
    structured_dbs =
      {
        all_constraints          = ([] : type_constraint_simpl list) ;
        aliases                  = UF.empty (fun s -> Format.asprintf "%a" Var.pp s) Var.compare;
        assignments              = (Map.create ~cmp:Var.compare : (type_variable, c_constructor_simpl) Map.t);
        grouped_by_variable      = (Map.create ~cmp:Var.compare : (type_variable,         constraints) Map.t);
        cycle_detection_toposort = ();
      } ;
    already_selected_and_propagators = List.map init_propagator_heuristic propagator_heuristics
  }

(* TODO : with our selectors, the selection depends on the order in which the constraints are added :-( :-( :-( :-(
   We need to return a lazy stream of constraints. *)

(* The order in which the propagators are applied to constraints is
   entirely accidental (dfs/bfs/something in-between). *)

(* sub-component: constraint selector (worklist / dynamic queries) *)
let select_and_propagate : 'old_input 'selector_output 'private_storage . ('old_input, 'selector_output, 'private_storage) selector -> ('selector_output , 'private_storage) propagator -> 'selector_output poly_set -> 'private_storage -> 'old_input -> structured_dbs -> 'selector_output poly_set * 'private_storage * new_constraints * new_assignments =
  fun selector propagator ->
  fun already_selected private_storage old_type_constraint dbs ->
  (* TODO: thread some state to know which selector outputs were already seen *)
  let private_storage , x = selector old_type_constraint private_storage dbs in
  match x with
    WasSelected selected_outputs ->
     let Set.{ set = already_selected ; duplicates = _ ; added = selected_outputs } = Set.add_list selected_outputs already_selected in
     (* Call the propagation rule *)
     let (private_storage, l) = List.fold_map_acc (fun private_storage selected -> let (a, b, c) = propagator private_storage dbs selected in (a, (b, c))) private_storage selected_outputs in
     let (new_constraints , new_assignments) = List.split l in
     (* return so that the new constraints are pushed to some kind of work queue and the new assignments stored *)
let () =
  (if Ast_typed.Debug.debug_new_typer && false then
   let s str = (fun ppf () -> Format.fprintf ppf str) in
   Format.printf "propagator produced\nnew_constraints = %a\nnew_assignments = %a\n"
     (PP_helpers.list_sep (PP_helpers.list_sep Ast_typed.PP_generic.type_constraint (s "\n")) (s "\n"))
     new_constraints
     (PP_helpers.list_sep (PP_helpers.list_sep Ast_typed.PP_generic.c_constructor_simpl (s "\n")) (s "\n"))
     new_assignments)
in
     (already_selected , private_storage , List.flatten new_constraints , List.flatten new_assignments)
  | WasNotSelected ->
     (already_selected, private_storage , [] , [])

let select_and_propagate_one new_constraint (new_states , new_constraints , dbs) (Propagator_state { selector; propagator; printer ; already_selected ; private_storage }) =
  let sel_propag = (select_and_propagate selector propagator) in
  let (already_selected , private_storage, new_constraints', new_assignments) = sel_propag already_selected private_storage new_constraint dbs in
  let assignments = List.fold_left (fun acc ({tv;c_tag=_;tv_list=_} as ele) -> Map.update tv (function None -> Some ele | x -> x) acc) dbs.assignments new_assignments in
  let dbs = { dbs with assignments } in
  Propagator_state { selector; propagator; printer ; already_selected ; private_storage } :: new_states, new_constraints' @ new_constraints, dbs

(* Takes a constraint, applies all selector+propagator pairs to it.
   Keeps track of which constraints have already been selected. *)
let select_and_propagate_all' : ex_propagator_state list -> type_constraint_simpl selector_input -> structured_dbs -> ex_propagator_state list * new_constraints * structured_dbs =
  fun already_selected_and_propagators new_constraint dbs ->
  List.fold_left
    (select_and_propagate_one new_constraint)
    ([], [] , dbs)
    already_selected_and_propagators

(* Takes a list of constraints, applies all selector+propagator pairs
   to each in turn. *)
let rec select_and_propagate_all : typer_state -> type_constraint selector_input list -> (typer_state,_) result =
  fun { already_selected_and_propagators ; structured_dbs } new_constraints ->
  match new_constraints with
  | [] -> ok { already_selected_and_propagators ; structured_dbs }
  | new_constraint :: tl ->
     let { state = dbs ; list = modified_constraints } = Normalizer.normalizers new_constraint structured_dbs in
     let (already_selected_and_propagators , new_constraints' , structured_dbs) =
       List.fold_left
         (fun (already_selected , nc , dbs) c ->
           let (already_selected , new_constraints' , dbs) = select_and_propagate_all' already_selected c dbs in
           (already_selected , new_constraints' @ nc , dbs))
         (already_selected_and_propagators , [] , dbs)
         modified_constraints in
     let new_constraints = new_constraints' @ tl in
     select_and_propagate_all { already_selected_and_propagators ; structured_dbs } new_constraints

(* This is the solver. *)
let aggregate_constraints = select_and_propagate_all



(* Later on, we'll ensure that all the heuristics register the
   existential/unification variables that they create, as well as the
   new constraints that they create. We will then check that they only
   use a small set of core axioms to derive new constraints, and
   produce traces justifying that instanciations satisfy all related
   constraints, and that all existential variables are instantiated
   (possibly by first generalizing the type and then using the
   polymorphic type argument to instantiate the existential). *)



(* This function is called when a program is fully compiled, and the
   typechecker's state is discarded. TODO: either get rid of the state
   earlier, or perform a sanity check here (e.g. that types have been
   inferred for all bindings and expressions, etc.

   Also, we should check at these places that we indeed do not need the
   state any further. Suzanne *)
let discard_state (_ : typer_state) = ()

let placeholder_for_state_of_new_typer () = initial_state
