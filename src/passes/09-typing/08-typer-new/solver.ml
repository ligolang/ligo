open Trace
open Typer_common.Errors
module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_typed.Types
open Typesystem.Solver_types

type nonrec 'a result = ('a, typer_error) result

(* TODO: move the propagator_heuristics list to a separate module which calls the solver with a bunch of heuristics *)
let propagator_heuristics =
  [
    Heuristic_break_ctor.heuristic ;
    Heuristic_specialize1.heuristic ;
  ]

let init_propagator_heuristic (Propagator_heuristic { selector ; propagator ; printer ; comparator ; initial_private_storage }) =
  Propagator_state { selector ; propagator ; printer ; already_selected = Set.create ~cmp:comparator ; private_storage = initial_private_storage }

let initial_state : _ typer_state = {
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
let select_and_propagate : 'old_input 'selector_output 'private_storage . ('old_input, 'selector_output, 'private_storage) selector -> ('selector_output , 'private_storage, typer_error) propagator -> 'selector_output poly_set -> 'private_storage -> 'old_input -> structured_dbs -> ('selector_output poly_set * 'private_storage * new_constraints) result =
  fun selector propagator ->
  fun already_selected private_storage old_type_constraint dbs ->
  (* TODO: thread some state to know which selector outputs were already seen *)
  let private_storage , x = selector old_type_constraint private_storage dbs in
  match x with
    WasSelected selected_outputs ->
    let Set.{ set = already_selected ; duplicates = _ ; added = selected_outputs } = Set.add_list selected_outputs already_selected in
    (* Call the propagation rule *)
    let%bind (private_storage, new_constraints) = bind_fold_map_list (fun private_storage selected -> propagator private_storage dbs selected) private_storage selected_outputs in
    (* return so that the new constraints are pushed to some kind of work queue *)
    let () =
      if Ast_typed.Debug.debug_new_typer && false then
      let s str = (fun ppf () -> Format.fprintf ppf str) in
      Format.printf "propagator produced\nnew_constraints = %a\n"
        (PP_helpers.list_sep (PP_helpers.list_sep Ast_typed.PP_generic.type_constraint (s "\n")) (s "\n"))
        new_constraints
    in
    ok (already_selected , private_storage , List.flatten new_constraints)
  | WasNotSelected ->
    ok (already_selected, private_storage , [])

let select_and_propagate_one new_constraint (new_states , new_constraints , dbs) (Propagator_state { selector; propagator; printer ; already_selected ; private_storage }) =
  let sel_propag = (select_and_propagate selector propagator) in
  let%bind (already_selected , private_storage, new_constraints') = sel_propag already_selected private_storage new_constraint dbs in
  ok @@ (Propagator_state { selector; propagator; printer ; already_selected ; private_storage } :: new_states, new_constraints' @ new_constraints, dbs)

(* Takes a constraint, applies all selector+propagator pairs to it.
   Keeps track of which constraints have already been selected. *)
let select_and_propagate_all' : typer_error ex_propagator_state list -> type_constraint_simpl selector_input -> structured_dbs -> (typer_error ex_propagator_state list * new_constraints * structured_dbs) result =
  fun already_selected_and_propagators new_constraint dbs ->
  bind_fold_list
    (select_and_propagate_one new_constraint)
    ([], [] , dbs)
    already_selected_and_propagators

(* Takes a list of constraints, applies all selector+propagator pairs
   to each in turn. *)
let rec select_and_propagate_all : _ typer_state -> type_constraint selector_input list -> _ typer_state result =
  fun { already_selected_and_propagators ; structured_dbs } new_constraints ->
  match new_constraints with
  | [] -> ok { already_selected_and_propagators ; structured_dbs }
  | new_constraint :: tl ->
     let { state = dbs ; list = modified_constraints } = Normalizer.normalizers new_constraint structured_dbs in
     let%bind (already_selected_and_propagators , new_constraints' , structured_dbs) =
       bind_fold_list
         (fun (already_selected , nc , dbs) c ->
           let%bind (already_selected , new_constraints' , dbs) = select_and_propagate_all' already_selected c dbs in
           ok (already_selected , new_constraints' @ nc , dbs))
         (already_selected_and_propagators , [] , dbs)
         modified_constraints in
     (* DFS *)
     (* TODO: find a way to treat the constraints in a more appropriate order *)
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
let discard_state (_ : _ typer_state) = ()

let placeholder_for_state_of_new_typer () = initial_state
