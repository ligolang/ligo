open Trace
module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_typed.Types
open Solver_types

(* sub-sub component: lazy selector (don't re-try all selectors every time)
 * For now: just re-try everytime *)

(* TODO : with our selectors, the selection depends on the order in which the constraints are added :-( :-( :-( :-(
   We need to return a lazy stream of constraints. *)

let select_and_propagate : ('old_input, 'selector_output) selector -> _ propagator -> _ -> 'a -> structured_dbs -> _ * new_constraints * new_assignments =
  fun selector propagator ->
  fun already_selected old_type_constraint dbs ->
  (* TODO: thread some state to know which selector outputs were already seen *)
  match selector old_type_constraint dbs with
    WasSelected selected_outputs ->
     let Set.{ set = already_selected ; duplicates = _ ; added = selected_outputs } = Set.add_list selected_outputs already_selected in
     (* Call the propagation rule *)
     let new_contraints_and_assignments = List.map (fun s -> propagator s dbs) selected_outputs in
     let (new_constraints , new_assignments) = List.split new_contraints_and_assignments in
     (* return so that the new constraints are pushed to some kind of work queue and the new assignments stored *)
     (already_selected , List.flatten new_constraints , List.flatten new_assignments)
  | WasNotSelected ->
     (already_selected, [] , [])

(* TODO: put the heuristics with their state in a list. *)
let select_and_propagate_break_ctor = select_and_propagate Heuristic_break_ctor.selector Heuristic_break_ctor.propagator
let select_and_propagate_specialize1 = select_and_propagate Heuristic_specialize1.selector Heuristic_specialize1.propagator

(* Takes a constraint, applies all selector+propagator pairs to it.
   Keeps track of which constraints have already been selected. *)
let select_and_propagate_all' : _ -> type_constraint_simpl selector_input -> structured_dbs -> _ * new_constraints * structured_dbs =
  let aux sel_propag new_constraint (already_selected , new_constraints , dbs) =
    let (already_selected , new_constraints', new_assignments) = sel_propag already_selected new_constraint dbs in
    let assignments = List.fold_left (fun acc ({tv;c_tag=_;tv_list=_} as ele) -> Map.update tv (function None -> Some ele | x -> x) acc) dbs.assignments new_assignments in
    let dbs = { dbs with assignments } in
    (already_selected , new_constraints' @ new_constraints , dbs)
  in
  fun already_selected new_constraint dbs ->
    (* The order in which the propagators are applied to constraints
       is entirely accidental (dfs/bfs/something in-between). *)
    let (already_selected , new_constraints , dbs) = (already_selected , [] , dbs) in

    (* We must have a different already_selected for each selector,
       so this is more verbose than a few uses of `aux'. *)
    let (already_selected' , new_constraints , dbs) = aux select_and_propagate_break_ctor new_constraint (already_selected.break_ctor , new_constraints , dbs) in
    let (already_selected , new_constraints , dbs) = ({already_selected with break_ctor = already_selected'}, new_constraints , dbs) in

    let (already_selected' , new_constraints , dbs) = aux select_and_propagate_specialize1 new_constraint (already_selected.specialize1 , new_constraints , dbs) in
    let (already_selected , new_constraints , dbs) = ({already_selected with specialize1 = already_selected'}, new_constraints , dbs) in

    (already_selected , new_constraints , dbs)

(* Takes a list of constraints, applies all selector+propagator pairs
   to each in turn. *)
let rec select_and_propagate_all : _ -> type_constraint selector_input list -> structured_dbs -> _ * structured_dbs =
  fun already_selected new_constraints dbs ->
    match new_constraints with
    | [] -> (already_selected, dbs)
    | new_constraint :: tl ->
      let { state = dbs ; list = modified_constraints } = Normalizer.normalizers new_constraint dbs in
      let (already_selected , new_constraints' , dbs) =
        List.fold_left
          (fun (already_selected , nc , dbs) c ->
             let (already_selected , new_constraints' , dbs) = select_and_propagate_all' already_selected c dbs in
             (already_selected , new_constraints' @ nc , dbs))
          (already_selected , [] , dbs)
          modified_constraints in
      let new_constraints = new_constraints' @ tl in
      select_and_propagate_all already_selected new_constraints dbs

(* sub-component: constraint selector (worklist / dynamic queries) *)

(* constraint propagation: (buch of constraints) → (new constraints * assignments) *)

(* Below is a draft *)

let initial_state : typer_state = {
    structured_dbs =
      {
        all_constraints          = ([] : type_constraint_simpl list) ;
        aliases                  = UF.empty (fun s -> Format.asprintf "%a" Var.pp s) Var.compare;
        assignments              = (Map.create ~cmp:Var.compare : (type_variable, c_constructor_simpl) Map.t);
        grouped_by_variable      = (Map.create ~cmp:Var.compare : (type_variable,         constraints) Map.t);
        cycle_detection_toposort = ();
      } ;
    already_selected = {
        break_ctor = Set.create ~cmp:Solver_should_be_generated.compare_output_break_ctor;
        specialize1 = Set.create ~cmp:Solver_should_be_generated.compare_output_specialize1 ;
      }
  }

(* This function is called when a program is fully compiled, and the
   typechecker's state is discarded. TODO: either get rid of the state
   earlier, or perform a sanity check here (e.g. that types have been
   inferred for all bindings and expressions, etc.

   Also, we should check at these places that we indeed do not need the
   state any further. Suzanne *)
let discard_state (_ : typer_state) = ()

(* This is the solver *)
let aggregate_constraints : typer_state -> type_constraint list -> typer_state result = fun state newc ->
  (* TODO: Iterate over constraints *)
  let _todo = ignore (state, newc) in
  let (a, b) = select_and_propagate_all state.already_selected newc state.structured_dbs in
  ok { already_selected = a ; structured_dbs = b }
(*let { constraints ; eqv } = state in
  ok { constraints = constraints @ newc ; eqv }*)

(* Later on, we'll ensure that all the heuristics register the
   existential/unification variables that they create, as well as the
   new constraints that they create. We will then check that they only
   use a small set of core axioms to derive new constraints, and
   produce traces justifying that instanciations satisfy all related
   constraints, and that all existential variables are instantiated
   (possibly by first generalizing the type and then using the
   polymorphic type argument to instantiate the existential). *)

let placeholder_for_state_of_new_typer () = initial_state
