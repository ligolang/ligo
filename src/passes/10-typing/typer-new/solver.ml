open Trace
open Typer_common.Errors
module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_typed.Types
open Solver_types
open Solver_helpers
open Proof_trace_checker

(* open Pretty_print_variables *)
module Formatt = Format
module SRope = Rope.SimpleRope

open Worklist_and_pending

let logfile = stderr (* open_out "/tmp/typer_log" *)

(*  ………………………………………………………………………………………………… Plugin-based solver below ………………………………………………………………………………………………… *)

(* Later on, we'll ensure that all the heuristics register the
   existential/unification variables that they create, as well as the
   new constraints that they create. We will then check that they only
   use a small set of core axioms to derive new constraints, and
   produce traces justifying that instanciations satisfy all related
   constraints, and that all existential variables are instantiated
   (possibly by first generalizing the type and then using the
   polymorphic type argument to instantiate the existential). *)

module Make_solver(Plugins : Plugins) : sig
  type indexers_plugins_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState).flds
  type nonrec typer_state = indexers_plugins_states typer_state
  val pp_typer_state  : Format.formatter -> typer_state -> unit
  val get_alias : Ast_typed.type_variable -> type_variable poly_unionfind -> (type_variable, typer_error) Trace.result
  val main : typer_state -> type_constraint list -> typer_state result
  val initial_state : typer_state
  val placeholder_for_state_of_new_typer : unit -> typer_state
  val discard_state : typer_state -> unit
end = struct
  module Indexers_plugins_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState)
  type indexers_plugins_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState).flds
  type nonrec typer_state = indexers_plugins_states Solver_types.typer_state

  module Solver_stages' = Solver_stages.M(Plugins)(struct type nonrec typer_state = typer_state type nonrec plugin_states = indexers_plugins_states end)
  open Solver_stages'

  type indexers_plugins_fields_unit = Plugins.Indexers.Indexers_plugins_fields(PerPluginUnit).flds
  let indexers_plugins_fields_unit : indexers_plugins_fields_unit = Plugins.Indexers.indexers_plugins_fields_unit

  let mk_repr state x = UnionFind.Poly2.repr x state.aliases

  let pp_typer_state = fun ppf ({ all_constraints; plugin_states; aliases ; already_selected_and_propagators } : typer_state) ->
    let open Solver_types in
    let open PP_helpers in
    let module MapPP = Plugins.Indexers.Map_indexer_plugins(PPPlugin) in
    let pp_indexers ppf states =
      Formatt.fprintf ppf "@[ <@ %a @ > @]" (fun ppf states -> let _ : indexers_plugins_fields_unit = MapPP.f ppf states in ()) states
    in
    Formatt.fprintf ppf "{@[<hv 2> @ all_constaints = %a;@ plugin_states = %a ;@ aliases = %a ;@ already_selected_and_propagators = %a @]@ }"
      (RedBlackTrees.PolySet.pp Ast_typed.PP.type_constraint_simpl_short) all_constraints
      pp_indexers plugin_states
      (UnionFind.Poly2.pp Ast_typed.PP.type_variable) aliases
      (list_sep pp_ex_propagator_state (fun ppf () -> Formatt.fprintf ppf " ;@ ")) already_selected_and_propagators

  let aux_remove state to_remove =
    (* let () = Formatt.printf "Remove constraint :\n  %a\n\n%!" Ast_typed.PP.type_constraint_simpl_short to_remove in *)
    (* let () = Formatt.printf "and state:%a\n" pp_typer_state state in *)
    let module MapRemoveConstraint = Plugins.Indexers.Map_indexer_plugins(RemoveConstraint) in
    let%bind plugin_states = MapRemoveConstraint.f (mk_repr state, to_remove) state.plugin_states in
    ok {state with plugin_states ; deleted_constraints = PolySet.add to_remove state.deleted_constraints}

  let aux_update state { remove_constraints; add_constraints; proof_trace } =
    let%bind () = check_proof_trace proof_trace in
    let%bind state = bind_fold_list aux_remove state remove_constraints in
    Format.printf "Returning from aux_update\n%!" ;
    ok (state, add_constraints)

  let aux_propagator heuristic state selector_output =
    (* TODO: before applying a propagator, check if it does
       not depend on constraints which were removed by the
       previous propagator *)
    let referenced_constraints = heuristic.plugin.get_referenced_constraints selector_output in
    let uses_deleted_constraints = List.exists (fun c -> (PolySet.mem c state.deleted_constraints)) referenced_constraints in
    if uses_deleted_constraints then
      ok (state, [])
    else
      let%bind updates = heuristic.plugin.propagator selector_output (mk_repr state) in
      let%bind (state, new_constraints) = bind_fold_map_list aux_update state updates in
      ok (state, List.flatten new_constraints)

  let aux_selector_alias demoted_repr new_repr state (Heuristic_state heuristic) =
    let selector_outputs = heuristic.plugin.alias_selector demoted_repr new_repr state.plugin_states in
    let aux = fun (l,already_selected) el ->
      if PolySet.mem el already_selected then (l,already_selected)
      else (el::l, PolySet.add el already_selected)
    in
    let selector_outputs,already_selected = List.fold_left aux ([], heuristic.already_selected) selector_outputs in
    let heuristic = { heuristic with already_selected } in
    Heuristic_selector (heuristic, selector_outputs)

  let aux_propagator_alias state (Heuristic_selector (heuristic, selector_outputs)) =
    let%bind (state, new_constraints) = bind_fold_map_list (aux_propagator heuristic) state selector_outputs in
    (* let () = queue_print (fun () -> Format.printf "Return with new constraints: (%a)\n%!" Ast_typed.PP.(list_sep_d (list_sep_d type_constraint_short)) new_constraints) in *)
    ok (state, (Heuristic_state heuristic, List.flatten new_constraints))

  let add_alias : (typer_state * c_alias) -> (typer_state * Worklist.t) result =
    fun (state , ({ reason_alias_simpl=_; a; b } as new_constraint)) ->
      (* let () = Format.printf "Add_alias %a=%a\n%!" Ast_typed.PP.type_variable a Ast_typed.PP.type_variable b in *)


      let { all_constraints ; added_constraints ; deleted_constraints; plugin_states ; aliases ; already_selected_and_propagators } = state in
      
      (* get the changed reprs due to that alias constraint *)
      let UnionFind.Poly2.{ partition = aliases; changed_reprs } =
        UnionFind.Poly2.equiv a b aliases in
      (* let () = Format.printf "changed_reprs :(%a)\n%!" Ast_typed.PP.(fun ppf ({demoted_repr=a;new_repr=b}: _ UnionFind.Poly2.changed_reprs) -> Format.fprintf ppf "%a -> %a" type_variable a type_variable b) changed_reprs in *)
      (* let () = Format.printf "New_aliases :%a\n%!" (UnionFind.Poly2.pp Ast_typed.PP.type_variable) aliases in *)

      (*apply heuristics' selector*)
      let selected = List.map (aux_selector_alias UnionFind.Poly2.(changed_reprs.demoted_repr) UnionFind.Poly2.(changed_reprs.new_repr) state) already_selected_and_propagators in

      (* Add alias constraint to the set of all constraints *)
      let all_constraints = PolySet.add (SC_Alias new_constraint) all_constraints in

      let plugin_states =
        let module MapMergeAliases = Plugins.Indexers.Map_indexer_plugins(MergeAliases) in
        MapMergeAliases.f changed_reprs plugin_states in

      let state = { all_constraints ; added_constraints ; deleted_constraints; plugin_states ; aliases ; already_selected_and_propagators } in

      (* apply all the alias_selectors and propagators given the new alias *)
      let%bind (state, new_constraints) = (
        if true then
          (* TODO: possible bug: here, should we use the demoted_repr
             and new_repr, or the ones as given by the alias? We should
             maintain as much as possible the illusion that aliased
             variables have always been aliased from the start,
             therefore if the alias constraint contains outdated
             references, it makes sense to update them before calling
             the .alias_selector functions. *)
          let%bind (state, hc) = bind_fold_map_list (aux_propagator_alias) state selected in
          let (already_selected_and_propagators, new_constraints) = List.split hc in
          let state = { state with already_selected_and_propagators } in
          ok (state, List.flatten new_constraints)
        else
          ok (state, [])
      ) in
      (* let () = Format.printf "End of add alias\n" in *)
      ok (state,{ Worklist.empty with pending_type_constraint = Pending.of_list new_constraints})
    

  let get_alias variable aliases =
    trace_option (corner_case (Format.asprintf "can't find alias root of variable %a" Var.pp variable)) @@
    (* TODO: after upgrading UnionFind, this will be an option, not an exception. *)
    try Some (UF.repr variable aliases) with Not_found -> None

  let aux_heuristic repr constraint_ state (Heuristic_state heuristic) =
    (* let () = queue_print (fun () -> Formatt.printf "Apply heuristic %s for constraint : %a\n%!" 
      heuristic.plugin.heuristic_name
      PP.type_constraint_ constraint_ in *)
    let selector_outputs = heuristic.plugin.selector repr constraint_ state.plugin_states in
    let aux = fun (l,already_selected) el ->
      if PolySet.mem el already_selected then (l,already_selected)
      else (el::l, PolySet.add el already_selected)
    in
    let selector_outputs,already_selected = List.fold_left aux ([], heuristic.already_selected) selector_outputs in
    let heuristic = { heuristic with already_selected } in
    let%bind (state, new_constraints) = bind_fold_map_list (aux_propagator heuristic) state selector_outputs in
    (* let () = queue_print (fun () -> Format.printf "Return with new constraints: (%a)\n%!" Ast_typed.PP.(list_sep_d (list_sep_d type_constraint_short)) new_constraints) in *)
    ok (state, (Heuristic_state heuristic, List.flatten new_constraints))

  (* apply all the selectors and propagators *)
  let add_constraint_and_apply_heuristics (state , constraint_) =
    (* let () = queue_print (fun () -> Format.printf "Add constraint and apply heuristics for constraint: %a\n%!" Ast_typed.PP.type_constraint_simpl constraint_) in *)
    if PolySet.mem constraint_ state.all_constraints then ok (state, Worklist.empty)
    else
      let repr = mk_repr state in
      let state =
        let module MapAddConstraint = Plugins.Indexers.Map_indexer_plugins(AddConstraint) in
        { state with plugin_states = MapAddConstraint.f (repr, constraint_) state.plugin_states }
      in
      let%bind (state, hc) = bind_fold_map_list (aux_heuristic repr constraint_) state state.already_selected_and_propagators in
      let (already_selected_and_propagators, new_constraints) = List.split hc in
      let state = { state with already_selected_and_propagators } in
      ok (state, { Worklist.empty with pending_type_constraint = Pending.of_list @@ List.flatten new_constraints })

  let pp_indented_constraint_list =
    let open PP_helpers in
    let open Ast_typed.PP in
    (list_sep type_constraint_short (tag "\n  "))

  let pp_indented_constraint_simpl_list =
    let open PP_helpers in
    let open Ast_typed.PP in
    (list_sep type_constraint_simpl_short (tag "\n  "))
  let _ = pp_indented_constraint_list, pp_indented_constraint_simpl_list (* unused warning *)

  (* Takes a list of constraints, applies all selector+propagator pairs
     to each in turn. *)
  let select_and_propagate_all : typer_state -> Worklist.t -> typer_state result =
    fun state initial_constraints ->
    (* To change the order in which the constraints are processed, modify this loop. *)
    let () = Formatt.printf "In select and propagate all\n" in
    let time_to_live = ref 10000 in
    until'
      (* repeat until the worklist is empty *)
      (Worklist.is_empty ~time_to_live)
      (fun (state, worklist) ->
         let () = Formatt.printf "\nStart iteration with new constraints :\n  %a\n" pp_indented_constraint_list (Pending.to_list worklist.pending_type_constraint) in
         (* let () = Formatt.printf "and state: %a\n" pp_typer_state state in *)

         (* let () = queue_print (fun () -> Formatt.printf "Start iteration with constraints :\n  %a\n\n" pp_indented_constraint_list (Pending.to_list worklist.pending_type_constraint)) in *)

         (* The worklist monad changes let%bind so that it executes
            the "in" part only if the bound expression left the
            worklist unchanged. In other words, processing stops at
            the first handler which does some work. *)

         choose_processor [
           (fun (state, worklist) ->
              Worklist.process_all ~time_to_live
                pending_type_constraint
                filter_already_added
                (state, worklist)
           );

           (fun (state, worklist) ->
              Worklist.process_all ~time_to_live
                pending_filtered_not_already_added_constraints
                simplify_constraint
                (state, worklist)
           );

           (fun (state, worklist) ->
              Worklist.process_all ~time_to_live
                pending_type_constraint_simpl
                split_aliases
                (state, worklist)
           );

           (fun (state, worklist) ->
              Worklist.process_all ~time_to_live
                pending_c_alias
                add_alias
                (state, worklist)
           );

           (fun (state, worklist) ->
              Worklist.process_all ~time_to_live
                pending_non_alias
                add_constraint_and_apply_heuristics
                (state, worklist)
           );
         ]
           (state, worklist)
      )

      (state, initial_constraints)
    >>|? fst
  (* already_selected_and_propagators ; all_constraints ; plugin_states ; aliases *)

  module All_vars = Typecheck_utils.All_vars(Plugins)
  let main : typer_state -> type_constraint list -> typer_state result =
    fun state initial_constraints ->
    let () = Formatt.printf "In solver main\n%!" in
    let%bind (state : typer_state) = select_and_propagate_all state {Worklist.empty with pending_type_constraint = Pending.of_list initial_constraints} in
    let () = Formatt.printf "Starting typechecking with assignment :\n  %a\n%!"
      pp_typer_state state in
    let failure = Typecheck.check (PolySet.elements state.all_constraints)
      (All_vars.all_vars state)
      (fun v -> UnionFind.Poly2.repr v state.aliases)
      (fun v -> Indexers_plugins_states.Assignments.find_opt v (Indexers_plugins_states.assignments state.plugin_states)#assignments) in
    let () = if not @@ Trace.to_bool failure then Pretty_print_variables.flush_pending_print state in
    let%bind () = failure in
    ok state
  
  (* This function is called when a program is fully compiled, and the
     typechecker's state is discarded. TODO: either get rid of the state
     earlier, or perform a sanity check here (e.g. that types have been
     inferred for all bindings and expressions, etc.

     Also, we should check at these places that we indeed do not need the
     state any further. Suzanne *)
  let discard_state (_ : typer_state) = ()

  let initial_state : typer_state =
    let module MapCreateState = Plugins.Indexers.Map_indexer_plugins(CreateState) in
    let plugin_states = MapCreateState.f () indexers_plugins_fields_unit in
    {
      all_constraints                  = PolySet.create ~cmp:Ast_typed.Compare.type_constraint_simpl ;
      added_constraints                = PolySet.create ~cmp:Ast_typed.Compare.type_constraint ;
      deleted_constraints              = PolySet.create ~cmp:Ast_typed.Compare.type_constraint_simpl ;
      aliases                          = UnionFind.Poly2.empty Var.pp Var.compare ;
      plugin_states                    = plugin_states ;
      already_selected_and_propagators = List.map init_propagator_heuristic Plugins.heuristics ;
    }



  let placeholder_for_state_of_new_typer () = initial_state
end

(* TODO: make the typer a fonctor and move this instantiation as further outwards as possible. *)
(* Instantiate the solver with a selection of plugins *)
include Make_solver(Plugins)
type nonrec _ typer_state = typer_state

(*  ………………………………………………………………………………………………… Plugin-based solver above ………………………………………………………………………………………………… *)

let json_typer_state = fun ({ all_constraints=_ ; plugin_states=_ ; aliases=_ ; already_selected_and_propagators } : _ typer_state) : Yojson.Safe.t ->
  let open Solver_types in
  `Assoc[ ("all_constraints", `String "TODO");
          ("plugin_states", (* (Ast_typed.Yojson.structured_dbs structured_dbs) *) `String "TODO");
          ("aliases", `String "TODO");
          ("already_selected_and_propagators",
           let list f lst = `List (List.map f lst) in
           (list json_ex_propagator_state already_selected_and_propagators))]
