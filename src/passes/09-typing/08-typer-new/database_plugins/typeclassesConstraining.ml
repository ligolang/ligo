open Ast_typed.Types
open UnionFind
open Trace

type 'typeVariable t = ('typeVariable, constraint_identifier_set (* c_typeclass_simpl *)) ReprMap.t

let create_state ~cmp =
  let merge c1 c2 = assert (Ast_typed.Compare.constraint_identifier_set c1 c2 = 0); c1 in
  ReprMap.create ~cmp ~merge

let tc_to_constraint_identifier : c_typeclass_simpl -> constraint_identifier =
  fun tc -> tc.id_typeclass_simpl

(* TODO: refined and non-refined constraints alike *)
(* TODO: store the c_typeclass_simpl, not the constraint_identifier
   (problem: we'd need a comparator, maybe the comparator for
   c_typeclass_simpl can work on the constraint_id within without
   comparing type variables (which should be disallowed in these
   plug-ins)?) *)
(* TODO: in all indexer plug-ins *and* heuristic plug-ins, we should
   use constraint types which are parameterized by the 'type_variable
   to make it opaque. *)

let register_typeclasses_constrained_by : _ -> c_typeclass_simpl -> _ t -> _ t =
  fun repr c state ->
  let tc = tc_to_constraint_identifier c in
  let aux' = function
      Some set -> PolySet.add tc set
    | None -> PolySet.add tc (PolySet.create ~cmp:Ast_typed.Compare.constraint_identifier) in
  let aux typeclasses_constrained_by tv =
    ReprMap.monotonic_update (repr tv) aux' typeclasses_constrained_by in
  List.fold_left
    aux
    state
    (List.rev c.args)

let add_constraint repr state new_constraint =
  match new_constraint with
  | SC_Typeclass c -> register_typeclasses_constrained_by repr c state
  | _ -> state

let remove_constraint repr state constraint_to_remove =
    match constraint_to_remove with
  | Ast_typed.Types.SC_Typeclass constraint_to_remove ->
    let tc = tc_to_constraint_identifier constraint_to_remove in
    let aux' = function
        Some set -> PolySet.remove tc set
      | None -> PolySet.remove tc (PolySet.create ~cmp:Ast_typed.Compare.constraint_identifier) in
    let aux typeclasses_constrained_by tv =
      ReprMap.monotonic_update (repr tv) aux' typeclasses_constrained_by in
    let state =
      List.fold_left
        aux
        state
        (List.rev constraint_to_remove.args) in
    ok state
  | _ -> ok state

let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun merge_keys state -> merge_keys.map state

let get_typeclasses_constraining tv (state : 'typeVariable t) =
  Option.unopt ~default:(PolySet.create ~cmp:Ast_typed.Compare.constraint_identifier)
  @@ ReprMap.find_opt tv state
