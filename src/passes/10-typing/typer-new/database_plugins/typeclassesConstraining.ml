open Ast_typed.Types
open UnionFind
open Trace

type 'typeVariable t = ('typeVariable, c_typeclass_simpl PolySet.t) ReprMap.t

let create_state ~cmp =
  let merge = PolySet.union in
  ReprMap.create ~cmp ~merge

(* TODO: in all indexer plug-ins *and* heuristic plug-ins, we should
   use constraint types which are parameterized by the 'type_variable
   to make it opaque. *)

let register_typeclasses_constraining : _ -> c_typeclass_simpl -> _ t -> _ t =
  fun repr c state ->
  let aux' = function
      Some set -> PolySet.add c set
    | None -> PolySet.add c (PolySet.create ~cmp:Ast_typed.Compare.c_typeclass_simpl) in
  let aux state tv =
    ReprMap.monotonic_update (repr tv) aux' state in
  List.fold_left
    aux
    state
    (List.rev c.args)

let add_constraint repr state new_constraint =
  match new_constraint with
  | SC_Typeclass c -> register_typeclasses_constraining repr c state
  | _ -> state

let remove_constraint repr state constraint_to_remove =
    match constraint_to_remove with
  | Ast_typed.Types.SC_Typeclass constraint_to_remove ->
    let aux' = function
        Some set -> PolySet.remove constraint_to_remove set
      | None -> PolySet.remove constraint_to_remove (PolySet.create ~cmp:Ast_typed.Compare.c_typeclass_simpl) in
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
  Option.unopt ~default:(PolySet.create ~cmp:Ast_typed.Compare.c_typeclass_simpl)
  @@ ReprMap.find_opt tv state

let get_typeclasses_constraining_list tv (state : 'typeVariable t) =
  PolySet.elements @@ get_typeclasses_constraining tv state

let get_refined_typeclasses_constraining_list tv (indexes : < typeclasses_constraining : 'type_variable t ; refined_typeclasses : 'type_variable RefinedTypeclasses.t ; .. > ) =
  List.map
    (fun tc -> RefinedTypeclasses.find tc indexes#refined_typeclasses)
    (get_typeclasses_constraining_list tv indexes#typeclasses_constraining)
