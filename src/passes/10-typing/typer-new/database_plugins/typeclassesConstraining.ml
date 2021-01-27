open Ast_typed.Types
open UnionFind
open Trace

type 'typeVariable t = ('typeVariable, c_typeclass_simpl PolySet.t) ReprMap.t
type ('type_variable, 'a) state = < typeclasses_constraining : 'type_variable t ; .. > as 'a

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

let add_constraint ?debug repr state new_constraint =
  let _ = debug in
  match new_constraint with
  | SC_Typeclass c -> register_typeclasses_constraining repr c state
  | _ -> state

let remove_constraint printer repr state constraint_to_remove =
  Format.printf "remove_constraint for typeclassesConstraining.... \n%!";
    match constraint_to_remove with
  | Ast_typed.Types.SC_Typeclass constraint_to_remove ->
    let aux' = function
        Some set -> PolySet.remove constraint_to_remove set
      | None -> 
        Format.printf "No set linked to tv";
        PolySet.remove constraint_to_remove @@ PolySet.create ~cmp:Ast_typed.Compare.c_typeclass_simpl in
    let aux typeclasses_constrained_by tv =
      Format.printf "In aux with tv : %a and repr tv : %a\n%!" Ast_typed.PP.type_variable tv printer @@ repr tv;
      ReprMap.monotonic_update (repr tv) aux' typeclasses_constrained_by in
    let state =
      List.fold_left
        aux
        state
        (List.rev constraint_to_remove.args) in
    Format.printf "  ok\n%!";
    ok state
  | _ -> 
    Format.printf "  ok\n%!";
    ok state

let merge_aliases : 'old 'new_ . ?debug:(Format.formatter -> 'new_ t -> unit) -> ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun ?debug merge_keys state -> 
    Format.printf "In merge alias for typeclassesConstraining\n%!";
    let state = merge_keys.map state in
    (match debug with Some (debug) -> Format.printf "Return with new state %a\n" debug state | _ -> ());
    state

let pp type_variable ppf state =
  let open PP_helpers in
  list_sep_d
    (pair
       type_variable
       (fun ppf set -> list_sep_d Ast_typed.PP.c_typeclass_simpl_short ppf (PolySet.elements set)))
    ppf
    (ReprMap.bindings state)

let name = "typeclasses_constraining"

let get_state_for_tests state = state


let get_typeclasses_constraining tv (state : ('type_variable, _) state) =
  Option.unopt ~default:(PolySet.create ~cmp:Ast_typed.Compare.c_typeclass_simpl)
  @@ ReprMap.find_opt tv state#typeclasses_constraining

let get_typeclasses_constraining_list tv (state : ('type_variable, _) state) =
  PolySet.elements @@ get_typeclasses_constraining tv state
