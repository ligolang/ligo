[@@@warning "-32"]
module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val grouped_by_variable : Type_variable.t  Grouped_by_variable.t
    val assignments : Type_variable.t Assignments.t
    val typeclasses_constraining : Type_variable.t Typeclasses_constraining.t
    val by_constraint_identifier : Type_variable.t By_constraint_identifier.t
  end
end

(* open Typesystem.Solver_types *)
open Trace
open Typer_common.Errors
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

module Utils = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  type type_variable = Type_variable.t

  let set_of_vars l = (Set.add_list l (Set.create ~cmp:Compare.type_variable )).set
  type flds = (module INDEXES(Type_variable)(Type_variable_abstraction).S)
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins

  let constraint_identifier_to_tc ((module Dbs) : flds) (ci : constraint_identifier) =
    (* TODO: this can fail: catch the exception and throw an error *)
    match By_constraint_identifier.find_opt ci Dbs.by_constraint_identifier with
      Some x -> x
    | None -> failwith "TODO: internal error : do something"

  let tc_to_constraint_identifier : c_typeclass_simpl -> constraint_identifier =
    fun tc -> tc.id_typeclass_simpl

  type 'v constraint_identifierMap = (constraint_identifier, 'v) RedBlackTrees.PolyMap.t
  (* type refined_typeclass_constraint_identifierMap = refined_typeclass constraint_identifierMap *)
  (* type constraint_identifier_set = constraint_identifier Set.t
  * type constraint_identifier_set_map = constraint_identifier_set typeVariableMap *)

  type private_storage = unit
  (* type private_storage = {
  *   (\* This rule maintains in its private storage a representation of
  *      the latest version of each typeclass *\)
  *   refined_typeclasses: refined_typeclass_constraint_identifierMap ;
  *   typeclasses_constrained_by : constraint_identifier_set_map ;
  * } *)

  let tc_to_constraint_identifier : c_typeclass_simpl -> constraint_identifier =
    fun tc -> tc.id_typeclass_simpl

  let splice f idx l =
    let rec splice acc f idx l =
      match l with
        [] -> failwith "invalid index into list"
      | hd :: tl ->
        if idx = 0
        then (List.append (List.rev acc) @@ List.append (f hd) @@ tl)
        else (splice (hd :: acc) f (idx - 1) tl)
    in splice [] f idx l

  let splice_or_none f idx l =
    let rec splice_or_none acc f idx l =
      match l with
        [] -> failwith "internal error: invalid index into list"
      | hd :: tl ->
        if idx = 0
        then (match f hd with
            | None -> None
            | Some new_hds -> Some (List.append (List.rev acc) @@ List.append new_hds @@ tl))
        else (splice_or_none (hd :: acc) f (idx - 1) tl)
    in splice_or_none [] f idx l

  (* Check that the typeclass is a rectangular matrix, with one column
    per argument. *)
  let check_typeclass_rectangular ({ reason_typeclass_simpl=_; tc; args } as tcs : c_typeclass_simpl) =
    let nargs = List.length args in
    if (List.for_all (fun allowed -> List.length allowed = nargs) tc)
    then ok tcs
    else fail typeclass_not_a_rectangular_matrix

  (* Check that the transposed typeclass is a rectangular matrix, with
    one row per argument. *)
  let check_typeclass_transposed_rectangular (tc : (type_variable * type_value list) list) =
    match tc with
      [] -> ok tc
    | (_, hd) :: tl ->
      let hdlen = List.length hd in
      if List.for_all (fun (_, l) -> List.length l = hdlen) tl
      then ok tc
      else fail typeclass_not_a_rectangular_matrix

  (* transpose ([x;z] ∈ [ [map(nat,unit) ; int    ; ] ;
                          [map(unit,nat) ; string ; ] ;
                          [map(int,int)  ; unit   ; ] ; ])
    will return [ x ? [ map(nat,unit) ; map(unit,nat) ; map(int,int) ; ] ;
                  z ? [ int           ; string        ; unit         ; ] ; ] *)
  let transpose : c_typeclass_simpl -> ((type_variable * type_value list) list, _) result =
    fun { reason_typeclass_simpl = _; tc; args } ->
    bind_fold_list
      (fun accs allowed_tuple ->
        List.map2 (fun (var, acc) allowed_type -> (var, allowed_type :: acc)) accs allowed_tuple
          ~ok ~fail:(fun _ _ -> fail @@ internal_error __LOC__ "typeclass is not represented by a rectangular matrix"))
      (List.map (fun var -> var, []) args)
      tc
    >>|? List.map (fun (var, acc) -> (var, List.rev acc))
    >>? check_typeclass_transposed_rectangular

  (* transpose_back [ x ? [ map(nat,unit) ; map(unit,nat) ; map(int,int) ; ] ;
                      z ? [ int           ; string        ; unit         ; ] ; ]
    will return ([x;z] ∈ [ [map(nat,unit) ; int    ; ] ;
                            [map(unit,nat) ; string ; ] ;
                            [map(int,int)  ; unit   ; ] ; ]) *)
  let transpose_back : _ -> _ -> (type_variable * type_value list) list -> (c_typeclass_simpl, _) result =
    fun (reason_typeclass_simpl, original_id) id_typeclass_simpl tcs ->
    let%bind tc =
      match tcs with
      | [] -> ok []
      | (_, hd_allowed_types) :: _ ->
        bind_fold_list
          (fun allowed_tuples allowed_types ->
            List.map2 (fun allowed_tuple allowed_type -> allowed_type :: allowed_tuple) allowed_tuples allowed_types
              ~ok ~fail:(fun _ _ -> fail @@ internal_error __LOC__ "transposed typeclass is not represented by a rectangular matrix"))
          (List.map (fun _ -> []) hd_allowed_types)
          (List.map snd tcs)
        >>|? List.map (fun allowed_typle -> List.rev allowed_typle)
    in
    let args = List.map fst tcs in
    check_typeclass_rectangular @@
    { reason_typeclass_simpl; original_id; id_typeclass_simpl; tc; args }

  type 'a all_equal = Empty | All_equal_to of 'a | Different
  let all_equal cmp = function
    | [] -> Empty
    | hd :: tl -> if List.for_all (fun x -> cmp x hd = 0) tl then All_equal_to hd else Different
  

  let get_tag_and_args_of_constant (tv : type_value) =
    match tv.wrap_content with
    | P_constant { p_ctor_tag; p_ctor_args } -> ok (p_ctor_tag, p_ctor_args)
    | P_row { p_row_tag; p_row_args } -> ignore (p_row_tag, p_row_args); failwith "TODO: return p_row_tag, p_row_args similarly to P_constant"
    | P_forall _ ->
      (* In this case we would need to do specialization.
        For now we just leave as-is and don't deduce anything *)
      failwith "Unsuported"
    | P_variable _ ->
      (* In this case we  *)
      failwith "TODO : P_variable"
    | P_apply _ ->
      (* In this case we would need to do β-reduction, if
        possible, or invoke another heuristic.
        For now we just leave as-is and don't deduce anything *)
      failwith "TODO"
end
