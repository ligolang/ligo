open Trace

(* module Core = Typesystem.Core *)
open Ast_typed.Types
(* open Ast_typed.Reasons *)
open Ast_typed.Combinators
open Database_plugins.All_plugins
(* module Assignments                   = Assignments
module GroupedByVariable             = GroupedByVariable
module CycleDetectionTopologicalSort = CycleDetectionTopologicalSort
module ByConstraintIdentifier        = ByConstraintIdentifier
module RefinedTypeclasses            = RefinedTypeclasses
module TypeclassesConstraining       = TypeclassesConstraining *)

open Db_index_tests_common

module Assignments_tests = struct
  include Test_vars
  include Assignments
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv
  let same_state sa sb =
    let sa = bindings sa in
    let sb = bindings sb in
    let%bind () = tst_assert "Length sa = Length sb" (List.length sa = List.length sb) in
    bind_list_iter
      (fun ((tva,cora) , (tvb,corb)) ->
        let%bind () = tst_assert "" (Ast_typed.Compare.type_variable tva tvb = 0) in
        let%bind () = tst_assert "" (Ast_typed.Compare.constructor_or_row cora corb = 0) in
        ok ()
      )
      (List.combine sa sb)
end

let assignments () =
  let open Assignments_tests in
  (* create empty state *)
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in
  (* assert state = [] *)
  let%bind () = tst_assert "length(bindings) = 0" @@ (List.length (bindings state) = 0) in

  (* add (tva, SC_Constructor ctor_a) to the state *)
  let ctor_a = make_c_constructor_simpl tva C_unit [] in
  let state' = add_constraint repr state (SC_Constructor ctor_a) in                                           
  (* assert state = [ (tva , `Constructor ctor_a) ] *)
  let%bind () =
    match bindings state' with
    | [(tv,cor)] ->
      let%bind () = tst_assert "state' : cor = ctor" @@ (Ast_typed.Compare.constructor_or_row cor (`Constructor ctor_a) = 0) in
      let%bind () = tst_assert "state' : tv = tva" @@ Var.equal tv tva in
      ok ()
    | x -> fail (test_err (Format.asprintf "state should only have one elment but has %d elements" @@ List.length x))
  in

  (* add (tvb, SC_Constructor ctor_b) to the state (tvb being an alias of tva, see repr) *)
  let ctor_b = make_c_constructor_simpl tvb C_unit [] in
  let state'' = add_constraint repr state' (SC_Constructor ctor_b) in
  (* assert that state did not update because a and b are aliases*)
  let%bind () = tst_assert "state'' = state'" @@ (List.length (bindings state'') = 1) in

  (* add (tvc, SC_Constructor ctor_c) *)
  let ctor_c = make_c_constructor_simpl tvc C_unit [] in
  let state''' = add_constraint repr state'' (SC_Constructor ctor_c) in
  (* assert that state''' now has two elements *)
  let%bind () = tst_assert "length (state''') = 2" (List.length (bindings state''') = 2) in

  (* merging tvc to tva *)
  let merge_keys  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvc in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state'''' = merge_aliases merge_keys state''' in
  (* assert that c has been merged to a in state'''' *)
  let%bind () =
    match bindings state'''' with
    | [(tv,cor)] ->
      let%bind () = tst_assert "state'''' : cor = ctor" @@ (Ast_typed.Compare.constructor_or_row cor (`Constructor ctor_a) = 0) in
      let%bind () = tst_assert "state'''' : tv = tva" @@ Var.equal tv tva in
      ok ()
    | l -> ok @@ Format.printf "%a" (PP_helpers.list_sep_d (PP_helpers.pair Ast_typed.PP.type_variable Ast_typed.PP.constructor_or_row)) l
  in 
  ok ()

type test_seq = Add_cstr of type_variable | Merge of (type_variable , type_variable) merge_keys
let invariant () =
  let open Assignments_tests in
  let repr : type_variable -> type_variable = fun tv -> tv in
  let merge_keys demoted_repr new_repr  : (type_variable, type_variable) merge_keys =
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in

  (* create empty state *)
  let istate = create_state ~cmp:Ast_typed.Compare.type_variable in
  let aux : _ t -> test_seq -> _ t = fun state seq ->
    match seq with
    | Add_cstr tv ->
      let tc = SC_Constructor (make_c_constructor_simpl tv C_unit []) in
      add_constraint repr state tc
    | Merge merge_keys -> merge_aliases merge_keys state
  in
  let state_a = List.fold_left aux istate
    [ Add_cstr tva ; Add_cstr tvb ; Add_cstr tvc ; Merge (merge_keys tva tvb) ; ]
  in
  let state_b = List.fold_left aux istate
    [ Add_cstr tva ; Add_cstr tvb ; Merge (merge_keys tva tvb) ; Add_cstr tvc ; ]
  in
  let%bind () = same_state state_a state_b in
  ok ()
  
