open Trace

(* module Core = Typesystem.Core *)
open Ast_typed.Types
(* open Ast_typed.Reasons *)
(* open Ast_typed.Combinators *)
open Database_plugins.All_plugins
(* module Assignments                   = Assignments
module GroupedByVariable             = GroupedByVariable
module CycleDetectionTopologicalSort = CycleDetectionTopologicalSort
module ByConstraintIdentifier        = ByConstraintIdentifier
module RefinedTypeclasses            = RefinedTypeclasses
module TypeclassesConstraining       = TypeclassesConstraining *)

open Db_index_tests_common

module GroupedByVariable_tests = struct
  include Test_vars
  include GroupedByVariable
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv
  let sc_a : type_constraint_simpl = constructor tva C_unit []
  let sc_b : type_constraint_simpl = constructor tvb C_unit []
  let sc_c : type_constraint_simpl = constructor tvc C_unit []
  let constraints_nb (l:constraints) (expected:int) =
    List.(
      length l.constructor = expected &&
      length l.poly = 0 &&
      length l.row = 0
    )
  let assert_ctor_equal ~(expected:type_constraint_simpl list) ~(actual:constraints) =
    (*order of lists do not matter*)
    let aux : type_constraint_simpl -> (unit,_) result =
      fun expected ->
        match expected with
        | SC_Constructor expected' -> (
          let opt = List.find_opt (fun a -> Ast_typed.Compare.c_constructor_simpl a expected' = 0) actual.constructor in
          match opt with
          | Some _ -> ok ()
          | None -> fail (test_err "ctor must be equal")
        ) 
        | _ -> fail (test_err "expecting constructors only")
    in
    bind_iter_list aux expected
end

let grouped_by_variable () =
  let open GroupedByVariable_tests in
  let state = create_state ~cmp:Ast_typed.Compare.type_variable in
  (* all 3 constraints as defined in GroupedByVariable_tests *)
  let clist = [ sc_a ; sc_b ; sc_c ] in
  let state = List.fold_left (fun acc el -> add_constraint repr acc el) state clist in
  (* 
    check that :
    - a is associated with sc_a and sc_b
    - c is associated wit sc_c
    - b has no associated constraint (repr(b) = a)
  *)
  let gbv = bindings state in
  let%bind () = tst_assert "state' = { a -> ... ; c -> ... }" (List.length gbv = 2) in
  let%bind () =
    let aux : (type_variable * constraints) -> (unit,_) result =
      fun (tv, cs) ->
        match tv with
        | a when Var.equal a tva ->
          let%bind () = tst_assert "two constraints related to tva" (constraints_nb cs 2) in
          let%bind () = assert_ctor_equal ~expected:[sc_a;sc_b] ~actual:cs in
          ok ()
        | c when Var.equal c tvc ->
          let%bind () = tst_assert "one constraint related to tvc" (constraints_nb cs 1) in
          let%bind () = assert_ctor_equal ~expected:[sc_c] ~actual:cs in
          ok ()
        | b when Var.equal b tvb -> fail (test_err "b should not be in the state")
        | _ -> fail @@ test_err "new variable discovered (impossible)"
    in
    bind_iter_list aux gbv
  in
  (* =============================================================================================
     TODO: THIS TEST IS DISABLED BECAUSE REMOVAL IS NOT IMPLEMENTED FOR CONSTRUCTOR CONSTRAINTS IN
     grouped_by_variable
     ============================================================================================= *)
  let%bind () = if false then
      (* remove sc_a from state *)
      let%bind state = trace Main_errors.typer_tracer @@ remove_constraint repr state sc_a in
      (* same check as above except sc_a should be deleted from tva's constraints *)
      let gbv = bindings state in
      let%bind () = tst_assert "state' = { a -> ... ; c -> ... }" (List.length gbv = 2) in
      let%bind () =
        let aux : (type_variable * constraints) -> (unit,_) result =
          fun (tv, cs) ->
            match tv with
            | a when Var.equal a tva ->
              let%bind () = tst_assert "two constraints related to tva" (constraints_nb cs 1) in
              let%bind () = assert_ctor_equal ~expected:[sc_b] ~actual:cs in
              ok ()
            | c when Var.equal c tvc ->
              let%bind () = tst_assert "one constraint related to tvc" (constraints_nb cs 1) in
              let%bind () = assert_ctor_equal ~expected:[sc_c] ~actual:cs in
              ok ()
            | b when Var.equal b tvb -> fail (test_err "b should not be in the state")
            | _ -> fail @@ test_err "new variable discovered (impossible)"
        in
        bind_iter_list aux gbv
      in
      ok ()
    else
      ok ()
  in

  (*
  let d = merge_aliases (*??*) in
  let e = get_constraints_by_lhs tv a in
  ignore (a,b,c,d,e) ; *)
  ok ()
