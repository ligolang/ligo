module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators
module Int_repr = Ligo_interpreter.Int_repr_copied
module Exc = Ligo_interpreter_exc

open Self_ast_typed.Helpers
open Ast_typed
open Trace

let invalid_constants =
  [ C_TEST_ORIGINATE
  ; C_TEST_GET_STORAGE
  ; C_TEST_GET_BALANCE
  ; C_TEST_SET_NOW
  ; C_TEST_SET_SOURCE
  ; C_TEST_SET_BAKER
  ; C_TEST_EXTERNAL_CALL_TO_CONTRACT
  ; C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN
  ; C_TEST_EXTERNAL_CALL_TO_ADDRESS
  ; C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN
  ; C_TEST_MICHELSON_EQUAL
  ; C_TEST_GET_NTH_BS
  ; C_TEST_LOG
  ; C_TEST_COMPILE_EXPRESSION
  ; C_TEST_COMPILE_EXPRESSION_SUBST
  ; C_TEST_STATE_RESET
  ; C_TEST_LAST_ORIGINATIONS
  ; C_TEST_COMPILE_META_VALUE
  ; C_TEST_RUN
  ; C_TEST_EVAL
  ; C_TEST_COMPILE_CONTRACT
  ; C_TEST_TO_CONTRACT ]

let type_constants =
  let open Stage_common.Constant in
[test_michelson_name; test_ligo_name; account_name; time_name ; typed_address_name ; mutation_name ; failure_name]

type 'err ty_exp_mapper = type_expression -> (unit, 'err) result

let rows : ('a -> (unit,_) result) -> rows -> (unit,_) result
= fun g {content; _} ->
  let* _ = Helpers.bind_map_lmap
  (fun {associated_type ; _} ->
    let* _ = g associated_type in
    ok @@ ()
  ) content in
  ok @@ ()

let rec traverse_type_expression : 'err ty_exp_mapper -> type_expression -> (unit , _) result = fun f te ->
  let module SSet = Set.Make (String) in
  let open Stage_common in
  let self = traverse_type_expression f in
  let* () = f te in
  match te.type_content with
  | T_sum temap -> rows self temap
  | T_record temap -> rows self temap
  | T_arrow arr ->
     let* _ = Maps.arrow self arr in
     ok ()
  | T_variable _ -> ok ()
  | T_module_accessor _ -> ok ()
  | T_singleton _ -> ok ()
  | T_constant { parameters } ->
     let* _ = bind_map_list self parameters in
     ok ()

let check_obj_ligo (t : Ast_typed.expression) =
  let equal_constant a b = Ast_typed.Compare.constant' a b = 0 in
  let folder_constant () expr = match expr.expression_content with
    | E_constant {cons_name}
         when List.mem invalid_constants cons_name ~equal:equal_constant ->
       fail @@ Errors.generic_error expr.location "Test functions cannot be used in code to be run/returned by the Michelson interpreter."
    | _ -> ok () in
  let traverser_types loc expr = match expr.type_content with
    | T_constant { injection ; _ } when List.mem type_constants (Ligo_string.extract injection) ~equal:String.equal ->
       fail @@ Errors.generic_error loc "Test types cannot be used in code to be run/returned by the Michelson interpreter"
    | _ -> ok () in
  let folder_types () (expr : expression) =
    traverse_type_expression (traverser_types expr.type_expression.location) expr.type_expression in
  let* () = fold_expression folder_types () t in
  let* () = fold_expression folder_constant () t in
  ok ()

let is_obj_ligo (t : Ast_typed.expression) : bool =
  let ret = check_obj_ligo t in
  match Trace.to_stdlib_result ret with
  | Ok _ -> true
  | Error _ -> false
