open Helpers
open Ast_typed
open Trace

let invalid_constants =
  [ C_TEST_ORIGINATE
  ; C_TEST_GET_STORAGE
  ; C_TEST_GET_STORAGE_OF_ADDRESS
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
  ; C_TEST_BOOTSTRAP_CONTRACT
  ; C_TEST_NTH_BOOTSTRAP_CONTRACT
  ; C_TEST_LAST_ORIGINATIONS
  ; C_TEST_COMPILE_META_VALUE
  ; C_TEST_MUTATE_EXPRESSION
  ; C_TEST_MUTATE_COUNT
  ; C_TEST_MUTATE_VALUE
  ; C_TEST_MUTATION_TEST
  ; C_TEST_MUTATION_TEST_ALL
  ; C_TEST_RUN
  ; C_TEST_EVAL
  ; C_TEST_COMPILE_CONTRACT
  ; C_TEST_TO_CONTRACT
  ; C_TEST_TO_ENTRYPOINT
  ; C_TEST_ORIGINATE_FROM_FILE
  ; C_TEST_TO_TYPED_ADDRESS
  ; C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS ]

let type_constants =
  let open Stage_common.Constant in
  [test_michelson_name; test_ligo_name; account_name; time_name ; typed_address_name ; mutation_name ; failure_name]

type 'err ty_exp_mapper = type_expression -> unit

let rows : ('a -> unit) -> rows -> unit
= fun g {content; _} ->
  let _ = LMap.map
  (fun {associated_type ; _} ->
    let () = g associated_type in
    ()
  ) content in
  ()

let rec traverse_type_expression : 'err ty_exp_mapper -> type_expression -> unit  = fun f te ->
  let module SSet = Set.Make (String) in
  let open Stage_common in
  let self = traverse_type_expression f in
  let () = f te in
  match te.type_content with
  | T_sum temap -> rows self temap
  | T_abstraction x -> self x.type_
  | T_record temap -> rows self temap
  | T_arrow arr ->
     let _ = Maps.arrow self arr in
     ()
  | T_variable _ -> ()
  | T_module_accessor _ -> ()
  | T_singleton _ -> ()
  | T_constant { parameters } ->
     let _ = List.map ~f:self parameters in
     ()

let check_obj_ligo ~raise (t : Ast_typed.expression) =
  let equal_constant a b = Ast_typed.Compare.constant' a b = 0 in
  let folder_constant () expr = match expr.expression_content with
    | E_constant {cons_name}
         when List.mem invalid_constants cons_name ~equal:equal_constant ->
       raise.raise @@ Errors.expected_obj_ligo expr.location
    | _ -> () in
  let traverser_types loc expr = match expr.type_content with
    | T_constant { injection ; _ } when List.mem type_constants (Ligo_string.extract injection) ~equal:String.equal ->
       raise.raise @@ Errors.expected_obj_ligo loc
    | _ -> () in
  let folder_types () (expr : expression) =
    traverse_type_expression (traverser_types expr.type_expression.location) expr.type_expression in
  let () = fold_expression folder_constant () t in
  let () = fold_expression folder_types () t in
  t
