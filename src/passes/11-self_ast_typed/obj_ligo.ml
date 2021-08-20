open Helpers
open Ast_typed
open Trace

let invalid_constants = function
  (* TODO: use ppx ? feels bad *)
  | C_TEST_ORIGINATE -> true
  | C_TEST_GET_STORAGE -> true
  | C_TEST_GET_STORAGE_OF_ADDRESS -> true
  | C_TEST_GET_BALANCE -> true
  | C_TEST_SET_NOW -> true
  | C_TEST_SET_SOURCE -> true
  | C_TEST_SET_BAKER -> true
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> true
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN -> true
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> true
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN -> true
  | C_TEST_MICHELSON_EQUAL -> true
  | C_TEST_GET_NTH_BS -> true
  | C_TEST_LOG -> true
  | C_TEST_STATE_RESET -> true
  | C_TEST_BOOTSTRAP_CONTRACT -> true
  | C_TEST_NTH_BOOTSTRAP_CONTRACT -> true
  | C_TEST_LAST_ORIGINATIONS -> true
  | C_TEST_COMPILE_META_VALUE -> true
  | C_TEST_MUTATE_COUNT -> true
  | C_TEST_MUTATE_VALUE -> true
  | C_TEST_MUTATION_TEST -> true
  | C_TEST_MUTATION_TEST_ALL -> true
  | C_TEST_SAVE_MUTATION -> true
  | C_TEST_RUN -> true
  | C_TEST_EVAL -> true
  | C_TEST_COMPILE_CONTRACT -> true
  | C_TEST_TO_CONTRACT -> true
  | C_TEST_TO_ENTRYPOINT -> true
  | C_TEST_ORIGINATE_FROM_FILE -> true
  | C_TEST_TO_TYPED_ADDRESS -> true
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS -> true
  | C_INT -> false
  | C_UNIT -> false
  | C_NEVER -> false
  | C_NIL -> false
  | C_NOW -> false
  | C_IS_NAT -> false
  | C_SOME -> false
  | C_NONE -> false
  | C_ASSERTION -> false
  | C_ASSERT_SOME -> false
  | C_ASSERT_INFERRED -> false
  | C_FAILWITH -> false
  | C_UPDATE -> false
  | C_ITER -> false
  | C_FOLD_WHILE -> false
  | C_FOLD_CONTINUE -> false
  | C_FOLD_STOP -> false
  | C_LOOP_LEFT -> false
  | C_LOOP_CONTINUE -> false
  | C_LOOP_STOP -> false
  | C_FOLD -> false
  | C_FOLD_LEFT -> false
  | C_FOLD_RIGHT -> false
  | C_NEG -> false
  | C_ABS -> false
  | C_ADD -> false
  | C_SUB -> false
  | C_MUL -> false
  | C_EDIV -> false
  | C_DIV -> false
  | C_MOD -> false
  | C_NOT -> false
  | C_AND -> false
  | C_OR -> false
  | C_XOR -> false
  | C_LSL -> false
  | C_LSR -> false
  | C_EQ -> false
  | C_NEQ -> false
  | C_LT -> false
  | C_GT -> false
  | C_LE -> false
  | C_GE -> false
  | C_SIZE -> false
  | C_CONCAT -> false
  | C_SLICE -> false
  | C_BYTES_PACK -> false
  | C_BYTES_UNPACK -> false
  | C_CONS -> false
  | C_PAIR -> false
  | C_CAR -> false
  | C_CDR -> false
  | C_TRUE -> false
  | C_FALSE -> false
  | C_LEFT -> false
  | C_RIGHT -> false
  | C_SET_EMPTY -> false
  | C_SET_LITERAL -> false
  | C_SET_ADD -> false
  | C_SET_REMOVE -> false
  | C_SET_ITER -> false
  | C_SET_FOLD -> false
  | C_SET_FOLD_DESC -> false
  | C_SET_MEM -> false
  | C_SET_UPDATE -> false
  | C_LIST_EMPTY -> false
  | C_LIST_LITERAL -> false
  | C_LIST_ITER -> false
  | C_LIST_MAP -> false
  | C_LIST_FOLD -> false
  | C_LIST_FOLD_LEFT -> false
  | C_LIST_FOLD_RIGHT -> false
  | C_LIST_HEAD_OPT -> false
  | C_LIST_TAIL_OPT -> false
  | C_MAP -> false
  | C_MAP_EMPTY -> false
  | C_MAP_LITERAL -> false
  | C_MAP_GET -> false
  | C_MAP_GET_FORCE -> false
  | C_MAP_ADD -> false
  | C_MAP_REMOVE -> false
  | C_MAP_UPDATE -> false
  | C_MAP_ITER -> false
  | C_MAP_MAP -> false
  | C_MAP_FOLD -> false
  | C_MAP_MEM -> false
  | C_MAP_FIND -> false
  | C_MAP_FIND_OPT -> false
  | C_MAP_GET_AND_UPDATE -> false
  | C_BIG_MAP -> false
  | C_BIG_MAP_EMPTY -> false
  | C_BIG_MAP_LITERAL -> false
  | C_BIG_MAP_GET_AND_UPDATE -> false
  | C_BIG_MAP_IDENTIFIER -> false
  | C_SHA256 -> false
  | C_SHA512 -> false
  | C_BLAKE2b -> false
  | C_HASH -> false
  | C_HASH_KEY -> false
  | C_CHECK_SIGNATURE -> false
  | C_CHAIN_ID -> false
  | C_CALL -> false
  | C_CONTRACT -> false
  | C_CONTRACT_OPT -> false
  | C_CONTRACT_ENTRYPOINT -> false
  | C_CONTRACT_ENTRYPOINT_OPT -> false
  | C_AMOUNT -> false
  | C_BALANCE -> false
  | C_SOURCE -> false
  | C_SENDER -> false
  | C_ADDRESS -> false
  | C_SELF -> false
  | C_SELF_ADDRESS -> false
  | C_IMPLICIT_ACCOUNT -> false
  | C_SET_DELEGATE -> false
  | C_CREATE_CONTRACT -> false
  | C_SHA3 -> false
  | C_KECCAK -> false
  | C_LEVEL -> false
  | C_VOTING_POWER -> false
  | C_TOTAL_VOTING_POWER -> false
  | C_TICKET -> false
  | C_READ_TICKET -> false
  | C_SPLIT_TICKET -> false
  | C_JOIN_TICKET -> false
  | C_PAIRING_CHECK -> false
  | C_SAPLING_VERIFY_UPDATE -> false
  | C_SAPLING_EMPTY_STATE -> false
  | C_POLYMORPHIC_ADD -> false

let type_constants =
  let open Stage_common.Constant in
  [test_michelson_name; account_name; time_name ; typed_address_name ; mutation_name ; failure_name]

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
  let folder_constant () expr = match expr.expression_content with
    | E_constant {cons_name}
         when invalid_constants cons_name ->
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
