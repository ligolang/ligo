type constant' =
  | C_UNIT
  | C_NIL
  | C_SOME
  | C_NONE
  | C_UPDATE
  (* Loops *)
  | C_ITER
  | C_LOOP_LEFT
  | C_LOOP_CONTINUE
  | C_LOOP_STOP
  | C_FOLD
  | C_FOLD_LEFT
  | C_FOLD_RIGHT
  (* CONVERSION *)
  | C_ABS
  | C_INT
  (* MATH *)
  | C_NEG
  | C_ADD
  | C_SUB
  | C_MUL
  | C_DIV
  | C_MOD
  (* LOGIC *)
  | C_NOT
  | C_AND
  | C_OR
  | C_XOR
  | C_LSL
  | C_LSR
  (* COMPARATOR *)
  | C_EQ
  | C_NEQ
  | C_LT
  | C_GT
  | C_LE
  | C_GE
  (* Bytes/ String *)
  | C_CONCAT
  | C_CONS
  | C_SIZE
  | C_SLICE
  (* Pair *)
  | C_PAIR
  | C_CAR
  | C_CDR
  | C_TRUE
  | C_FALSE
  | C_LEFT
  | C_RIGHT
  (* Set *)
  | C_SET_EMPTY
  | C_SET_LITERAL
  | C_SET_ADD
  | C_SET_REMOVE
  | C_SET_ITER
  | C_SET_FOLD
  | C_SET_FOLD_DESC
  | C_SET_MEM
  | C_SET_UPDATE
  | C_SET_SIZE
  (* List *)
  | C_LIST_EMPTY
  | C_LIST_LITERAL
  | C_LIST_ITER
  | C_LIST_MAP
  | C_LIST_FOLD
  | C_LIST_FOLD_LEFT
  | C_LIST_FOLD_RIGHT
  | C_LIST_SIZE
  (* Maps *)
  | C_MAP
  | C_MAP_EMPTY
  | C_MAP_LITERAL
  | C_MAP_GET
  | C_MAP_GET_FORCE
  | C_MAP_ADD
  | C_MAP_REMOVE
  | C_MAP_UPDATE
  | C_MAP_ITER
  | C_MAP_MAP
  | C_MAP_FOLD
  | C_MAP_FIND
  | C_MAP_FIND_OPT
  | C_MAP_GET_AND_UPDATE
  | C_MAP_SIZE
  | C_MAP_MEM
  (* Big Maps *)
  | C_BIG_MAP
  | C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL
  | C_BIG_MAP_GET_AND_UPDATE
  (* Blockchain *)
  | C_CREATE_CONTRACT
  (* Check - used for checking conditions and giving errors *)
  | C_CHECK_SELF
  | C_CHECK_EMIT_EVENT
  | C_CHECK_ENTRYPOINT
  (* Tests - ligo interpreter only *)
  | C_TEST_ADDRESS [@only_interpreter]
  | C_TEST_SIZE [@only_interpreter]
  | C_TEST_ORIGINATE [@only_interpreter]
  | C_TEST_GET_STORAGE_OF_ADDRESS [@only_interpreter]
  | C_TEST_GET_BALANCE [@only_interpreter]
  | C_TEST_SET_SOURCE [@only_interpreter]
  | C_TEST_SET_BAKER [@only_interpreter]
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS [@only_interpreter]
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN [@only_interpreter]
  | C_TEST_GET_NTH_BS [@only_interpreter]
  | C_TEST_PRINT [@only_interpreter]
  | C_TEST_TO_STRING [@only_interpreter]
  | C_TEST_UNESCAPE_STRING [@only_interpreter]
  | C_TEST_STATE_RESET [@only_interpreter]
  | C_TEST_BOOTSTRAP_CONTRACT [@only_interpreter]
  | C_TEST_NTH_BOOTSTRAP_CONTRACT [@only_interpreter]
  | C_TEST_LAST_ORIGINATIONS [@only_interpreter]
  | C_TEST_MUTATE_CONTRACT [@only_interpreter]
  | C_TEST_MUTATE_VALUE [@only_interpreter]
  | C_TEST_SAVE_MUTATION [@only_interpreter]
  | C_TEST_RUN [@only_interpreter]
  | C_TEST_COMPILE_CONTRACT [@only_interpreter]
  | C_TEST_DECOMPILE [@only_interpreter]
  | C_TEST_TO_CONTRACT [@only_interpreter]
  | C_TEST_TO_ENTRYPOINT [@only_interpreter]
  | C_TEST_COMPILE_CONTRACT_FROM_FILE [@only_interpreter]
  | C_TEST_COMPILE_AST_CONTRACT [@only_interpreter]
  | C_TEST_TO_TYPED_ADDRESS [@only_interpreter]
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS [@only_interpreter]
  | C_TEST_SET_BIG_MAP [@only_interpreter]
  | C_TEST_CAST_ADDRESS [@only_interpreter]
  | C_TEST_CREATE_CHEST [@only_interpreter]
  | C_TEST_CREATE_CHEST_KEY [@only_interpreter]
  | C_TEST_RANDOM [@only_interpreter]
  | C_TEST_GENERATOR_EVAL [@only_interpreter]
  | C_TEST_ADD_ACCOUNT [@only_interpreter]
  | C_TEST_NEW_ACCOUNT [@only_interpreter]
  | C_TEST_BAKER_ACCOUNT [@only_interpreter]
  | C_TEST_REGISTER_DELEGATE [@only_interpreter]
  | C_TEST_BAKE_UNTIL_N_CYCLE_END [@only_interpreter]
  | C_TEST_GET_VOTING_POWER [@only_interpreter]
  | C_TEST_GET_TOTAL_VOTING_POWER [@only_interpreter]
  | C_TEST_REGISTER_CONSTANT [@only_interpreter]
  | C_TEST_CONSTANT_TO_MICHELSON [@only_interpreter]
  | C_TEST_REGISTER_FILE_CONSTANTS [@only_interpreter]
  | C_TEST_PUSH_CONTEXT [@only_interpreter]
  | C_TEST_POP_CONTEXT [@only_interpreter]
  | C_TEST_DROP_CONTEXT [@only_interpreter]
  | C_TEST_FAILWITH [@only_interpreter]
  | C_TEST_READ_CONTRACT_FROM_FILE [@only_interpreter]
  | C_TEST_SIGN [@only_interpreter]
  | C_TEST_GET_ENTRYPOINT [@only_interpreter]
  | C_TEST_INT64_OF_INT [@only_interpreter]
  | C_TEST_INT64_TO_INT [@only_interpreter]
  | C_TEST_LAST_EVENTS [@only_interpreter]
  | C_TEST_TRY_WITH [@only_interpreter]
  | C_TEST_SET_PRINT_VALUES [@only_interpreter]
  (* New with EDO*)
  | C_GLOBAL_CONSTANT
  (* JsLIGO *)
  | C_POLYMORPHIC_ADD [@print "C_POLYMORPHIC_ADD"]
  | C_POLYMORPHIC_SUB [@print "C_POLYMORPHIC_SUB"]
  | C_SUB_MUTEZ
  | C_OPTION_MAP
[@@deriving eq,compare,yojson,hash, print_constant, only_interpreter_tags, read_constant ]


type deprecated = {
  name : string ;
  const : constant' ;
}

type rich_constant =
  | Const of constant'
  [@@deriving eq,compare,yojson,hash]

let const_name (Const c) = c
type 'e t = {
  cons_name: constant' ; (* this is in enum *)
  arguments: 'e list ;
  } [@@deriving eq,compare,yojson,hash, fold, map]

let pp f ppf = fun {cons_name;arguments} ->
  Format.fprintf ppf "@[%a@[<hv 1>(%a)@]@]"
    pp_constant' cons_name
    Simple_utils.PP_helpers.(list_sep_d f) arguments

let fold_map : ('acc -> 'a ->  'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {cons_name;arguments} ->
  let acc,arguments = List.fold_map ~f ~init:acc arguments in
  (acc,{cons_name;arguments})
