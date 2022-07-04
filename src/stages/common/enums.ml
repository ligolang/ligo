type z = Z.t [@@deriving ord]
type ligo_string = Simple_utils.Ligo_string.t [@@deriving yojson, ord, hash]

let [@warning "-32"] z_to_yojson x = `String (Z.to_string x)
let [@warning "-32"] z_of_yojson x =
  try match x with
    | `String s -> Ok (Z.of_string s)
    | _ -> Simple_utils.Utils.error_yojson_format "JSON string"
  with
  | Invalid_argument _ ->
    Error "Invalid formatting.
            The Zarith library does not know how to handle this formatting."

let bytes_to_yojson b = `String (Bytes.to_string b)

type layout =
  | L_comb
  | L_tree [@@deriving hash]

let hash_fold_bytes st b = Hash.fold_string st (Bytes.to_string b)
let hash_fold_z st z = Hash.fold_int64 st (Z.to_int64 z)

type literal =
  | Literal_unit
  | Literal_int of z
  | Literal_nat of z
  | Literal_timestamp of z
  | Literal_mutez of z
  | Literal_string of ligo_string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_operation of bytes
  | Literal_bls12_381_g1 of bytes
  | Literal_bls12_381_g2 of bytes
  | Literal_bls12_381_fr of bytes
  | Literal_chest of bytes
  | Literal_chest_key of bytes
[@@deriving yojson, ord, hash]

let literal_to_enum = function
  | Literal_unit        ->  1
  | Literal_int _       ->  2
  | Literal_nat _       ->  3
  | Literal_timestamp _ ->  4
  | Literal_mutez _     ->  5
  | Literal_string _    ->  6
  | Literal_bytes _     ->  7
  | Literal_address _   ->  8
  | Literal_signature _ ->  9
  | Literal_key _       -> 10
  | Literal_key_hash _  -> 11
  | Literal_chain_id _  -> 12
  | Literal_operation _ -> 13
  | Literal_bls12_381_g1 _ -> 14
  | Literal_bls12_381_g2 _ -> 15
  | Literal_bls12_381_fr _ -> 16
  | Literal_chest _ -> 17
  | Literal_chest_key _ -> 18

type constant' =
  | C_UNIT
  | C_NIL
  | C_SOME
  | C_NONE
  | C_UNOPT
  | C_UNOPT_WITH_ERROR
  | C_ASSERT_INFERRED
  | C_UPDATE
  (* Loops *)
  | C_ITER
  | C_LOOP_LEFT
  | C_LOOP_CONTINUE
  | C_LOOP_STOP
  | C_FOLD
  | C_FOLD_LEFT
  | C_FOLD_RIGHT
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
  | C_BYTES_UNPACK
  | C_CONS
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
  (* List *)
  | C_LIST_EMPTY
  | C_LIST_LITERAL
  | C_LIST_ITER
  | C_LIST_MAP
  | C_LIST_FOLD
  | C_LIST_FOLD_LEFT
  | C_LIST_FOLD_RIGHT
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
  (* Big Maps *)
  | C_BIG_MAP
  | C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL
  | C_BIG_MAP_GET_AND_UPDATE
  (* Blockchain *)
  | C_CALL
  | C_CONTRACT
  | C_CONTRACT_OPT
  | C_CONTRACT_WITH_ERROR
  | C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT
  | C_ADDRESS
  | C_SELF
  | C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT
  | C_SET_DELEGATE
  | C_CREATE_CONTRACT
  | C_OPEN_CHEST
  | C_VIEW
  (* Tests - ligo interpreter only *)
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
  | C_TEST_MUTATE_VALUE [@only_interpreter]
  | C_TEST_MUTATION_TEST [@only_interpreter]
  | C_TEST_MUTATION_TEST_ALL [@only_interpreter]
  | C_TEST_SAVE_MUTATION [@only_interpreter]
  | C_TEST_RUN [@only_interpreter]
  | C_TEST_COMPILE_CONTRACT [@only_interpreter]
  | C_TEST_DECOMPILE [@only_interpreter]
  | C_TEST_TO_CONTRACT [@only_interpreter]
  | C_TEST_TO_ENTRYPOINT [@only_interpreter]
  | C_TEST_COMPILE_CONTRACT_FROM_FILE [@only_interpreter]
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
  (* New with EDO*)
  | C_SAPLING_VERIFY_UPDATE
  | C_SAPLING_EMPTY_STATE
  | C_GLOBAL_CONSTANT
  (* JsLIGO *)
  | C_POLYMORPHIC_ADD [@print "C_POLYMORPHIC_ADD"]
  | C_POLYMORPHIC_SUB [@print "C_POLYMORPHIC_SUB"]
  | C_SUB_MUTEZ
  | C_OPTION_MAP
[@@deriving enum, yojson, print_constant, only_interpreter_tags, read_constant ]

type deprecated = {
  name : string ;
  const : constant' ;
}

type rich_constant =
  | Const of constant'
