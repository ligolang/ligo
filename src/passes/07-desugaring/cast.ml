module I = Ast_sugar
module O = Ast_core

(*those should be generated*)

let cast_var (orig: 'a Var.t Location.wrap) = { orig with wrap_content = Var.todo_cast orig.wrap_content}
let cast_type_constant (i:I.type_constant) = match i with
  | I.TC_unit -> O.TC_unit
  | I.TC_string -> O.TC_string
  | I.TC_bytes -> O.TC_bytes
  | I.TC_nat -> O.TC_nat
  | I.TC_int -> O.TC_int
  | I.TC_mutez -> O.TC_mutez
  | I.TC_operation -> O.TC_operation
  | I.TC_address -> O.TC_address
  | I.TC_key -> O.TC_key
  | I.TC_key_hash -> O.TC_key_hash
  | I.TC_chain_id -> O.TC_chain_id
  | I.TC_signature -> O.TC_signature
  | I.TC_timestamp -> O.TC_timestamp
let cast_type_operator (i:I.type_operator) = match i with
  | I.TC_contract -> O.TC_contract
  | I.TC_option -> O.TC_option
  | I.TC_list -> O.TC_list
  | I.TC_set -> O.TC_set
  | I.TC_map -> O.TC_map
  | I.TC_big_map -> O.TC_big_map
  | I.TC_map_or_big_map -> O.TC_map_or_big_map
  | I.TC_michelson_pair -> O.TC_michelson_pair
  | I.TC_michelson_or -> O.TC_michelson_or
  | I.TC_michelson_pair_right_comb -> O.TC_michelson_pair_right_comb
  | I.TC_michelson_pair_left_comb -> O.TC_michelson_pair_left_comb
  | I.TC_michelson_or_right_comb -> O.TC_michelson_or_right_comb
  | I.TC_michelson_or_left_comb -> O.TC_michelson_or_left_comb
let cast_literal (i:I.literal) =  match i with
  | I.Literal_unit -> O.Literal_unit
  | I.Literal_int a -> O.Literal_int a
  | I.Literal_nat a -> O.Literal_nat a
  | I.Literal_timestamp a -> O.Literal_timestamp a
  | I.Literal_mutez a -> O.Literal_mutez a
  | I.Literal_string a -> O.Literal_string a
  | I.Literal_bytes a -> O.Literal_bytes a
  | I.Literal_address a -> O.Literal_address a
  | I.Literal_signature a -> O.Literal_signature a
  | I.Literal_key a -> O.Literal_key a
  | I.Literal_key_hash a -> O.Literal_key_hash a
  | I.Literal_chain_id a -> O.Literal_chain_id a
  | I.Literal_operation a -> O.Literal_operation a
let cast_constant (i:I.constant') = match i with
  | I.C_INT -> O.C_INT
  | I.C_UNIT -> O.C_UNIT
  | I.C_NIL -> O.C_NIL
  | I.C_NOW -> O.C_NOW
  | I.C_IS_NAT -> O.C_IS_NAT
  | I.C_SOME -> O.C_SOME
  | I.C_NONE -> O.C_NONE
  | I.C_ASSERTION -> O.C_ASSERTION
  | I.C_ASSERT_INFERRED -> O.C_ASSERT_INFERRED
  | I.C_FAILWITH -> O.C_FAILWITH
  | I.C_UPDATE -> O.C_UPDATE
  | I.C_ITER -> O.C_ITER
  | I.C_FOLD_WHILE -> O.C_FOLD_WHILE
  | I.C_FOLD_CONTINUE -> O.C_FOLD_CONTINUE
  | I.C_FOLD_STOP -> O.C_FOLD_STOP
  | I.C_LOOP_LEFT -> O.C_LOOP_LEFT
  | I.C_LOOP_CONTINUE -> O.C_LOOP_CONTINUE
  | I.C_LOOP_STOP -> O.C_LOOP_STOP
  | I.C_FOLD -> O.C_FOLD
  | I.C_NEG -> O.C_NEG
  | I.C_ABS -> O.C_ABS
  | I.C_ADD -> O.C_ADD
  | I.C_SUB -> O.C_SUB
  | I.C_MUL -> O.C_MUL
  | I.C_EDIV -> O.C_EDIV
  | I.C_DIV -> O.C_DIV
  | I.C_MOD -> O.C_MOD
  | I.C_NOT -> O.C_NOT
  | I.C_AND -> O.C_AND
  | I.C_OR -> O.C_OR
  | I.C_XOR -> O.C_XOR
  | I.C_LSL -> O.C_LSL
  | I.C_LSR -> O.C_LSR
  | I.C_EQ -> O.C_EQ
  | I.C_NEQ -> O.C_NEQ
  | I.C_LT -> O.C_LT
  | I.C_GT -> O.C_GT
  | I.C_LE -> O.C_LE
  | I.C_GE -> O.C_GE
  | I.C_SIZE -> O.C_SIZE
  | I.C_CONCAT -> O.C_CONCAT
  | I.C_SLICE -> O.C_SLICE
  | I.C_BYTES_PACK -> O.C_BYTES_PACK
  | I.C_BYTES_UNPACK -> O.C_BYTES_UNPACK
  | I.C_CONS -> O.C_CONS
  | I.C_PAIR -> O.C_PAIR
  | I.C_CAR -> O.C_CAR
  | I.C_CDR -> O.C_CDR
  | I.C_LEFT -> O.C_LEFT
  | I.C_RIGHT -> O.C_RIGHT
  | I.C_SET_EMPTY -> O.C_SET_EMPTY
  | I.C_SET_LITERAL -> O.C_SET_LITERAL
  | I.C_SET_ADD -> O.C_SET_ADD
  | I.C_SET_REMOVE -> O.C_SET_REMOVE
  | I.C_SET_ITER -> O.C_SET_ITER
  | I.C_SET_FOLD -> O.C_SET_FOLD
  | I.C_SET_MEM -> O.C_SET_MEM
  | I.C_LIST_EMPTY -> O.C_LIST_EMPTY
  | I.C_LIST_LITERAL -> O.C_LIST_LITERAL
  | I.C_LIST_ITER -> O.C_LIST_ITER
  | I.C_LIST_MAP -> O.C_LIST_MAP
  | I.C_LIST_FOLD -> O.C_LIST_FOLD
  | I.C_MAP -> O.C_MAP
  | I.C_MAP_EMPTY -> O.C_MAP_EMPTY
  | I.C_MAP_LITERAL -> O.C_MAP_LITERAL
  | I.C_MAP_GET -> O.C_MAP_GET
  | I.C_MAP_GET_FORCE -> O.C_MAP_GET_FORCE
  | I.C_MAP_ADD -> O.C_MAP_ADD
  | I.C_MAP_REMOVE -> O.C_MAP_REMOVE
  | I.C_MAP_UPDATE -> O.C_MAP_UPDATE
  | I.C_MAP_ITER -> O.C_MAP_ITER
  | I.C_MAP_MAP -> O.C_MAP_MAP
  | I.C_MAP_FOLD -> O.C_MAP_FOLD
  | I.C_MAP_MEM -> O.C_MAP_MEM
  | I.C_MAP_FIND -> O.C_MAP_FIND
  | I.C_MAP_FIND_OPT -> O.C_MAP_FIND_OPT
  | I.C_BIG_MAP -> O.C_BIG_MAP
  | I.C_BIG_MAP_EMPTY -> O.C_BIG_MAP_EMPTY
  | I.C_BIG_MAP_LITERAL -> O.C_BIG_MAP_LITERAL
  | I.C_SHA256 -> O.C_SHA256
  | I.C_SHA512 -> O.C_SHA512
  | I.C_BLAKE2b -> O.C_BLAKE2b
  | I.C_HASH -> O.C_HASH
  | I.C_HASH_KEY -> O.C_HASH_KEY
  | I.C_CHECK_SIGNATURE -> O.C_CHECK_SIGNATURE
  | I.C_CHAIN_ID -> O.C_CHAIN_ID
  | I.C_CALL -> O.C_CALL
  | I.C_CONTRACT -> O.C_CONTRACT
  | I.C_CONTRACT_OPT -> O.C_CONTRACT_OPT
  | I.C_CONTRACT_ENTRYPOINT -> O.C_CONTRACT_ENTRYPOINT
  | I.C_CONTRACT_ENTRYPOINT_OPT -> O.C_CONTRACT_ENTRYPOINT_OPT
  | I.C_AMOUNT -> O.C_AMOUNT
  | I.C_BALANCE -> O.C_BALANCE
  | I.C_SOURCE -> O.C_SOURCE
  | I.C_SENDER -> O.C_SENDER
  | I.C_ADDRESS -> O.C_ADDRESS
  | I.C_SELF -> O.C_SELF
  | I.C_SELF_ADDRESS -> O.C_SELF_ADDRESS
  | I.C_IMPLICIT_ACCOUNT -> O.C_IMPLICIT_ACCOUNT
  | I.C_SET_DELEGATE -> O.C_SET_DELEGATE
  | I.C_CREATE_CONTRACT -> O.C_CREATE_CONTRACT
  | I.C_CONVERT_TO_LEFT_COMB -> O.C_CONVERT_TO_LEFT_COMB
  | I.C_CONVERT_TO_RIGHT_COMB -> O.C_CONVERT_TO_RIGHT_COMB
  | I.C_CONVERT_FROM_LEFT_COMB -> O.C_CONVERT_FROM_LEFT_COMB
  | I.C_CONVERT_FROM_RIGHT_COMB -> O.C_CONVERT_FROM_RIGHT_COMB

let cast_constructor (Constructor i:I.constructor') = O.Constructor i

let cast_type_constant_rev (i:O.type_constant) = match i with
  | O.TC_unit -> I.TC_unit
  | O.TC_string -> I.TC_string
  | O.TC_bytes -> I.TC_bytes
  | O.TC_nat -> I.TC_nat
  | O.TC_int -> I.TC_int
  | O.TC_mutez -> I.TC_mutez
  | O.TC_operation -> I.TC_operation
  | O.TC_address -> I.TC_address
  | O.TC_key -> I.TC_key
  | O.TC_key_hash -> I.TC_key_hash
  | O.TC_chain_id -> I.TC_chain_id
  | O.TC_signature -> I.TC_signature
  | O.TC_timestamp -> I.TC_timestamp
let cast_type_operator_rev (i:O.type_operator) = match i with
  | O.TC_contract -> I.TC_contract
  | O.TC_option -> I.TC_option
  | O.TC_list -> I.TC_list
  | O.TC_set -> I.TC_set
  | O.TC_map -> I.TC_map
  | O.TC_big_map -> I.TC_big_map
  | O.TC_map_or_big_map -> I.TC_map_or_big_map
  | O.TC_michelson_pair -> I.TC_michelson_pair
  | O.TC_michelson_or -> I.TC_michelson_or
  | O.TC_michelson_pair_right_comb -> I.TC_michelson_pair_right_comb
  | O.TC_michelson_pair_left_comb -> I.TC_michelson_pair_left_comb
  | O.TC_michelson_or_right_comb -> I.TC_michelson_or_right_comb
  | O.TC_michelson_or_left_comb -> I.TC_michelson_or_left_comb
let cast_literal_rev (i:O.literal) =  match i with
  | O.Literal_unit -> I.Literal_unit
  | O.Literal_int a -> I.Literal_int a
  | O.Literal_nat a -> I.Literal_nat a
  | O.Literal_timestamp a -> I.Literal_timestamp a
  | O.Literal_mutez a -> I.Literal_mutez a
  | O.Literal_string a -> I.Literal_string a
  | O.Literal_bytes a -> I.Literal_bytes a
  | O.Literal_address a -> I.Literal_address a
  | O.Literal_signature a -> I.Literal_signature a
  | O.Literal_key a -> I.Literal_key a
  | O.Literal_key_hash a -> I.Literal_key_hash a
  | O.Literal_chain_id a -> I.Literal_chain_id a
  | O.Literal_operation a -> I.Literal_operation a
let cast_constant_rev (i:O.constant') = match i with
  | O.C_INT -> I.C_INT
  | O.C_UNIT -> I.C_UNIT
  | O.C_NIL -> I.C_NIL
  | O.C_NOW -> I.C_NOW
  | O.C_IS_NAT -> I.C_IS_NAT
  | O.C_SOME -> I.C_SOME
  | O.C_NONE -> I.C_NONE
  | O.C_ASSERTION -> I.C_ASSERTION
  | O.C_ASSERT_INFERRED -> I.C_ASSERT_INFERRED
  | O.C_FAILWITH -> I.C_FAILWITH
  | O.C_UPDATE -> I.C_UPDATE
  | O.C_ITER -> I.C_ITER
  | O.C_FOLD_WHILE -> I.C_FOLD_WHILE
  | O.C_FOLD_CONTINUE -> I.C_FOLD_CONTINUE
  | O.C_FOLD_STOP -> I.C_FOLD_STOP
  | O.C_LOOP_LEFT -> I.C_LOOP_LEFT
  | O.C_LOOP_CONTINUE -> I.C_LOOP_CONTINUE
  | O.C_LOOP_STOP -> I.C_LOOP_STOP
  | O.C_FOLD -> I.C_FOLD
  | O.C_NEG -> I.C_NEG
  | O.C_ABS -> I.C_ABS
  | O.C_ADD -> I.C_ADD
  | O.C_SUB -> I.C_SUB
  | O.C_MUL -> I.C_MUL
  | O.C_EDIV -> I.C_EDIV
  | O.C_DIV -> I.C_DIV
  | O.C_MOD -> I.C_MOD
  | O.C_NOT -> I.C_NOT
  | O.C_AND -> I.C_AND
  | O.C_OR -> I.C_OR
  | O.C_XOR -> I.C_XOR
  | O.C_LSL -> I.C_LSL
  | O.C_LSR -> I.C_LSR
  | O.C_EQ -> I.C_EQ
  | O.C_NEQ -> I.C_NEQ
  | O.C_LT -> I.C_LT
  | O.C_GT -> I.C_GT
  | O.C_LE -> I.C_LE
  | O.C_GE -> I.C_GE
  | O.C_SIZE -> I.C_SIZE
  | O.C_CONCAT -> I.C_CONCAT
  | O.C_SLICE -> I.C_SLICE
  | O.C_BYTES_PACK -> I.C_BYTES_PACK
  | O.C_BYTES_UNPACK -> I.C_BYTES_UNPACK
  | O.C_CONS -> I.C_CONS
  | O.C_PAIR -> I.C_PAIR
  | O.C_CAR -> I.C_CAR
  | O.C_CDR -> I.C_CDR
  | O.C_LEFT -> I.C_LEFT
  | O.C_RIGHT -> I.C_RIGHT
  | O.C_SET_EMPTY -> I.C_SET_EMPTY
  | O.C_SET_LITERAL -> I.C_SET_LITERAL
  | O.C_SET_ADD -> I.C_SET_ADD
  | O.C_SET_REMOVE -> I.C_SET_REMOVE
  | O.C_SET_ITER -> I.C_SET_ITER
  | O.C_SET_FOLD -> I.C_SET_FOLD
  | O.C_SET_MEM -> I.C_SET_MEM
  | O.C_LIST_EMPTY -> I.C_LIST_EMPTY
  | O.C_LIST_LITERAL -> I.C_LIST_LITERAL
  | O.C_LIST_ITER -> I.C_LIST_ITER
  | O.C_LIST_MAP -> I.C_LIST_MAP
  | O.C_LIST_FOLD -> I.C_LIST_FOLD
  | O.C_MAP -> I.C_MAP
  | O.C_MAP_EMPTY -> I.C_MAP_EMPTY
  | O.C_MAP_LITERAL -> I.C_MAP_LITERAL
  | O.C_MAP_GET -> I.C_MAP_GET
  | O.C_MAP_GET_FORCE -> I.C_MAP_GET_FORCE
  | O.C_MAP_ADD -> I.C_MAP_ADD
  | O.C_MAP_REMOVE -> I.C_MAP_REMOVE
  | O.C_MAP_UPDATE -> I.C_MAP_UPDATE
  | O.C_MAP_ITER -> I.C_MAP_ITER
  | O.C_MAP_MAP -> I.C_MAP_MAP
  | O.C_MAP_FOLD -> I.C_MAP_FOLD
  | O.C_MAP_MEM -> I.C_MAP_MEM
  | O.C_MAP_FIND -> I.C_MAP_FIND
  | O.C_MAP_FIND_OPT -> I.C_MAP_FIND_OPT
  | O.C_BIG_MAP -> I.C_BIG_MAP
  | O.C_BIG_MAP_EMPTY -> I.C_BIG_MAP_EMPTY
  | O.C_BIG_MAP_LITERAL -> I.C_BIG_MAP_LITERAL
  | O.C_SHA256 -> I.C_SHA256
  | O.C_SHA512 -> I.C_SHA512
  | O.C_BLAKE2b -> I.C_BLAKE2b
  | O.C_HASH -> I.C_HASH
  | O.C_HASH_KEY -> I.C_HASH_KEY
  | O.C_CHECK_SIGNATURE -> I.C_CHECK_SIGNATURE
  | O.C_CHAIN_ID -> I.C_CHAIN_ID
  | O.C_CALL -> I.C_CALL
  | O.C_CONTRACT -> I.C_CONTRACT
  | O.C_CONTRACT_OPT -> I.C_CONTRACT_OPT
  | O.C_CONTRACT_ENTRYPOINT -> I.C_CONTRACT_ENTRYPOINT
  | O.C_CONTRACT_ENTRYPOINT_OPT -> I.C_CONTRACT_ENTRYPOINT_OPT
  | O.C_AMOUNT -> I.C_AMOUNT
  | O.C_BALANCE -> I.C_BALANCE
  | O.C_SOURCE -> I.C_SOURCE
  | O.C_SENDER -> I.C_SENDER
  | O.C_ADDRESS -> I.C_ADDRESS
  | O.C_SELF -> I.C_SELF
  | O.C_SELF_ADDRESS -> I.C_SELF_ADDRESS
  | O.C_IMPLICIT_ACCOUNT -> I.C_IMPLICIT_ACCOUNT
  | O.C_SET_DELEGATE -> I.C_SET_DELEGATE
  | O.C_CREATE_CONTRACT -> I.C_CREATE_CONTRACT
  | O.C_CONVERT_TO_LEFT_COMB -> I.C_CONVERT_TO_LEFT_COMB
  | O.C_CONVERT_TO_RIGHT_COMB -> I.C_CONVERT_TO_RIGHT_COMB
  | O.C_CONVERT_FROM_LEFT_COMB -> I.C_CONVERT_FROM_LEFT_COMB
  | O.C_CONVERT_FROM_RIGHT_COMB -> I.C_CONVERT_FROM_RIGHT_COMB