type expression_
and expression_variable = expression_ Var.t Location.wrap
type type_
and type_variable = type_ Var.t

type ligo_string = Simple_utils.Ligo_string.t

type constructor' = Constructor of string
type label = Label of string

module CMap = Map.Make( struct type t = constructor' let compare (Constructor a) (Constructor b) = compare a b end)
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)


type 'a label_map = 'a LMap.t
type 'a constructor_map = 'a CMap.t

  and type_constant =
    | TC_unit
    | TC_string
    | TC_bytes
    | TC_nat
    | TC_int
    | TC_mutez
    | TC_operation
    | TC_address
    | TC_key
    | TC_key_hash
    | TC_chain_id
    | TC_signature
    | TC_timestamp

  and type_operator =
    | TC_contract
    | TC_option
    | TC_list
    | TC_set
    | TC_map
    | TC_big_map
    | TC_map_or_big_map
    | TC_michelson_pair
    | TC_michelson_or
    | TC_michelson_pair_right_comb
    | TC_michelson_pair_left_comb
    | TC_michelson_or_right_comb
    | TC_michelson_or_left_comb

module type AST_PARAMETER_TYPE = sig
  type type_meta
end

module Ast_generic_type (PARAMETER : AST_PARAMETER_TYPE) = struct
  open PARAMETER


  type type_content =
    | T_sum of ctor_content constructor_map
    | T_record of field_content label_map
    | T_arrow of arrow
    | T_variable of type_variable
    | T_constant of type_constant
    | T_operator of (type_operator * type_expression list)

  and arrow = {type1: type_expression; type2: type_expression}
  
  and ctor_content = {ctor_type : type_expression ; michelson_annotation : string option ; ctor_decl_pos : int}

  and field_content = {field_type : type_expression ; field_annotation : string option ; field_decl_pos : int}


  and type_expression = {type_content: type_content; location: Location.t; type_meta: type_meta}

  open Trace
  let type_operator_name = function
        TC_contract                  -> "TC_contract"
      | TC_option                    -> "TC_option"
      | TC_list                      -> "TC_list"
      | TC_set                       -> "TC_set"
      | TC_map                       -> "TC_map"
      | TC_big_map                   -> "TC_big_map"
      | TC_map_or_big_map            -> "TC_map_or_big_map"
      | TC_michelson_pair            -> "TC_michelson_pair"
      | TC_michelson_or              -> "TC_michelson_or"
      | TC_michelson_pair_right_comb -> "TC_michelson_pair_right_comb" 
      | TC_michelson_pair_left_comb  -> "TC_michelson_pair_left_comb" 
      | TC_michelson_or_right_comb   -> "TC_michelson_or_right_comb" 
      | TC_michelson_or_left_comb    -> "TC_michelson_or_left_comb" 

  let type_expression'_of_string = function
    | "TC_contract" , [x]     -> ok @@ T_operator(TC_contract, [x])
    | "TC_option"   , [x]     -> ok @@ T_operator(TC_option, [x])
    | "TC_list"     , [x]     -> ok @@ T_operator(TC_list, [x])
    | "TC_set"      , [x]     -> ok @@ T_operator(TC_set, [x])
    | "TC_map"      , [x ; y] -> ok @@ T_operator(TC_map, [x ; y])
    | "TC_big_map"  , [x ; y] -> ok @@ T_operator(TC_big_map, [x; y])
    | ("TC_contract" | "TC_option" | "TC_list" | "TC_set" | "TC_map" | "TC_big_map"), _ ->
      failwith "internal error: wrong number of arguments for type operator"

    | "TC_unit"      , [] -> ok @@ T_constant(TC_unit)
    | "TC_string"    , [] -> ok @@ T_constant(TC_string)
    | "TC_bytes"     , [] -> ok @@ T_constant(TC_bytes)
    | "TC_nat"       , [] -> ok @@ T_constant(TC_nat)
    | "TC_int"       , [] -> ok @@ T_constant(TC_int)
    | "TC_mutez"     , [] -> ok @@ T_constant(TC_mutez)
    | "TC_operation" , [] -> ok @@ T_constant(TC_operation)
    | "TC_address"   , [] -> ok @@ T_constant(TC_address)
    | "TC_key"       , [] -> ok @@ T_constant(TC_key)
    | "TC_key_hash"  , [] -> ok @@ T_constant(TC_key_hash)
    | "TC_chain_id"  , [] -> ok @@ T_constant(TC_chain_id)
    | "TC_signature" , [] -> ok @@ T_constant(TC_signature)
    | "TC_timestamp" , [] -> ok @@ T_constant(TC_timestamp)
    | _,               [] ->
      failwith "internal error: wrong number of arguments for type constant"
    | _                       ->
      failwith "internal error: unknown type operator"

  let string_of_type_operator = function
    | TC_contract                  , lst -> "TC_contract"                  , lst
    | TC_option                    , lst -> "TC_option"                    , lst
    | TC_list                      , lst -> "TC_list"                      , lst
    | TC_set                       , lst -> "TC_set"                       , lst
    | TC_map                       , lst -> "TC_map"                       , lst
    | TC_big_map                   , lst -> "TC_big_map"                   , lst
    | TC_map_or_big_map            , lst -> "TC_map_or_big_map"            , lst
    | TC_michelson_pair            , lst -> "TC_michelson_pair"            , lst
    | TC_michelson_or              , lst -> "TC_michelson_or"              , lst
    | TC_michelson_pair_right_comb , lst -> "TC_michelson_pair_right_comb" , lst
    | TC_michelson_pair_left_comb  , lst -> "TC_michelson_pair_left_comb"  , lst
    | TC_michelson_or_right_comb   , lst -> "TC_michelson_or_right_comb"   , lst
    | TC_michelson_or_left_comb    , lst -> "TC_michelson_or_left_comb"    , lst

  let string_of_type_constant = function
    | TC_unit      -> "TC_unit", []
    | TC_string    -> "TC_string", []
    | TC_bytes     -> "TC_bytes", []
    | TC_nat       -> "TC_nat", []
    | TC_int       -> "TC_int", []
    | TC_mutez     -> "TC_mutez", []
    | TC_operation -> "TC_operation", []
    | TC_address   -> "TC_address", []
    | TC_key       -> "TC_key", []
    | TC_key_hash  -> "TC_key_hash", []
    | TC_chain_id  -> "TC_chain_id", []
    | TC_signature -> "TC_signature", []
    | TC_timestamp -> "TC_timestamp", []

  let string_of_type_expression' = function
    | T_operator o -> string_of_type_operator o
    | T_constant c -> string_of_type_constant c
    | T_sum _ | T_record _ | T_arrow _ | T_variable _ ->
      failwith "not a type operator or constant"

end

type literal =
  | Literal_unit
  | Literal_int of Z.t
  | Literal_nat of Z.t
  | Literal_timestamp of Z.t
  | Literal_mutez of Z.t
  | Literal_string of ligo_string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_operation of
      Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation
and constant' =
  | C_INT
  | C_UNIT
  | C_NIL
  | C_NOW
  | C_IS_NAT
  | C_SOME
  | C_NONE
  | C_ASSERTION
  | C_ASSERT_INFERRED
  | C_FAILWITH
  | C_UPDATE
  (* Loops *)
  | C_ITER
  | C_FOLD_WHILE
  | C_FOLD_CONTINUE
  | C_FOLD_STOP
  | C_LOOP_LEFT
  | C_LOOP_CONTINUE
  | C_LOOP_STOP
  | C_FOLD
  (* MATH *)
  | C_NEG
  | C_ABS
  | C_ADD
  | C_SUB
  | C_MUL
  | C_EDIV
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
  | C_SIZE
  | C_CONCAT
  | C_SLICE
  | C_BYTES_PACK
  | C_BYTES_UNPACK
  | C_CONS
  (* Pair *)
  | C_PAIR
  | C_CAR
  | C_CDR
  | C_LEFT
  | C_RIGHT
  (* Set *)
  | C_SET_EMPTY
  | C_SET_LITERAL
  | C_SET_ADD
  | C_SET_REMOVE
  | C_SET_ITER
  | C_SET_FOLD
  | C_SET_MEM
  (* List *)
  | C_LIST_EMPTY
  | C_LIST_LITERAL
  | C_LIST_ITER
  | C_LIST_MAP
  | C_LIST_FOLD
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
  | C_MAP_MEM
  | C_MAP_FIND
  | C_MAP_FIND_OPT
  (* Big Maps *)
  | C_BIG_MAP
  | C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL
  (* Crypto *)
  | C_SHA256
  | C_SHA512
  | C_BLAKE2b
  | C_HASH
  | C_HASH_KEY
  | C_CHECK_SIGNATURE
  | C_CHAIN_ID
  (* Blockchain *)
  | C_CALL
  | C_CONTRACT
  | C_CONTRACT_OPT
  | C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT
  | C_AMOUNT
  | C_BALANCE
  | C_SOURCE
  | C_SENDER
  | C_ADDRESS
  | C_SELF
  | C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT
  | C_SET_DELEGATE
  | C_CREATE_CONTRACT
  | C_CONVERT_TO_LEFT_COMB
  | C_CONVERT_TO_RIGHT_COMB
  | C_CONVERT_FROM_LEFT_COMB
  | C_CONVERT_FROM_RIGHT_COMB
