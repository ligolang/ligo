type expression_
and expression_variable = expression_ Var.t
type type_
and type_variable = type_ Var.t


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
    | TC_bool
    | TC_operation
    | TC_address
    | TC_key
    | TC_key_hash
    | TC_chain_id
    | TC_signature
    | TC_timestamp
    | TC_void
module type AST_PARAMETER_TYPE = sig
  type type_meta
end

module Ast_generic_type (PARAMETER : AST_PARAMETER_TYPE) = struct
  open PARAMETER

  type type_content =
    | T_sum of type_expression constructor_map
    | T_record of type_expression label_map
    | T_arrow of arrow
    | T_variable of type_variable
    | T_constant of type_constant
    | T_operator of type_operator

  and arrow = {type1: type_expression; type2: type_expression}

  and type_operator =
    | TC_contract of type_expression
    | TC_option of type_expression
    | TC_list of type_expression
    | TC_set of type_expression
    | TC_map of type_expression * type_expression
    | TC_big_map of type_expression * type_expression
    | TC_arrow of type_expression * type_expression


  and type_expression = {type_content: type_content; type_meta: type_meta}

  open Trace
  let map_type_operator f = function
      TC_contract x -> TC_contract (f x)
    | TC_option x -> TC_option (f x)
    | TC_list x -> TC_list (f x)
    | TC_set x -> TC_set (f x)
    | TC_map (x , y) -> TC_map (f x , f y)
    | TC_big_map (x , y)-> TC_big_map (f x , f y)
    | TC_arrow (x, y) -> TC_arrow (f x, f y)

  let bind_map_type_operator f = function
      TC_contract x -> let%bind x = f x in ok @@ TC_contract x
    | TC_option x -> let%bind x = f x in ok @@ TC_option x
    | TC_list x -> let%bind x = f x in ok @@ TC_list x
    | TC_set x -> let%bind x = f x in ok @@ TC_set x
    | TC_map (x , y) -> let%bind x = f x in let%bind y = f y in ok @@ TC_map (x , y)
    | TC_big_map (x , y)-> let%bind x = f x in let%bind y = f y in ok @@ TC_big_map (x , y)
    | TC_arrow (x , y)-> let%bind x = f x in let%bind y = f y in ok @@ TC_arrow (x , y)

  let type_operator_name = function
        TC_contract _ -> "TC_contract"
      | TC_option   _ -> "TC_option"
      | TC_list     _ -> "TC_list"
      | TC_set      _ -> "TC_set"
      | TC_map      _ -> "TC_map"
      | TC_big_map  _ -> "TC_big_map"
      | TC_arrow    _ -> "TC_arrow"

  let type_expression'_of_string = function
    | "TC_contract" , [x]     -> ok @@ T_operator(TC_contract x)
    | "TC_option"   , [x]     -> ok @@ T_operator(TC_option x)
    | "TC_list"     , [x]     -> ok @@ T_operator(TC_list x)
    | "TC_set"      , [x]     -> ok @@ T_operator(TC_set x)
    | "TC_map"      , [x ; y] -> ok @@ T_operator(TC_map (x , y))
    | "TC_big_map"  , [x ; y] -> ok @@ T_operator(TC_big_map (x, y))
    | ("TC_contract" | "TC_option" | "TC_list" | "TC_set" | "TC_map" | "TC_big_map"), _ ->
      failwith "internal error: wrong number of arguments for type operator"

    | "TC_unit"      , [] -> ok @@ T_constant(TC_unit)
    | "TC_string"    , [] -> ok @@ T_constant(TC_string)
    | "TC_bytes"     , [] -> ok @@ T_constant(TC_bytes)
    | "TC_nat"       , [] -> ok @@ T_constant(TC_nat)
    | "TC_int"       , [] -> ok @@ T_constant(TC_int)
    | "TC_mutez"     , [] -> ok @@ T_constant(TC_mutez)
    | "TC_bool"      , [] -> ok @@ T_constant(TC_bool)
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
    | TC_contract  x       -> "TC_contract" , [x]
    | TC_option    x       -> "TC_option"   , [x]
    | TC_list      x       -> "TC_list"     , [x]
    | TC_set       x       -> "TC_set"      , [x]
    | TC_map       (x , y) -> "TC_map"      , [x ; y]
    | TC_big_map   (x , y) -> "TC_big_map"  , [x ; y]
    | TC_arrow     (x , y) -> "TC_arrow"    , [x ; y]

  let string_of_type_constant = function
    | TC_unit      -> "TC_unit", []
    | TC_string    -> "TC_string", []
    | TC_bytes     -> "TC_bytes", []
    | TC_nat       -> "TC_nat", []
    | TC_int       -> "TC_int", []
    | TC_mutez     -> "TC_mutez", []
    | TC_bool      -> "TC_bool", []
    | TC_operation -> "TC_operation", []
    | TC_address   -> "TC_address", []
    | TC_key       -> "TC_key", []
    | TC_key_hash  -> "TC_key_hash", []
    | TC_chain_id  -> "TC_chain_id", []
    | TC_signature -> "TC_signature", []
    | TC_timestamp -> "TC_timestamp", []
    | TC_void      -> "TC_void", []

  let string_of_type_expression' = function
    | T_operator o -> string_of_type_operator o
    | T_constant c -> string_of_type_constant c
    | T_sum _ | T_record _ | T_arrow _ | T_variable _ ->
      failwith "not a type operator or constant"

end

type literal =
  | Literal_unit
  | Literal_bool of bool
  | Literal_int of int
  | Literal_nat of int
  | Literal_timestamp of int
  | Literal_mutez of int
  | Literal_string of string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_void
  | Literal_operation of
      Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation
and ('a,'tv) matching_content =
  | Match_bool of {
      match_true : 'a ;
      match_false : 'a ;
    }
  | Match_list of {
      match_nil : 'a ;
      match_cons : expression_variable * expression_variable * 'a * 'tv;
    }
  | Match_option of {
      match_none : 'a ;
      match_some : expression_variable * 'a * 'tv;
    }
  | Match_tuple of (expression_variable list * 'a) * 'tv list
  | Match_variant of ((constructor' * expression_variable) * 'a) list * 'tv

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
  | C_CONTINUE
  | C_STOP
  | C_FOLD
  (* MATH *)
  | C_NEG
  | C_ABS
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
