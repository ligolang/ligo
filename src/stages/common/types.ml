
type expression_
type type_

type expression_variable = expression_ Var.t
type type_variable = type_ Var.t
type constructor = Constructor of string
type label = Label of string
module CMap = Map.Make( struct type t = constructor let compare (Constructor a) (Constructor b) = compare a b end)
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)

type 'a label_map = 'a LMap.t
type 'a constructor_map = 'a CMap.t


let bind_lmap (l:_ label_map) =
  let open Trace in
  let open LMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux l (ok empty)

let bind_cmap (c:_ constructor_map) =
  let open Trace in
  let open CMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux c (ok empty)

let bind_fold_lmap f init (lmap:_ LMap.t) =
  let open Trace in
  let aux k v prev =
    prev >>? fun prev' ->
    f prev' k v
  in
  LMap.fold aux lmap init

let bind_map_lmap f map = bind_lmap (LMap.map f map)
let bind_map_cmap f map = bind_cmap (CMap.map f map)

type access =
  | Access_tuple of int
  | Access_record of string

and access_path = access list

and literal =
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
  | Literal_operation of Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation

(* The ast is a tree of node, 'a is the type of the node (type_variable or {type_variable, previous_type}) *)
type 'a type_expression' = 
  | T_tuple of 'a list 
  | T_sum of 'a constructor_map
  | T_record of 'a label_map
  | T_arrow of 'a * 'a
  | T_variable of type_variable
  | T_constant of type_constant
  | T_operator of 'a type_operator
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

and 'a type_operator = 
  | TC_contract of 'a
  | TC_option of 'a
  | TC_list of 'a
  | TC_set of 'a
  | TC_map of 'a * 'a
  | TC_big_map of 'a * 'a
  | TC_arrow of 'a * 'a

type type_base =
  | Base_unit 
  | Base_string 
  | Base_bytes 
  | Base_nat 
  | Base_int 
  | Base_mutez
  | Base_bool
  | Base_operation 
  | Base_address
  | Base_void
  | Base_timestamp
  | Base_signature
  | Base_key
  | Base_key_hash
  | Base_chain_id

and ('a,'tv) matching =
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
  | Match_variant of ((constructor * expression_variable) * 'a) list * 'tv

type constant = 
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
  | C_LIST_CONS
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
  | C_CONTRACT_ENTRYPOINT
  | C_AMOUNT
  | C_BALANCE
  | C_SOURCE
  | C_SENDER
  | C_ADDRESS
  | C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT
  | C_SET_DELEGATE
  | C_STEPS_TO_QUOTA
