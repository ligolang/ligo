(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Stacking. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Tree_abstraction = struct

  open Ast_imperative
  (*
    Each front-end has its owns constants.

    Constants are special names that have their own case in the AST. E_constant
    for regular constants, and T_constant for type constants. Both types are
    defined in `Ast_core/types.ml`.
    For instance, "2 + 2" in Pascaligo is translated to `E_constant ("ADD" , [
      E_literal (Literal_int 2) ;
      E_literal (Literal_int 2) ;
    ])`.

    They are used to represent what can't expressed in the languages:
    - Primitives. Like "int", "string", "unit" for types. Or "+" for values.
    - Tezos specific stuff. Like "operation" for types. Or "source" for values.
    - What can't be represented in the language yet. Like "list" or "List.fold".

    Each constant is expressed as a pair:
    - The left-hand-side is the reserved name in the given front-end.
    - The right-hand-side is the name that will be used in the AST.
  *)
  let type_constants s =
    match s with
      "chain_id"  -> Some TC_chain_id
    | "unit"      -> Some TC_unit
    | "string"    -> Some TC_string
    | "bytes"     -> Some TC_bytes
    | "nat"       -> Some TC_nat
    | "int"       -> Some TC_int
    | "tez"       -> Some TC_mutez
    | "operation" -> Some TC_operation
    | "address"   -> Some TC_address
    | "key"       -> Some TC_key
    | "key_hash"  -> Some TC_key_hash
    | "signature" -> Some TC_signature
    | "timestamp" -> Some TC_timestamp
    | "list"                      -> Some (TC_list)
    | "option"                    -> Some (TC_option)
    | "set"                       -> Some (TC_set)
    | "map"                       -> Some (TC_map)
    | "big_map"                   -> Some (TC_big_map)
    | "contract"                  -> Some (TC_contract)
    | "michelson_pair_right_comb" -> Some (TC_michelson_pair_right_comb)
    | "michelson_pair_left_comb"  -> Some (TC_michelson_pair_left_comb)
    | "michelson_or_right_comb"   -> Some (TC_michelson_or_right_comb)
    | "michelson_or_left_comb"    -> Some (TC_michelson_or_left_comb)
    | _                           -> None

  let type_constant_to_string tc = 
    match tc with
      TC_chain_id  -> "chain_id"  
    | TC_unit      -> "unit"      
    | TC_string    -> "string"    
    | TC_bytes     -> "bytes"     
    | TC_nat       -> "nat"       
    | TC_int       -> "int"       
    | TC_mutez     -> "tez"       
    | TC_operation -> "operation" 
    | TC_address   -> "address"   
    | TC_key       -> "key"       
    | TC_key_hash  -> "key_hash"  
    | TC_signature -> "signature" 
    | TC_timestamp -> "timestamp" 
    | TC_list -> "list"                      
    | TC_option -> "option"                    
    | TC_set -> "set"                       
    | TC_map -> "map"                       
    | TC_big_map -> "big_map"                   
    | TC_contract -> "contract"                  
    | TC_michelson_pair -> "michelson_pair"
    | TC_michelson_or -> "michelson_or"
    | TC_michelson_pair_right_comb -> "michelson_pair_right_comb" 
    | TC_michelson_pair_left_comb -> "michelson_pair_left_comb"  
    | TC_michelson_or_right_comb -> "michelson_or_right_comb"   
    | TC_michelson_or_left_comb -> "michelson_or_left_comb"    
    | TC_map_or_big_map -> "map_or_big_map"


  let pseudo_modules = function
    | "Tezos.chain_id"           -> Some C_CHAIN_ID
    | "Tezos.balance"            -> Some C_BALANCE
    | "Tezos.now"                -> Some C_NOW
    | "Tezos.amount"             -> Some C_AMOUNT
    | "Tezos.sender"             -> Some C_SENDER
    | "Tezos.address"            -> Some C_ADDRESS
    | "Tezos.self"               -> Some C_SELF
    | "Tezos.self_address"       -> Some C_SELF_ADDRESS
    | "Tezos.implicit_account"   -> Some C_IMPLICIT_ACCOUNT
    | "Tezos.source"             -> Some C_SOURCE
    | "Tezos.failwith"           -> Some C_FAILWITH
    | "Tezos.create_contract"    -> Some C_CREATE_CONTRACT
    | "Tezos.transaction"        -> Some C_CALL
    | "Tezos.set_delegate"       -> Some C_SET_DELEGATE
    | "Tezos.get_contract_opt"   -> Some C_CONTRACT_OPT
    | "Tezos.get_entrypoint_opt" -> Some C_CONTRACT_ENTRYPOINT_OPT

    (* Crypto module *)

    | "Crypto.check"    -> Some C_CHECK_SIGNATURE
    | "Crypto.hash_key" -> Some C_HASH_KEY    
    | "Crypto.blake2b"  -> Some C_BLAKE2b
    | "Crypto.sha256"   -> Some C_SHA256    
    | "Crypto.sha512"   -> Some C_SHA512
    
    (* Bytes module *)

    | "Bytes.pack"   -> Some C_BYTES_PACK    
    | "Bytes.unpack" -> Some C_BYTES_UNPACK
    | "Bytes.length" -> Some C_SIZE    
    | "Bytes.concat" -> Some C_CONCAT
    | "Bytes.sub"    -> Some C_SLICE

    (* List module *)   

    | "List.length" -> Some C_SIZE
    | "List.size"   -> Some C_SIZE
    | "List.iter"   -> Some C_LIST_ITER
    | "List.map"    -> Some C_LIST_MAP
    | "List.fold"   -> Some C_LIST_FOLD

    (* Set module *)   

    | "Set.empty"    -> Some C_SET_EMPTY
    | "Set.literal"  -> Some C_SET_LITERAL
    | "Set.cardinal" -> Some C_SIZE
    | "Set.mem"      -> Some C_SET_MEM
    | "Set.add"      -> Some C_SET_ADD
    | "Set.remove"   -> Some C_SET_REMOVE
    | "Set.iter"     -> Some C_SET_ITER
    | "Set.fold"     -> Some C_SET_FOLD

    (* Map module *)   

    | "Map.find_opt" -> Some C_MAP_FIND_OPT
    | "Map.update"   -> Some C_MAP_UPDATE
    | "Map.iter"     -> Some C_MAP_ITER
    | "Map.map"      -> Some C_MAP_MAP
    | "Map.fold"     -> Some C_MAP_FOLD
    | "Map.mem"      -> Some C_MAP_MEM
    | "Map.size"     -> Some C_SIZE
    | "Map.add"      -> Some C_MAP_ADD
    | "Map.remove"   -> Some C_MAP_REMOVE
    | "Map.empty"    -> Some C_MAP_EMPTY
    | "Map.literal"  -> Some C_MAP_LITERAL
    
    (* Big_map module *)   
    
    | "Big_map.find"     -> Some C_MAP_FIND
    | "Big_map.find_opt" -> Some C_MAP_FIND_OPT
    | "Big_map.update"   -> Some C_MAP_UPDATE
    | "Big_map.literal"  -> Some C_BIG_MAP_LITERAL
    | "Big_map.empty"    -> Some C_BIG_MAP_EMPTY
    | "Big_map.mem"      -> Some C_MAP_MEM
    | "Big_map.remove"   -> Some C_MAP_REMOVE
    | "Big_map.add"      -> Some C_MAP_ADD

    (* Bitwise module *)

    | "Bitwise.or"          -> Some C_OR
    | "Bitwise.and"         -> Some C_AND
    | "Bitwise.xor"         -> Some C_XOR
    | "Bitwise.shift_left"  -> Some C_LSL
    | "Bitwise.shift_right" -> Some C_LSR

    (* String module *)

    | "String.length"   -> Some C_SIZE
    | "String.size"     -> Some C_SIZE  (* Deprecated *)
    | "String.slice"    -> Some C_SLICE (* Deprecated *)
    | "String.sub"      -> Some C_SLICE
    | "String.concat"   -> Some C_CONCAT

    (* michelson pair/or type converter module *)

    | "Layout.convert_to_right_comb" -> Some C_CONVERT_TO_RIGHT_COMB
    | "Layout.convert_to_left_comb" -> Some C_CONVERT_TO_LEFT_COMB
    | "Layout.convert_from_right_comb" -> Some C_CONVERT_FROM_RIGHT_COMB
    | "Layout.convert_from_left_comb" -> Some C_CONVERT_FROM_LEFT_COMB

    | _ -> None


  let pseudo_module_to_string = function
    | C_CHAIN_ID                -> "Tezos.chain_id"
    | C_BALANCE                 -> "Tezos.balance"
    | C_NOW                     -> "Tezos.now"
    | C_AMOUNT                  -> "Tezos.amount"
    | C_SENDER                  -> "Tezos.sender"
    | C_ADDRESS                 -> "Tezos.address"
    | C_SELF                    -> "Tezos.self"
    | C_SELF_ADDRESS            -> "Tezos.self_address"
    | C_IMPLICIT_ACCOUNT        -> "Tezos.implicit_account"
    | C_SOURCE                  -> "Tezos.source"
    | C_FAILWITH                -> "Tezos.failwith"
    | C_CREATE_CONTRACT         -> "Tezos.create_contract"
    | C_CALL                    -> "Tezos.transaction"
    | C_SET_DELEGATE            -> "Tezos.set_delegate"
    | C_CONTRACT_OPT            -> "Tezos.get_contract_opt"
    | C_CONTRACT_ENTRYPOINT_OPT -> "Tezos.get_entrypoint_opt"
    | C_CONTRACT                -> "Tezos.get_contract"
    | C_CONTRACT_ENTRYPOINT     -> "Tezos.get_entrypoint"

    (* Crypto module *)

    | C_CHECK_SIGNATURE -> "Crypto.check"
    | C_HASH_KEY        -> "Crypto.hash_key"
    | C_BLAKE2b         -> "Crypto.blake2b"
    | C_SHA256          -> "Crypto.sha256"
    | C_SHA512          -> "Crypto.sha512"
    
    (* Bytes module *)

    | C_BYTES_PACK   -> "Bytes.pack"
    | C_BYTES_UNPACK -> "Bytes.unpack"
    | C_SIZE         -> "Bytes.length"
    | C_CONCAT       -> "Bytes.concat"
    | C_SLICE        -> "Bytes.sub"

    (* List module *)   

  (*  | C_SIZE      -> "List.size" *)
    | C_LIST_ITER -> "List.iter"
    | C_LIST_MAP  -> "List.map"
    | C_LIST_FOLD -> "List.fold"

    (* Set module *)   

    | C_SET_EMPTY   -> "Set.empty"
    | C_SET_LITERAL -> "Set.literal"
   (* | C_SIZE        -> "Set.cardinal"*)
    | C_SET_MEM     -> "Set.mem"
    | C_SET_ADD     -> "Set.add"
    | C_SET_REMOVE  -> "Set.remove"
    | C_SET_ITER    -> "Set.iter"
    | C_SET_FOLD    -> "Set.fold"

    (* Map module *)   

    | C_MAP_FIND_OPT -> "Map.find_opt"
    | C_MAP_UPDATE   -> "Map.update"
    | C_MAP_ITER     -> "Map.iter"
    | C_MAP_MAP      -> "Map.map"
    | C_MAP_FOLD     -> "Map.fold"
    | C_MAP_MEM      -> "Map.mem"
  (*  | C_SIZE         -> "Map.size" *)
    | C_MAP_ADD      -> "Map.add"
    | C_MAP_REMOVE   -> "Map.remove"
    | C_MAP_EMPTY    -> "Map.empty"
    | C_MAP_LITERAL  -> "Map.literal"
    
    (* Big_map module *)   
    
    | C_MAP_FIND        -> "Big_map.find"
  (*  | C_MAP_FIND_OPT    -> "Big_map.find_opt"
    | C_MAP_UPDATE      -> "Big_map.update" *)
    | C_BIG_MAP_LITERAL -> "Big_map.literal"
    | C_BIG_MAP_EMPTY   -> "Big_map.empty"
  (*  | C_MAP_MEM         -> "Big_map.mem"
    | C_MAP_REMOVE      -> "Big_map.remove"
    | C_MAP_ADD         -> "Big_map.add" *)

    (* Bitwise module *)

    | C_OR  -> "Bitwise.or"
    | C_AND -> "Bitwise.and"
    | C_XOR -> "Bitwise.xor"
    | C_LSL -> "Bitwise.shift_left"
    | C_LSR -> "Bitwise.shift_right"

    (* String module *)

  (*  | C_SIZE   -> "String.length" (* will never trigger, rename size *)
    | C_SLICE  -> "String.sub"
    | C_CONCAT -> "String.concat" *)

    (* michelson pair/or type converter module *)

    | C_CONVERT_TO_RIGHT_COMB   -> "Layout.convert_to_right_comb"
    | C_CONVERT_TO_LEFT_COMB    -> "Layout.convert_to_left_comb"
    | C_CONVERT_FROM_RIGHT_COMB -> "Layout.convert_from_right_comb"
    | C_CONVERT_FROM_LEFT_COMB  -> "Layout.convert_from_left_comb"

    (* Not parsed *)
    | C_SOME -> "Some"
    | C_NONE -> "None"

    | _ as c -> failwith @@ Format.asprintf "Constant not handled : %a" Stage_common.PP.constant c 


  module Pascaligo = struct
    let constants = function
    (* Tezos module (ex-Michelson) *)
    | "chain_id"               -> Some C_CHAIN_ID            (* Deprecated *)
    | "get_chain_id"           -> Some C_CHAIN_ID            (* Deprecated *)    
    | "balance"                -> Some C_BALANCE             (* Deprecated *)    
    | "now"                    -> Some C_NOW                 (* Deprecated *)
    | "amount"                 -> Some C_AMOUNT              (* Deprecated *)
    | "sender"                 -> Some C_SENDER              (* Deprecated *)
    | "address"                -> Some C_ADDRESS             (* Deprecated *)    
    | "self_address"           -> Some C_SELF_ADDRESS        (* Deprecated *)
    | "implicit_account"       -> Some C_IMPLICIT_ACCOUNT    (* Deprecated *)    
    | "source"                 -> Some C_SOURCE              (* Deprecated *)    
    | "failwith"               -> Some C_FAILWITH
    | "transaction"            -> Some C_CALL                    (* Deprecated *)
    | "set_delegate"           -> Some C_SET_DELEGATE            (* Deprecated *)
    | "get_contract"           -> Some C_CONTRACT                (* Deprecated *)
    | "get_contract_opt"       -> Some C_CONTRACT_OPT            (* Deprecated *)
    | "get_entrypoint"         -> Some C_CONTRACT_ENTRYPOINT     (* Deprecated *)
    | "get_entrypoint_opt"     -> Some C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

    | "Michelson.is_nat" -> Some C_IS_NAT  (* Deprecated *)
    | "is_nat"           -> Some C_IS_NAT
    | "int"              -> Some C_INT
    | "abs"              -> Some C_ABS
    | "ediv"             -> Some C_EDIV
    | "unit"             -> Some C_UNIT

    | "NEG"              -> Some C_NEG
    | "ADD"              -> Some C_ADD
    | "SUB"              -> Some C_SUB
    | "TIMES"            -> Some C_MUL
    | "DIV"              -> Some C_DIV
    | "MOD"              -> Some C_MOD
    | "EQ"               -> Some C_EQ
    | "NOT"              -> Some C_NOT
    | "AND"              -> Some C_AND
    | "OR"               -> Some C_OR
    | "GT"               -> Some C_GT
    | "GE"               -> Some C_GE
    | "LT"               -> Some C_LT
    | "LE"               -> Some C_LE
    | "CONS"             -> Some C_CONS
    | "cons"             -> Some C_CONS (* Deprecated *)
    | "NEQ"              -> Some C_NEQ

    (* Crypto module *)

    | "crypto_check"    -> Some C_CHECK_SIGNATURE       (* Deprecated *)
    | "crypto_hash_key" -> Some C_HASH_KEY              (* Deprecated *)    
    | "blake2b"         -> Some C_BLAKE2b               (* Deprecated *)    
    | "sha_256"         -> Some C_SHA256                (* Deprecated *)       
    | "sha_512"         -> Some C_SHA512                (* Deprecated *)

    (* Bytes module *)

    | "bytes_pack"   -> Some C_BYTES_PACK    (* Deprecated *)    
    | "bytes_unpack" -> Some C_BYTES_UNPACK  (* Deprecated *)    
    | "Bytes.size"   -> Some C_SIZE          (* Deprecated *)
    | "bytes_concat" -> Some C_CONCAT        (* Deprecated *)    
    | "bytes_slice"  -> Some C_SLICE         (* Deprecated *)
    | "Bytes.slice"  -> Some C_SLICE         (* Deprecated *)

    (* List module *)

    | "list_size"   -> Some C_SIZE       (* Deprecated *)
    | "list_iter"   -> Some C_LIST_ITER  (* Deprecated *)    
    | "list_map"    -> Some C_LIST_MAP   (* Deprecated *)    
    | "list_fold"   -> Some C_LIST_FOLD  (* Deprecated *)

    (* Set module *)

    
    | "Set.size"    -> Some C_SIZE        (* Deprecated *)
    | "set_size"    -> Some C_SIZE        (* Deprecated *)
    | "set_empty"   -> Some C_SET_EMPTY   (* Deprecated *)    
    | "set_mem"     -> Some C_SET_MEM     (* Deprecated *)    
    | "set_add"     -> Some C_SET_ADD     (* Deprecated *)    
    | "set_remove"  -> Some C_SET_REMOVE  (* Deprecated *)    
    | "set_iter"    -> Some C_SET_ITER    (* Deprecated *)    
    | "set_fold"    -> Some C_SET_FOLD    (* Deprecated *)

    (* Map module *)

    | "get_force"    -> Some C_MAP_FIND      (* Deprecated *)
    | "map_get"      -> Some C_MAP_FIND_OPT  (* Deprecated *)    
    | "map_update"   -> Some C_MAP_UPDATE    (* Deprecated *)
    | "map_remove"   -> Some C_MAP_REMOVE    (* Deprecated *)    
    | "map_iter"     -> Some C_MAP_ITER      (* Deprecated *)    
    | "map_map"      -> Some C_MAP_MAP       (* Deprecated *)    
    | "map_fold"     -> Some C_MAP_FOLD      (* Deprecated *)    
    | "map_mem"      -> Some C_MAP_MEM       (* Deprecated *)    
    | "map_size"     -> Some C_SIZE          (* Deprecated *)


    (* Bitwise module *)

    | "bitwise_or"          -> Some C_OR      (* Deprecated *)    
    | "bitwise_and"         -> Some C_AND     (* Deprecated *)    
    | "bitwise_xor"         -> Some C_XOR     (* Deprecated *)    
    | "bitwise_lsl"         -> Some C_LSL     (* Deprecated *)    
    | "bitwise_lsr"         -> Some C_LSR     (* Deprecated *)

    (* String module *)
    
    | "string_slice"    -> Some C_SLICE    (* Deprecated *)
    | "string_concat"   -> Some C_CONCAT   (* Deprecated *)

    (* Others *)

    | "assert"          -> Some C_ASSERTION
    | "size"            -> Some C_SIZE (* Deprecated *)
    
    | "Layout.convert_to_right_comb" -> Some C_CONVERT_TO_RIGHT_COMB
    | "Layout.convert_to_left_comb" -> Some C_CONVERT_TO_LEFT_COMB

    | _ as c            -> pseudo_modules c

    let constant_to_string = function
    (* Tezos module (ex-Michelson) *)
    | C_FAILWITH -> "failwith"

    | C_IS_NAT -> "is_nat"
    | C_INT    -> "int"
    | C_ABS    -> "abs"
    | C_EDIV   -> "ediv"
    | C_UNIT   -> "unit"

    | C_NEG  -> "NEG"
    | C_ADD  -> "ADD"
    | C_SUB  -> "SUB"
    | C_MUL  -> "TIMES"
    | C_DIV  -> "DIV"
    | C_MOD  -> "MOD"
    | C_EQ   -> "EQ"
    | C_NOT  -> "NOT"
    | C_AND  -> "AND"
    | C_OR   -> "OR"
    | C_GT   -> "GT"
    | C_GE   -> "GE"
    | C_LT   -> "LT"
    | C_LE   -> "LE"
    | C_CONS -> "CONS"
    | C_NEQ  -> "NEQ"

    (*->  Others *)

    | C_ASSERTION -> "assert"
    
    | C_CONVERT_TO_RIGHT_COMB -> "Layout.convert_to_right_comb"
    | C_CONVERT_TO_LEFT_COMB  -> "Layout.convert_to_left_comb"

    | _ as c            -> pseudo_module_to_string c

    let type_constants = type_constants
    let type_constant_to_string = type_constant_to_string
  end

  module Cameligo = struct
    let constants = function
    (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)

    | "chain_id"                   -> Some C_CHAIN_ID            (* Deprecated *)    
    | "Current.balance"            -> Some C_BALANCE             (* Deprecated *)
    | "balance"                    -> Some C_BALANCE             (* Deprecated *)    
    | "Current.time"               -> Some C_NOW                 (* Deprecated *)
    | "time"                       -> Some C_NOW                 (* Deprecated *)    
    | "Current.amount"             -> Some C_AMOUNT              (* Deprecated *)
    | "amount"                     -> Some C_AMOUNT              (* Deprecated *)    
    | "Current.sender"             -> Some C_SENDER              (* Deprecated *)
    | "sender"                     -> Some C_SENDER              (* Deprecated *)    
    | "Current.address"            -> Some C_ADDRESS             (* Deprecated *)    
    | "Current.self_address"       -> Some C_SELF_ADDRESS        (* Deprecated *)    
    | "Current.implicit_account"   -> Some C_IMPLICIT_ACCOUNT    (* Deprecated *)    
    | "Current.source"             -> Some C_SOURCE              (* Deprecated *)
    | "source"                     -> Some C_SOURCE              (* Deprecated *)    
    | "Current.failwith"           -> Some C_FAILWITH            (* Deprecated *)
    | "failwith"                   -> Some C_FAILWITH
    
    | "Operation.transaction"        -> Some C_CALL              (* Deprecated *)
    | "Operation.set_delegate"       -> Some C_SET_DELEGATE      (* Deprecated *)
    | "Operation.get_contract"       -> Some C_CONTRACT          (* Deprecated *)
    | "Operation.get_contract_opt"   -> Some C_CONTRACT_OPT      (* Deprecated *)
    | "Operation.get_entrypoint"     -> Some C_CONTRACT_ENTRYPOINT (* Deprecated *)
    | "Operation.get_entrypoint_opt" -> Some C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

    | "Michelson.is_nat" -> Some C_IS_NAT  (* Deprecated *)
    | "is_nat"           -> Some C_IS_NAT
    | "int"              -> Some C_INT
    | "abs"              -> Some C_ABS
    | "ediv"             -> Some C_EDIV
    | "unit"             -> Some C_UNIT

    | "NEG"              -> Some C_NEG
    | "ADD"              -> Some C_ADD
    | "SUB"              -> Some C_SUB
    | "TIMES"            -> Some C_MUL
    | "DIV"              -> Some C_DIV
    | "MOD"              -> Some C_MOD
    | "EQ"               -> Some C_EQ
    | "NOT"              -> Some C_NOT
    | "AND"              -> Some C_AND
    | "OR"               -> Some C_OR
    | "GT"               -> Some C_GT
    | "GE"               -> Some C_GE
    | "LT"               -> Some C_LT
    | "LE"               -> Some C_LE
    | "CONS"             -> Some C_CONS
    | "NEQ"              -> Some C_NEQ

    (* Bytes module *)

    | "Bytes.size"   -> Some C_SIZE       (* Deprecated *)
    | "Bytes.slice"  -> Some C_SLICE      (* Deprecated *)

    (* Set module *)   
    | "Set.size"     -> Some C_SIZE (* Deprecated *)

    (* Map module *)
    | "Map.find"     -> Some C_MAP_FIND     (* Deprecated *)

    (* Bitwise module *)

    | "Bitwise.lor"         -> Some C_OR  (* Deprecated *)
    | "Bitwise.land"        -> Some C_AND (* Deprecated *)
    | "Bitwise.lxor"        -> Some C_XOR (* Deprecated *)

    (* Loop module *)

    | "Loop.fold_while" -> Some C_FOLD_WHILE    (* Deprecated *)
    | "Loop.resume"     -> Some C_FOLD_CONTINUE (* Deprecated *)
    | "continue"        -> Some C_FOLD_CONTINUE (* Deprecated *)
    | "Loop.stop"       -> Some C_FOLD_STOP     (* Deprecated *)
    | "stop"            -> Some C_FOLD_STOP     (* Deprecated *)

    (* Others *)

    | "assert" -> Some C_ASSERTION

    | _ as c -> pseudo_modules c

    let constant_to_string = function
    (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)
    | C_FAILWITH -> "failwith"
    
    | C_IS_NAT -> "is_nat"
    | C_INT    -> "int"
    | C_ABS    -> "abs"
    | C_EDIV   -> "ediv"
    | C_UNIT   -> "unit"

    | C_NEG  -> "NEG"
    | C_ADD  -> "ADD"
    | C_SUB  -> "SUB"
    | C_MUL  -> "TIMES"
    | C_DIV  -> "DIV"
    | C_MOD  -> "MOD"
    | C_EQ   -> "EQ"
    | C_NOT  -> "NOT"
    | C_AND  -> "AND"
    | C_OR   -> "OR"
    | C_GT   -> "GT"
    | C_GE   -> "GE"
    | C_LT   -> "LT"
    | C_LE   -> "LE"
    | C_CONS -> "CONS"
    | C_NEQ  -> "NEQ"

    (* Others *)

    | C_ASSERTION -> "assert"

    | _ as c -> pseudo_module_to_string c

    let type_constants = type_constants
    let type_constant_to_string = type_constant_to_string
  end

  module Reasonligo = struct
    let constants = function
    (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)

    | "chain_id"                   -> Some C_CHAIN_ID            (* Deprecated *)    
    | "Current.balance"            -> Some C_BALANCE             (* Deprecated *)
    | "balance"                    -> Some C_BALANCE             (* Deprecated *)    
    | "Current.time"               -> Some C_NOW                 (* Deprecated *)
    | "time"                       -> Some C_NOW                 (* Deprecated *)    
    | "Current.amount"             -> Some C_AMOUNT              (* Deprecated *)
    | "amount"                     -> Some C_AMOUNT              (* Deprecated *)    
    | "Current.sender"             -> Some C_SENDER              (* Deprecated *)
    | "sender"                     -> Some C_SENDER              (* Deprecated *)    
    | "Current.address"            -> Some C_ADDRESS             (* Deprecated *)    
    | "Current.self_address"       -> Some C_SELF_ADDRESS        (* Deprecated *)    
    | "Current.implicit_account"   -> Some C_IMPLICIT_ACCOUNT    (* Deprecated *)    
    | "Current.source"             -> Some C_SOURCE              (* Deprecated *)
    | "source"                     -> Some C_SOURCE              (* Deprecated *)    
    | "Current.failwith"           -> Some C_FAILWITH            (* Deprecated *)
    | "failwith"                   -> Some C_FAILWITH
    
    | "Operation.transaction"        -> Some C_CALL              (* Deprecated *)
    | "Operation.set_delegate"       -> Some C_SET_DELEGATE      (* Deprecated *)
    | "Operation.get_contract"       -> Some C_CONTRACT          (* Deprecated *)
    | "Operation.get_contract_opt"   -> Some C_CONTRACT_OPT      (* Deprecated *)
    | "Operation.get_entrypoint"     -> Some C_CONTRACT_ENTRYPOINT (* Deprecated *)
    | "Operation.get_entrypoint_opt" -> Some C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

    | "Michelson.is_nat" -> Some C_IS_NAT  (* Deprecated *)
    | "is_nat"           -> Some C_IS_NAT
    | "int"              -> Some C_INT
    | "abs"              -> Some C_ABS
    | "ediv"             -> Some C_EDIV
    | "unit"             -> Some C_UNIT

    | "NEG"              -> Some C_NEG
    | "ADD"              -> Some C_ADD
    | "SUB"              -> Some C_SUB
    | "TIMES"            -> Some C_MUL
    | "DIV"              -> Some C_DIV
    | "MOD"              -> Some C_MOD
    | "EQ"               -> Some C_EQ
    | "NOT"              -> Some C_NOT
    | "AND"              -> Some C_AND
    | "OR"               -> Some C_OR
    | "GT"               -> Some C_GT
    | "GE"               -> Some C_GE
    | "LT"               -> Some C_LT
    | "LE"               -> Some C_LE
    | "CONS"             -> Some C_CONS
    | "NEQ"              -> Some C_NEQ

    (* Bytes module *)

    | "Bytes.size"   -> Some C_SIZE       (* Deprecated *)
    | "Bytes.slice"  -> Some C_SLICE      (* Deprecated *)

    (* Set module *)   
    | "Set.size"     -> Some C_SIZE (* Deprecated *)

    (* Map module *)
    | "Map.find"     -> Some C_MAP_FIND     (* Deprecated *)

    (* Bitwise module *)

    | "Bitwise.lor"         -> Some C_OR  (* Deprecated *)
    | "Bitwise.land"        -> Some C_AND (* Deprecated *)
    | "Bitwise.lxor"        -> Some C_XOR (* Deprecated *)

    (* Loop module *)

    | "Loop.fold_while" -> Some C_FOLD_WHILE    (* Deprecated *)
    | "Loop.resume"     -> Some C_FOLD_CONTINUE (* Deprecated *)
    | "continue"        -> Some C_FOLD_CONTINUE (* Deprecated *)
    | "Loop.stop"       -> Some C_FOLD_STOP     (* Deprecated *)
    | "stop"            -> Some C_FOLD_STOP     (* Deprecated *)

    (* Others *)

    | "assert" -> Some C_ASSERTION

    | _ as c -> pseudo_modules c

    let constant_to_string = function
    (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)
    | C_FAILWITH -> "failwith"
    
    | C_IS_NAT -> "is_nat"
    | C_INT    -> "int"
    | C_ABS    -> "abs"
    | C_EDIV   -> "ediv"
    | C_UNIT   -> "unit"

    | C_NEG  -> "NEG"
    | C_ADD  -> "ADD"
    | C_SUB  -> "SUB"
    | C_MUL  -> "TIMES"
    | C_DIV  -> "DIV"
    | C_MOD  -> "MOD"
    | C_EQ   -> "EQ"
    | C_NOT  -> "NOT"
    | C_AND  -> "AND"
    | C_OR   -> "OR"
    | C_GT   -> "GT"
    | C_GE   -> "GE"
    | C_LT   -> "LT"
    | C_LE   -> "LE"
    | C_CONS -> "CONS"
    | C_NEQ  -> "NEQ"

    (* Others *)

    | C_ASSERTION -> "assert"

    | _ as c -> pseudo_module_to_string c

    let type_constants = type_constants
    let type_constant_to_string = type_constant_to_string
  end
end

module Stacking = struct
  (*
    Most constants pass through the Spilling unchanged. So they need to be
    compiled down to Michelson. This is the last step.

    When compiling the constant, we need to provide its arity (through the type
    predicate, defined in `Helpers.Stacking`, and its michelson code.
    In the case of an n-ary constant, we assume that the stack has the form:
    `x1 :: x2 :: x3 ... :: xn :: _`.

    This step requires knowledge of Michelson. Knowledge of
    `Tezos_utils.Michelson` will help too, so that no Michelson has to actually
    be written by hand.
  *)

  include Helpers.Stacking
  open Tezos_utils.Michelson
  open Mini_c

  let get_operators c : predicate option =
  match c with
    | C_ADD             -> Some ( simple_binary @@ prim I_ADD)
    | C_SUB             -> Some ( simple_binary @@ prim I_SUB)
    | C_MUL             -> Some ( simple_binary @@ prim I_MUL)
    | C_EDIV            -> Some ( simple_binary @@ prim I_EDIV)
    | C_DIV             -> Some ( simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car])
    | C_MOD             -> Some ( simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr])
    | C_NEG             -> Some ( simple_unary @@ prim I_NEG)
    | C_OR              -> Some ( simple_binary @@ prim I_OR)
    | C_AND             -> Some ( simple_binary @@ prim I_AND)
    | C_XOR             -> Some ( simple_binary @@ prim I_XOR)
    | C_LSL             -> Some ( simple_binary @@ prim I_LSL)
    | C_LSR             -> Some ( simple_binary @@ prim I_LSR)
    | C_NOT             -> Some ( simple_unary @@ prim I_NOT)
    | C_PAIR            -> Some ( simple_binary @@ prim I_PAIR)
    | C_CAR             -> Some ( simple_unary @@ prim I_CAR)
    | C_CDR             -> Some ( simple_unary @@ prim I_CDR)
    | C_EQ              -> Some ( simple_binary @@ seq [prim I_COMPARE ; prim I_EQ])
    | C_NEQ             -> Some ( simple_binary @@ seq [prim I_COMPARE ; prim I_NEQ])
    | C_LT              -> Some ( simple_binary @@ seq [prim I_COMPARE ; prim I_LT])
    | C_LE              -> Some ( simple_binary @@ seq [prim I_COMPARE ; prim I_LE])
    | C_GT              -> Some ( simple_binary @@ seq [prim I_COMPARE ; prim I_GT])
    | C_GE              -> Some ( simple_binary @@ seq [prim I_COMPARE ; prim I_GE])
    | C_UPDATE          -> Some ( simple_ternary @@ prim I_UPDATE)
    | C_SOME            -> Some ( simple_unary  @@ prim I_SOME)
    | C_MAP_FIND        -> Some ( simple_binary @@ seq [prim I_GET ; i_assert_some_msg (i_push_string "MAP FIND")])
    | C_MAP_MEM         -> Some ( simple_binary @@ prim I_MEM)
    | C_MAP_FIND_OPT    -> Some ( simple_binary @@ prim I_GET)
    | C_MAP_ADD         -> Some ( simple_ternary @@ seq [dip (i_some) ; prim I_UPDATE])
    | C_MAP_UPDATE      -> Some ( simple_ternary @@ prim I_UPDATE)
    | C_FOLD_WHILE      -> Some ( simple_binary @@ seq [i_swap ; (i_push (prim T_bool) (prim D_True));prim ~children:[seq [dip i_dup; i_exec; i_unpair]] I_LOOP ;i_swap ; i_drop])
    | C_FOLD_CONTINUE   -> Some ( simple_unary @@ seq [(i_push (prim T_bool) (prim D_True)); i_pair])
    | C_FOLD_STOP       -> Some ( simple_unary @@ seq [(i_push (prim T_bool) (prim D_False)); i_pair])
    | C_SIZE            -> Some ( simple_unary @@ prim I_SIZE)
    | C_FAILWITH        -> Some ( simple_unary @@ prim I_FAILWITH)
    | C_ASSERT_INFERRED -> Some ( simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit]))
    | C_ASSERTION       -> Some ( simple_unary @@ i_if (seq [i_push_unit]) (seq [i_push_string "failed assertion" ; i_failwith]))
    | C_INT             -> Some ( simple_unary @@ prim I_INT)
    | C_ABS             -> Some ( simple_unary @@ prim I_ABS)
    | C_IS_NAT          -> Some ( simple_unary @@ prim I_ISNAT)
    | C_CONS            -> Some ( simple_binary @@ prim I_CONS)
    | C_UNIT            -> Some ( simple_constant @@ prim I_UNIT)
    | C_BALANCE         -> Some ( simple_constant @@ prim I_BALANCE)
    | C_AMOUNT          -> Some ( simple_constant @@ prim I_AMOUNT)
    | C_ADDRESS         -> Some ( simple_unary @@ prim I_ADDRESS)
    | C_SELF_ADDRESS    -> Some ( simple_constant @@ seq [prim I_SELF; prim I_ADDRESS])
    | C_IMPLICIT_ACCOUNT -> Some ( simple_unary @@ prim I_IMPLICIT_ACCOUNT)
    | C_SET_DELEGATE    -> Some ( simple_unary @@ prim I_SET_DELEGATE)
    | C_NOW             -> Some ( simple_constant @@ prim I_NOW)
    | C_CALL            -> Some ( simple_ternary @@ prim I_TRANSFER_TOKENS)
    | C_SOURCE          -> Some ( simple_constant @@ prim I_SOURCE)
    | C_SENDER          -> Some ( simple_constant @@ prim I_SENDER)
    | C_SET_MEM         -> Some ( simple_binary @@ prim I_MEM)
    | C_SET_ADD         -> Some ( simple_binary @@ seq [dip (i_push (prim T_bool) (prim D_True)) ; prim I_UPDATE])
    | C_SET_REMOVE      -> Some ( simple_binary @@ seq [dip (i_push (prim T_bool) (prim D_False)) ; prim I_UPDATE])
    | C_SLICE           -> Some ( simple_ternary @@ seq [prim I_SLICE ; i_assert_some_msg (i_push_string "SLICE")])
    | C_SHA256          -> Some ( simple_unary @@ prim I_SHA256)
    | C_SHA512          -> Some ( simple_unary @@ prim I_SHA512)
    | C_BLAKE2b         -> Some ( simple_unary @@ prim I_BLAKE2B)
    | C_CHECK_SIGNATURE -> Some ( simple_ternary @@ prim I_CHECK_SIGNATURE)
    | C_HASH_KEY        -> Some ( simple_unary @@ prim I_HASH_KEY)
    | C_BYTES_PACK      -> Some ( simple_unary @@ prim I_PACK)
    | C_CONCAT          -> Some ( simple_binary @@ prim I_CONCAT)
    | C_CHAIN_ID        -> Some ( simple_constant @@ prim I_CHAIN_ID)
    | _                 -> None

end
