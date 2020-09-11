(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Stacking. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Tree_abstraction = struct

  open Ast_imperative

  module type Constant = sig
    val constants      : string -> rich_constant option
    val type_constants : string -> type_constant option
    val constant_to_string      : rich_constant -> string
    val type_constant_to_string : type_constant -> string
  end

  let some_const c = Some (Const c)
  let some_deprecated name const = Some (Deprecated {name;const})

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

  let pseudo_modules x =
    match x with
    | "Tezos.chain_id"           -> some_const C_CHAIN_ID
    | "Tezos.balance"            -> some_const C_BALANCE
    | "Tezos.now"                -> some_const C_NOW
    | "Tezos.amount"             -> some_const C_AMOUNT
    | "Tezos.sender"             -> some_const C_SENDER
    | "Tezos.address"            -> some_const C_ADDRESS
    | "Tezos.self"               -> some_const C_SELF
    | "Tezos.self_address"       -> some_const C_SELF_ADDRESS
    | "Tezos.implicit_account"   -> some_const C_IMPLICIT_ACCOUNT
    | "Tezos.source"             -> some_const C_SOURCE
    | "Tezos.failwith"           -> some_const C_FAILWITH
    | "Tezos.create_contract"    -> some_const C_CREATE_CONTRACT
    | "Tezos.transaction"        -> some_const C_CALL
    | "Tezos.set_delegate"       -> some_const C_SET_DELEGATE
    | "Tezos.get_contract_opt"   -> some_const C_CONTRACT_OPT
    | "Tezos.get_entrypoint_opt" -> some_const C_CONTRACT_ENTRYPOINT_OPT

    (* Crypto module *)

    | "Crypto.check"    -> some_const C_CHECK_SIGNATURE
    | "Crypto.hash_key" -> some_const C_HASH_KEY
    | "Crypto.blake2b"  -> some_const C_BLAKE2b
    | "Crypto.sha256"   -> some_const C_SHA256
    | "Crypto.sha512"   -> some_const C_SHA512

    (* Bytes module *)

    | "Bytes.pack"   -> some_const C_BYTES_PACK
    | "Bytes.unpack" -> some_const C_BYTES_UNPACK
    | "Bytes.length" -> some_const C_SIZE
    | "Bytes.concat" -> some_const C_CONCAT
    | "Bytes.sub"    -> some_const C_SLICE

    (* List module *)

    | "List.length" -> some_const C_SIZE
    | "List.size"   -> some_const C_SIZE
    | "List.iter"   -> some_const C_LIST_ITER
    | "List.map"    -> some_const C_LIST_MAP
    | "List.fold"   -> some_const C_LIST_FOLD

    (* Set module *)

    | "Set.empty"    -> some_const C_SET_EMPTY
    | "Set.literal"  -> some_const C_SET_LITERAL
    | "Set.cardinal" -> some_const C_SIZE
    | "Set.mem"      -> some_const C_SET_MEM
    | "Set.add"      -> some_const C_SET_ADD
    | "Set.remove"   -> some_const C_SET_REMOVE
    | "Set.iter"     -> some_const C_SET_ITER
    | "Set.fold"     -> some_const C_SET_FOLD

    (* Map module *)

    | "Map.find_opt" -> some_const C_MAP_FIND_OPT
    | "Map.update"   -> some_const C_MAP_UPDATE
    | "Map.iter"     -> some_const C_MAP_ITER
    | "Map.map"      -> some_const C_MAP_MAP
    | "Map.fold"     -> some_const C_MAP_FOLD
    | "Map.mem"      -> some_const C_MAP_MEM
    | "Map.size"     -> some_const C_SIZE
    | "Map.add"      -> some_const C_MAP_ADD
    | "Map.remove"   -> some_const C_MAP_REMOVE
    | "Map.empty"    -> some_const C_MAP_EMPTY
    | "Map.literal"  -> some_const C_MAP_LITERAL

    (* Big_map module *)

    | "Big_map.find"     -> some_const C_MAP_FIND
    | "Big_map.find_opt" -> some_const C_MAP_FIND_OPT
    | "Big_map.update"   -> some_const C_MAP_UPDATE
    | "Big_map.literal"  -> some_const C_BIG_MAP_LITERAL
    | "Big_map.empty"    -> some_const C_BIG_MAP_EMPTY
    | "Big_map.mem"      -> some_const C_MAP_MEM
    | "Big_map.remove"   -> some_const C_MAP_REMOVE
    | "Big_map.add"      -> some_const C_MAP_ADD

    (* Bitwise module *)

    | "Bitwise.or"          -> some_const C_OR
    | "Bitwise.and"         -> some_const C_AND
    | "Bitwise.xor"         -> some_const C_XOR
    | "Bitwise.shift_left"  -> some_const C_LSL
    | "Bitwise.shift_right" -> some_const C_LSR

    (* String module *)

    | "String.length"   -> some_const C_SIZE
    | "String.size"     -> some_deprecated x C_SIZE  (* Deprecated *)
    | "String.slice"    -> some_deprecated x C_SLICE (* Deprecated *)
    | "String.sub"      -> some_const C_SLICE
    | "String.concat"   -> some_const C_CONCAT

    (* michelson pair/or type converter module *)

    | "Layout.convert_to_right_comb" -> some_const C_CONVERT_TO_RIGHT_COMB
    | "Layout.convert_to_left_comb" -> some_const C_CONVERT_TO_LEFT_COMB
    | "Layout.convert_from_right_comb" -> some_const C_CONVERT_FROM_RIGHT_COMB
    | "Layout.convert_from_left_comb" -> some_const C_CONVERT_FROM_LEFT_COMB

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
    let constants x =
      let some_deprecated = some_deprecated x in
      match x with
      (* Tezos module (ex-Michelson) *)
      | "chain_id"               -> some_deprecated C_CHAIN_ID            (* Deprecated *)
      | "get_chain_id"           -> some_deprecated C_CHAIN_ID            (* Deprecated *)
      | "balance"                -> some_deprecated C_BALANCE             (* Deprecated *)
      | "now"                    -> some_deprecated C_NOW                 (* Deprecated *)
      | "amount"                 -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "sender"                 -> some_deprecated C_SENDER              (* Deprecated *)
      | "address"                -> some_deprecated C_ADDRESS             (* Deprecated *)
      | "self_address"           -> some_deprecated C_SELF_ADDRESS        (* Deprecated *)
      | "implicit_account"       -> some_deprecated C_IMPLICIT_ACCOUNT    (* Deprecated *)
      | "source"                 -> some_deprecated C_SOURCE              (* Deprecated *)
      | "failwith"               -> some_const      C_FAILWITH
      | "transaction"            -> some_deprecated C_CALL                    (* Deprecated *)
      | "set_delegate"           -> some_deprecated C_SET_DELEGATE            (* Deprecated *)
      | "get_contract"           -> some_deprecated C_CONTRACT                (* Deprecated *)
      | "get_contract_opt"       -> some_deprecated C_CONTRACT_OPT            (* Deprecated *)
      | "get_entrypoint"         -> some_deprecated C_CONTRACT_ENTRYPOINT     (* Deprecated *)
      | "get_entrypoint_opt"     -> some_deprecated C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

      | "Michelson.is_nat" -> some_deprecated C_IS_NAT  (* Deprecated *)
      | "is_nat"           -> some_const C_IS_NAT
      | "int"              -> some_const C_INT
      | "abs"              -> some_const C_ABS
      | "ediv"             -> some_const C_EDIV
      | "unit"             -> some_const C_UNIT

      | "NEG"              -> some_const C_NEG
      | "ADD"              -> some_const C_ADD
      | "SUB"              -> some_const C_SUB
      | "TIMES"            -> some_const C_MUL
      | "DIV"              -> some_const C_DIV
      | "MOD"              -> some_const C_MOD
      | "EQ"               -> some_const C_EQ
      | "NOT"              -> some_const C_NOT
      | "AND"              -> some_const C_AND
      | "OR"               -> some_const C_OR
      | "GT"               -> some_const C_GT
      | "GE"               -> some_const C_GE
      | "LT"               -> some_const C_LT
      | "LE"               -> some_const C_LE
      | "CONS"             -> some_const C_CONS
      | "cons"             -> some_deprecated C_CONS (* Deprecated *)
      | "NEQ"              -> some_const C_NEQ

      (* Crypto module *)

      | "crypto_check"    -> some_deprecated C_CHECK_SIGNATURE       (* Deprecated *)
      | "crypto_hash_key" -> some_deprecated C_HASH_KEY              (* Deprecated *)
      | "blake2b"         -> some_deprecated C_BLAKE2b               (* Deprecated *)
      | "sha_256"         -> some_deprecated C_SHA256                (* Deprecated *)
      | "sha_512"         -> some_deprecated C_SHA512                (* Deprecated *)

      (* Bytes module *)

      | "bytes_pack"   -> some_deprecated C_BYTES_PACK    (* Deprecated *)
      | "bytes_unpack" -> some_deprecated C_BYTES_UNPACK  (* Deprecated *)
      | "Bytes.size"   -> some_deprecated C_SIZE          (* Deprecated *)
      | "bytes_concat" -> some_deprecated C_CONCAT        (* Deprecated *)
      | "bytes_slice"  -> some_deprecated C_SLICE         (* Deprecated *)
      | "Bytes.slice"  -> some_deprecated C_SLICE         (* Deprecated *)

      (* List module *)

      | "list_size"   -> some_deprecated C_SIZE       (* Deprecated *)
      | "list_iter"   -> some_deprecated C_LIST_ITER  (* Deprecated *)
      | "list_map"    -> some_deprecated C_LIST_MAP   (* Deprecated *)
      | "list_fold"   -> some_deprecated C_LIST_FOLD  (* Deprecated *)

      (* Set module *)


      | "Set.size"    -> some_deprecated C_SIZE        (* Deprecated *)
      | "set_size"    -> some_deprecated C_SIZE        (* Deprecated *)
      | "set_empty"   -> some_deprecated C_SET_EMPTY   (* Deprecated *)
      | "set_mem"     -> some_deprecated C_SET_MEM     (* Deprecated *)
      | "set_add"     -> some_deprecated C_SET_ADD     (* Deprecated *)
      | "set_remove"  -> some_deprecated C_SET_REMOVE  (* Deprecated *)
      | "set_iter"    -> some_deprecated C_SET_ITER    (* Deprecated *)
      | "set_fold"    -> some_deprecated C_SET_FOLD    (* Deprecated *)

      (* Map module *)

      | "get_force"    -> some_deprecated C_MAP_FIND      (* Deprecated *)
      | "map_get"      -> some_deprecated C_MAP_FIND_OPT  (* Deprecated *)
      | "map_update"   -> some_deprecated C_MAP_UPDATE    (* Deprecated *)
      | "map_remove"   -> some_deprecated C_MAP_REMOVE    (* Deprecated *)
      | "map_iter"     -> some_deprecated C_MAP_ITER      (* Deprecated *)
      | "map_map"      -> some_deprecated C_MAP_MAP       (* Deprecated *)
      | "map_fold"     -> some_deprecated C_MAP_FOLD      (* Deprecated *)
      | "map_mem"      -> some_deprecated C_MAP_MEM       (* Deprecated *)
      | "map_size"     -> some_deprecated C_SIZE          (* Deprecated *)


      (* Bitwise module *)

      | "bitwise_or"          -> some_deprecated C_OR      (* Deprecated *)
      | "bitwise_and"         -> some_deprecated C_AND     (* Deprecated *)
      | "bitwise_xor"         -> some_deprecated C_XOR     (* Deprecated *)
      | "bitwise_lsl"         -> some_deprecated C_LSL     (* Deprecated *)
      | "bitwise_lsr"         -> some_deprecated C_LSR     (* Deprecated *)

      (* String module *)

      | "string_slice"    -> some_deprecated C_SLICE    (* Deprecated *)
      | "string_concat"   -> some_deprecated C_CONCAT   (* Deprecated *)

      (* Others *)

      | "assert"          -> some_const C_ASSERTION
      | "assert_some"     -> some_const C_ASSERT_SOME
      | "size"            -> some_deprecated C_SIZE (* Deprecated *)

      | "Layout.convert_to_right_comb" -> some_const C_CONVERT_TO_RIGHT_COMB
      | "Layout.convert_to_left_comb" -> some_const C_CONVERT_TO_LEFT_COMB

      | _ as c            -> pseudo_modules c

    let constant'_to_string = function
      (* Tezos module (ex-Michelson) *)
      | C_FAILWITH -> "failwith"

      | C_IS_NAT     -> "is_nat"
      | C_INT        -> "int"
      | C_ABS        -> "abs"
      | C_EDIV       -> "ediv"
      | C_UNIT       -> "unit"
      | C_LIST_EMPTY -> "nil"

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

      | C_ASSERTION   -> "assert"
      | C_ASSERT_SOME -> "assert_some"

      | C_CONVERT_TO_RIGHT_COMB -> "Layout.convert_to_right_comb"
      | C_CONVERT_TO_LEFT_COMB  -> "Layout.convert_to_left_comb"

      | _ as c            -> pseudo_module_to_string c

    let constant_to_string = function
      | Deprecated {name;_} -> name
      | Const x -> constant'_to_string x

    let type_constants = type_constants
    let type_constant_to_string = type_constant_to_string
  end

  module Cameligo = struct
    let constants x =
      let some_deprecated = some_deprecated x in
      match x with
      (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)

      | "chain_id"                   -> some_deprecated C_CHAIN_ID            (* Deprecated *)
      | "Current.balance"            -> some_deprecated C_BALANCE             (* Deprecated *)
      | "balance"                    -> some_deprecated C_BALANCE             (* Deprecated *)
      | "Current.time"               -> some_deprecated C_NOW                 (* Deprecated *)
      | "time"                       -> some_deprecated C_NOW                 (* Deprecated *)
      | "Current.amount"             -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "amount"                     -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "Current.sender"             -> some_deprecated C_SENDER              (* Deprecated *)
      | "sender"                     -> some_deprecated C_SENDER              (* Deprecated *)
      | "Current.address"            -> some_deprecated C_ADDRESS             (* Deprecated *)
      | "Current.self_address"       -> some_deprecated C_SELF_ADDRESS        (* Deprecated *)
      | "Current.implicit_account"   -> some_deprecated C_IMPLICIT_ACCOUNT    (* Deprecated *)
      | "Current.source"             -> some_deprecated C_SOURCE              (* Deprecated *)
      | "source"                     -> some_deprecated C_SOURCE              (* Deprecated *)
      | "Current.failwith"           -> some_deprecated C_FAILWITH            (* Deprecated *)
      | "failwith"                   -> some_const C_FAILWITH

      | "Operation.transaction"        -> some_deprecated C_CALL              (* Deprecated *)
      | "Operation.set_delegate"       -> some_deprecated C_SET_DELEGATE      (* Deprecated *)
      | "Operation.get_contract"       -> some_deprecated C_CONTRACT          (* Deprecated *)
      | "Operation.get_contract_opt"   -> some_deprecated C_CONTRACT_OPT      (* Deprecated *)
      | "Operation.get_entrypoint"     -> some_deprecated C_CONTRACT_ENTRYPOINT (* Deprecated *)
      | "Operation.get_entrypoint_opt" -> some_deprecated C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

      | "Michelson.is_nat" -> some_deprecated C_IS_NAT  (* Deprecated *)
      | "is_nat"           -> some_const C_IS_NAT
      | "int"              -> some_const C_INT
      | "abs"              -> some_const C_ABS
      | "ediv"             -> some_const C_EDIV
      | "unit"             -> some_const C_UNIT

      | "NEG"              -> some_const C_NEG
      | "ADD"              -> some_const C_ADD
      | "SUB"              -> some_const C_SUB
      | "TIMES"            -> some_const C_MUL
      | "DIV"              -> some_const C_DIV
      | "MOD"              -> some_const C_MOD
      | "EQ"               -> some_const C_EQ
      | "NOT"              -> some_const C_NOT
      | "AND"              -> some_const C_AND
      | "OR"               -> some_const C_OR
      | "GT"               -> some_const C_GT
      | "GE"               -> some_const C_GE
      | "LT"               -> some_const C_LT
      | "LE"               -> some_const C_LE
      | "CONS"             -> some_const C_CONS
      | "NEQ"              -> some_const C_NEQ

      (* Bytes module *)

      | "Bytes.size"   -> some_deprecated C_SIZE       (* Deprecated *)
      | "Bytes.slice"  -> some_deprecated C_SLICE      (* Deprecated *)

      (* Set module *)
      | "Set.size"     -> some_deprecated C_SIZE (* Deprecated *)

      (* Map module *)
      | "Map.find"     -> some_deprecated C_MAP_FIND     (* Deprecated *)

      (* Bitwise module *)

      | "Bitwise.lor"         -> some_deprecated C_OR  (* Deprecated *)
      | "Bitwise.land"        -> some_deprecated C_AND (* Deprecated *)
      | "Bitwise.lxor"        -> some_deprecated C_XOR (* Deprecated *)

      (* Loop module *)

      | "Loop.fold_while" -> some_deprecated C_FOLD_WHILE    (* Deprecated *)
      | "Loop.resume"     -> some_deprecated C_FOLD_CONTINUE (* Deprecated *)
      | "continue"        -> some_deprecated C_FOLD_CONTINUE (* Deprecated *)
      | "Loop.stop"       -> some_deprecated C_FOLD_STOP     (* Deprecated *)
      | "stop"            -> some_deprecated C_FOLD_STOP     (* Deprecated *)

      (* Others *)

      | "assert"       -> some_const C_ASSERTION
      | "assert_some"  -> some_const C_ASSERT_SOME

      | _ as c -> pseudo_modules c

    let constant'_to_string = function
      (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)
      | C_FAILWITH -> "failwith"

      | C_IS_NAT     -> "is_nat"
      | C_INT        -> "int"
      | C_ABS        -> "abs"
      | C_EDIV       -> "ediv"
      | C_UNIT       -> "unit"
      | C_LIST_EMPTY -> "[]"

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

      | C_ASSERTION   -> "assert"
      | C_ASSERT_SOME -> "assert_some"

      | _ as c -> pseudo_module_to_string c

    let constant_to_string = function
      | Deprecated {name;_} -> name
      | Const x -> constant'_to_string x

    let type_constants = type_constants
    let type_constant_to_string = type_constant_to_string
  end

  module Reasonligo = struct
    let constants x =
      let some_deprecated = some_deprecated x in
      match x with
      (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)

      | "chain_id"                   -> some_deprecated C_CHAIN_ID            (* Deprecated *)
      | "Current.balance"            -> some_deprecated C_BALANCE             (* Deprecated *)
      | "balance"                    -> some_deprecated C_BALANCE             (* Deprecated *)
      | "Current.time"               -> some_deprecated C_NOW                 (* Deprecated *)
      | "time"                       -> some_deprecated C_NOW                 (* Deprecated *)
      | "Current.amount"             -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "amount"                     -> some_deprecated C_AMOUNT              (* Deprecated *)
      | "Current.sender"             -> some_deprecated C_SENDER              (* Deprecated *)
      | "sender"                     -> some_deprecated C_SENDER              (* Deprecated *)
      | "Current.address"            -> some_deprecated C_ADDRESS             (* Deprecated *)
      | "Current.self_address"       -> some_deprecated C_SELF_ADDRESS        (* Deprecated *)
      | "Current.implicit_account"   -> some_deprecated C_IMPLICIT_ACCOUNT    (* Deprecated *)
      | "Current.source"             -> some_deprecated C_SOURCE              (* Deprecated *)
      | "source"                     -> some_deprecated C_SOURCE              (* Deprecated *)
      | "Current.failwith"           -> some_deprecated C_FAILWITH            (* Deprecated *)
      | "failwith"                   -> some_const C_FAILWITH

      | "Operation.transaction"        -> some_deprecated C_CALL              (* Deprecated *)
      | "Operation.set_delegate"       -> some_deprecated C_SET_DELEGATE      (* Deprecated *)
      | "Operation.get_contract"       -> some_deprecated C_CONTRACT          (* Deprecated *)
      | "Operation.get_contract_opt"   -> some_deprecated C_CONTRACT_OPT      (* Deprecated *)
      | "Operation.get_entrypoint"     -> some_deprecated C_CONTRACT_ENTRYPOINT (* Deprecated *)
      | "Operation.get_entrypoint_opt" -> some_deprecated C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

      | "Michelson.is_nat" -> some_deprecated C_IS_NAT  (* Deprecated *)
      | "is_nat"           -> some_const C_IS_NAT
      | "int"              -> some_const C_INT
      | "abs"              -> some_const C_ABS
      | "ediv"             -> some_const C_EDIV
      | "unit"             -> some_const C_UNIT

      | "NEG"              -> some_const C_NEG
      | "ADD"              -> some_const C_ADD
      | "SUB"              -> some_const C_SUB
      | "TIMES"            -> some_const C_MUL
      | "DIV"              -> some_const C_DIV
      | "MOD"              -> some_const C_MOD
      | "EQ"               -> some_const C_EQ
      | "NOT"              -> some_const C_NOT
      | "AND"              -> some_const C_AND
      | "OR"               -> some_const C_OR
      | "GT"               -> some_const C_GT
      | "GE"               -> some_const C_GE
      | "LT"               -> some_const C_LT
      | "LE"               -> some_const C_LE
      | "CONS"             -> some_const C_CONS
      | "NEQ"              -> some_const C_NEQ

      (* Bytes module *)

      | "Bytes.size"   -> some_deprecated C_SIZE       (* Deprecated *)
      | "Bytes.slice"  -> some_deprecated C_SLICE      (* Deprecated *)

      (* Set module *)
      | "Set.size"     -> some_deprecated C_SIZE (* Deprecated *)

      (* Map module *)
      | "Map.find"     -> some_deprecated C_MAP_FIND     (* Deprecated *)

      (* Bitwise module *)

      | "Bitwise.lor"         -> some_deprecated C_OR  (* Deprecated *)
      | "Bitwise.land"        -> some_deprecated C_AND (* Deprecated *)
      | "Bitwise.lxor"        -> some_deprecated C_XOR (* Deprecated *)

      (* Loop module *)

      | "Loop.fold_while" -> some_deprecated C_FOLD_WHILE    (* Deprecated *)
      | "Loop.resume"     -> some_deprecated C_FOLD_CONTINUE (* Deprecated *)
      | "continue"        -> some_deprecated C_FOLD_CONTINUE (* Deprecated *)
      | "Loop.stop"       -> some_deprecated C_FOLD_STOP     (* Deprecated *)
      | "stop"            -> some_deprecated C_FOLD_STOP     (* Deprecated *)

      (* Others *)

      | "assert"      -> some_const C_ASSERTION
      | "assert_some" -> some_const C_ASSERT_SOME

      | _ as c -> pseudo_modules c

    let constant'_to_string = function
      (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)
      | C_FAILWITH -> "failwith"

      | C_IS_NAT     -> "is_nat"
      | C_INT        -> "int"
      | C_ABS        -> "abs"
      | C_EDIV       -> "ediv"
      | C_UNIT       -> "unit"
      | C_LIST_EMPTY -> "[]"

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

      | C_ASSERTION   -> "assert"
      | C_ASSERT_SOME -> "assert_some"

      | _ as c -> pseudo_module_to_string c

    let constant_to_string = function
      | Deprecated {name;_} -> name
      | Const x -> constant'_to_string x

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
    | C_ADD               -> Some ( simple_binary @@ prim "ADD")
    | C_SUB               -> Some ( simple_binary @@ prim "SUB")
    | C_MUL               -> Some ( simple_binary @@ prim "MUL")
    | C_EDIV              -> Some ( simple_binary @@ prim "EDIV")
    | C_DIV               -> Some ( simple_binary @@ seq [prim "EDIV" ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car])
    | C_MOD               -> Some ( simple_binary @@ seq [prim "EDIV" ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr])
    | C_NEG               -> Some ( simple_unary @@ prim "NEG")
    | C_OR                -> Some ( simple_binary @@ prim "OR")
    | C_AND               -> Some ( simple_binary @@ prim "AND")
    | C_XOR               -> Some ( simple_binary @@ prim "XOR")
    | C_LSL               -> Some ( simple_binary @@ prim "LSL")
    | C_LSR               -> Some ( simple_binary @@ prim "LSR")
    | C_NOT               -> Some ( simple_unary @@ prim "NOT")
    | C_PAIR              -> Some ( simple_binary @@ prim "PAIR")
    | C_CAR               -> Some ( simple_unary @@ prim "CAR")
    | C_CDR               -> Some ( simple_unary @@ prim "CDR")
    | C_EQ                -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "EQ"])
    | C_NEQ               -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "NEQ"])
    | C_LT                -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "LT"])
    | C_LE                -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "LE"])
    | C_GT                -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "GT"])
    | C_GE                -> Some ( simple_binary @@ seq [prim "COMPARE" ; prim "GE"])
    | C_UPDATE            -> Some ( simple_ternary @@ prim "UPDATE")
    | C_SOME              -> Some ( simple_unary  @@ prim "SOME")
    | C_MAP_FIND          -> Some ( simple_binary @@ seq [prim "GET" ; i_assert_some_msg (i_push_string "MAP FIND")])
    | C_MAP_MEM           -> Some ( simple_binary @@ prim "MEM")
    | C_MAP_FIND_OPT      -> Some ( simple_binary @@ prim "GET")
    | C_MAP_ADD           -> Some ( simple_ternary @@ seq [dip (i_some) ; prim "UPDATE"])
    | C_MAP_UPDATE        -> Some ( simple_ternary @@ prim "UPDATE")
    | C_FOLD_WHILE        -> Some ( simple_binary @@ seq [i_swap ; (i_push (prim "bool") (prim "True"));prim ~children:[seq [dip i_dup; i_exec; i_unpair]] "LOOP" ;i_swap ; i_drop])
    | C_FOLD_CONTINUE     -> Some ( simple_unary @@ seq [(i_push (prim "bool") (prim "True")); i_pair])
    | C_FOLD_STOP         -> Some ( simple_unary @@ seq [(i_push (prim "bool") (prim "False")); i_pair])
    | C_SIZE              -> Some ( simple_unary @@ prim "SIZE")
    | C_FAILWITH          -> Some ( simple_unary @@ prim "FAILWITH")
    | C_ASSERT_SOME       -> Some ( simple_unary @@ i_assert_some)
    | C_ASSERT_INFERRED   -> Some ( simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit]))
    | C_ASSERTION         -> Some ( simple_unary @@ i_if (seq [i_push_unit]) (seq [i_push_string "failed assertion" ; i_failwith]))
    | C_INT               -> Some ( simple_unary @@ prim "INT")
    | C_ABS               -> Some ( simple_unary @@ prim "ABS")
    | C_IS_NAT            -> Some ( simple_unary @@ prim "ISNAT")
    | C_CONS              -> Some ( simple_binary @@ prim "CONS")
    | C_UNIT              -> Some ( simple_constant @@ prim "UNIT")
    | C_BALANCE           -> Some ( simple_constant @@ prim "BALANCE")
    | C_AMOUNT            -> Some ( simple_constant @@ prim "AMOUNT")
    | C_ADDRESS           -> Some ( simple_unary @@ prim "ADDRESS")
    | C_SELF_ADDRESS      -> Some ( simple_constant @@ seq [prim "SELF"; prim "ADDRESS"])
    | C_IMPLICIT_ACCOUNT  -> Some ( simple_unary @@ prim "IMPLICIT_ACCOUNT")
    | C_SET_DELEGATE      -> Some ( simple_unary @@ prim "SET_DELEGATE")
    | C_NOW               -> Some ( simple_constant @@ prim "NOW")
    | C_CALL              -> Some ( simple_ternary @@ prim "TRANSFER_TOKENS")
    | C_SOURCE            -> Some ( simple_constant @@ prim "SOURCE")
    | C_SENDER            -> Some ( simple_constant @@ prim "SENDER")
    | C_SET_MEM           -> Some ( simple_binary @@ prim "MEM")
    | C_SET_ADD           -> Some ( simple_binary @@ seq [dip (i_push (prim "bool") (prim "True")) ; prim "UPDATE"])
    | C_SET_REMOVE        -> Some ( simple_binary @@ seq [dip (i_push (prim "bool") (prim "False")) ; prim "UPDATE"])
    | C_SLICE             -> Some ( simple_ternary @@ seq [prim "SLICE" ; i_assert_some_msg (i_push_string "SLICE")])
    | C_SHA256            -> Some ( simple_unary @@ prim "SHA256")
    | C_SHA512            -> Some ( simple_unary @@ prim "SHA512")
    | C_BLAKE2b           -> Some ( simple_unary @@ prim "BLAKE2B")
    | C_CHECK_SIGNATURE   -> Some ( simple_ternary @@ prim "CHECK_SIGNATURE")
    | C_HASH_KEY          -> Some ( simple_unary @@ prim "HASH_KEY")
    | C_BYTES_PACK        -> Some ( simple_unary @@ prim "PACK")
    | C_CONCAT            -> Some ( simple_binary @@ prim "CONCAT")
    | C_CHAIN_ID          -> Some ( simple_constant @@ prim "CHAIN_ID")
    | _                   -> None

end
