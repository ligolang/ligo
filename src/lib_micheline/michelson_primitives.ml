(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Micheline_main

type prim =
  | K_parameter
  | K_storage
  | K_code
  | D_False
  | D_Elt
  | D_Left
  | D_None
  | D_Pair
  | D_Right
  | D_Some
  | D_True
  | D_Unit
  | I_PACK
  | I_UNPACK
  | I_BLAKE2B
  | I_SHA256
  | I_SHA512
  | I_ABS
  | I_ADD
  | I_AMOUNT
  | I_AND
  | I_BALANCE
  | I_CAR
  | I_CDR
  | I_CHECK_SIGNATURE
  | I_COMPARE
  | I_CONCAT
  | I_CONS
  | I_CREATE_ACCOUNT
  | I_CREATE_CONTRACT
  | I_IMPLICIT_ACCOUNT
  | I_DIP
  | I_DROP
  | I_DUP
  | I_EDIV
  | I_EMPTY_MAP
  | I_EMPTY_SET
  | I_EQ
  | I_EXEC
  | I_FAILWITH
  | I_GE
  | I_GET
  | I_GT
  | I_HASH_KEY
  | I_IF
  | I_IF_CONS
  | I_IF_LEFT
  | I_IF_NONE
  | I_INT
  | I_LAMBDA
  | I_LE
  | I_LEFT
  | I_LOOP
  | I_LSL
  | I_LSR
  | I_LT
  | I_MAP
  | I_MEM
  | I_MUL
  | I_NEG
  | I_NEQ
  | I_NIL
  | I_NONE
  | I_NOP
  | I_NOT
  | I_NOW
  | I_OR
  | I_PAIR
  | I_PUSH
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_SENDER
  | I_SELF
  | I_SLICE
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SWAP
  | I_TRANSFER_TOKENS
  | I_SET_DELEGATE
  | I_UNIT
  | I_UPDATE
  | I_XOR
  | I_ITER
  | I_LOOP_LEFT
  | I_ADDRESS
  | I_CONTRACT
  | I_ISNAT
  | I_CAST
  | I_RENAME
  | T_bool
  | T_contract
  | T_int
  | T_key
  | T_key_hash
  | T_lambda
  | T_list
  | T_map
  | T_big_map
  | T_nat
  | T_option
  | T_or
  | T_pair
  | T_set
  | T_signature
  | T_string
  | T_bytes
  | T_mutez
  | T_timestamp
  | T_unit
  | T_operation
  | T_address

let valid_case name =
  let is_lower = function  '_' | 'a'..'z' -> true | _ -> false in
  let is_upper = function  '_' | 'A'..'Z' -> true | _ -> false in
  let rec for_all a b f =
    Compare.Int.(a > b) || f a && for_all (a + 1) b f in
  let len = String.length name in
  Compare.Int.(len <> 0)
  &&
  Compare.Char.(String.get name 0 <> '_')
  &&
  ((is_upper (String.get name 0)
    && for_all 1 (len - 1) (fun i -> is_upper (String.get name i)))
   ||
   (is_upper (String.get name 0)
    && for_all 1 (len - 1) (fun i -> is_lower (String.get name i)))
   ||
   (is_lower (String.get name 0)
    && for_all 1 (len - 1) (fun i -> is_lower (String.get name i))))

let string_of_prim = function
  | K_parameter -> "parameter"
  | K_storage -> "storage"
  | K_code -> "code"
  | D_False -> "False"
  | D_Elt -> "Elt"
  | D_Left -> "Left"
  | D_None -> "None"
  | D_Pair -> "Pair"
  | D_Right -> "Right"
  | D_Some -> "Some"
  | D_True -> "True"
  | D_Unit -> "Unit"
  | I_PACK -> "PACK"
  | I_UNPACK -> "UNPACK"
  | I_BLAKE2B -> "BLAKE2B"
  | I_SHA256 -> "SHA256"
  | I_SHA512 -> "SHA512"
  | I_ABS -> "ABS"
  | I_ADD -> "ADD"
  | I_AMOUNT -> "AMOUNT"
  | I_AND -> "AND"
  | I_BALANCE -> "BALANCE"
  | I_CAR -> "CAR"
  | I_CDR -> "CDR"
  | I_CHECK_SIGNATURE -> "CHECK_SIGNATURE"
  | I_COMPARE -> "COMPARE"
  | I_CONCAT -> "CONCAT"
  | I_CONS -> "CONS"
  | I_CREATE_ACCOUNT -> "CREATE_ACCOUNT"
  | I_CREATE_CONTRACT -> "CREATE_CONTRACT"
  | I_IMPLICIT_ACCOUNT -> "IMPLICIT_ACCOUNT"
  | I_DIP -> "DIP"
  | I_DROP -> "DROP"
  | I_DUP -> "DUP"
  | I_EDIV -> "EDIV"
  | I_EMPTY_MAP -> "EMPTY_MAP"
  | I_EMPTY_SET -> "EMPTY_SET"
  | I_EQ -> "EQ"
  | I_EXEC -> "EXEC"
  | I_FAILWITH -> "FAILWITH"
  | I_GE -> "GE"
  | I_GET -> "GET"
  | I_GT -> "GT"
  | I_HASH_KEY -> "HASH_KEY"
  | I_IF -> "IF"
  | I_IF_CONS -> "IF_CONS"
  | I_IF_LEFT -> "IF_LEFT"
  | I_IF_NONE -> "IF_NONE"
  | I_INT -> "INT"
  | I_LAMBDA -> "LAMBDA"
  | I_LE -> "LE"
  | I_LEFT -> "LEFT"
  | I_LOOP -> "LOOP"
  | I_LSL -> "LSL"
  | I_LSR -> "LSR"
  | I_LT -> "LT"
  | I_MAP -> "MAP"
  | I_MEM -> "MEM"
  | I_MUL -> "MUL"
  | I_NEG -> "NEG"
  | I_NEQ -> "NEQ"
  | I_NIL -> "NIL"
  | I_NONE -> "NONE"
  | I_NOP -> "NOP"
  | I_NOT -> "NOT"
  | I_NOW -> "NOW"
  | I_OR -> "OR"
  | I_PAIR -> "PAIR"
  | I_PUSH -> "PUSH"
  | I_RIGHT -> "RIGHT"
  | I_SIZE -> "SIZE"
  | I_SOME -> "SOME"
  | I_SOURCE -> "SOURCE"
  | I_SENDER -> "SENDER"
  | I_SELF -> "SELF"
  | I_SLICE -> "SLICE"
  | I_STEPS_TO_QUOTA -> "STEPS_TO_QUOTA"
  | I_SUB -> "SUB"
  | I_SWAP -> "SWAP"
  | I_TRANSFER_TOKENS -> "TRANSFER_TOKENS"
  | I_SET_DELEGATE -> "SET_DELEGATE"
  | I_UNIT -> "UNIT"
  | I_UPDATE -> "UPDATE"
  | I_XOR -> "XOR"
  | I_ITER -> "ITER"
  | I_LOOP_LEFT -> "LOOP_LEFT"
  | I_ADDRESS -> "ADDRESS"
  | I_CONTRACT -> "CONTRACT"
  | I_ISNAT -> "ISNAT"
  | I_CAST -> "CAST"
  | I_RENAME -> "RENAME"
  | T_bool -> "bool"
  | T_contract -> "contract"
  | T_int -> "int"
  | T_key -> "key"
  | T_key_hash -> "key_hash"
  | T_lambda -> "lambda"
  | T_list -> "list"
  | T_map -> "map"
  | T_big_map -> "big_map"
  | T_nat -> "nat"
  | T_option -> "option"
  | T_or -> "or"
  | T_pair -> "pair"
  | T_set -> "set"
  | T_signature -> "signature"
  | T_string -> "string"
  | T_bytes -> "bytes"
  | T_mutez -> "mutez"
  | T_timestamp -> "timestamp"
  | T_unit -> "unit"
  | T_operation -> "operation"
  | T_address -> "address"

type failure =
    Unknown_primitive_name of string
  | Invalid_case of string
  | Invalid_primitive_name of string Micheline_main.canonical * Micheline_main.canonical_location

let prim_of_string : string -> (prim , failure) result = function
  | "parameter" -> Ok K_parameter
  | "storage" -> Ok K_storage
  | "code" -> Ok K_code
  | "False" -> Ok D_False
  | "Elt" -> Ok D_Elt
  | "Left" -> Ok D_Left
  | "None" -> Ok D_None
  | "Pair" -> Ok D_Pair
  | "Right" -> Ok D_Right
  | "Some" -> Ok D_Some
  | "True" -> Ok D_True
  | "Unit" -> Ok D_Unit
  | "PACK" -> Ok I_PACK
  | "UNPACK" -> Ok I_UNPACK
  | "BLAKE2B" -> Ok I_BLAKE2B
  | "SHA256" -> Ok I_SHA256
  | "SHA512" -> Ok I_SHA512
  | "ABS" -> Ok I_ABS
  | "ADD" -> Ok I_ADD
  | "AMOUNT" -> Ok I_AMOUNT
  | "AND" -> Ok I_AND
  | "BALANCE" -> Ok I_BALANCE
  | "CAR" -> Ok I_CAR
  | "CDR" -> Ok I_CDR
  | "CHECK_SIGNATURE" -> Ok I_CHECK_SIGNATURE
  | "COMPARE" -> Ok I_COMPARE
  | "CONCAT" -> Ok I_CONCAT
  | "CONS" -> Ok I_CONS
  | "CREATE_ACCOUNT" -> Ok I_CREATE_ACCOUNT
  | "CREATE_CONTRACT" -> Ok I_CREATE_CONTRACT
  | "IMPLICIT_ACCOUNT" -> Ok I_IMPLICIT_ACCOUNT
  | "DIP" -> Ok I_DIP
  | "DROP" -> Ok I_DROP
  | "DUP" -> Ok I_DUP
  | "EDIV" -> Ok I_EDIV
  | "EMPTY_MAP" -> Ok I_EMPTY_MAP
  | "EMPTY_SET" -> Ok I_EMPTY_SET
  | "EQ" -> Ok I_EQ
  | "EXEC" -> Ok I_EXEC
  | "FAILWITH" -> Ok I_FAILWITH
  | "GE" -> Ok I_GE
  | "GET" -> Ok I_GET
  | "GT" -> Ok I_GT
  | "HASH_KEY" -> Ok I_HASH_KEY
  | "IF" -> Ok I_IF
  | "IF_CONS" -> Ok I_IF_CONS
  | "IF_LEFT" -> Ok I_IF_LEFT
  | "IF_NONE" -> Ok I_IF_NONE
  | "INT" -> Ok I_INT
  | "LAMBDA" -> Ok I_LAMBDA
  | "LE" -> Ok I_LE
  | "LEFT" -> Ok I_LEFT
  | "LOOP" -> Ok I_LOOP
  | "LSL" -> Ok I_LSL
  | "LSR" -> Ok I_LSR
  | "LT" -> Ok I_LT
  | "MAP" -> Ok I_MAP
  | "MEM" -> Ok I_MEM
  | "MUL" -> Ok I_MUL
  | "NEG" -> Ok I_NEG
  | "NEQ" -> Ok I_NEQ
  | "NIL" -> Ok I_NIL
  | "NONE" -> Ok I_NONE
  | "NOP" -> Ok I_NOP
  | "NOT" -> Ok I_NOT
  | "NOW" -> Ok I_NOW
  | "OR" -> Ok I_OR
  | "PAIR" -> Ok I_PAIR
  | "PUSH" -> Ok I_PUSH
  | "RIGHT" -> Ok I_RIGHT
  | "SIZE" -> Ok I_SIZE
  | "SOME" -> Ok I_SOME
  | "SOURCE" -> Ok I_SOURCE
  | "SENDER" -> Ok I_SENDER
  | "SELF" -> Ok I_SELF
  | "SLICE" -> Ok I_SLICE
  | "STEPS_TO_QUOTA" -> Ok I_STEPS_TO_QUOTA
  | "SUB" -> Ok I_SUB
  | "SWAP" -> Ok I_SWAP
  | "TRANSFER_TOKENS" -> Ok I_TRANSFER_TOKENS
  | "SET_DELEGATE" -> Ok I_SET_DELEGATE
  | "UNIT" -> Ok I_UNIT
  | "UPDATE" -> Ok I_UPDATE
  | "XOR" -> Ok I_XOR
  | "ITER" -> Ok I_ITER
  | "LOOP_LEFT" -> Ok I_LOOP_LEFT
  | "ADDRESS" -> Ok I_ADDRESS
  | "CONTRACT" -> Ok I_CONTRACT
  | "ISNAT" -> Ok I_ISNAT
  | "CAST" -> Ok I_CAST
  | "RENAME" -> Ok I_RENAME
  | "bool" -> Ok T_bool
  | "contract" -> Ok T_contract
  | "int" -> Ok T_int
  | "key" -> Ok T_key
  | "key_hash" -> Ok T_key_hash
  | "lambda" -> Ok T_lambda
  | "list" -> Ok T_list
  | "map" -> Ok T_map
  | "big_map" -> Ok T_big_map
  | "nat" -> Ok T_nat
  | "option" -> Ok T_option
  | "or" -> Ok T_or
  | "pair" -> Ok T_pair
  | "set" -> Ok T_set
  | "signature" -> Ok T_signature
  | "string" -> Ok T_string
  | "bytes" -> Ok T_bytes
  | "mutez" -> Ok T_mutez
  | "timestamp" -> Ok T_timestamp
  | "unit" -> Ok T_unit
  | "operation" -> Ok T_operation
  | "address" -> Ok T_address
  | n ->
      if valid_case n then
        Error (Unknown_primitive_name n)
      else
        Error (Invalid_case n)

let (>>?) x f : (_ , failure) result = match x with
  | Ok x -> f x
  | Error _ as err -> err

let prims_of_strings : string canonical -> (prim Micheline_main.canonical , failure) result = fun expr ->
  let rec convert : (canonical_location , string) node -> ((canonical_location , prim) node , failure) result  = function
    | Int _ | String _ | Bytes _ as expr -> Ok expr
    | Seq (_, args) -> (
        let aux : ((canonical_location , prim) node list , failure) result -> (canonical_location , string) node -> ((_ , prim) node list , failure) result =
          fun acc arg ->
            acc >>? fun acc' ->
            convert arg >>? fun arg' ->
            Ok (arg' :: acc')
        in
        (List.fold_left aux (Ok []) args) >>? fun args' ->
        Ok (Seq (0, List.rev args'))
      )
    | Prim (_, prim, args, annot) -> (
        prim_of_string prim >>? fun prim' ->
        let aux : (_ list , failure) result -> _ -> (_ list , failure) result = fun acc arg ->
          acc >>? fun args ->
          convert arg >>? fun arg ->
          Ok (arg :: args)
        in
        List.fold_left aux (Ok []) args >>? fun args' ->
        Ok (Prim (0, prim', List.rev args', annot))
      )
  in
  convert (root expr) >>? fun expr ->
  Ok (strip_locations expr)

let strings_of_prims expr =
  let rec convert = function
    | Int _ | String _ | Bytes _ as expr -> expr
    | Prim (_, prim, args, annot) ->
        let prim = string_of_prim prim in
        let args = List.map convert args in
        Prim (0, prim, args, annot)
    | Seq (_, args) ->
        let args = List.map convert args in
        Seq (0, args) in
  strip_locations (convert (root expr))

let prim_encoding =
  let open Data_encoding in
  def "michelson.v1.primitives" @@
  string_enum [
    ("parameter", K_parameter) ;
    ("storage", K_storage) ;
    ("code", K_code) ;
    ("False", D_False) ;
    ("Elt", D_Elt) ;
    ("Left", D_Left) ;
    ("None", D_None) ;
    ("Pair", D_Pair) ;
    ("Right", D_Right) ;
    ("Some", D_Some) ;
    ("True", D_True) ;
    ("Unit", D_Unit) ;
    ("PACK", I_PACK) ;
    ("UNPACK", I_UNPACK) ;
    ("BLAKE2B", I_BLAKE2B) ;
    ("SHA256", I_SHA256) ;
    ("SHA512", I_SHA512) ;
    ("ABS", I_ABS) ;
    ("ADD", I_ADD) ;
    ("AMOUNT", I_AMOUNT) ;
    ("AND", I_AND) ;
    ("BALANCE", I_BALANCE) ;
    ("CAR", I_CAR) ;
    ("CDR", I_CDR) ;
    ("CHECK_SIGNATURE", I_CHECK_SIGNATURE) ;
    ("COMPARE", I_COMPARE) ;
    ("CONCAT", I_CONCAT) ;
    ("CONS", I_CONS) ;
    ("CREATE_ACCOUNT", I_CREATE_ACCOUNT) ;
    ("CREATE_CONTRACT", I_CREATE_CONTRACT) ;
    ("IMPLICIT_ACCOUNT", I_IMPLICIT_ACCOUNT) ;
    ("DIP", I_DIP) ;
    ("DROP", I_DROP) ;
    ("DUP", I_DUP) ;
    ("EDIV", I_EDIV) ;
    ("EMPTY_MAP", I_EMPTY_MAP) ;
    ("EMPTY_SET", I_EMPTY_SET) ;
    ("EQ", I_EQ) ;
    ("EXEC", I_EXEC) ;
    ("FAILWITH", I_FAILWITH) ;
    ("GE", I_GE) ;
    ("GET", I_GET) ;
    ("GT", I_GT) ;
    ("HASH_KEY", I_HASH_KEY) ;
    ("IF", I_IF) ;
    ("IF_CONS", I_IF_CONS) ;
    ("IF_LEFT", I_IF_LEFT) ;
    ("IF_NONE", I_IF_NONE) ;
    ("INT", I_INT) ;
    ("LAMBDA", I_LAMBDA) ;
    ("LE", I_LE) ;
    ("LEFT", I_LEFT) ;
    ("LOOP", I_LOOP) ;
    ("LSL", I_LSL) ;
    ("LSR", I_LSR) ;
    ("LT", I_LT) ;
    ("MAP", I_MAP) ;
    ("MEM", I_MEM) ;
    ("MUL", I_MUL) ;
    ("NEG", I_NEG) ;
    ("NEQ", I_NEQ) ;
    ("NIL", I_NIL) ;
    ("NONE", I_NONE) ;
    ("NOT", I_NOT) ;
    ("NOW", I_NOW) ;
    ("OR", I_OR) ;
    ("PAIR", I_PAIR) ;
    ("PUSH", I_PUSH) ;
    ("RIGHT", I_RIGHT) ;
    ("SIZE", I_SIZE) ;
    ("SOME", I_SOME) ;
    ("SOURCE", I_SOURCE) ;
    ("SENDER", I_SENDER) ;
    ("SELF", I_SELF) ;
    ("STEPS_TO_QUOTA", I_STEPS_TO_QUOTA) ;
    ("SUB", I_SUB) ;
    ("SWAP", I_SWAP) ;
    ("TRANSFER_TOKENS", I_TRANSFER_TOKENS) ;
    ("SET_DELEGATE", I_SET_DELEGATE) ;
    ("UNIT", I_UNIT) ;
    ("UPDATE", I_UPDATE) ;
    ("XOR", I_XOR) ;
    ("ITER", I_ITER) ;
    ("LOOP_LEFT", I_LOOP_LEFT) ;
    ("ADDRESS", I_ADDRESS) ;
    ("CONTRACT", I_CONTRACT) ;
    ("ISNAT", I_ISNAT) ;
    ("CAST", I_CAST) ;
    ("RENAME", I_RENAME) ;
    ("bool", T_bool) ;
    ("contract", T_contract) ;
    ("int", T_int) ;
    ("key", T_key) ;
    ("key_hash", T_key_hash) ;
    ("lambda", T_lambda) ;
    ("list", T_list) ;
    ("map", T_map) ;
    ("big_map", T_big_map) ;
    ("nat", T_nat) ;
    ("option", T_option) ;
    ("or", T_or) ;
    ("pair", T_pair) ;
    ("set", T_set) ;
    ("signature", T_signature) ;
    ("string", T_string) ;
    ("bytes", T_bytes) ;
    ("mutez", T_mutez) ;
    ("timestamp", T_timestamp) ;
    ("unit", T_unit) ;
    ("operation", T_operation) ;
    ("address", T_address) ;
    (* Alpha_002 addition *)
    ("SLICE", I_SLICE) ;
  ]

