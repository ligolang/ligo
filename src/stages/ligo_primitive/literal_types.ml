open Var

(* type constants *)
type t =
  | String
  | Bytes
  | Int
  | Operation
  | Nat
  | Tez
  | Unit
  | Address
  | Signature
  | Key
  | Key_hash
  | Timestamp
  | Chain_id
  | List
  | Map
  | Big_map
  | Set
  | Contract
  | Michelson_or
  | Michelson_pair
  | Baker_hash
  | Pvss_key
  | Sapling_transaction
  | Sapling_state
  | Baker_operation
  | Bls12_381_g1
  | Bls12_381_g2
  | Bls12_381_fr
  | Never
  | Ticket
  | Michelson_program [@only_interpreter]
  | Michelson_contract [@only_interpreter]
  | Ast_contract [@only_interpreter]
  | Typed_address [@only_interpreter]
  | Mutation [@only_interpreter]
  | Chest
  | Chest_key
  | Chest_opening_result
  | Tx_rollup_l2_address
  | External of string
  | Gen [@only_interpreter]
  | Int64 [@only_interpreter]
[@@deriving ord, eq, yojson, hash, sexp, is { tags = [ "only_interpreter" ] }]

let to_string = function
  | String -> "string"
  | Bytes -> "bytes"
  | Int -> "int"
  | Operation -> "operation"
  | Nat -> "nat"
  | Tez -> "tez"
  | Unit -> "unit"
  | Address -> "address"
  | Signature -> "signature"
  | Key -> "key"
  | Key_hash -> "key_hash"
  | Timestamp -> "timestamp"
  | Chain_id -> "chain_id"
  | List -> "list"
  | Map -> "map"
  | Big_map -> "big_map"
  | Set -> "set"
  | Contract -> "contract"
  | Michelson_or -> "michelson_or"
  | Michelson_pair -> "michelson_pair"
  | Baker_hash -> "baker_hash"
  | Pvss_key -> "pvss_key"
  | Sapling_transaction -> "sapling_transaction"
  | Sapling_state -> "sapling_state"
  | Baker_operation -> "baker_operation"
  | Bls12_381_g1 -> "bls12_381_g1"
  | Bls12_381_g2 -> "bls12_381_g2"
  | Bls12_381_fr -> "bls12_381_fr"
  | Never -> "never"
  | Ticket -> "ticket"
  | Michelson_program -> "michelson_program"
  | Michelson_contract -> "michelson_contract"
  | Ast_contract -> "ast_contract"
  | Typed_address -> "typed_address"
  | Mutation -> "mutation"
  | Chest -> "chest"
  | Chest_key -> "chest_key"
  | Chest_opening_result -> "chest_opening_result"
  | Tx_rollup_l2_address -> "tx_rollup_l2_address"
  | External s -> "external_" ^ s
  | Gen -> "pbt_gen"
  | Int64 -> "int64"


let of_string = function
  | "string" -> String
  | "bytes" -> Bytes
  | "int" -> Int
  | "operation" -> Operation
  | "nat" -> Nat
  | "tez" -> Tez
  | "unit" -> Unit
  | "address" -> Address
  | "signature" -> Signature
  | "key" -> Key
  | "key_hash" -> Key_hash
  | "timestamp" -> Timestamp
  | "chain_id" -> Chain_id
  | "list" -> List
  | "map" -> Map
  | "big_map" -> Big_map
  | "set" -> Set
  | "contract" -> Contract
  | "michelson_or" -> Michelson_or
  | "michelson_pair" -> Michelson_pair
  | "baker_hash" -> Baker_hash
  | "pvss_key" -> Pvss_key
  | "sapling_transaction" -> Sapling_transaction
  | "sapling_state" -> Sapling_state
  | "baker_operation" -> Baker_operation
  | "bls12_381_g1" -> Bls12_381_g1
  | "bls12_381_g2" -> Bls12_381_g2
  | "bls12_381_fr" -> Bls12_381_fr
  | "never" -> Never
  | "ticket" -> Ticket
  | "michelson_program" -> Michelson_program
  | "michelson_contract" -> Michelson_contract
  | "ast_contract" -> Ast_contract
  | "typed_address" -> Typed_address
  | "mutation" -> Mutation
  | "chest" -> Chest
  | "chest_key" -> Chest_key
  | "chest_opening_result" -> Chest_opening_result
  | "tx_rollup_l2_address" -> Tx_rollup_l2_address
  | "external_int" -> External "int"
  | "external_ediv" -> External "ediv"
  | "external_u_ediv" -> External "u_ediv"
  | "pbt_gen" -> Gen
  | "int64" -> Int64
  | "external_and" -> External "and"
  | "external_u_and" -> External "u_and"
  | _ -> failwith "Forgot to add constant name in constant.ml?"


let pp ppf l = Format.fprintf ppf "%s" (to_string l)
let string = String
let bytes = Bytes
let int = Int
let operation = Operation
let nat = Nat
let tez = Tez
let unit = Unit
let address = Address
let signature = Signature
let key = Key
let key_hash = Key_hash
let timestamp = Timestamp
let chain_id = Chain_id
let list = List
let map = Map
let big_map = Big_map
let set = Set
let contract = Contract
let michelson_or = Michelson_or
let michelson_pair = Michelson_pair
let baker_hash = Baker_hash
let pvss_key = Pvss_key
let sapling_transaction = Sapling_transaction
let sapling_state = Sapling_state
let baker_operation = Baker_operation
let bls12_381_g1 = Bls12_381_g1
let bls12_381_g2 = Bls12_381_g2
let bls12_381_fr = Bls12_381_fr
let never = Never
let ticket = Ticket
let michelson_program = Michelson_program
let michelson_contract = Michelson_contract
let ast_contract = Ast_contract
let typed_address = Typed_address
let mutation = Mutation
let chest = Chest
let chest_key = Chest_key
let chest_opening_result = Chest_opening_result
let tx_rollup_l2_address = Tx_rollup_l2_address
let external_failwith = External "failwith"
let external_int = External "int"
let external_ediv = External "ediv"
let external_u_ediv = External "u_ediv"
let gen = Gen
let int64 = Int64
let v_bool = Type_var.of_input_var "bool"
let v_string = Type_var.of_input_var (to_string String)
let v_bytes = Type_var.of_input_var (to_string Bytes)
let v_int = Type_var.of_input_var (to_string Int)
let v_operation = Type_var.of_input_var (to_string Operation)
let v_nat = Type_var.of_input_var (to_string Nat)
let v_tez = Type_var.of_input_var (to_string Tez)
let v_unit = Type_var.of_input_var (to_string Unit)
let v_address = Type_var.of_input_var (to_string Address)
let v_signature = Type_var.of_input_var (to_string Signature)
let v_key = Type_var.of_input_var (to_string Key)
let v_key_hash = Type_var.of_input_var (to_string Key_hash)
let v_timestamp = Type_var.of_input_var (to_string Timestamp)
let v_chain_id = Type_var.of_input_var (to_string Chain_id)
let v_option = Type_var.of_input_var "option"
let v_list = Type_var.of_input_var (to_string List)
let v_map = Type_var.of_input_var (to_string Map)
let v_big_map = Type_var.of_input_var (to_string Big_map)
let v_set = Type_var.of_input_var (to_string Set)
let v_contract = Type_var.of_input_var (to_string Contract)
let v_michelson_or = Type_var.of_input_var (to_string Michelson_or)
let v_michelson_pair = Type_var.of_input_var (to_string Michelson_pair)
let v_baker_hash = Type_var.of_input_var (to_string Baker_hash)
let v_pvss_key = Type_var.of_input_var (to_string Pvss_key)
let v_sapling_trasaction = Type_var.of_input_var (to_string Sapling_transaction)
let v_sapling_state = Type_var.of_input_var (to_string Sapling_state)
let v_baker_operation = Type_var.of_input_var (to_string Baker_operation)
let v_bls12_381_g1 = Type_var.of_input_var (to_string Bls12_381_g1)
let v_bls12_381_g2 = Type_var.of_input_var (to_string Bls12_381_g2)
let v_bls12_381_fr = Type_var.of_input_var (to_string Bls12_381_fr)
let v_never = Type_var.of_input_var (to_string Never)
let v_ticket = Type_var.of_input_var (to_string Ticket)
let v_test_michelson = Type_var.of_input_var (to_string Michelson_program)
let v_michelson_contract = Type_var.of_input_var (to_string Michelson_contract)
let v_ast_contract = Type_var.of_input_var (to_string Ast_contract)
let v_typed_address = Type_var.of_input_var (to_string Typed_address)
let v_mutation = Type_var.of_input_var (to_string Mutation)
let v_chest = Type_var.of_input_var (to_string Chest)
let v_chest_key = Type_var.of_input_var (to_string Chest_key)
let v_chest_opening_result = Type_var.of_input_var (to_string Chest_opening_result)
let v_tx_rollup_l2_address = Type_var.of_input_var (to_string Tx_rollup_l2_address)
let v_external_int = Type_var.of_input_var (to_string @@ External "int")
let v_external_ediv = Type_var.of_input_var (to_string @@ External "ediv")
let v_external_u_ediv = Type_var.of_input_var (to_string @@ External "u_ediv")
let v_external_and = Type_var.of_input_var (to_string @@ External "and")
let v_external_u_and = Type_var.of_input_var (to_string @@ External "u_and")
let v_gen = Type_var.of_input_var (to_string @@ Gen)
let v_int64 = Type_var.of_input_var (to_string @@ Int64)
