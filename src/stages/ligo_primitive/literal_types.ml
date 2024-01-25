open Var

module External = struct
  type t =
    | Bytes
    | Int
    | Ediv
    | And
    | Or
    | Xor
    | Lsl
    | Lsr
    | Map_find_opt
    | Map_add
    | Map_remove
    | Map_remove_value
  [@@deriving ord, eq, yojson, hash, sexp, is { tags = [ "only_interpreter" ] }]

  let to_string = function
    | Bytes -> "bytes"
    | Int -> "int"
    | Ediv -> "ediv"
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"
    | Lsl -> "lsl"
    | Lsr -> "lsr"
    | Map_find_opt -> "map_find_opt"
    | Map_add -> "map_add"
    | Map_remove -> "map_remove"
    | Map_remove_value -> "map_remove_value"
end

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
  | Dynamic_entrypoint
  | Michelson_program [@only_interpreter]
  | Michelson_contract [@only_interpreter]
  | Typed_address [@only_interpreter]
  | Mutation [@only_interpreter]
  | Tx_rollup_l2_address
  | External of External.t
  | Gen [@only_interpreter]
  | Int64 [@only_interpreter]
  | Views [@only_interpreter]
  | Chest
  | Chest_key
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
  | Typed_address -> "typed_address"
  | Mutation -> "mutation"
  | Tx_rollup_l2_address -> "tx_rollup_l2_address"
  | External s -> "external_" ^ External.to_string s
  | Gen -> "pbt_gen"
  | Int64 -> "int64"
  | Views -> "views"
  | Dynamic_entrypoint -> "dynamic_entrypoint"
  | Chest -> "chest"
  | Chest_key -> "chest_key"


let of_string_opt = function
  | "string" -> Some String
  | "bytes" -> Some Bytes
  | "int" -> Some Int
  | "operation" -> Some Operation
  | "nat" -> Some Nat
  | "tez" -> Some Tez
  | "unit" -> Some Unit
  | "address" -> Some Address
  | "signature" -> Some Signature
  | "key" -> Some Key
  | "key_hash" -> Some Key_hash
  | "timestamp" -> Some Timestamp
  | "chain_id" -> Some Chain_id
  | "list" -> Some List
  | "map" -> Some Map
  | "big_map" -> Some Big_map
  | "set" -> Some Set
  | "contract" -> Some Contract
  | "michelson_or" -> Some Michelson_or
  | "michelson_pair" -> Some Michelson_pair
  | "baker_hash" -> Some Baker_hash
  | "pvss_key" -> Some Pvss_key
  | "sapling_transaction" -> Some Sapling_transaction
  | "sapling_state" -> Some Sapling_state
  | "baker_operation" -> Some Baker_operation
  | "bls12_381_g1" -> Some Bls12_381_g1
  | "bls12_381_g2" -> Some Bls12_381_g2
  | "bls12_381_fr" -> Some Bls12_381_fr
  | "never" -> Some Never
  | "ticket" -> Some Ticket
  | "michelson_program" -> Some Michelson_program
  | "michelson_contract" -> Some Michelson_contract
  | "typed_address" -> Some Typed_address
  | "mutation" -> Some Mutation
  | "tx_rollup_l2_address" -> Some Tx_rollup_l2_address
  | "pbt_gen" -> Some Gen
  | "int64" -> Some Int64
  | "views" -> Some Views
  | "external_bytes" -> Some (External Bytes)
  | "external_int" -> Some (External Int)
  | "external_ediv" -> Some (External Ediv)
  | "external_and" -> Some (External And)
  | "external_or" -> Some (External Or)
  | "external_xor" -> Some (External Xor)
  | "external_lsl" -> Some (External Lsl)
  | "external_lsr" -> Some (External Lsr)
  | "external_map_find_opt" -> Some (External Map_find_opt)
  | "external_map_add" -> Some (External Map_add)
  | "external_map_remove" -> Some (External Map_remove)
  | "external_map_remove_value" -> Some (External Map_remove_value)
  | "dynamic_entrypoint" -> Some Dynamic_entrypoint
  | "chest" -> Some Chest
  | "chest_key" -> Some Chest_key
  | _ -> None


let to_arity = function
  | String -> 0
  | Bytes -> 0
  | Int -> 0
  | Operation -> 0
  | Nat -> 0
  | Tez -> 0
  | Unit -> 0
  | Address -> 0
  | Signature -> 0
  | Key -> 0
  | Key_hash -> 0
  | Timestamp -> 0
  | Chain_id -> 0
  | List -> 1
  | Map -> 2
  | Big_map -> 2
  | Set -> 1
  | Contract -> 1
  | Michelson_or -> 2
  | Michelson_pair -> 2
  | Baker_hash -> 0
  | Pvss_key -> 0
  | Sapling_transaction -> 1
  | Sapling_state -> 1
  | Baker_operation -> 0
  | Bls12_381_g1 -> 0
  | Bls12_381_g2 -> 0
  | Bls12_381_fr -> 0
  | Never -> 0
  | Ticket -> 1
  | Michelson_program -> 0
  | Michelson_contract -> 2
  | Typed_address -> 2
  | Mutation -> 0
  | Tx_rollup_l2_address -> 0
  | External Bytes -> 1
  | External Int -> 1
  | External Ediv -> 2
  | External And -> 2
  | External Or -> 2
  | External Xor -> 2
  | External Lsl -> 2
  | External Lsr -> 2
  | External Map_find_opt -> 2
  | External Map_add -> 3
  | External Map_remove -> 2
  | External Map_remove_value -> 2
  | Gen -> 1
  | Int64 -> 0
  | Views -> 1
  | Dynamic_entrypoint -> 2
  | Chest -> 0
  | Chest_key -> 0


let string = String
let dynamic_entrypoint = Dynamic_entrypoint
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
let typed_address = Typed_address
let mutation = Mutation
let tx_rollup_l2_address = Tx_rollup_l2_address
let external_int = External Int
let external_ediv = External Ediv
let gen = Gen
let int64 = Int64
let views = Views
let chest = Chest
let chest_key = Chest_key
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
let v_typed_address = Type_var.of_input_var (to_string Typed_address)
let v_mutation = Type_var.of_input_var (to_string Mutation)
let v_tx_rollup_l2_address = Type_var.of_input_var (to_string Tx_rollup_l2_address)
let v_external_int = Type_var.of_input_var (to_string @@ External Int)
let v_external_bytes = Type_var.of_input_var (to_string @@ External Bytes)
let v_external_ediv = Type_var.of_input_var (to_string @@ External Ediv)
let v_external_and = Type_var.of_input_var (to_string @@ External And)
let v_external_or = Type_var.of_input_var (to_string @@ External Or)
let v_external_xor = Type_var.of_input_var (to_string @@ External Xor)
let v_external_lsl = Type_var.of_input_var (to_string @@ External Lsl)
let v_external_lsr = Type_var.of_input_var (to_string @@ External Lsr)
let v_external_map_find_opt = Type_var.of_input_var (to_string @@ External Map_find_opt)
let v_external_map_add = Type_var.of_input_var (to_string @@ External Map_add)
let v_external_map_remove = Type_var.of_input_var (to_string @@ External Map_remove)

let v_external_map_remove_value =
  Type_var.of_input_var (to_string @@ External Map_remove_value)


let v_gen = Type_var.of_input_var (to_string @@ Gen)
let v_int64 = Type_var.of_input_var (to_string @@ Int64)
let v_views = Type_var.of_input_var (to_string @@ Views)
let v_chest = Type_var.of_input_var (to_string @@ Chest)
let v_chest_key = Type_var.of_input_var (to_string @@ Chest_key)
