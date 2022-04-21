open Types
(* type constants *)
type t =
    Bool
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
  | Map_or_big_map
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
  | Michelson_program
  | Test_exec_error
  | Test_exec_result
  | Typed_address
  | Mutation
  | Chest
  | Chest_key
  | Chest_opening_result
  [@@deriving ord, eq]

let to_string = function
  | Bool                 -> "bool"
  | String               -> "string"
  | Bytes                -> "bytes"
  | Int                  -> "int"
  | Operation            -> "operation"
  | Nat                  -> "nat"
  | Tez                  -> "tez"
  | Unit                 -> "unit"
  | Address              -> "address"
  | Signature            -> "signature"
  | Key                  -> "key"
  | Key_hash             -> "key_hash"
  | Timestamp            -> "timestamp"
  | Chain_id             -> "chain_id"
  | List                 -> "list"
  | Map                  -> "map"
  | Big_map              -> "big_map"
  | Set                  -> "set"
  | Contract             -> "contract"
  | Michelson_or         -> "michelson_or"
  | Michelson_pair       -> "michelson_pair"
  | Map_or_big_map       -> "map_or_big_map"
  | Baker_hash           -> "baker_hash"
  | Pvss_key             -> "pvss_key"
  | Sapling_transaction  -> "sapling_transaction"
  | Sapling_state        -> "sapling_state"
  | Baker_operation      -> "baker_operation"
  | Bls12_381_g1         -> "bls12_381_g1"
  | Bls12_381_g2         -> "bls12_381_g2"
  | Bls12_381_fr         -> "bls12_381_fr"
  | Never                -> "never"
  | Ticket               -> "ticket"
  | Michelson_program    -> "michelson_program"
  | Test_exec_error      -> "test_exec_error"
  | Test_exec_result     -> "test_exec_result"
  | Typed_address        -> "typed_address"
  | Mutation             -> "mutation"
  | Chest                -> "chest"
  | Chest_key            -> "chest_key"
  | Chest_opening_result -> "chest_opening_result"

  let of_string = function
  | "bool"                 -> Bool
  | "string"               -> String
  | "bytes"                -> Bytes
  | "int"                  -> Int
  | "operation"            -> Operation
  | "nat"                  -> Nat
  | "tez"                  -> Tez
  | "unit"                 -> Unit
  | "address"              -> Address
  | "signature"            -> Signature
  | "key"                  -> Key
  | "key_hash"             -> Key_hash
  | "timestamp"            -> Timestamp
  | "chain_id"             -> Chain_id
  | "list"                 -> List
  | "map"                  -> Map
  | "big_map"              -> Big_map
  | "set"                  -> Set
  | "contract"             -> Contract
  | "michelson_or"         -> Michelson_or
  | "michelson_pair"       -> Michelson_pair
  | "map_or_big_map"       -> Map_or_big_map
  | "baker_hash"           -> Baker_hash
  | "pvss_key"             -> Pvss_key
  | "sapling_transaction"  -> Sapling_transaction
  | "sapling_state"        -> Sapling_state
  | "baker_operation"      -> Baker_operation
  | "bls12_381_g1"         -> Bls12_381_g1
  | "bls12_381_g2"         -> Bls12_381_g2
  | "bls12_381_fr"         -> Bls12_381_fr
  | "never"                -> Never
  | "ticket"               -> Ticket
  | "michelson_program"    -> Michelson_program
  | "test_exec_error"      -> Test_exec_error
  | "test_exec_result"     -> Test_exec_result
  | "typed_address"        -> Typed_address
  | "mutation"             -> Mutation
  | "chest"                -> Chest
  | "chest_key"            -> Chest_key
  | "chest_opening_result" -> Chest_opening_result
  | _ -> failwith "Forgot to add constant name in constant.ml?"

let bool                 = Bool
let string               = String
let bytes                = Bytes
let int                  = Int
let operation            = Operation
let nat                  = Nat
let tez                  = Tez
let unit                 = Unit
let address              = Address
let signature            = Signature
let key                  = Key
let key_hash             = Key_hash
let timestamp            = Timestamp
let chain_id             = Chain_id
let list                 = List
let map                  = Map
let big_map              = Big_map
let set                  = Set
let contract             = Contract
let michelson_or         = Michelson_or
let michelson_pair       = Michelson_pair
let map_or_big_map       = Map_or_big_map
let baker_hash           = Baker_hash
let pvss_key             = Pvss_key
let sapling_transaction  = Sapling_transaction
let sapling_state        = Sapling_state
let baker_operation      = Baker_operation
let bls12_381_g1         = Bls12_381_g1
let bls12_381_g2         = Bls12_381_g2
let bls12_381_fr         = Bls12_381_fr
let never                = Never
let ticket               = Ticket
let michelson_program    = Michelson_program
let test_exec_error      = Test_exec_error
let test_exec_result     = Test_exec_result
let typed_address        = Typed_address
let mutation             = Mutation
let chest                = Chest
let chest_key            = Chest_key
let chest_opening_result = Chest_opening_result

let v_bool                 : type_variable = TypeVar.of_input_var (to_string Bool)
let v_string               : type_variable = TypeVar.of_input_var (to_string String)
let v_bytes                : type_variable = TypeVar.of_input_var (to_string Bytes)
let v_int                  : type_variable = TypeVar.of_input_var (to_string Int)
let v_operation            : type_variable = TypeVar.of_input_var (to_string Operation)
let v_nat                  : type_variable = TypeVar.of_input_var (to_string Nat)
let v_tez                  : type_variable = TypeVar.of_input_var (to_string Tez)
let v_unit                 : type_variable = TypeVar.of_input_var (to_string Unit)
let v_address              : type_variable = TypeVar.of_input_var (to_string Address)
let v_signature            : type_variable = TypeVar.of_input_var (to_string Signature)
let v_key                  : type_variable = TypeVar.of_input_var (to_string Key)
let v_key_hash             : type_variable = TypeVar.of_input_var (to_string Key_hash)
let v_timestamp            : type_variable = TypeVar.of_input_var (to_string Timestamp)
let v_chain_id             : type_variable = TypeVar.of_input_var (to_string Chain_id)
let v_option               : type_variable = TypeVar.of_input_var ("option")
let v_list                 : type_variable = TypeVar.of_input_var (to_string List)
let v_map                  : type_variable = TypeVar.of_input_var (to_string Map)
let v_big_map              : type_variable = TypeVar.of_input_var (to_string Big_map)
let v_set                  : type_variable = TypeVar.of_input_var (to_string Set)
let v_contract             : type_variable = TypeVar.of_input_var (to_string Contract)
let v_michelson_or         : type_variable = TypeVar.of_input_var (to_string Michelson_or)
let v_michelson_pair       : type_variable = TypeVar.of_input_var (to_string Michelson_pair)
let v_map_or_big_map       : type_variable = TypeVar.of_input_var (to_string Map_or_big_map)
let v_baker_hash           : type_variable = TypeVar.of_input_var (to_string Baker_hash)
let v_pvss_key             : type_variable = TypeVar.of_input_var (to_string Pvss_key)
let v_sapling_trasaction   : type_variable = TypeVar.of_input_var (to_string Sapling_transaction)
let v_sapling_state        : type_variable = TypeVar.of_input_var (to_string Sapling_state)
let v_baker_operation      : type_variable = TypeVar.of_input_var (to_string Baker_operation)
let v_bls12_381_g1         : type_variable = TypeVar.of_input_var (to_string Bls12_381_g1)
let v_bls12_381_g2         : type_variable = TypeVar.of_input_var (to_string Bls12_381_g2)
let v_bls12_381_fr         : type_variable = TypeVar.of_input_var (to_string Bls12_381_fr)
let v_never                : type_variable = TypeVar.of_input_var (to_string Never)
let v_ticket               : type_variable = TypeVar.of_input_var (to_string Ticket)
let v_test_michelson       : type_variable = TypeVar.of_input_var (to_string Michelson_program)
let v_test_exec_error      : type_variable = TypeVar.of_input_var (to_string Test_exec_error)
let v_test_exec_result     : type_variable = TypeVar.of_input_var (to_string Test_exec_result)
let v_typed_address        : type_variable = TypeVar.of_input_var (to_string Typed_address)
let v_mutation             : type_variable = TypeVar.of_input_var (to_string Mutation)
let v_chest                : type_variable = TypeVar.of_input_var (to_string Chest)
let v_chest_key            : type_variable = TypeVar.of_input_var (to_string Chest_key)
let v_chest_opening_result : type_variable = TypeVar.of_input_var (to_string Chest_opening_result)
