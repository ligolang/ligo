include Ast_aggregated.Types
open Ligo_prim
module Z = Simple_utils.Z
module Tezos_protocol = Memory_proto_alpha
module Tezos_raw_protocol = Memory_proto_alpha.Raw_protocol
module Tez = Memory_proto_alpha.Protocol.Alpha_context.Tez
module Timestamp = Memory_proto_alpha.Protocol.Alpha_context.Timestamp

module Contract = struct
  include Tezos_protocol.Protocol.Alpha_context.Contract

  let to_yojson (c : t) = [%to_yojson: string] (to_b58check c)
  let of_yojson _ = failwith "contract_of_yojson: not implemented"
end

module Public_key_hash = struct
  include Tezos_crypto.Signature.Public_key_hash

  let to_yojson (pkh : t) = [%to_yojson: string] (to_b58check pkh)
  let of_yojson _ = failwith "public_key_hash_of_yojson: not implemented"
end

module Public_key = struct
  include Tezos_crypto.Signature.Public_key

  let to_yojson (pk : t) = [%to_yojson: string] (to_b58check pk)
  let of_yojson _ = failwith "public_key_of_yojson: not implemented"
end

module Signature = struct
  include Tezos_crypto.Signature

  let to_yojson (s : t) = [%to_yojson: string] (to_b58check s)
  let of_yojson _ = failwith "signature_of_yojson: not implemented"
end

module Bls12_381_G1 = struct
  include Bls12_381.G1

  let to_yojson (g1 : t) = [%to_yojson: bytes] (to_bytes g1)
  let of_yojson _ = failwith "bls12_381_g1_of_yojson: not implemented"
end

module Bls12_381_G2 = struct
  include Bls12_381.G2

  let to_yojson (g2 : t) = [%to_yojson: bytes] (to_bytes g2)
  let of_yojson _ = failwith "bls12_381_g2_of_yojson: not implemented"
end

module Bls12_381_Fr = struct
  include Bls12_381.Fr

  let to_yojson (fr : t) = [%to_yojson: bytes] (to_bytes fr)
  let of_yojson _ = failwith "bls12_381_fr_of_yojson: not implemented"
end

module Chain_id = struct
  include Tezos_crypto.Hashed.Chain_id

  let to_yojson (c : t) = [%to_yojson: bytes] (to_bytes c)
  let of_yojson _ = failwith "chain_id_of_yojson: not implemented"
end

module Generator = struct
  include QCheck.Gen

  let to_yojson _ _ = `Null
  let of_yojson _ = failwith "generator_of_yojson: not implemented"
end

type mcode = unit Tezos_utils.Michelson.michelson [@@deriving yojson]
type mutation = Location.t * Ast_aggregated.expression * string [@@deriving yojson]

type contract =
  { address : Contract.t
  ; entrypoint : string option
  }
[@@deriving yojson]

type constant_val =
  | C_unit [@name "unit"]
  | C_bool of bool [@name "bool"]
  | C_int of Z.t [@name "int"]
  | C_int64 of Int64.t [@name "int64"]
  | C_nat of Z.t [@name "nat"]
  | C_timestamp of Z.t [@name "timestamp"]
  | C_string of string [@name "string"]
  | C_bytes of bytes [@name "bytes"]
  | C_mutez of Z.t [@name "mutez"]
  | C_address of Contract.t
      (*should be represented as michelson data ? not convenient *)
      [@name "address"]
  | C_contract of contract [@name "contract"]
  | C_key_hash of Public_key_hash.t [@name "key_hash"]
  | C_key of Public_key.t [@name "key"]
  | C_signature of Signature.t [@name "signature"]
  | C_bls12_381_g1 of Bls12_381_G1.t [@name "bls12_381_g1"]
  | C_bls12_381_g2 of Bls12_381_G2.t [@name "bls12_381_g2"]
  | C_bls12_381_fr of Bls12_381_Fr.t [@name "bls12_381_fr"]
  | C_chain_id of Chain_id.t [@name "chain_id"]
[@@deriving yojson]

type micheline_value =
  { code : mcode
  ; code_ty : mcode
  }
[@@deriving yojson]

type typed_michelson_code =
  { micheline_repr : micheline_value
  ; ast_ty : Ast_aggregated.type_expression
  }
[@@deriving yojson]

type michelson_code =
  | Ty_code of typed_michelson_code
  | Untyped_code of mcode
[@@deriving yojson]

type calltrace = Location.t list [@@deriving yojson]
type location = int [@@deriving yojson]

type env_item =
  { item : value_expr
  ; no_mutation : bool
  ; inline : bool
  }

and env = (Value_var.t * env_item) list

and func_val =
  { rec_name : Value_var.t option
  ; orig_lambda : Ast_aggregated.expression
  ; arg_binder : Value_var.t
  ; arg_mut_flag : Param.mutable_flag
  ; body : Ast_aggregated.expression
  ; env : env
  }

and thunk_val =
  { value : Ast_aggregated.expression
  ; context : env
  }

and value_expr =
  { ast_type : Ast_aggregated.type_expression
  ; eval_term : value
  }

and gen =
  { generator : value Generator.t
  ; gen_type : Ast_aggregated.type_expression
  }

and value =
  | V_Ct of constant_val [@name "constant"]
  | V_List of value list [@name "list"]
  | V_Record of value Record.t [@name "record"]
  | V_Map of (value * value) list [@name "map"]
  | V_Set of value list [@name "set"]
  | V_Construct of (string * value) [@name "constructor"]
  | V_Michelson of michelson_code [@name "michelson"]
  | V_Michelson_contract of mcode [@name "contract_code"]
  | V_Ast_contract of
      { main : Ast_aggregated.expression
      ; views : (Value_var.t list * Ast_aggregated.expression) option
      } [@name "ast_contract"]
  | V_Mutation of mutation [@name "mutation"]
  | V_Func_val of func_val [@name "function"]
  | V_Gen of gen [@name "generator"]
  | V_Location of location [@name "location"]
  | V_Typed_address of Contract.t
      (* This is a copy of C_address in constant *) [@name "typed_address"]
  | V_Views of (string * func_val) list
[@@deriving yojson]

type bigmap_state = (value * value) list

type bigmap_data =
  { key_type : Tezos_raw_protocol.Script_repr.expr
  ; value_type : Tezos_raw_protocol.Script_repr.expr
  ; version : bigmap_state
  }

type bigmap = int * bigmap_data
type bigmaps = bigmap list
type toplevel_env = (string * value) list [@@deriving yojson]
