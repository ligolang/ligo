include Ast_aggregated.Types
open Ligo_prim

module Tezos_protocol = Tezos_protocol_014_PtKathma
module Tezos_raw_protocol = Tezos_raw_protocol_014_PtKathma

module Tez = Proto_alpha_utils.Memory_proto_alpha.Protocol.Alpha_context.Tez
module Timestamp = Memory_proto_alpha.Protocol.Alpha_context.Timestamp

type mcode = unit Tezos_utils.Michelson.michelson
type mcontract = Tezos_protocol.Protocol.Alpha_context.Contract.t

type mutation = Location.t * Ast_aggregated.expression * string

type env_item = {
    item: value_expr ;
    no_mutation : bool ;
    inline : bool
  }

and env = (Value_var.t * env_item) list

and func_val = {
    rec_name : Value_var.t option ;
    orig_lambda : Ast_aggregated.expression ;
    arg_binder : Value_var.t ;
    body : Ast_aggregated.expression ;
    env : env ;
  }

and thunk_val = {
    value : Ast_aggregated.expression ;
    context : env ;
  }

and typed_michelson_code = { code_ty : mcode ; code : mcode; ast_ty : Ast_aggregated.type_expression }

and michelson_code =
  | Ty_code of typed_michelson_code
  | Untyped_code of mcode

and contract =
  { address : mcontract;
    entrypoint: string option }

and constant_val =
  | C_unit
  | C_bool of bool
  | C_int of Z.t
  | C_int64 of Int64.t
  | C_nat of Z.t
  | C_timestamp of Z.t
  | C_string of string
  | C_bytes of bytes
  | C_mutez of Z.t
  | C_address of mcontract (*should be represented as michelson data ? not convenient *)
  | C_contract of contract
  | C_key_hash of Tezos_protocol.Protocol.Alpha_context.public_key_hash
  | C_key of Tezos_protocol.Protocol.Alpha_context.public_key
  | C_signature of Tezos_protocol.Protocol.Alpha_context.signature
  | C_bls12_381_g1 of Bls12_381.G1.t
  | C_bls12_381_g2 of Bls12_381.G2.t
  | C_bls12_381_fr of Bls12_381.Fr.t
  | C_chain_id of string

and micheline_value = (unit, string) Tezos_micheline.Micheline.node *
                        (unit, string) Tezos_micheline.Micheline.node

and value_expr = { ast_type : Ast_aggregated.type_expression ;
                   eval_term : value }

and gen = { generator : value QCheck.Gen.t ;
            gen_type : Ast_aggregated.type_expression }

and value =
  | V_Ct of constant_val
  | V_List of value list
  | V_Record of value Record.t
  | V_Map of (value * value) list
  | V_Set of value list
  | V_Construct of (string * value)
  | V_Michelson of michelson_code
  | V_Michelson_contract of mcode
  | V_Ast_contract of { main: Ast_aggregated.expression ; views: (Value_var.t list * Ast_aggregated.expression) option }
  | V_Mutation of mutation
  | V_Func_val of func_val
  | V_Gen of gen

and calltrace = Location.t list

type bigmap_state = (value * value) list
type bigmap_data = {
      key_type : Tezos_raw_protocol.Script_repr.expr;
      value_type : Tezos_raw_protocol.Script_repr.expr;
      version : bigmap_state }
type bigmap = int * bigmap_data
type bigmaps = bigmap list
