include Ast_typed.Types

module Tez = Proto_alpha_utils.Memory_proto_alpha.Protocol.Alpha_context.Tez
module Timestamp = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp
module Int = Int_repr_copied

type mutation = Location.t * Ast_typed.expression

type env_item =
  | Expression of { name: expression_variable ; item: value_expr }
  | Module of { name: module_variable ; item: env }

and env = env_item list

and func_val = {
    rec_name : expression_variable option ;
    orig_lambda : Ast_typed.expression ;
    arg_binder : expression_variable ;
    body : Ast_typed.expression ;
    env : env ;
  }

and michelson_code =
  | Contract of unit Tezos_utils.Michelson.michelson
  | Ty_code of (unit Tezos_utils.Michelson.michelson * unit Tezos_utils.Michelson.michelson * Ast_typed.type_expression)

and contract =
  { address : Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.t;
    entrypoint: string option }

and constant_val =
  | C_unit
  | C_bool of bool
  | C_int of Int.z Int.num
  | C_nat of Int.n Int.num
  | C_timestamp of Z.t
  | C_string of string
  | C_bytes of bytes
  | C_mutez of Int.n Int.num
  | C_address of Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.t (*should be represented as michelson data ? not convenient *)
  | C_contract of contract
  | C_key_hash of Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.public_key_hash


and micheline_value = (unit, string) Tezos_micheline.Micheline.node *
                        (unit, string) Tezos_micheline.Micheline.node

and value_expr = { ast_type : Ast_typed.type_expression ;
                   eval_term : value }
and value =
  | V_Func_val of func_val
  | V_Ct of constant_val
  | V_List of value list
  | V_Record of value label_map
  | V_Map of (value * value) list
  | V_Set of value list
  | V_Construct of (string * value)
  | V_Michelson of michelson_code
  | V_Ligo of (string * string)
  | V_Mutation of mutation
  | V_Failure of exception_type

and fail_reason = Val of value | Reason of string

and calltrace = Location.t list

and exception_type =
  Object_lang_ex of { location: Location.t ; errors: Tezos_error_monad.TzCore.error list ; calltrace : calltrace }
| Meta_lang_ex of { location : Location.t ; reason : fail_reason ; calltrace : calltrace }

and bootstrap_contract =
  int * unit Tezos_utils.Michelson.michelson * unit Tezos_utils.Michelson.michelson * Ast_typed.type_expression * Ast_typed.type_expression

type bigmap_state = (value * value) list
type bigmap_data = {
      key_type : Tezos_raw_protocol_009_PsFLoren.Script_repr.expr;
      value_type : Tezos_raw_protocol_009_PsFLoren.Script_repr.expr;
      version : bigmap_state }
type bigmap = int * bigmap_data
