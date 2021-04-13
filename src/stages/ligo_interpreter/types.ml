include Ast_typed.Types

module Env = Map.Make(
  struct
    type t = expression_variable
    let compare (a:expression_variable) (b:expression_variable) = Var.compare a.wrap_content b.wrap_content
  end
)

module Tez = Proto_alpha_utils.Memory_proto_alpha.Protocol.Alpha_context.Tez
module Timestamp = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp
module Int = Int_repr_copied

type env = value Env.t

and func_val = {
    orig_lambda : Ast_typed.expression ;
    arg_binder : expression_variable ;
    body : Ast_typed.expression ;
    env : env ;
  }

and michelson_code =
  | Contract of unit Tezos_utils.Michelson.michelson
  | Ty_code of (unit Tezos_utils.Michelson.michelson * unit Tezos_utils.Michelson.michelson * Ast_typed.type_expression)

and constant_val =
  | C_unit
  | C_bool of bool
  | C_int of Int.z Int.num
  | C_nat of Int.n Int.num
  | C_timestamp of Z.t
  | C_string of string
  | C_bytes of bytes
  | C_address of Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.t (*should be represented as michelson data ? not convenient *)

and value =
  | V_Func_val of func_val
  | V_Func_rec of (expression_variable * expression_variable * Ast_typed.expression * env)
  | V_Ct of constant_val
  | V_List of value list
  | V_Record of value label_map
  | V_Map of (value * value) list
  | V_Set of value list
  | V_Construct of (string * value)
  | V_Michelson of michelson_code
  | V_Ligo of (string * string)
