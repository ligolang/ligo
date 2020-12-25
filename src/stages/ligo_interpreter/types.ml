include Ast_typed.Types

module Env = Map.Make(
  struct
    type t = expression_variable
    let compare (a:expression_variable) (b:expression_variable) = Var.compare a.wrap_content b.wrap_content
  end
)

(*TODO temporary hack to handle failwiths *)
exception Temporary_hack of string

module Tez = Proto_alpha_utils.Memory_proto_alpha.Protocol.Alpha_context.Tez
module Timestamp = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp
module Int = Int_repr_copied

type env = value Env.t

and func_val = {
    arg_binder : expression_variable ;
    body : Ast_typed.expression ;
    env : env ;
  }

and constant_val =
  | C_unit
  | C_bool of bool
  | C_int of Int.z Int.num
  | C_nat of Int.n Int.num
  | C_timestamp of Z.t
  | C_mutez of Tez.t
  | C_string of string
  | C_bytes of bytes
  | C_address of string
  | C_signature of string
  | C_key of string
  | C_key_hash of string
  | C_chain_id of string
  | C_operation of operation

and value =
  | V_Func_val of func_val
  | V_Func_rec of (expression_variable * expression_variable * Ast_typed.expression * env)
  | V_Ct of constant_val
  | V_List of value list
  | V_Record of value label_map
  | V_Map of (value * value) list
  | V_Set of value list
  | V_packed of value
  | V_Construct of (string * value)
  | V_Failure of string (*temporary*)

and operation =
  | Internal_transaction of { addr : int ; code : Ast_typed.expression ; env : env ; amount : Tez.t }
  | Create_contract of { addr : int ; code : value ; amount : Tez.t ; storage : value }