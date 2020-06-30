include Ast_typed.Types

(*types*)
module Env = Map.Make(
  struct
    type t = expression_variable
    let compare (a:expression_variable) (b:expression_variable) = Var.compare a.wrap_content b.wrap_content
  end
)

(*TODO temporary hack to handle failwiths *)
exception Temporary_hack of string

type env = value Env.t

and constant_val =
  | C_unit
  | C_bool of bool
  | C_int of Z.t
  | C_nat of Z.t
  | C_timestamp of Z.t
  | C_mutez of Z.t
  | C_string of string
  | C_bytes of bytes
  | C_address of string
  | C_signature of string
  | C_key of string
  | C_key_hash of string
  | C_chain_id of string
  | C_operation of Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation

and value =
  | V_Func_val of (expression_variable * Ast_typed.expression * env)
  | V_Func_rec of (expression_variable * expression_variable * Ast_typed.expression * env)
  | V_Ct of constant_val
  | V_List of value list
  | V_Record of value label_map
  | V_Map of (value * value) list
  | V_Set of value list
  | V_Construct of (string * value)
  | V_Failure of string (*temporary*)
