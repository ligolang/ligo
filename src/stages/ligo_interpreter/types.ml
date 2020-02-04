include Stage_common.Types

(*types*)
module Env = Map.Make(
  struct
    type t = expression_variable
    let compare a b = Var.compare a b
  end
)

(*TODO temporary hack to handle failwiths *)
exception Temprorary_hack of string

type env = value Env.t

and constant_val =
  | C_unit
  | C_bool of bool
  | C_int of int
  | C_nat of int
  | C_timestamp of int
  | C_mutez of int
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
  | V_Ct of constant_val
  | V_List of value list
  | V_Record of value label_map
  | V_Map of (value * value) list
  | V_Set of value list
  | V_Construct of (string * value)
  | V_Failure of string (*temporary*)
