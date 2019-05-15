type type_name = string

type type_base =
  | Base_unit
  | Base_bool
  | Base_int | Base_nat | Base_tez
  | Base_string | Base_bytes | Base_address
  | Base_operation

type type_value =
  | T_pair of (type_value * type_value)
  | T_or of type_value * type_value
  | T_function of type_value * type_value
  | T_deep_closure of environment * type_value * type_value
  | T_base of type_base
  | T_map of (type_value * type_value)
  | T_list of type_value
  | T_contract of type_value
  | T_option of type_value

and environment_element = string * type_value

and environment = environment_element list

type environment_wrap = {
  pre_environment : environment ;
  post_environment : environment ;
}

type var_name = string
type fun_name = string

type value =
  | D_unit
  | D_bool of bool
  | D_nat of int
  | D_tez of int
  | D_int of int
  | D_string of string
  | D_bytes of bytes
  | D_pair of value * value
  | D_left of value
  | D_right of value
  | D_some of value
  | D_none
  | D_map of (value * value) list
  | D_list of value list
  (* | `Macro of anon_macro ... The future. *)
  | D_function of anon_function
  | D_operation of Memory_proto_alpha.Alpha_context.packed_internal_operation

and selector = var_name list

and expression' =
  | E_literal of value
  | E_environment_capture of selector
  | E_environment_select of environment
  | E_environment_load of (expression * environment)
  | E_constant of string * expression list
  | E_application of expression * expression
  | E_variable of var_name
  | E_make_empty_map of (type_value * type_value)
  | E_make_empty_list of type_value
  | E_make_none of type_value
  | E_if_bool of expression * expression * expression
  | E_if_none of expression * expression * ((var_name * type_value) * expression)
  | E_if_left of expression * ((var_name * type_value) * expression) * ((var_name * type_value) * expression)
  | E_let_in of ((var_name * type_value) * expression * expression)
  | E_sequence of (expression * expression)
  | E_assignment of (string * [`Left | `Right] list * expression)

and expression = {
  content : expression' ;
  type_value : type_value ;
  is_toplevel : bool ;
}

and assignment = var_name * expression

and statement' =
  | S_environment_select of environment
  | S_environment_load of (expression * environment)
  | S_environment_add of (var_name * type_value)
  | S_declaration of assignment (* First assignment *)
  | S_assignment of assignment
  | S_do of expression
  | S_cond of expression * block * block
  | S_patch of string * [`Left | `Right] list * expression
  | S_if_none of expression * block * ((var_name * type_value) * block)
  | S_while of expression * block

and statement = statement' * environment_wrap

and toplevel_statement = assignment * environment_wrap

and anon_function = {
  binder : string ;
  input : type_value ;
  output : type_value ;
  body : block ;
  result : expression ;
}

and block' = statement list

and block = block' * environment_wrap

and program = toplevel_statement list
