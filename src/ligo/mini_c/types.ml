module Append_tree = Tree.Append

type type_name = string

type type_base =
  | Base_unit
  | Base_bool
  | Base_int | Base_nat
  | Base_string | Base_bytes
  | Base_operation

type type_value =
  | T_pair of (type_value * type_value)
  | T_or of type_value * type_value
  | T_function of type_value * type_value
  | T_deep_closure of environment_small * type_value * type_value
  | T_shallow_closure of environment * type_value * type_value
  | T_base of type_base
  | T_map of (type_value * type_value)
  | T_list of type_value
  | T_option of type_value


and environment_element = string * type_value

and environment_small' = environment_element Append_tree.t'

and environment_small = environment_element Append_tree.t

and environment = environment_small list

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

and expression' =
  | E_literal of value
  | E_function of anon_function_expression
  | E_constant of string * expression list
  | E_application of expression * expression
  | E_variable of var_name
  | E_empty_map of (type_value * type_value)
  | E_empty_list of type_value
  | E_make_none of type_value
  | E_Cond of expression * expression * expression

and expression = expression' * type_value * environment (* Environment in which the expressions are evaluated *)

and assignment = var_name * expression

and statement' =
  | S_environment_extend
  | S_environment_restrict
  | S_declaration of assignment (* First assignment *)
  | S_assignment of assignment
  | S_cond of expression * block * block
  | S_patch of string * [`Left | `Right] list * expression
  | S_if_none of expression * block * (var_name * block)
  | S_while of expression * block

and statement = statement' * environment_wrap

and toplevel_statement = assignment * environment_wrap

and anon_function_content = {
  binder : string ;
  input : type_value ;
  output : type_value ;
  body : block ;
  result : expression ;
  capture_type : capture ;
}

and anon_function = {
  content : anon_function_content ;
  capture : value option ;
}

and anon_function_expression = anon_function_content

and capture =
  | No_capture (* For functions that don't capture their environments. Quotes. *)
  | Shallow_capture of environment (* Duplicates the whole environment. A single DUP. Heavier GETs and SETs at use. *)
  | Deep_capture of environment_small (* Retrieves only the values it needs. Multiple SETs on init. Lighter GETs and SETs at use. *)

and block' = statement list

and block = block' * environment_wrap

and program = toplevel_statement list
