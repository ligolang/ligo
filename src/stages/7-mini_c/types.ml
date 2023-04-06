open Ligo_prim
module Location = Simple_utils.Location

type 'a annotated = string option * 'a

type type_content =
  | T_tuple of type_expression annotated list
  | T_or of (type_expression annotated * type_expression annotated)
  | T_function of (type_expression * type_expression)
  | T_base of type_base
  | T_map of (type_expression * type_expression)
  | T_big_map of (type_expression * type_expression)
  | T_list of type_expression
  | T_set of type_expression
  | T_contract of type_expression
  | T_ticket of type_expression
  | T_sapling_state of Z.t
  | T_sapling_transaction of Z.t
  | T_option of type_expression

and type_expression =
  { type_content : type_content
  ; location : Location.t
  ; source_type : Ast_typed.type_expression option
  }

and type_base =
  | TB_unit
  | TB_bool
  | TB_string
  | TB_bytes
  | TB_nat
  | TB_int
  | TB_mutez
  | TB_operation
  | TB_address
  | TB_key
  | TB_key_hash
  | TB_chain_id
  | TB_signature
  | TB_timestamp
  | TB_baker_hash
  | TB_pvss_key
  | TB_baker_operation
  | TB_bls12_381_g1
  | TB_bls12_381_g2
  | TB_bls12_381_fr
  | TB_never
  | TB_tx_rollup_l2_address
  | TB_type_int of Z.t

and environment_element = Value_var.t * type_expression
and environment = environment_element list
and var_name = Value_var.t
and fun_name = Value_var.t

type inline = bool

type value =
  | D_unit
  | D_bool of bool
  | D_nat of Z.t
  | D_timestamp of Z.t
  | D_mutez of Z.t
  | D_int of Z.t
  | D_string of string
  | D_bytes of bytes
  | D_pair of value * value
  | D_left of value
  | D_right of value
  | D_some of value
  | D_none
  | D_map of (value * value) list
  | D_big_map of (value * value) list
  | D_ticket of (value * value)
  | D_list of value list
  | D_set of value list
  (* | `Macro of anon_macro ... The future. *)
  | D_operation of bytes

and selector = var_name list
and binder = var_name * type_expression

and expression_content =
  | E_literal of Literal_value.t
  | E_closure of anon_function
  | E_rec of rec_function
  | E_constant of constant
  | E_application of (expression * expression)
  | E_variable of var_name
  | E_iterator of Constant.constant' * (binder * expression) * expression
  | E_fold of ((binder * expression) * expression * expression)
  | E_fold_right of ((binder * expression) * (expression * type_expression) * expression)
  | E_if_bool of (expression * expression * expression)
  | E_if_none of expression * expression * (binder * expression)
  | E_if_cons of expression * expression * ((binder * binder) * expression)
  | E_if_left of expression * (binder * expression) * (binder * expression)
  | E_let_in of expression * inline * (binder * expression)
  | E_tuple of expression list
  | E_let_tuple of expression * (binder list * expression)
  (* E_proj (record, index, field_count): we use the field_count to
     know whether the index is the last field or not, since Michelson
     treats the last element of a comb differently than the rest. We
     could alternatively put `unit` at the end of all our combs, but
     that would break compatibility and is not a standard Michelson
     convention... *)
  | E_proj of expression * int * int
  (* E_update (record, index, update, field_count): field_count as for E_proj *)
  | E_update of expression * int * expression * int
  | E_raw_michelson of
      ((Location.t, string) Tezos_micheline.Micheline.node list * expression list)
  (* E_global_constant (hash, args) *)
  | E_global_constant of string * expression list
  | E_create_contract of
      type_expression * type_expression * (binder * expression) * expression list
  (* Mutability stuff *)
  | E_let_mut_in of expression * (binder * expression)
  | E_deref of var_name
  | E_assign of var_name * expression
  | E_for_each of expression * type_expression * (binder list * expression)
  | E_for of expression * expression * expression * (binder * expression)
  (* (start, final, incr, (binder, body)) *)
  | E_while of expression * expression

and expression =
  { content : expression_content
  ; type_expression : type_expression
  ; location : Location.t
  }

and constant =
  { cons_name : Constant.constant'
  ; arguments : expression list
  }

and anon_function =
  { binder : Value_var.t
  ; body : expression
  }

and rec_function =
  { func : anon_function
  ; rec_binder : Value_var.t
  }

(* backend expression metadata *)
type binder_meta =
  { location : Location.t
  ; name : string option
  ; source_type : Ast_typed.type_expression option
  }

type meta =
  { location : Location.t
  ; (* source location on any node *)
    env : binder_meta option list
  ; (* environment descriptor on special environment Seq nodes *)
    binder : binder_meta option
  ; (* binder descriptor on the translated type of binders (since
       backend environments are lists of types) *)
    source_type : Ast_typed.type_expression option
  }

let dummy_meta : meta =
  { location = Location.dummy; env = []; binder = None; source_type = None }
