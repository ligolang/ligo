[@@@warning "-30"]
module Map = Simple_utils.Map
module Location = Simple_utils.Location

type name = string
type type_name = string
type constructor_name = string

type 'a name_map = 'a Map.String.t
type 'a type_name_map = 'a Map.String.t

type program = declaration Location.wrap list

and declaration =
  | Declaration_type of (type_name * type_expression)
  | Declaration_constant of (name * type_expression option * expression)
  (* | Macro_declaration of macro_declaration *)

and expr = expression
and te = type_expression
and te_map = type_expression type_name_map
and expr_map = expression name_map

and type_expression =
  | T_tuple of te list
  | T_sum of te_map
  | T_record of te_map
  | T_function of te * te
  | T_variable of type_name
  | T_constant of type_name * te list

and lambda = {
  binder : (name * type_expression option) ;
  input_type : type_expression option ;
  output_type : type_expression option ;
  result : expr ;
}

and let_in = {
  binder : (name * type_expression option) ;
  rhs    : expr ;
  result : expr ;
}

and expression' =
  (* Base *)
  | E_literal of literal
  | E_constant of (name * expr list) (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of name
  | E_lambda of lambda
  | E_application of (expr * expr)
  | E_let_in of let_in
  (* E_Tuple *)
  | E_tuple of expr list
  (* Sum *)
  | E_constructor of (name * expr) (* For user defined constructors *)
  (* E_record *)
  | E_record of expr_map
  (* TODO: Change it to (expr * access) *)
  | E_accessor of (expr * access_path)
  (* Data Structures *)
  | E_map of (expr * expr) list
  | E_big_map of (expr * expr) list
  | E_list of expr list
  | E_set of expr list
  | E_look_up of (expr * expr)
  (* Matching *)
  | E_matching of (expr * matching_expr)
  (* Replace Statements *)
  | E_sequence of (expr * expr)
  | E_loop of (expr * expr)
  | E_assign of (name * access_path * expr)
  | E_skip
  (* Annotate *)
  | E_annotation of expr * type_expression

and expression = {
  expression : expression' ;
  location : Location.t ;
}

and access =
  | Access_tuple of int
  | Access_record of string

and access_path = access list

and literal =
  | Literal_unit
  | Literal_bool of bool
  | Literal_int of int
  | Literal_nat of int
  | Literal_mutez of int
  | Literal_string of string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_timestamp of int
  | Literal_operation of Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation

and 'a matching =
  | Match_bool of {
      match_true : 'a ;
      match_false : 'a ;
    }
  | Match_list of {
      match_nil : 'a ;
      match_cons : name * name * 'a ;
    }
  | Match_option of {
      match_none : 'a ;
      match_some : name * 'a ;
    }
  | Match_tuple of name list * 'a
  | Match_variant of ((constructor_name * name) * 'a) list

and matching_expr = expression matching
