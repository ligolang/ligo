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
  | Declaration_type of named_type_expression
  | Declaration_constant of named_expression
  (* | Macro_declaration of macro_declaration *)

and value = annotated_expression

and annotated_expression = {
  expression: expression ;
  type_annotation: te option ;
}

and named_expression = {
  name: name ;
  annotated_expression: ae ;
}

and named_type_expression = {
  type_name: type_name ;
  type_expression: type_expression ;
}

and te = type_expression
and ae = annotated_expression
and te_map = type_expression type_name_map
and ae_map = annotated_expression name_map

and type_expression =
  | T_tuple of te list
  | T_sum of te_map
  | T_record of te_map
  | T_function of te * te
  | T_variable of type_name
  | T_constant of type_name * te list

and lambda = {
  binder : name ;
  input_type : type_expression option ;
  output_type : type_expression option ;
  result : ae ;
}

and let_in = {
  binder : name;
  rhs    : ae;
  result : ae;
}

and expression =
  (* Base *)
  | E_literal of literal
  | E_constant of (name * ae list) (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of name
  | E_lambda of lambda
  | E_application of (ae * ae)
  | E_let_in of let_in
  (* E_Tuple *)
  | E_tuple of ae list
  (* Sum *)
  | E_constructor of (name * ae) (* For user defined constructors *)
  (* E_record *)
  | E_record of ae_map
  | E_accessor of (ae * access_path)
  (* Data Structures *)
  | E_map of (ae * ae) list
  | E_list of ae list
  | E_look_up of (ae * ae)
  (* Matching *)
  | E_matching of (ae * matching_expr)
  | E_failwith of ae
  (* Replace Statements *)
  | E_sequence of (ae * ae)
  | E_loop of (ae * ae)
  | E_assign of (name * access_path * ae)
  | E_skip

and access =
  | Access_tuple of int
  | Access_record of string
  | Access_map of ae

and access_path = access list

and literal =
  | Literal_unit
  | Literal_bool of bool
  | Literal_int of int
  | Literal_nat of int
  | Literal_tez of int
  | Literal_string of string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_operation of Memory_proto_alpha.Alpha_context.packed_internal_operation

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

and matching_expr = annotated_expression matching
