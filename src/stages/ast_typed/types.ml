[@@@warning "-30"]

module S = Ast_simplified

module SMap = Map.String

type name = string
type type_name = Type_name of string
type constructor_name = string

type 'a name_map = 'a SMap.t
type 'a type_name_map = 'a SMap.t

type program = declaration Location.wrap list

and declaration =
  | Declaration_constant of (named_expression * (full_environment * full_environment))
  (* | Macro_declaration of macro_declaration *)

and environment_element_definition =
  | ED_binder
  | ED_declaration of (annotated_expression * free_variables)

and free_variables = name list

and environment_element = {
  type_value : type_value ;     (* SUBST ??? *)
  source_environment : full_environment ;
  definition : environment_element_definition ;
}
and environment = (string * environment_element) list
and type_environment = (string * type_value) list (* SUBST ??? *)
and small_environment = (environment * type_environment)
and full_environment = small_environment List.Ne.t

and annotated_expression = {
  expression : expression ;
  type_annotation : tv ;        (* SUBST *)
  environment : full_environment ;
  location : Location.t ;
}

and named_expression = {
  name: name ;
  annotated_expression: ae ;
}

and tv = type_value
and ae = annotated_expression
and tv_map = type_value type_name_map
and ae_map = annotated_expression name_map

and type_value' =
  | T_tuple of tv list
  | T_sum of tv_map
  | T_record of tv_map
  | T_constant of type_name * tv list (* SUBST ??? I think not, at least not necessary for now and the types don't match *)
  | T_variable of type_name     (* SUBST *)
  | T_function of (tv * tv)

and type_value = {
  type_value' : type_value' ;
  simplified : S.type_expression option ; (* If we have the simplified this AST fragment comes from, it is stored here, for easier untyping. *)
}

(* This is used in E_assign of (named_type_value * access_path * ae).
   In mini_c, we need the type associated with `x` in the assignment
   expression `x.y.z := 42`, so it is stored here. *)
and named_type_value = {
  type_name: name ;
  type_value : type_value ;
}

(* E_lamba and other expressions are always wrapped as an annotated_expression. *)
and lambda = {
  binder : name ;
  (* input_type: tv ;
   * output_type: tv ; *)
  body : ae ;
}

and let_in = {
  binder: name;
  rhs: ae;
  result: ae;
}

and expression =
  (* Base *)
  | E_literal of literal
  | E_constant of (name * ae list) (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of name
  | E_application of (ae * ae)
  | E_lambda of lambda
  | E_let_in of let_in
  (* Tuple *)
  | E_tuple of ae list
  | E_tuple_accessor of (ae * int) (* Access n'th tuple's element *)
  (* Sum *)
  | E_constructor of (name * ae) (* For user defined constructors *)
  (* Record *)
  | E_record of ae_map
  | E_record_accessor of (ae * string)
  (* Data Structures *)
  | E_map of (ae * ae) list
  | E_big_map of (ae * ae) list
  | E_list of ae list
  | E_set of ae list
  | E_look_up of (ae * ae)
  (* Advanced *)
  | E_matching of (ae * matching_expr)
  (* Replace Statements *)
  | E_sequence of (ae * ae)
  | E_loop of (ae * ae)
  | E_assign of (named_type_value * access_path * ae)

and value = annotated_expression (* todo (for refactoring) *)

and literal =
  | Literal_unit
  | Literal_bool of bool
  | Literal_int of int
  | Literal_nat of int
  | Literal_timestamp of int
  | Literal_mutez of int
  | Literal_string of string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_operation of Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation

and access =
  | Access_tuple of int
  | Access_record of string

and access_path = access list

and 'a matching =
  | Match_bool of {
      match_true : 'a ;
      match_false : 'a ;
    }
  | Match_list of {
      match_nil : 'a ;
      match_cons : ((name * type_value) * (name * type_value)) * 'a ;
    }
  | Match_option of {
      match_none : 'a ;
      match_some : (name * type_value) * 'a ;
    }
  | Match_tuple of (name list * 'a)
  | Match_variant of (((constructor_name * name) * 'a) list * type_value)

and matching_expr = ae matching
