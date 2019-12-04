[@@@warning "-30"]

module S = Ast_simplified
include Stage_common.Types

type program = declaration Location.wrap list

and declaration =
  | Declaration_constant of (named_expression * (full_environment * full_environment))
  (* | Macro_declaration of macro_declaration *)

and environment_element_definition =
  | ED_binder
  | ED_declaration of (annotated_expression * free_variables)

and free_variables = expression_variable list

and environment_element = {
  type_value : type_value ;  
  source_environment : full_environment ;
  definition : environment_element_definition ;
}
and environment = (expression_variable * environment_element) list
and type_environment = (type_variable * type_value) list (* SUBST ??? *)
and small_environment = (environment * type_environment)
and full_environment = small_environment List.Ne.t

and annotated_expression = {
  expression : expression ;
  type_annotation : type_value ;        (* SUBST *)
  environment : full_environment ;
  location : Location.t ;
}

and named_expression = {
  name: expression_variable ;
  annotated_expression: ae ;
}

and ae = annotated_expression
and type_value' = type_value type_expression'
and type_value = {
  type_value' : type_value';
  simplified : S.type_expression option ; (* If we have the simplified this AST fragment comes from, it is stored here, for easier untyping. *)
}

(* This is used in E_assign of (named_type_value * access_path * ae).
   In mini_c, we need the type associated with `x` in the assignment
   expression `x.y.z := 42`, so it is stored here. *)
and named_type_value = {
  type_name: expression_variable ;
  type_value : type_value ;
}

(* E_lamba and other expressions are always wrapped as an annotated_expression. *)
and lambda = {
  binder : expression_variable ;
  (* input_type: tv ;
   * output_type: tv ; *)
  body : ae ;
}

and let_in = {
  binder: expression_variable;
  rhs: ae;
  result: ae;
}

and 'a expression' =
  (* Base *)
  | E_literal of literal
  | E_constant of (constant * ('a) list) (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of (('a) * ('a))
  | E_lambda of lambda
  | E_let_in of let_in
  (* Tuple *)
  | E_tuple of ('a) list
  | E_tuple_accessor of (('a) * int) (* Access n'th tuple's element *)
  (* Sum *)
  | E_constructor of (constructor * ('a)) (* For user defined constructors *)
  (* Record *)
  | E_record of ('a) label_map
  | E_record_accessor of (('a) * label)
  (* Data Structures *)
  | E_map of (('a) * ('a)) list
  | E_big_map of (('a) * ('a)) list
  | E_list of ('a) list
  | E_set of ('a) list
  | E_look_up of (('a) * ('a))
  (* Advanced *)
  | E_matching of (('a) * matching_expr)
  (* Replace Statements *)
  | E_sequence of (('a) * ('a))
  | E_loop of (('a) * ('a))
  | E_assign of (named_type_value * access_path * ('a))
 
and expression = ae expression'

and value = annotated_expression (* todo (for refactoring) *)

and matching_expr = (ae,type_value) matching
