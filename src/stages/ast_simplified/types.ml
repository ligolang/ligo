[@@@warning "-30"]
module Location = Simple_utils.Location
include Stage_common.Types

type program = declaration Location.wrap list

and inline = bool

and type_expression =  {
  type_expression' : type_expression type_expression'
  }
and declaration =
  | Declaration_type of (type_variable * type_expression)
  | Declaration_constant of (expression_variable * type_expression option * inline * expression)
  (* | Macro_declaration of macro_declaration *)

and expr = expression

and lambda = {
  binder : (expression_variable * type_expression option) ;
  input_type : type_expression option ;
  output_type : type_expression option ;
  result : expr ;
}

and let_in = {
  binder     : (expression_variable * type_expression option) ;
  rhs        : expr ;
  result     : expr ;
  inline     : inline;
}

and expression' =
  (* Base *)
  | E_literal of literal
  | E_constant of (constant * expr list) (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_lambda of lambda
  | E_application of (expr * expr)
  | E_let_in of let_in
  (* E_Tuple *)
  | E_tuple of expr list
  (* Sum *)
  | E_constructor of (constructor * expr) (* For user defined constructors *)
  (* E_record *)
  | E_record of expr label_map
  (* TODO: Change it to (expr * access) *)
  | E_accessor of (expr * access_path)
  | E_update of update
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
  | E_assign of (expression_variable * access_path * expr)
  | E_skip
  (* Annotate *)
  | E_ascription of expr * type_expression

and expression = {
  expression : expression' ;
  location : Location.t ;
}
and update = { record: expr; update: (label *expr) }

and matching_expr = (expr,unit) matching
