[@@@warning "-30"]
include Stage_common.Types

type type_content =
  | T_variable        of type_variable
  | T_sum             of ty_expr rows
  | T_record          of ty_expr rows
  | T_tuple           of ty_expr list
  | T_arrow           of ty_expr arrow
  | T_app             of ty_expr type_app
  | T_module_accessor of type_variable module_access
  | T_singleton       of literal
  | T_abstraction     of ty_expr abstraction
  | T_for_all         of ty_expr abstraction

and type_expression = {type_content: type_content; location: Location.t}
and ty_expr = type_expression


type declaration = (expr,ty_expr,attributes,attributes,attributes) declaration'
and declarations = (expr,ty_expr,attributes,attributes,attributes) declarations'
and declaration_content = (expr,ty_expr,attributes,attributes,attributes) declaration_content'
and module_expr = (expr,ty_expr,attributes,attributes,attributes) module_expr'
and module_ = declarations

(* | Macro_declaration of macro_declaration *)
and expression = {expression_content: expression_content; location: Location.t}
and expr = expression

and expression_content =
  (* Base *)
  | E_literal of literal
  | E_constant of expr constant (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of expr application
  | E_lambda of (expr, ty_expr) lambda
  | E_type_abstraction of expr type_abs
  | E_recursive of (expr, ty_expr) recursive
  | E_let_in of let_in
  | E_type_in of (expr, ty_expr) type_in
  | E_mod_in of (expr, ty_expr,attributes,attributes,attributes) mod_in'
  | E_raw_code  of expr raw_code
  (* Variant *)
  | E_constructor of expr constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression label_map
  | E_accessor of expr accessor
  | E_update   of expr update
  (* Advanced *)
  | E_ascription of (expr, ty_expr) ascription
  | E_module_accessor of expression_variable module_access
  (* Sugar *)
  | E_cond of expr conditional
  | E_sequence of expr sequence
  | E_skip
  | E_tuple of expression list
  (* Data Structures *)
  | E_map of (expression * expression) list
  | E_big_map of (expression * expression) list
  | E_list of expression list
  | E_set of expression list
  (* Imperative *)
  | E_assign   of (expr,ty_expr) assign


and let_in = {
  let_binder: ty_expr binder;
  rhs: expression ;
  let_result: expression ;
  attributes : attributes ;
  mut: bool;
}

and matching = (expression , type_expression) match_exp

and program = declaration list

