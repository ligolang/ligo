[@@@warning "-30-32"]
module Ligo_string = Simple_utils.Ligo_string

include Stage_common.Types

type sugar_type_expression_option = Ast_sugar.type_expression option
type sugar_expression_option = Ast_sugar.expression option

type string_option = string option

type type_attribute = { public: bool }
and module_attribute = type_attribute

and module_              = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declarations'
and declaration          = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration'
and declaration_content  = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration_content'
and declaration_module   = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration_module'
and declaration_constant = (expression , ty_expr , known_attributes) declaration_constant'
and declaration_type     = (ty_expr , type_attribute) declaration_type'

and type_content =
  | T_variable        of type_variable
  | T_sum             of rows
  | T_record          of rows
  | T_arrow           of ty_expr arrow
  | T_app             of ty_expr type_app
  | T_module_accessor of type_variable module_access
  | T_singleton       of literal
  | T_abstraction     of ty_expr abstraction
  | T_for_all         of ty_expr abstraction

and rows = { fields : row_element label_map ; layout : layout option }

and row_element = ty_expr row_element_mini_c

and type_expression = {
  type_content  : type_content ;
  sugar    : sugar_type_expression_option ;
  location : location ;
  }
and ty_expr = type_expression

and expression = {
  expression_content  : expression_content ;
  sugar    : sugar_expression_option ;
  location : location ;
  }
and expr = expression

and expression_label_map = expression label_map
and expression_content =
  | E_literal of literal
  | E_constant of expr constant
  | E_variable of expression_variable
  | E_application of expr application
  | E_lambda    of (expr, ty_expr) lambda
  | E_type_abstraction of expr type_abs
  | E_recursive of (expr, ty_expr) recursive
  | E_let_in    of let_in
  | E_type_in of (expr, ty_expr) type_in
  | E_mod_in  of mod_in
  | E_raw_code of expr raw_code
  | E_constructor of expr constructor
  | E_matching of matching_expr
  | E_record of expression_label_map
  | E_record_accessor of expr record_accessor
  | E_record_update   of expr record_update
  | E_ascription      of (expr,ty_expr) ascription
  | E_module_accessor of expression_variable module_access

and type_expression_option = type_expression option

and mod_in = (expression,ty_expr,known_attributes,type_attribute,module_attribute) mod_in'
and module_expr = (expression,ty_expr,known_attributes,type_attribute,module_attribute) module_expr'

and let_in = {
    let_binder: ty_expr binder ;
    rhs: expression ;
    let_result: expression ;
    attr: known_attributes ;
  }

and matching_expr = (expr, ty_expr) match_exp
