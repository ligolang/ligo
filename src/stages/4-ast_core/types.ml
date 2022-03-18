[@@@warning "-30-32"]
module Ligo_string = Simple_utils.Ligo_string

include Stage_common.Types

type sugar_type_expression_option = Ast_sugar.type_expression option
type sugar_expression_option = Ast_sugar.expression option

type string_option = string option

type type_attribute = {
  public: bool;
}

and module_attribute = {
  public: bool;
}

and module_ = declaration Location.wrap list

and declaration_constant = {
  binder : ty_expr binder;
  attr : known_attributes ;
  expr : expression ;
}

and declaration_type = {
  type_binder : type_variable ;
  type_expr   : ty_expr ;
  type_attr   : type_attribute
}

and declaration_module = {
  module_binder : module_variable ;
  module_       : module_ ;
  module_attr   : module_attribute
}
and declaration =
  | Declaration_type     of declaration_type
  (* A Declaration_constant is described by
   *   a name
   *   an optional type annotation
   *   a boolean indicating whether it should be inlined
   *   an expression *)
  | Declaration_constant of declaration_constant
  | Declaration_module   of declaration_module
  | Module_alias         of module_alias

and type_content =
  | T_variable        of type_variable
  | T_sum             of rows
  | T_record          of rows
  | T_arrow           of ty_expr arrow
  | T_app             of ty_expr type_app
  | T_module_accessor of ty_expr module_access
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
  | E_mod_alias  of expr mod_alias
  | E_raw_code of expr raw_code
  | E_constructor of expr constructor
  | E_matching of matching_expr
  | E_record of expression_label_map
  | E_record_accessor of expr record_accessor
  | E_record_update   of expr record_update
  | E_ascription      of (expr,ty_expr) ascription
  | E_module_accessor of expr module_access

and type_expression_option = type_expression option

and let_in = {
    let_binder: ty_expr binder ;
    rhs: expression ;
    let_result: expression ;
    attr: known_attributes ;
  }

and mod_in = {
  module_binder: module_variable ;
  rhs          : module_ ;
  let_result   : expression ;
}

and module' = declaration location_wrap list

and matching_expr = (expr, ty_expr) match_exp

(* Env types*)

and environment_element_definition =
  | ED_binder
  | ED_declaration of environment_element_definition_declaration

and environment_element_definition_declaration = {
    expression: expression ;
    free_variables: free_variables ;
  }

and free_variables = expression_variable list

and environment_element = {
    type_value: type_expression ;
    definition: environment_element_definition ;
  }

and expression_environment = environment_binding list

and environment_binding = {
    expr_var: expression_variable ;
    env_elt: environment_element ;
  }

and type_environment = type_environment_binding list

and type_environment_binding = {
    type_variable: type_variable ;
    type_: type_expression ;
  }

and module_environment = module_environment_binding list

and module_environment_binding = {
  module_variable : module_variable ;
  module_ : environment ;
}
and environment = {
  expression_environment: expression_environment ;
  type_environment: type_environment ;
  module_environment : module_environment ;
  }
