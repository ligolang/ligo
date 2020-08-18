[@@@warning "-30"]

open Types_utils
include Stage_common.Enums (*@ follow ../common/enums.ml *)

type string_option = string option

type attribute = {
  inline: bool ;
} 
type program_loc = declaration location_wrap
and program = program_loc list
and binder = { 
  var : expression_variable ;
  ty : type_expression ;
  }
and declaration_type = {
    type_binder : type_variable ;
    type_expr : type_expression ;
  }
and declaration_constant = {
    binder : binder;
    attr : attribute ;
    expr : expression ;
  }
and declaration =
  | Declaration_type of declaration_type
  (* A Declaration_constant is described by
   *   a name
   *   an optional type annotation
   *   a boolean indicating whether it should be inlined
   *   an expression *)
  | Declaration_constant of declaration_constant
(* | Macro_declaration of macro_declaration *)

and field_label_map = row_element label_map
and type_expression_list = type_expression list

and content_type_constant = {
    type_constant : type_constant ;
    arguments : type_expression_list ;
  }
and type_content =
  | T_sum of field_label_map
  | T_record of field_label_map
  | T_arrow of arrow
  | T_variable of type_variable
  (* TODO: remove this when we remove the old typer *)
  | T_wildcard
  | T_constant of content_type_constant

and arrow = {
    type1: type_expression ;
    type2: type_expression ;
  }
and row_element = {
    associated_type : type_expression ;
    michelson_annotation : string_option ;
    decl_pos : int ;
  }

and type_expression = {
  content  : type_content ;
  sugar    : sugar_type_expression_option ;
  location : location ;
  }

and expression = {
  content  : expression_content ;
  sugar    : sugar_expression_option ;
  location : location ;
  }

and expression_label_map = expression label_map
and expression_content =
  | E_literal of literal
  | E_constant of constant
  | E_variable of expression_variable
  | E_application of application
  | E_lambda of lambda
  | E_recursive of recursive
  | E_let_in of let_in
  | E_raw_code of raw_code
  | E_constructor of constructor
  | E_matching of matching
  | E_record of expression_label_map
  | E_record_accessor of record_accessor
  | E_record_update   of record_update
  | E_ascription of ascription

and expression_list = expression list
and constant = {
    cons_name: constant' ;
    arguments: expression_list ;
  }

and application = {
    lamb: expression ;
    args: expression ;
  }


and lambda = {
    binder: binder ;
    result: expression ;
  }

and recursive = {
    fun_name : expression_variable ;
    fun_type : type_expression ;
    lambda : lambda ;
  }
 
and let_in = {
    let_binder: binder ;
    rhs: expression ;
    let_result: expression ;
    inline: bool ;
  }

and raw_code = { 
  language : string ;
  code : expression ;
  }

and constructor = {
    constructor: label ;
    element: expression ;
  }

and record_accessor = {
    record: expression ;
    path: label ;
  }
and record_update = {
    record: expression ;
    path: label ;
    update: expression ;
  }
and match_cons = {
    hd : expression_variable ;
    tl : expression_variable ;
    body : expression ;
  }
and match_list = {
    match_nil  : expression ;
    match_cons : match_cons ;
  }
and match_some = {
    opt : expression_variable ;
    body : expression ;
  }
and match_option = {
    match_none : expression ;
    match_some : match_some ;
  }
and match_variant = {
    constructor : label ;
    proj : expression_variable ;
    body : expression ;
  }
and match_variant_list = match_variant list
and matching_expr =
  | Match_list of match_list
  | Match_option of match_option
  | Match_variant of match_variant_list

and matching = {
    matchee: expression ;
    cases: matching_expr ;
  }

and ascription = {
    anno_expr: expression ;
    type_annotation: type_expression ;
  }

and env_def_declaration = {
  expr : expression ;
  free_variables : free_variables ;
}
and environment_element_definition =
  | ED_binder
  | ED_declaration of env_def_declaration

and free_variables = expression_variable list

and environment_element = {
    type_value: type_expression ;
    source_environment: environment ;
    definition: environment_element_definition ;
  }

and expr_env_binding = {
    binder : expression_variable ;
    element : environment_element ;
  }

and expr_environment = expr_env_binding list

and type_env_binding = {
    binder : type_variable ;
    element : type_expression ;
}
and type_environment = type_env_binding list

and environment = {
  expr_environment : expr_environment ;
  type_environment : type_environment ;
}
