[@@@warning "-30-32"]

include Stage_common.Types

type ast_core_type_expression = Ast_core.type_expression

type te_lmap = row_element label_map
and type_meta = ast_core_type_expression option

and type_content =
  | T_variable of type_variable
  | T_constant of type_injection
  | T_sum      of rows
  | T_record   of rows
  | T_arrow    of ty_expr arrow
  | T_singleton of literal
  | T_abstraction of ty_expr abstraction
  | T_for_all of ty_expr abstraction

and type_injection = {
  language : string ;
  injection : Stage_common.Constant.t ;
  parameters : ty_expr list ;
}

and rows = {
  content : row_element label_map;
  layout : layout ;
}

and te_list = type_expression list

and annot_option = string option

and row_element = type_expression row_element_mini_c


and type_expression = {
    type_content: type_content;
    type_meta: type_meta [@hash.ignore] ;
    orig_var: type_variable option [@hash.ignore] ;
    location: location [@hash.ignore] ;
  }
and ty_expr = type_expression

and expression_variable_list = expression_variable list
and type_expression_list = type_expression list

and matching_content_case = {
    constructor : label ;
    pattern : expression_variable ;
    body : expression ;
  }

and matching_content_case_list = matching_content_case list

and matching_content_variant = {
    cases: matching_content_case_list;
    tv: type_expression;
  }

and matching_content_record = {
  fields : (type_expression binder) label_map;
  body : expression;
  tv : type_expression;
}

and matching_expr =
  | Match_variant of matching_content_variant
  | Match_record of matching_content_record

and type_attribute = { public : bool ; hidden : bool }
and module_attribute = type_attribute

and program              = module_
and module_expr          = (expression , ty_expr , known_attributes , type_attribute , module_attribute) module_expr'
and module_              = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declarations'
and declaration          = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration'
and declaration_content  = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration_content'
and declaration_module   = (expression , ty_expr , known_attributes , type_attribute , module_attribute) declaration_module'
and declaration_constant = (expression , ty_expr , known_attributes) declaration_constant'
and declaration_type     = (ty_expr , type_attribute) declaration_type'

and expression = {
    expression_content: expression_content ;
    location: location ;
    type_expression: type_expression ;
  }

and expr = expression

and map_kv = {
    key : expression ;
    value : expression ;
  }

and expression_label_map = expression label_map
and expression_list = expression list

and expression_content =
  (* Base *)
  | E_literal of literal
  | E_constant of constant (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of application
  | E_lambda of lambda
  | E_recursive of recursive
  | E_let_in    of let_in
  | E_mod_in    of mod_in
  | E_raw_code  of raw_code
  | E_type_inst of type_inst
  | E_type_abstraction of expr type_abs
  (* Variant *)
  | E_constructor of constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression_label_map
  | E_record_accessor of record_accessor
  | E_record_update   of record_update
  | E_module_accessor of expression_variable module_access
  | E_assign   of (expr,ty_expr) assign

and type_inst = {
    forall: expression ;
    type_: type_expression ;
  }

and constant = {
    cons_name: constant' ;
    arguments: expression_list ;
  }

and application = {
  lamb: expression ;
  args: expression ;
  }

and lambda =  {
    binder: ty_expr binder ;
    result: expression ;
  }

and let_in = {
    let_binder: ty_expr binder ;
    rhs: expression ;
    let_result: expression ;
    attr: known_attributes ;
  }

and mod_in = (expression , ty_expr , known_attributes , type_attribute , module_attribute) mod_in'

and raw_code = {
  language : string;
  code : expression;
  }

and recursive = {
  fun_name : expression_variable;
  fun_type : type_expression;
  lambda   : lambda;
  }

and constructor = {
    constructor: label;
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

and matching = {
    matchee: expression ;
    cases: matching_expr ;
  }
