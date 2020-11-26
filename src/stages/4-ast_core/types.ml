[@@@warning "-30"]

include Stage_common.Types

type sugar_type_expression_option = Ast_sugar.type_expression option [@@deriving yojson]
type sugar_expression_option = Ast_sugar.expression option [@@deriving yojson]

type string_option = string option

let location_of_yojson loc = Location.of_yojson loc
let location_to_yojson loc = Location.to_yojson loc

type attribute = {
  inline: bool ;
}


type program_loc = declaration Location.wrap
and program = program_loc list

and declaration_constant = {
    binder : ty_expr binder;
    attr : attribute ;
    expr : expression ;
  }
and declaration =
  | Declaration_type of ty_expr declaration_type
  (* A Declaration_constant is described by
   *   a name
   *   an optional type annotation
   *   a boolean indicating whether it should be inlined
   *   an expression *)
  | Declaration_constant of declaration_constant

(* | Macro_declaration of macro_declaration *)

and type_content =
  | T_variable of type_variable
  | T_sum      of rows
  | T_record   of rows
  | T_arrow    of ty_expr arrow
  | T_app      of ty_expr type_app

and rows = { fields : row_element label_map ; layout : layout option }

and row_element = ty_expr row_element_mini_c

and type_expression = {
  type_content  : type_content ;
  sugar    : sugar_type_expression_option ;
  location : location ;
  }
and ty_expr = type_expression

and expression = {
  content  : expression_content ;
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
  | E_recursive of (expr, ty_expr) recursive
  | E_let_in    of let_in
  | E_type_in of (expr, ty_expr) type_in
  | E_raw_code of expr raw_code
  | E_constructor of expr constructor
  | E_matching of matching
  | E_record of expression_label_map
  | E_record_accessor of expr record_accessor
  | E_record_update   of expr record_update
  | E_ascription      of (expr,ty_expr) ascription

and type_expression_option = type_expression option

and let_in = {
    let_binder: ty_expr binder ;
    rhs: expression ;
    let_result: expression ;
    inline: bool ;
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
