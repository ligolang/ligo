[@@@warning "-30"]

module Location = Simple_utils.Location

include Stage_common.Types

type type_content =
  | T_variable of type_variable
  | T_sum      of ty_expr rows
  | T_record   of ty_expr rows
  | T_tuple    of ty_expr  list
  | T_arrow    of ty_expr arrow
  | T_annoted  of (type_expression * string)
  | T_app      of ty_expr type_app



and type_expression = {type_content: type_content; location: Location.t}
and ty_expr = type_expression

type program = declaration program'
  [@@deriving yojson]
and declaration =
  | Declaration_type of ty_expr declaration_type
  (* A Declaration_constant is described by
   *   a name
   *   an optional type annotation
   *   attributes
   *   an expression *)
  | Declaration_constant of (expr,ty_expr) declaration_constant

(* | Macro_declaration of macro_declaration *)
and expression = {expression_content: expression_content; location: Location.t}
and expr = expression

and expression_content =
  (* Base *)
  | E_literal of literal
  | E_constant of constant (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of expr application
  | E_lambda of (expr, ty_expr) lambda
  | E_recursive of (expr, ty_expr) recursive
  | E_let_in of (expr, ty_expr) let_in
  | E_type_in of (expr, ty_expr) type_in
  | E_raw_code of expr raw_code
  (* Variant *)
  | E_constructor of expr constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression label_map
  | E_accessor of expr accessor
  | E_update   of expr update
  (* Advanced *)
  | E_ascription of (expr, ty_expr) ascription
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
  | E_assign   of expr assign
  | E_for      of expr for_
  | E_for_each of expr for_each
  | E_while    of expr while_loop

and constant =
  { cons_name: rich_constant (* this is at the end because it is huge *)
  ; arguments: expression list }



and matching_expr =
  | Match_variant of ((label * expression_variable) * expression) list
  | Match_list of {
      match_nil  : expression ;
      match_cons : expression_variable * expression_variable * expression ;
    }
  | Match_option of {
      match_none : expression ;
      match_some : expression_variable * expression ;
    }
  | Match_tuple of ty_expr binder list  * expression
  | Match_record of (label * ty_expr binder) list * expression
  | Match_variable of ty_expr binder * expression

and matching =
  { matchee: expression
  ; cases: matching_expr
  }

and environment_element_definition =
  | ED_binder
  | ED_declaration of (expression * free_variables)

and free_variables = expression_variable list

and environment_element =
  { type_value: type_expression
  ; source_environment: environment
  ; definition: environment_element_definition }

and expr_environment = (expression_variable * environment_element) list
and type_environment = (type_variable * type_expression) list

(* SUBST ??? *)
and environment = expr_environment * type_environment
