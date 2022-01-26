module Utils = Simple_utils.Utils
module CST = Cst.Jsligo
module AST = Ast_imperative
module Token = Lexing_jsligo.Token
val ghost : < attributes : 'a list; payload : string; region : CST.Region.t >
type nested_match_repr =
    PatternVar of AST.type_expression AST.binder
  | TupleVar of AST.type_expression AST.binder * nested_match_repr list
  | RecordVar of AST.type_expression AST.binder * AST.label list *
      nested_match_repr list
val nseq_to_list : 'a * 'a list -> 'a list
val npseq_to_list : 'a * ('b * 'a) list -> 'a list
val npseq_to_ne_list : 'a * ('b * 'c) list -> 'a * 'c list
val pseq_to_list : ('a * ('b * 'a) list) option -> 'a list
val get_value : 'a CST.reg -> 'a
val build_ins : string list
val r_split : 'a CST.reg -> 'a * AST.location
val compile_variable : string CST.reg -> AST.type_variable
val compile_attributes : string CST.reg list -> string list
module Compile_type :
  sig
    type type_compiler_opt = CST.type_expr -> AST.type_expression option
    val get_t_int_singleton_opt : CST.type_expr -> Z.t option
    val get_t_string_singleton_opt : CST.type_expr -> string option
    val type_compiler_opt_list : type_compiler_opt list -> type_compiler_opt
    val try_type_compilers :
      type_compiler_opt list ->
      CST.type_expr -> (unit -> AST.type_expression) -> AST.type_expression
    val compile_type_function_args :
      raise:[> `Concrete_jsligo_michelson_type_wrong of
                 CST.type_expr * string
             | `Concrete_jsligo_michelson_type_wrong_arity of
                 AST.location * string
             | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
            ] Simple_utils.Trace.raise ->
      CST.fun_type_args -> AST.type_expression
    val compile_sapling :
      raise:[> `Concrete_jsligo_michelson_type_wrong of
                 CST.type_expr * string
             | `Concrete_jsligo_michelson_type_wrong_arity of
                 AST.location * string
             | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
            ] Simple_utils.Trace.raise ->
      type_compiler_opt
    val compile_michelson_pair_or :
      raise:[> `Concrete_jsligo_michelson_type_wrong of
                 CST.type_expr * string
             | `Concrete_jsligo_michelson_type_wrong_arity of
                 AST.location * string
             | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
            ] Simple_utils.Trace.raise ->
      type_compiler_opt
    val compile_type_expression :
      raise:[> `Concrete_jsligo_michelson_type_wrong of
                 CST.type_expr * string
             | `Concrete_jsligo_michelson_type_wrong_arity of
                 AST.location * string
             | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
            ] Simple_utils.Trace.raise ->
      CST.type_expr -> AST.type_expression
  end
val expression_to_variable :
  raise:[> `Concrete_jsligo_expected_a_variable of CST.expr ]
        Simple_utils.Trace.raise ->
  CST.expr -> CST.variable
val selection_to_variable :
  raise:[> `Concrete_jsligo_expected_a_field_name of CST.selection ]
        Simple_utils.Trace.raise ->
  CST.selection -> CST.variable
val compile_expression_to_int :
  raise:[> `Concrete_jsligo_expected_an_int of CST.expr ]
        Simple_utils.Trace.raise ->
  CST.expr -> Z.t
val compile_selection :
  raise:[> `Concrete_jsligo_expected_an_int of CST.expr ]
        Simple_utils.Trace.raise ->
  CST.selection -> 'a AST.access * AST.location
val array_item_to_expression :
  raise:[> `Concrete_jsligo_expected_an_expression of CST.array_item ]
        Simple_utils.Trace.raise ->
  CST.array_item -> CST.expr
val arguments_to_expr_nseq :
  CST.arguments -> CST.expr Utils.nseq * AST.location
val get_t_string_singleton_opt : CST.type_expr -> string option
type statement_result =
    Binding of (AST.expression -> AST.expression)
  | Expr of AST.expression
  | Break of AST.expression
  | Return of AST.expression
type constr_types =
    Match_nil of AST.expression
  | Match_cons of AST.type_variable * AST.type_variable
val compile_tuple_expression :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  ?loc:AST.location -> CST.expr Utils.nseq -> AST.expression
val compile_arguments :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.arguments -> AST.expression
val compile_bin_op :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  AST.constant' -> CST.plus CST.bin_op CST.reg -> AST.expression
val compile_un_op :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  AST.constant' -> CST.minus CST.un_op CST.reg -> AST.expression
val compile_expression :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.expr -> AST.expression
val conv :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  const:bool -> CST.pattern -> nested_match_repr
val get_binder : nested_match_repr -> AST.type_expression AST.binder
val fold_nested_z :
  ((AST.expression -> AST.expression) ->
   nested_match_repr -> AST.expression -> AST.expression) ->
  (AST.expression -> AST.expression) ->
  nested_match_repr list -> AST.expression -> AST.expression
val nestrec :
  AST.expression ->
  (AST.expression -> AST.expression) ->
  nested_match_repr list -> AST.expression
val compile_array_let_destructuring :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  const:bool ->
  AST.expression ->
  (CST.pattern, string CST.wrap) Utils.nsepseq CST.brackets CST.reg ->
  AST.expression -> AST.expression
val compile_object_let_destructuring :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  const:bool ->
  AST.expression ->
  (CST.pattern, string CST.wrap) Utils.nsepseq CST.braces CST.reg ->
  AST.expression -> AST.expression
val compile_parameter :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.expr ->
  AST.type_expression AST.binder *
  (AST.type_expression AST.binder * AST.Types.attributes * AST.expression)
  list
val compile_function_body_to_expression :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.body -> AST.expression
val compile_let_to_declaration :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  const:bool ->
  CST.attributes -> CST.val_binding CST.reg -> AST.declaration list
val merge_statement_results :
  statement_result -> statement_result -> statement_result
val is_failwith_call : AST.expression -> bool
val compile_pattern :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  const:bool ->
  CST.pattern ->
  AST.type_expression AST.binder * (AST.expression -> AST.expression)
val filter_private : CST.attributes -> CST.attributes
val compile_statements :
  ?wrap:bool ->
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.statements -> statement_result
val compile_statement :
  ?wrap:bool ->
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.statement -> statement_result
val statement_result_to_expression : statement_result -> AST.expression
val compile_statements_to_expression :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.statements -> AST.expression
val compile_statement_to_declaration :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  export:bool -> CST.statement -> AST.declaration list
val compile_statements_to_program :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.t -> AST.module_
val compile_namespace :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.statements -> AST.module_
val compile_module :
  raise:[> `Concrete_jsligo_array_rest_not_supported of CST.array_item
         | `Concrete_jsligo_expected_a_function of CST.expr
         | `Concrete_jsligo_expected_a_variable of CST.expr
         | `Concrete_jsligo_expected_an_expression of CST.array_item
         | `Concrete_jsligo_expected_an_int of CST.expr
         | `Concrete_jsligo_invalid_case of string * CST.expr
         | `Concrete_jsligo_invalid_list_pattern_match of CST.array_item list
         | `Concrete_jsligo_michelson_type_wrong of CST.type_expr * string
         | `Concrete_jsligo_michelson_type_wrong_arity of
             AST.location * string
         | `Concrete_jsligo_not_a_valid_parameter of CST.expr
         | `Concrete_jsligo_not_supported_assignment of CST.expr
         | `Concrete_jsligo_property_not_supported of CST.property
         | `Concrete_jsligo_recursion_on_non_function of AST.location
         | `Concrete_jsligo_rest_not_supported_here of CST.property
         | `Concrete_jsligo_statement_not_supported_at_toplevel of
             CST.statement
         | `Concrete_jsligo_unknown_constant of string * AST.location
         | `Concrete_jsligo_unknown_constructor of string * AST.location
         | `Concrete_jsligo_unsupported_match_object_property of CST.property
         | `Concrete_jsligo_unsupported_match_pattern of CST.expr
         | `Concrete_jsligo_unsupported_pattern_type of CST.pattern
         | `Concrete_jsligo_unsupported_string_singleton of CST.type_expr
         | `Concrete_jsligo_untyped_recursive_fun of CST.Region.t ]
        Simple_utils.Trace.raise ->
  CST.t -> AST.declaration AST.location_wrap list
