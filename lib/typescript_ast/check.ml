(* Checking and collecting all error nodes in the C CST
   (tree-sitter-generated) *)

(* Dependencies *)

open Core
open Typescript_ast.Ts_wrap
module Region = Simple_utils.Region
module Snippet = Simple_utils.Snippet
module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Syntax_err = Typescript_ast.Syntax_err
module Wrap = Lexing_shared.Wrap
open Syntax_err

(* Monadic let-binder for result values *)

let ( let* ) v f = Core.Result.bind v ~f

(* Utilities *)

let swap f x y = f y x

(* Source map for converting vertical and horizontal offset ranges
   into regions *)

let get_region : (ts_tree -> Region.t) ref =
  ref (fun _ -> failwith "Internal error: Check.get_region")

(* The input source (default: a hundred lines) *)

let input : Buffer.t ref = ref (Buffer.create (80 * 100))

(* Formatting error messages (snippets) *)

let no_colour = true

let format_msg error node =
  let region = !get_region node in
  Printf.sprintf
    "%s%s"
    (Format.asprintf "%a" (Snippet.pp_lift ~no_colour) region)
    (to_string error)

(* Some literals *)

let check_leaf node errors ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ -> errors

let check_kwd = check_leaf
let check_identifier = check_leaf ~err:Identifier
let check_type_identifier = check_leaf ~err:Type_name
let check_string = check_leaf ~err:String_literal
let check_regex = check_leaf ~err:Regexp
let check_number = check_leaf ~err:Number_literal

(* Checking forests *)

let check_named_children node check errors =
  let children = collect_named_children node in
  List.fold_left ~f:(swap check) ~init:errors children

let check_children children check errors =
  List.fold_left ~f:(swap check) ~init:errors children

(* Wrapping the fetching of nodes *)

let update_errors errors = function
  | Ok _ -> errors
  | Error error -> error :: errors

let first_child_named name node ~err errors =
  let msg = format_msg err node in
  update_errors errors @@ Ts_wrap.first_child_named name node ~msg

let child_ranked index node ~err errors =
  let msg = format_msg err node in
  update_errors errors @@ Ts_wrap.child_ranked index node ~msg

let named_child_ranked index node ~err errors =
  let msg = format_msg err node in
  update_errors errors @@ Ts_wrap.named_child_ranked index node ~msg

let last_child node ~err errors =
  let msg = format_msg err node in
  update_errors errors @@ Ts_wrap.last_child node ~msg

let next_sibling node ~err errors =
  let msg = format_msg err node in
  update_errors errors @@ Ts_wrap.next_sibling node ~msg

(* Symbols *)

let make_sym node ~err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ -> errors

(* Check enclosed constructs *)

let check_enclosed node check opening closing ~open_err ~close_err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg open_err node :: errors
  | _ ->
    first_child_named opening node ~err:open_err errors
    |> first_child_named closing node ~err:close_err
    |> check_named_children node check

let check_braces node check err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ ->
    check_enclosed node check "{" "}" ~open_err:Left_brace ~close_err:Right_brace errors

let check_chevrons node check err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ ->
    check_enclosed
      node
      check
      "<"
      ">"
      ~open_err:Left_chevron
      ~close_err:Right_chevron
      errors

let check_brackets node check err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ ->
    check_enclosed
      node
      check
      "["
      "]"
      ~open_err:Left_bracket
      ~close_err:Right_bracket
      errors

let check_parens node check err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ ->
    check_enclosed
      node
      check
      "("
      ")"
      ~open_err:Left_parenthesis
      ~close_err:Right_parenthesis
      errors

(* Traversing the CST *)

let rec check_program ~filename ~file (map : Loc_map.t) node =
  (* Setting up the extraction of source regions *)
  let () = get_region := Ts_wrap.get_region filename map in
  (* Setting the input as a top-level string buffer *)
  let () = Buffer.add_string !input file in
  (* Collating errors from the stripped AST *)
  check_statements node []

(* STATEMENTS

   The JavaScript tree-sitter grammar has the non-terminals
   "statement" be a supertype, that is, a hidden rule. *)

and check_statements node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Statement node :: errors
  | _ -> check_named_children node check_statement errors

and check_statement node errors =
  match get_name node with
  | "export_statement" -> check_export_statement node errors
  | "import_statement" -> check_import_statement node errors
  | "debugger_statement" -> check_debugger_statement node errors
  | "expression_statement" -> check_expression_statement node errors
  | "statement_block" -> check_statement_block node errors
  | "if_statement" -> check_if_statement node errors
  | "switch_statement" -> check_switch_statement node errors
  | "for_statement" -> check_for_statement node errors
  | "for_in_statement" -> check_for_in_statement node errors
  | "while_statement" -> check_while_statement node errors
  | "do_statement" -> check_do_statement node errors
  | "try_statement" -> check_try_statement node errors
  | "with_statement" -> check_with_statement node errors
  | "break_statement" -> check_break_statement node errors
  | "continue_statement" -> check_continue_statement node errors
  | "return_statement" -> check_return_statement node errors
  | "throw_statement" -> check_throw_statement node errors
  | "empty_statement" -> check_empty_statement node errors
  | "labeled_statement" -> check_labeled_statement node errors
  (* Inlining declarations cases (hidden rule) *)
  | "function_declaration" -> check_function_declaration node errors
  | "generator_function_declaration" -> check_generator_function_declaration node errors
  | "class_declaration" -> check_class_declaration node errors
  | "lexical_declaration" -> check_lexical_declaration node errors
  | "variable_declaration" -> check_variable_declaration node errors
  | "function_signature" -> check_function_signature node errors
  | "abstract_class_declaration" -> check_abstract_class_declaration node errors
  | "module" -> check_module node errors
  | "internal_module" -> check_internal_module node errors
  | "type_alias_declaration" -> check_type_alias_declaration node errors
  | "enum_declaration" -> check_enum_declaration node errors
  | "interface_declaration" -> check_interface_declaration node errors
  | "import_alias" -> check_import_alias node errors
  | "ambient_declaration" -> check_ambient_declaration node errors
  | _ -> format_msg Statement node :: errors

(* Export statement *)

and check_export_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Export node :: errors
  | _ -> errors (* TODO *)

and check_namespace_export node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Namespace_export node :: errors
  | _ -> errors (* TODO *)

and check_export_clause node errors =
  check_braces node check_export_specifier Export_clause errors

and check_module_export_name node errors =
  match get_name node with
  | "identifier" -> check_identifier node errors
  | "string" -> check_string node errors
  | _ -> format_msg Identifier_or_string node :: errors

and check_export_specifier node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Identifier_or_string node :: errors
  | _ -> errors (* TODO *)

(* Import statement *)

and check_import_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Import node :: errors
  | _ -> errors (* TODO *)

and check_import_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Import_clause node :: errors
  | _ -> errors (* TODO *)

and check_namespace_import node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Namespace_import node :: errors
  | _ -> errors (* TODO *)

and check_named_imports node errors =
  check_braces node check_import_specifier Named_imports errors

and check_import_specifier node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Import_specifier node :: errors
  | _ -> errors (* TODO *)

and check_import_require_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Import_require_clause node :: errors
  | _ -> errors (* TODO *)

and check_import_attribute node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Import_attribute node :: errors
  | _ -> errors (* TODO *)

(* Debugger statement *)

and check_debugger_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Debugger node :: errors
  | _ -> errors (* TODO *)

(* Expression statements

   {@js[
   expression_statement: $ => seq($._expressions, $._semicolon),
   _expressions: $ => choice($.expression, $.sequence_expression),
   sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [check_expression]. *)

and check_expression_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Expression node :: errors
  | _ -> errors (* TODO *)

(* Statement blocks *)

and check_statement_block node errors = check_braces node check_statement Block errors

(* If statement *)

and check_if_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg If node :: errors
  | _ -> errors (* TODO *)

and check_else_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Else node :: errors
  | _ -> errors (* TODO *)

(* Switch statement *)

and check_switch_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Switch node :: errors
  | _ -> errors (* TODO *)

and check_switch_body node errors =
  check_braces node check_in_switch_body Switch_body errors

and check_in_switch_body node errors =
  match get_name node with
  | "switch_case" -> check_switch_case node errors
  | "switch_default" -> check_switch_default node errors
  | _ -> format_msg Switch_body node :: errors

and check_switch_case node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Case node :: errors
  | _ -> errors (* TODO *)

and check_switch_default node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Default node :: errors
  | _ -> errors (* TODO *)

(* For statement *)

and check_for_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg For node :: errors
  | _ -> errors (* TODO *)

(* For-in statement *)

and check_for_in_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg For_or_await node :: errors
  | _ -> errors (* TODO *)

(* While statement *)

and check_while_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg While node :: errors
  | _ -> errors (* TODO *)

(* Do statement *)

and check_do_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Do node :: errors
  | _ -> errors (* TODO *)

(* Try statement *)

and check_try_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Try node :: errors
  | _ -> errors (* TODO *)

and check_catch_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Catch node :: errors
  | _ -> errors (* TODO *)

and check_finally_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Finally node :: errors
  | _ -> errors (* TODO *)

(* With statement *)

and check_with_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg With node :: errors
  | _ -> errors (* TODO *)

(* Break statement *)

and check_break_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Break node :: errors
  | _ -> errors (* TODO *)

(* Continue statement *)

and check_continue_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Continue node :: errors
  | _ -> errors (* TODO *)

(* Return statement

   NOTE: The Javascript grammar states:

   {@js[
   return_statement: $ =>
   seq('return', optional($._expressions), $._semicolon),

   _semicolon: $ => choice($._automatic_semicolon, ';')
   ]}

   but the child of rank 1 is sometimes missing, as if
   "_automatic_semicolon" can be the empty word. Other rules use
   `optional(_automatic_semicolon)`, which adds to the mystery. *)

and check_return_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Return node :: errors
  | _ -> errors (* TODO *)

(* Throw statement *)

and check_throw_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Throw node :: errors
  | _ -> errors (* TODO *)

(* Empty statement *)

and check_empty_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Empty_statement node :: errors
  | _ -> errors (* TODO *)

(* Labeled statement *)

and check_labeled_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Label node :: errors
  | _ -> errors (* TODO *)

(* DECLARATION

   The JavaScript tree-sitter grammar has the non-terminal
   "declaration" be a supertype, that is, a hidden rule. *)

and check_declaration node errors =
  match get_name node with
  | "function_declaration" -> check_function_declaration node errors
  | "generator_function_declaration" -> check_generator_function_declaration node errors
  | "class_declaration" -> check_class_declaration node errors
  | "lexical_declaration" -> check_lexical_declaration node errors
  | "variable_declaration" -> check_variable_declaration node errors
  | "function_signature" -> check_function_signature node errors
  | "abstract_class_declaration" -> check_abstract_class_declaration node errors
  | "module" -> check_module node errors
  | "internal_module" -> check_internal_module node errors
  | "type_alias_declaration" -> check_type_alias_declaration node errors
  | "enum_declaration" -> check_enum_declaration node errors
  | "interface_declaration" -> check_interface_declaration node errors
  | "import_alias" -> check_import_alias node errors
  | "ambient_declaration" -> check_ambient_declaration node errors
  | _ -> format_msg Declaration node :: errors

(* Function declaration (see [check_function_signature]) *)

and check_function_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Function_declaration node :: errors
  | _ -> errors (* TODO *)

and check_return_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_expression node :: errors
  | "type_annotation" -> check_type_annotation node errors
  | "asserts_annotation" -> check_asserts_annotation node errors
  | _ -> check_type_predicate_annotation node errors

(* Generator function declaration (see function declaration) *)

and check_generator_function_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Generator_function_declaration node :: errors
  | _ -> errors (* TODO *)

(* Class declaration (see [check_class]) *)

and check_class_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Class_declaration node :: errors
  | _ -> errors (* TODO *)

(* Lexical declaration (see [check_variable_declaration]) *)

and check_lexical_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Let_or_const node :: errors
  | _ -> errors (* TODO *)

and check_variable_declarator node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Variable node :: errors
  | _ -> errors (* TODO *)

and check_lhs_pattern node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Pattern node :: errors
  | "identifier" -> check_identifier node errors
  | _ -> check_destructuring_pattern node errors

(* Variable declaration (see [check_lexical_declaration]) *)

and check_variable_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Var node :: errors
  | _ -> errors (* TODO *)

(* Function signature (See [check_function_declaration]) *)

and check_function_signature node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Async_or_function node :: errors
  | _ -> errors (* TODO *)

(* Abstract class declaration ( see [check_class_declaration]) *)

and check_abstract_class_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Abstract node :: errors
  | _ -> errors (* TODO *)

(* Module *)

and check_module node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Module_name node :: errors
  | _ -> errors (* TODO *)

(* Internal module (a.k.a. namespaces) *)

and check_internal_module node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Namespace_name node :: errors
  | _ -> errors (* TODO *)

(* Type alias declaration *)

and check_type_alias_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_name node :: errors
  | _ -> errors (* TODO *)

and check_type_parameters node errors =
  check_chevrons node check_type_parameter Type_parameters errors

and check_type_parameter node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Const_or_type_name node :: errors
  | _ -> errors (* TODO *)

and check_constraint node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Extends node :: errors
  | _ -> errors (* TODO *)

and check_default_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Equal node :: errors
  | _ -> errors (* TODO *)

(* Enum declaration *)

and check_enum_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Const_or_enum node :: errors
  | _ -> errors (* TODO *)

and check_enum_body node errors = check_braces node check_in_enum_body Left_brace errors

and check_in_enum_body node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Enumeration_name node :: errors
  | "enum_assignment" -> check_enum_assignment node errors
  | _ -> check_property_name node errors

and check_enum_assignment node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Enumeration_name node :: errors
  | _ -> errors (* TODO *)

(* Interface declaration *)

and check_interface_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Interface node :: errors
  | _ -> errors (* TODO *)

and check_interface_body node errors = check_object_type node errors

and check_extends_type_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Extends node :: errors
  | _ -> errors (* TODO *)

(* Import alias *)

and check_import_alias node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Import node :: errors
  | _ -> errors (* TODO *)

(* Ambient declaration *)

and check_ambient_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Declare node :: errors
  | _ -> errors (* TODO *)

(* EXPRESSION

   The JavaScript tree-sitter grammar has the non-terminals
   "expression" and "primary_expression" be supertypes, that is,
   hidden rules. Therefore we have to match all the RHS of those
   non-terminals in [check_expression]. *)

and check_expression node errors =
  match get_name node with
  (* Rest of "expression": *)
  | "glimmer_template" -> check_glimmer_template node errors
  | "assignment_expression" -> check_assignment_expression node errors
  | "augmented_assignment_expression" -> check_augmented_assignment_expression node errors
  | "await_expression" -> check_await_expression node errors
  | "unary_expression" -> check_unary_expression node errors
  | "binary_expression" -> check_binary_expression node errors
  | "ternary_expression" -> check_ternary_expression node errors
  | "update_expression" -> check_update_expression node errors
  | "new_expression" -> check_new_expression node errors
  | "yield_expression" -> check_yield_expression node errors
  | "as_expression" -> check_as_expression node errors
  | "satisfies_expression" -> check_satisfies_expression node errors
  | "instantiation_expression" -> check_instantiation_expression node errors
  | "internal_module" -> check_internal_module node errors
  | "type_assertion" -> check_type_assertion node errors
  | _ -> check_primary_expression node errors

(* Primary expression *)

and check_primary_expression node errors =
  match get_name node with
  | "subscript_expression" -> check_subscript_expression node errors
  | "member_expression" -> check_member_expression node errors
  | "parenthesized_expression" -> check_parenthesized_expression node errors
  | "identifier" -> check_identifier node errors
  | "undefined" -> check_kwd node errors ~err:Undefined
  | "this" -> check_kwd node errors ~err:This
  | "super" -> check_kwd node errors ~err:Super
  | "number" -> check_number node errors
  | "string" -> check_string node errors
  | "template_string" -> check_template_string node errors
  | "regex" -> check_regex node errors
  | "true" -> check_kwd node errors ~err:True
  | "false" -> check_kwd node errors ~err:False
  | "null" -> check_kwd node errors ~err:Null
  | "object" -> check_object node errors
  | "array" -> check_array node errors
  | "function_expression" -> check_function_expression node errors
  | "arrow_function" -> check_arrow_function node errors
  | "generator_function" -> check_generator_function node errors
  | "class" -> check_class node errors
  | "meta_property" -> check_meta_property node errors
  | "call_expression" -> check_call_expression node errors
  | "non_null_expression" -> check_non_null_expression node errors
  | _ -> format_msg Expression node :: errors

(* Glimmer template (not supported) *)

and check_glimmer_template node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Glimmer_template node :: errors
  | _ -> errors

(* Assignment expression *)

and check_assignment_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Using_or_expression node :: errors
  | _ -> errors (* TODO *)

(* Augmented assignment expression *)

and check_augmented_assignment_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg LHS_of_augmented_assgmnt node :: errors
  | _ -> errors (* TODO *)

(* Await expression *)

and check_await_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Await node :: errors
  | _ -> errors (* TODO *)

(* Unary expression *)

and check_unary_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Unary_operator node :: errors
  | _ -> errors (* TODO *)

(* Binary expression *)

and check_binary_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Expression node :: errors
  | _ -> errors (* TODO *)

(* Ternary expression *)

and check_ternary_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Expression node :: errors
  | _ -> errors (* TODO *)

(* Update expression *)

and check_update_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Expression node :: errors
  | _ -> errors (* TODO *)

(* New expression

   Note that the constructor field is a primary expression, but
   "primary_expression" is a supertype, that is, a hidden rule. We
   assume it is an "expression", since primary expressions are a subset
   of them. *)

and check_new_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg New node :: errors
  | _ -> errors (* TODO *)

(* Yield expression *)

and check_yield_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Yield node :: errors
  | _ -> errors (* TODO *)

(* As-expression *)

and check_as_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg As node :: errors
  | _ -> errors (* TODO *)

(* Statisfies-expression *)

and check_satisfies_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Satisfies node :: errors
  | _ -> errors (* TODO *)

(* Instantiation expression *)

and check_instantiation_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Expression node :: errors
  | _ -> errors (* TODO *)

(* Type assertion *)

and check_type_assertion node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_arguments node :: errors
  | _ -> errors (* TODO *)

(* Subscript expression (see [check_member_expression]) *)

and check_subscript_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Expression node :: errors
  | _ -> errors (* TODO *)

(* Member expression *)

and check_member_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Member_expression node :: errors
  | _ -> errors (* TODO *)

(* Parenthesised expression *)

and check_parenthesized_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Parenthesized_expression node :: errors
  | _ -> errors (* TODO *)

(* Template strings *)

and check_template_string node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Template_string node :: errors
  | _ -> errors (* TODO *)

(* Object *)

and check_object node errors =
  check_braces node check_object_field Object_expression errors

and check_object_field node errors =
  ignore node;
  errors (* TODO *)

(* Pairs *)

and check_pair node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Key_value_pair node :: errors
  | _ -> errors (* TODO *)

(* Array (expression) *)

and check_array node errors = check_brackets node check_array_cell Array errors

and check_array_cell node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Array_cell node :: errors
  | _ -> errors (* TODO *)

and check_spread_element node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Spread node :: errors
  | _ -> errors (* TODO *)

(* Function (expression) *)

and check_function_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Function_expression node :: errors
  | _ -> errors (* TODO *)

(* Arrow function *)

and check_arrow_function node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Arrow_function node :: errors
  | _ -> errors (* TODO *)

and check_arrow_function_body node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Block_or_expression node :: errors
  | "statement_block" -> check_statement_block node errors
  | _ -> check_expression node errors (* Hidden *)

(* Generator function *)

and check_generator_function node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Generator_function node :: errors
  | _ -> errors (* TODO *)

(* Class *)

and check_class node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Class_expression node :: errors
  | _ -> errors (* TODO *)

and check_class_heritage node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Extends_or_implements node :: errors
  | _ -> errors (* TODO *)

and check_implements_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Implements_clause node :: errors
  | _ -> errors (* TODO *)

and check_extends_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Extends_clause node :: errors
  | _ -> errors (* TODO *)

and check_class_body node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Class_body node :: errors
  | _ -> errors (* TODO *)

and check_class_member (decorators, node) errors =
  match get_name node with
  | "method_definition" -> check_method_definition_with_decorators decorators node errors
  | "method_signature" -> check_method_signature node errors
  | "class_static_block" -> check_class_static_block node errors
  | "abstract_method_signature" -> check_abstract_method_signature node errors
  | "index_signature" -> check_index_signature node errors
  | "public_field_definition" -> check_public_field_definition node errors
  | _ -> format_msg Class_member node :: errors

and check_method_definition_with_decorators decorators node errors =
  let errors = List.fold_left ~f:(swap check_decorator) ~init:errors decorators in
  check_method_definition node errors

and check_method_definition node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Method_definition node :: errors
  | _ -> errors (* TODO *)

and check_class_static_block node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Static_block node :: errors
  | _ -> errors (* TODO *)

and check_abstract_method_signature node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Abstract_method_signature node :: errors
  | _ -> errors (* TODO *)

and check_public_field_definition node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Public_field_definition node :: errors
  | _ -> errors (* TODO *)

(* Meta-property *)

and check_meta_property node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Meta_property node :: errors
  | _ -> errors (* TODO *)

(* Call expression *)

and check_call_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Call_expression node :: errors
  | _ -> errors (* TODO *)

and check_type_arguments node errors =
  check_chevrons node check_type Type_arguments errors

and check_arguments node errors = check_parens node check_argument Arguments errors

and check_argument node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Argument node :: errors
  | "spread_element" -> check_spread_element node errors
  | _ -> check_expression node errors (* Hidden *)

(* Non-null expression *)

and check_non_null_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Non_null_expression node :: errors
  | _ -> errors (* TODO *)

(* Sequence expression *)

and check_sequence_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Expression node :: errors
  | _ -> check_named_children node check_expression errors

(* TYPE

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and check_type node errors =
  match get_name node with
  | "function_type" -> check_function_type node errors
  | "readonly_type" -> check_readonly_type node errors
  | "constructor_type" -> check_constructor_type node errors
  | "infer_type" -> check_infer_type node errors
  (* A couple of aliases *)
  | "member_expression" ->
    check_type_query_member_expression_in_type_annotation node errors
  | "call_expression" -> check_type_query_call_expression_in_type_annotation node errors
  (* "primary_type" is hidden *)
  | _ -> check_primary_type node errors

(* Primary type *)

and check_primary_type node errors =
  match get_name node with
  | "parenthesized_type" -> check_parenthesized_type node errors
  | "predefined_type" -> check_predefined_type node errors
  | "type_identifier" -> check_type_identifier node errors (* Including "const" *)
  | "nested_type_identifier" -> check_nested_type_identifier node errors
  | "generic_type" -> check_generic_type node errors
  | "object_type" -> check_object_type node errors
  | "array_type" -> check_array_type node errors
  | "tuple_type" -> check_tuple_type node errors
  | "flow_maybe_type" -> check_flow_maybe_type node errors
  | "type_query" -> check_type_query node errors
  | "index_type_query" -> check_index_type_query node errors
  | "this_type" -> check_kwd node errors ~err:This
  | "existential_type" -> check_existential_type node errors
  | "literal_type" -> check_literal_type node errors
  | "lookup_type" -> check_lookup_type node errors
  | "conditional_type" -> check_conditional_type node errors
  | "template_literal_type" -> check_template_literal_type node errors
  | "intersection_type" -> check_intersection_type node errors
  | "union_type" -> check_union_type node errors
  | _ -> format_msg Type_expression node :: errors

(* Type queries in type annotations (expressions) *)

and check_type_query_member_expression_in_type_annotation node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Member_or_call node :: errors
  | _ -> errors (* TODO *)

and check_type_query_property node errors =
  match get_name node with
  | "property_identifier" -> check_identifier node errors
  | "private_property_identifier" -> check_identifier node errors
  | _ -> format_msg Property_identifier node :: errors

and check_type_query_call_expression_in_type_annotation node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Member_expression node :: errors
  | _ -> errors (* TODO *)

(* Flow maybe type

   flow_maybe_type: $ => prec.right(seq('?', $.primary_type))
 *)

and check_flow_maybe_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type node :: errors
  | _ -> errors (* TODO *)

(* Parenthesized type *)

and check_parenthesized_type node errors =
  check_parens node check_type Parenthesized_type errors

(* Predefined type *)

and check_predefined_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Predefined_type node :: errors
  | _ -> errors (* TODO *)

(* Nested type identifier *)

and check_nested_type_identifier node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Nested_type_identifier node :: errors
  | _ -> errors (* TODO *)

(* Nested identifier *)

and check_nested_identifier node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Identifier_or_member node :: errors
  | _ -> errors (* TODO *)

(* Generic type *)

and check_generic_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Generic_type node :: errors
  | _ -> errors (* TODO *)

(* Object type *)

and check_object_type node errors =
  check_braces node check_object_type_field Object_type errors

and check_object_type_field node errors =
  match get_name node with
  | "export_statement" -> check_export_statement node errors
  | "property_signature" -> check_property_signature node errors
  | "call_signature" -> check_call_signature node errors
  | "construct_signature" -> check_construct_signature node errors
  | "index_signature" -> check_index_signature node errors
  | "method_signature" -> check_method_signature node errors
  | _ -> format_msg Object_type_field node :: errors

(* Property signature *)

and check_property_signature node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Property_signature node :: errors
  | _ -> errors (* TODO *)

(* Call signature *)

and check_call_signature node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Call_signature node :: errors
  | _ -> errors (* TODO *)

(* Asserts annotation *)

and check_asserts_annotation node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Asserts_annotation node :: errors
  | _ -> errors (* TODO *)

and check_asserts node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Asserts node :: errors
  | _ -> errors (* TODO *)

(* Type predicate annotation *)

and check_type_predicate_annotation node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_predicate node :: errors
  | _ -> errors (* TODO *)

(* Construct signature *)

and check_construct_signature node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Construct_signature node :: errors
  | _ -> errors (* TODO *)

(* Index signature *)

and check_index_signature node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Index_signature node :: errors
  | _ -> errors (* TODO *)

and check_plus_minus node errors =
  match get_name node with
  | "+" -> errors
  | "-" -> errors
  | _ -> format_msg Plus_or_minus node :: errors

and check_mapped_type_clause node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Mapped_type_signature node :: errors
  | _ -> errors (* TODO *)

and check_omitting_type_annotation node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Omitting_type_annotation node :: errors
  | _ -> errors (* TODO *)

and check_adding_type_annotation node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Adding_type_annotation node :: errors
  | _ -> errors (* TODO *)

and check_opting_type_annotation node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Opting_type_annotation node :: errors
  | _ -> errors (* TODO *)

(* Method signature *)

and check_method_signature node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Method_signature node :: errors
  | _ -> errors (* TODO *)

(* Array type *)

and check_array_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Array_type node :: errors
  | _ -> errors (* TODO *)

(* Tuple type *)

and check_tuple_type node errors =
  check_brackets node check_tuple_type_member Tuple_type errors

and check_tuple_type_member node errors =
  match get_name node with
  | "required_parameter" -> check_tuple_parameter node errors (* Alias *)
  | "optional_parameter" -> check_optional_tuple_parameter node errors (* Alias *)
  | "optional_type" -> check_optional_type node errors
  | "rest_type" -> check_rest_type node errors
  | _ -> check_type node errors (* "type" is a hidden rule *)

and check_tuple_parameter node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Tuple_parameter node :: errors
  | _ -> errors (* TODO *)

and check_optional_tuple_parameter node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Optional_tuple_parameter node :: errors
  | _ -> errors (* TODO *)

(* Type annotation *)

and check_type_annotation node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_annotation node :: errors
  | _ -> errors (* TODO *)

(* Rest pattern *)

and check_rest_pattern node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Rest_pattern node :: errors
  | _ -> errors (* TODO *)

(* Optional type *)

and check_optional_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Optional_type node :: errors
  | _ -> errors (* TODO *)

(* Rest type *)

and check_rest_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Rest_type node :: errors
  | _ -> errors (* TODO *)

(* LHS expression *)

and check_lhs_expression node errors =
  match get_name node with
  | "member_expression" -> check_member_expression node errors
  | "subscript_expression" -> check_subscript_expression node errors
  | "identifier" -> check_identifier node errors
  | "undefined" -> check_kwd node errors ~err:Undefined
  | "object_pattern" -> check_object_pattern node errors
  | "array_pattern" -> check_array_pattern node errors
  | "non_null_expression" -> check_non_null_expression node errors
  | _ -> format_msg Expression node :: errors

(* Type query *)

and check_type_query node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_query node :: errors
  | _ -> errors (* TODO *)

and check_type_query_subscript_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_query_subscript node :: errors
  | _ -> errors (* TODO *)

and check_type_query_member_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_query_member node :: errors
  | _ -> errors (* TODO *)

and check_object_denotation node errors =
  match get_name node with
  | "identifier" -> check_identifier node errors
  | "this" -> check_kwd node errors ~err:This
  | "subscript_expression" -> check_type_query_subscript_expression node errors
  | "member_expression" -> check_type_query_member_expression node errors
  | "call_expression" -> check_type_query_call_expression node errors
  | _ -> format_msg Object_denotation node :: errors

and check_property_field node errors = check_type_query_property node errors

and check_type_query_instantiation_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_query_instantiation node :: errors
  | _ -> errors (* TODO *)

and check_function_field node errors =
  match get_name node with
  | "import" -> check_kwd node errors ~err:Import
  | "identifier" -> check_identifier node errors
  | "member_expression" -> check_type_query_member_expression node errors
  | "subscript_expression" -> check_type_query_subscript_expression node errors
  | _ -> format_msg Function_denotation node :: errors

and check_type_query_call_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_query_call node :: errors
  | _ -> errors (* TODO *)

(* Index type query *)

and check_index_type_query node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Index_type_query node :: errors
  | _ -> errors (* TODO *)

(* Existential type *)

and check_existential_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Existential_type node :: errors
  | _ -> errors (* TODO *)

(* Literal type *)

and check_literal_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Literal_type node :: errors
  | _ -> errors (* TODO *)

(* Look up type

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and check_lookup_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Lookup_type node :: errors
  | _ -> errors (* TODO *)

(* Conditional type *)

and check_conditional_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Conditional_type node :: errors
  | _ -> errors (* TODO *)

(* Template literal type *)

and check_template_literal_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Template_literal_type node :: errors
  | _ -> errors (* TODO *)

(* Intersection type *)

and check_intersection_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Intersection_type node :: errors
  | _ -> errors (* TODO *)

(* Union type *)

and check_union_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Union_type node :: errors
  | _ -> errors (* TODO *)

(* Function type *)

and check_function_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Function_type node :: errors
  | _ -> errors (* TODO *)

(* Type predicate *)

and check_type_predicate node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Type_predicate node :: errors
  | _ -> errors (* TODO *)

(* Readonly type *)

and check_readonly_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Readonly_type node :: errors
  | _ -> errors (* TODO *)

(* Constructor type *)

and check_constructor_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Constructor_type node :: errors
  | _ -> errors (* TODO *)

and check_formal_parameters node errors =
  check_parens node check_formal_parameter Parameters errors

and check_formal_parameter node errors =
  match get_name node with
  | "required_parameter" -> check_required_parameter node errors
  | "optional_parameter" -> check_optional_parameter node errors
  | _ -> format_msg Parameter node :: errors

and check_optional_parameter node errors = check_required_parameter node errors

and check_required_parameter node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Required_parameter node :: errors
  | _ -> errors (* TODO *)

(* Infer type *)

and check_infer_type node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Infer node :: errors
  | _ -> errors (* TODO *)

(* Decorator *)

and check_decorator node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Decorator node :: errors
  | _ -> errors (* TODO *)

and check_decorator_member_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Decorator_member node :: errors
  | _ -> errors (* TODO *)

and check_decorator_call_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Decorator_call node :: errors
  | _ -> errors (* TODO *)

and check_decorator_parenthesized_expression node errors =
  check_parens node check_decorator_in_parens Parenthesized_decorator errors

and check_decorator_in_parens node errors =
  match get_name node with
  | "identifier" -> check_identifier node errors
  | "member_expression" -> check_decorator_member_expression node errors
  | _ -> check_call_expression node errors

(* Accessibility modifier *)

and check_accessibility_modifier node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Public_private_protected node :: errors
  | _ -> errors (* TODO *)

(* Override modifier *)

and check_override_modifier node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Override node :: errors
  | _ -> errors (* TODO *)

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

(* Object pattern *)

and check_object_pattern node errors =
  check_braces node check_object_pattern_field Object_pattern errors

and check_object_pattern_field node errors =
  match get_name node with
  | "pair_pattern" -> check_pair_pattern node errors
  | "rest_pattern" -> check_rest_pattern node errors
  | "object_assignment_pattern" -> check_object_assignment_pattern node errors
  | "shorthand_property_identifier_pattern" ->
    check_shorthand_property_identifier_pattern node errors
  | _ -> format_msg Object_pattern_field node :: errors

(* Pair pattern *)

and check_pair_pattern node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Pair_pattern node :: errors
  | _ -> errors (* TODO *)

(* Assignment pattern *)

and check_assignment_pattern node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Assignment_pattern node :: errors
  | _ -> errors (* TODO *)

(* Property names *)

and check_property_name node errors =
  match get_name node with
  | "property_identifier" -> check_identifier node errors
  | "private_property_identifier" -> check_identifier node errors
  | "string" -> check_string node errors
  | "number" -> check_number node errors
  | "computed_property_name" -> check_computed_property_name node errors
  | _ -> format_msg Property_name node :: errors

and check_computed_property_name node errors =
  check_brackets node check_expression Computed_property_name errors

and check_shorthand_property_identifier_pattern node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Identifier node :: errors
  | _ -> errors (* TODO *)

(* Object assignment pattern *)

and check_object_assignment_pattern node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Object_assignment_pattern node :: errors
  | _ -> errors (* TODO *)

and check_object_lhs_pattern node errors =
  match get_name node with
  | "shorthand_property_identifier_pattern" ->
    check_shorthand_property_identifier_pattern node errors
  | _ -> check_destructuring_pattern node errors

(* Rule "_destructuring_pattern" is inlined. *)

and check_destructuring_pattern node errors =
  match get_name node with
  | "object_pattern" -> check_object_pattern node errors
  | "array_pattern" -> check_array_pattern node errors
  | _ -> format_msg Object_or_array_pattern node :: errors

(* Array pattern *)

and check_array_pattern node errors =
  check_brackets node check_array_pattern_cell Array_pattern errors

and check_array_pattern_cell node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Array_cell_pattern node :: errors
  | "assignment_pattern" -> check_assignment_pattern node errors
  | _ -> check_pattern node errors (* hidden rule *)

(* General patterns (hidden rule) *)

and check_pattern node errors =
  match get_name node with
  | "rest_pattern" -> check_rest_pattern node errors
  | _ -> check_lhs_expression node errors
