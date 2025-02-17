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
    (Syntax_err.to_string error)

(* Some literals *)

let check_identifier node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Identifier node :: errors
  | _ -> errors

let check_string node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.String_literal node :: errors
  | _ -> errors

let check_regex node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Regexp node :: errors
  | _ -> errors

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

let check_braces node check ~err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ ->
    check_enclosed
      node
      check
      "{"
      "}"
      ~open_err:Syntax_err.Left_brace
      ~close_err:Syntax_err.Right_brace
      errors

let check_chevrons node check ~err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ ->
    check_enclosed
      node
      check
      "<"
      ">"
      ~open_err:Syntax_err.Left_chevron
      ~close_err:Syntax_err.Right_chevron
      errors

let check_brackets node check ~err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ ->
    check_enclosed
      node
      check
      "["
      "]"
      ~open_err:Syntax_err.Left_bracket
      ~close_err:Syntax_err.Right_bracket
      errors

let check_parens node check ~err errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg err node :: errors
  | _ ->
    check_enclosed
      node
      check
      "("
      ")"
      ~open_err:Syntax_err.Left_parenthesis
      ~close_err:Syntax_err.Right_parenthesis
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
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Statement node :: errors
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
  | _ -> format_msg Syntax_err.Statement node :: errors

(* Export statement *)

and check_export_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Export node :: errors
  | _ -> errors (* TODO *)

(* Import statement *)

and check_import_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Import node :: errors
  | _ -> errors (* TODO *)

(* Debugger statement *)

and check_debugger_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Debugger node :: errors
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
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Expression node :: errors
  | _ -> errors (* TODO *)

(* Statement blocks *)

and check_statement_block node errors =
  check_braces node check_statement ~err:Syntax_err.Block errors

(* If statement *)

and check_if_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.If node :: errors
  | _ -> errors (* TODO *)

(* Switch statement *)

and check_switch_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Switch node :: errors
  | _ -> errors (* TODO *)

(* For statement *)

and check_for_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.For node :: errors
  | _ -> errors (* TODO *)

(* For-in statement *)

and check_for_in_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.For_or_await node :: errors
  | _ -> errors (* TODO *)

(* While statement *)

and check_while_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.While node :: errors
  | _ -> errors (* TODO *)

(* Do statement *)

and check_do_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Do node :: errors
  | _ -> errors (* TODO *)

(* Try statement *)

and check_try_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Try node :: errors
  | _ -> errors (* TODO *)

(* With statement *)

and check_with_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.With node :: errors
  | _ -> errors (* TODO *)

(* Break statement *)

and check_break_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Break node :: errors
  | _ -> errors (* TODO *)

(* Continue statement *)

and check_continue_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Continue node :: errors
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
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Return node :: errors
  | _ -> errors (* TODO *)

(* Throw statement *)

and check_throw_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Throw node :: errors
  | _ -> errors (* TODO *)

(* Empty statement *)

and check_empty_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Empty_statement node :: errors
  | _ -> errors (* TODO *)

(* Labeled statement *)

and check_labeled_statement node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Label node :: errors
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
  | _ -> format_msg Syntax_err.Declaration node :: errors

(* Function declaration (see [check_function_signature]) *)

and check_function_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Syntax_err.Function_declaration node :: errors
  | _ -> errors (* TODO *)

(* Generator function declaration (see function declaration) *)

and check_generator_function_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Syntax_err.Generator_function_declaration node :: errors
  | _ -> errors (* TODO *)

(* Class declaration (see [check_class]) *)

and check_class_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Class_declaration node :: errors
  | _ -> errors (* TODO *)

(* Lexical declaration (see [check_variable_declaration]) *)

and check_lexical_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Let_or_const node :: errors
  | _ -> errors (* TODO *)

(* Variable declaration (see [check_lexical_declaration]) *)

and check_variable_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Var node :: errors
  | _ -> errors (* TODO *)

(* Function signature (See [check_function_declaration]) *)

and check_function_signature node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Async_or_function node :: errors
  | _ -> errors (* TODO *)

(* Abstract class declaration ( see [check_class_declaration]) *)

and check_abstract_class_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Abstract node :: errors
  | _ -> errors (* TODO *)

(* Module *)

and check_module node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Module_name node :: errors
  | _ -> errors (* TODO *)

(* Internal module (a.k.a. namespaces) *)

and check_internal_module node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Namespace_name node :: errors
  | _ -> errors (* TODO *)

(* Type alias declaration *)

and check_type_alias_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Type_name node :: errors
  | _ -> errors (* TODO *)

(* Enum declaration *)

and check_enum_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Const_or_enum node :: errors
  | _ -> errors (* TODO *)

(* Interface declaration *)

and check_interface_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Interface node :: errors
  | _ -> errors (* TODO *)

(* Import alias *)

and check_import_alias node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Import node :: errors
  | _ -> errors (* TODO *)

(* Ambient declaration *)

and check_ambient_declaration node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Declare node :: errors
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
  | "undefined" -> mk_kwd_undefined node errors
  | "this" -> mk_kwd_this node errors
  | "super" -> mk_kwd_super node errors
  | "number" -> check_number node errors
  | "string" -> check_string node errors
  | "template_string" -> check_template_string node errors
  | "regex" -> check_regex node errors
  | "true" -> mk_kwd_true node errors
  | "false" -> mk_kwd_false node errors
  | "null" -> mk_kwd_null node errors
  | "object" -> check_object node errors
  | "array" -> check_array node errors
  | "function_expression" -> check_function_expression node errors
  | "arrow_function" -> check_arrow_function node errors
  | "generator_function" -> check_generator_function node errors
  | "class" -> check_class node errors
  | "meta_property" -> check_meta_property node errors
  | "call_expression" -> check_call_expression node errors
  | "non_null_expression" -> check_non_null_expression node errors
  | _ -> format_msg Syntax_err.Expression node :: errors

(* Glimmer template (not supported) *)

and check_glimmer_template node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Glimmer_template node :: errors
  | _ -> errors

(* Assignment expression *)

and check_assignment_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Syntax_err.Using_or_expression node :: errors
  | _ -> errors (* TODO *)

(* Augmented assignment expression *)

and check_augmented_assignment_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Syntax_err.LHS_of_augmented_assgmnt node :: errors
  | _ -> errors (* TODO *)

(* Await expression *)

and check_await_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Await node :: errors
  | _ -> errors (* TODO *)

(* Unary expression *)

and check_unary_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Unary_operator node :: errors
  | _ -> errors (* TODO *)

(* Binary expression *)

and check_binary_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Expression node :: errors
  | _ -> errors (* TODO *)

(* Ternary expression *)

and check_ternary_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Expression node :: errors
  | _ -> errors (* TODO *)

(* Update expression *)

and check_update_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Expression node :: errors
  | _ -> errors (* TODO *)

(* New expression

   Note that the constructor field is a primary expression, but
   "primary_expression" is a supertype, that is, a hidden rule. We
   assume it is an "expression", since primary expressions are a subset
   of them. *)

and check_new_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.New node :: errors
  | _ -> errors (* TODO *)

(* Yield expression *)

and check_yield_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Yield node :: errors
  | _ -> errors (* TODO *)

(* As-expression *)

and check_as_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.As node :: errors
  | _ -> errors (* TODO *)

(* Statisfies-expression *)

and check_satisfies_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Satisfies node :: errors
  | _ -> errors (* TODO *)

(* Instantiation expression *)

and check_instantiation_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Expression node :: errors
  | _ -> errors (* TODO *)

(* Type assertion *)

and check_type_assertion node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Type_arguments node :: errors
  | _ -> errors (* TODO *)

(* Subscript expression (see [check_member_expression]) *)

and check_subscript_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Expression node :: errors
  | _ -> errors (* TODO *)

(* Member expression *)

and check_member_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Member_expression node :: errors
  | _ -> errors (* TODO *)

(* Parenthesised expression *)

and check_parenthesized_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Syntax_err.Parenthesized_expression node :: errors
  | _ -> errors (* TODO *)

(* Template strings *)

and check_template_string node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Template_string node :: errors
  | _ -> errors (* TODO *)

(* Object *)

and check_object node errors =
  check_braces node check_object_field ~err:Syntax_err.Object_expression errors

and check_object_field node errors =
  ignore node;
  errors (* TODO *)

(* Pairs *)

and check_pair node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Key_value_pair node :: errors
  | _ -> errors (* TODO *)

(* Array (expression) *)

and check_array node errors =
  check_brackets node check_array_cell ~err:Syntax_err.Array errors

and check_array_cell node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Array_cell node :: errors
  | _ -> errors (* TODO *)

(* Function (expression) *)

and check_function_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Syntax_err.Function_expression node :: errors
  | _ -> errors (* TODO *)

(* Arrow function *)

and check_arrow_function node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Arrow_function node :: errors
  | _ -> errors (* TODO *)

(* Generator function *)

and check_generator_function node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Syntax_err.Generator_function node :: errors
  | _ -> errors (* TODO *)

(* Class *)

and check_class node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Class_expression node :: errors
  | _ -> errors (* TODO *)

(* Meta-property *)

and check_meta_property node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Meta_property node :: errors
  | _ -> errors (* TODO *)

(* Call expression *)

and check_call_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Call_expression node :: errors
  | _ -> errors (* TODO *)

(* Non-null expression *)

and check_non_null_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    format_msg Syntax_err.Non_null_expression node :: errors
  | _ -> errors (* TODO *)

(* Sequence expression *)

and check_sequence_expression node errors =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> format_msg Syntax_err.Expression node :: errors
  | _ -> check_named_children node check_expression errors
