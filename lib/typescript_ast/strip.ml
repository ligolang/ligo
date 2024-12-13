(* Decoding the tree-sitter CST for TypeScript and stripping it *)

open Core

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Utils = Simple_utils.Utils
module Region = Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

(* Local dependencies *)

module Ast = Typescript_ast.Ast
module S = Typescript_ast.Ast_stripped

(* Utilities *)

type 'a reg = 'a Region.reg
type 'a wrap = 'a Wrap.wrap

let ( let* ) v f = Result.bind v ~f
let ( <@ ) f g x = f (g x)

let mk_reg region value = Region.{region; value}

let error_reg ?(hint: string option) (region : Region.t) (msg : string) =
  let hint =
    match hint with
      None | Some "" -> ""
    | Some msg -> "\nHint: " ^ msg in
  Error (Printf.sprintf "%s:\n%s%s" (region#to_string `Byte) msg hint)

let error ?hint (wrap : 'a wrap) (msg : string) = error_reg ?hint wrap#region msg

let rev_erase_options =
  let f acc = function
    | None -> acc
    | Some elt -> elt :: acc
  in
  List.fold_left ~f ~init:[]

let strip_opt strip = function
  | None -> Ok None
  | Some node -> strip node

let strip_list_opt strip = function
  | None -> Ok []
  | Some list -> strip list

let opt_to_error strip (node: _ wrap) msg =
  match strip node with
  | None -> error node msg
  | Some node -> Ok node

(*
let only_one strip (node: _ wrap) msg =
  match strip node with
  | Ok [node] -> Ok node
  | Ok _ -> error node msg
  | Error msg -> Error msg
*)

(* Stripping *)

let rec strip_statements (node : Ast.statements) : (S.t, _) result =
  match node with
  | None -> Ok []
  | Some stmts ->
     let stmts = Nonempty_list.to_list stmts#payload in
     let f acc stmt =
       let* stmt' = strip_statement stmt in
       Ok (stmt' :: acc)
     in
     let* stmts = List.fold_result ~f ~init:[] stmts in
     Ok (rev_erase_options stmts)

and strip_statement (node : Ast.statement) : (S.statement option, _) result =
  match node with
  | S_export_statement s -> strip_S_export_statement s
  | S_import_statement s -> strip_S_import_statement s
  | S_debugger_statement s -> strip_S_debugger_statement s
  | S_expression_statement s -> strip_S_expression_statement s
  | S_declaration_statement s -> strip_S_declaration_statement s
  | S_statement_block s -> strip_S_statement_block s
  | S_if_statement s -> strip_S_if_statement s
  | S_switch_statement s -> strip_S_switch_statement s
  | S_for_statement s -> strip_S_for_statement s
  | S_for_in_statement s -> strip_S_for_in_statement s
  | S_while_statement s -> strip_S_while_statement s
  | S_do_statement s -> strip_S_do_statement s
  | S_try_statement s -> strip_S_try_statement s
  | S_with_statement s -> strip_S_with_statement s
  | S_break_statement s -> strip_S_break_statement s
  | S_continue_statement s -> strip_S_continue_statement s
  | S_return_statement s -> strip_S_return_statement s
  | S_throw_statement s -> strip_S_throw_statement s
  | S_empty_statement s -> strip_S_empty_statement s
  | S_labeled_statement s -> strip_S_labeled_statement s

(* Export statement *)

and strip_S_export_statement (node : Ast.export_statement wrap) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_S_export_statement"

(* Import statement *)

and strip_S_import_statement (node : Ast.import_statement wrap) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_S_import_statement"

(* Debugger statement *)

and strip_S_debugger_statement (node : Ast.kwd_debugger) : (S.statement option, _) result =
  error node "Debugger statements are not supported in JsLIGO."

(* Expression statement *)

and strip_S_expression_statement (node : Ast.expression_statement) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_S_expression_statement"

(* Declaration statement *)

and strip_S_declaration_statement (node : Ast.declaration) : (S.statement option, _) result =
  let* declaration = strip_declaration node in
  Ok (Some (S.S_decl declaration))

(* Statement block *)

and strip_S_statement_block (node : Ast.statement_block) : (S.statement option, _) result =
  let* statements = strip_statement_block node in
  Ok (Some (S.S_block statements))

and strip_statement_block (node : Ast.statement_block) : (S.statement list reg, _) result =
  let Braces statements = node in
  let region = statements#region in
  let statements = statements#payload.contents in
  let* statements = strip_statements statements in
  Ok (mk_reg region statements)

(* If statement *)

and strip_S_if_statement (node : Ast.if_statement wrap) : (S.statement option, _) result =
  let if_stmt, region = node#payload, node#region in
  let Ast.{kwd_if=_; condition; consequence; alternative} = if_stmt in
  let* test = strip_parenthesized_expression condition in
  let* test =
    match test with
    | [test] -> Ok test
    | _ -> let region = Ast.region_of_parens condition
           and msg = "Exactly one test expression is supported in JsLIGO." in
           error_reg region msg in
  let* if_so = strip_statement consequence in
  let* if_so =
    match if_so with
    | None -> let region = Ast.region_of_statement consequence
              and msg = "Empty consequence is not supported in JsLIGO." in
              error_reg region msg
    | Some if_so -> Ok if_so in
  let* if_not = strip_opt (strip_statement <@ snd) alternative in
  let if_stmt = S.{test; if_so; if_not} in
  Ok (Some (S.S_if (mk_reg region if_stmt)))

and strip_parenthesized_expression (node : Ast.parenthesized_expression) : (S.expr list, _) result =
  let Ast.Parens expressions = node in
  let expressions = expressions#payload.contents in
  strip_expressions expressions

and strip_expressions (node : Ast.expressions) : (S.expr list, _) result =
  let expressions = Nonempty_list.to_list node#payload in
  Result.all @@ List.map ~f:strip_expression expressions

(* Switch statement *)

and strip_S_switch_statement (node : Ast.switch_statement wrap) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_S_switch_statement"

(* For statement *)

and strip_S_for_statement (node : Ast.for_statement wrap) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_S_for_statement"

(* For-in statement *)

and strip_S_for_in_statement (node : Ast.for_in_statement wrap) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_S_for_in_statement"

(* While statement *)

and strip_S_while_statement (node : Ast.while_statement wrap) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_S_while_statement"

(* Do statement *)

and strip_S_do_statement (node : Ast.do_statement wrap) : (S.statement option, _) result =
  error node "Do-while loops are not supported in JsLIGO."

(* Try statement *)

and strip_S_try_statement (node : Ast.try_statement wrap) : (S.statement option, _) result =
  error node "Exceptions are not supported in JsLIGO."

(* With statement *)

and strip_S_with_statement (node : Ast.with_statement wrap) : (S.statement option, _) result =
  ignore node; Error "With-statements are not supported in JsLIGO."

(* Break statement *)

and strip_S_break_statement (node : Ast.break_statement wrap) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_break_statement"

(* Continue statement *)

and strip_S_continue_statement (node : Ast.continue_statement wrap) : (S.statement option, _) result =
  error node "Continue statements are not supported in JsLIGO."

(* Return statement *)

and strip_S_return_statement (node : Ast.return_statement wrap) : (S.statement option, _) result =
  ignore node; Error "TODO: strip_return_statement"

(* Throw statement *)

and strip_S_throw_statement (node : Ast.throw_statement wrap) : (S.statement option, _) result =
  error node "Exceptions are not supported in JsLIGO."

(* Empty statement *)

and strip_S_empty_statement (node : Region.t) : (S.statement option, _) result =
  ignore node; Ok None

(* Labeled statement *)

and strip_S_labeled_statement (node : Ast.labeled_statement wrap) : (S.statement option, _) result =
  error node "Labeled statements are not supported in JsLIGO."

(* DECLARATIONS *)

and strip_declaration (node : Ast.declaration) : (S.declaration, _) result =
  match node with
  | D_function_declaration d -> strip_D_function_declaration d
  | D_generator_function_declaration d -> strip_D_generator_function_declaration d
  | D_class_declaration d -> strip_D_class_declaration d
  | D_lexical_declaration d -> strip_D_lexical_declaration d
  | D_variable_declaration d -> strip_D_variable_declaration d
  | D_function_signature d -> strip_D_function_signature d
  | D_abstract_class_declaration d -> strip_D_abstract_class_declaration d
  | D_module d -> strip_D_module_declaration d
  | D_internal_module d -> strip_D_internal_module d
  | D_type_alias_declaration d -> strip_D_type_alias_declaration d
  | D_enum_declaration d -> strip_D_enum_declaration d
  | D_interface_declaration d -> strip_D_interface_declaration d
  | D_import_alias d -> strip_D_import_alias d
  | D_ambient_declaration d -> strip_D_ambient_declaration d

(* Function declaration *)

and strip_D_function_declaration (node : Ast.function_declaration wrap) : (S.declaration, _) result =
  ignore node; Error "TODO: strip_D_function_declaration"

(* Generator function declaration *)

and strip_D_generator_function_declaration (node : Ast.generator_function_declaration wrap) : (S.declaration, _) result =
  error node "Generator functions are not supported in JsLIGO."

(* Class declaration *)

and strip_D_class_declaration (node : Ast.class_declaration wrap) : (S.declaration, _) result =
  ignore node; Error "TODO: strip_D_class_declaration"

(* Lexical declaration *)

and strip_D_lexical_declaration (node : Ast.lexical_declaration wrap) : (S.declaration, _) result =
  ignore node; Error "TODO: strip_D_lexical_declaration"

(* Variable declaration *)

and strip_D_variable_declaration (node : Ast.variable_declaration wrap) : (S.declaration, _) result =
  error node "Variable declared with 'var' are not supported in JsLIGO."
        ~hint:"Use the 'let' modifier."

(* Function signature *)

and strip_D_function_signature (node : Ast.function_signature wrap) : (S.declaration, _) result =
  ignore node; Error "TODO: strip_D_function_signature"

(* Abstract class declaration *)

and strip_D_abstract_class_declaration (node : Ast.abstract_class_declaration wrap) : (S.declaration, _) result =
  error node "Abstract classes are not supported in JsLIGO."

(* Module declaration *)

and strip_D_module_declaration (node : Ast.module_declaration wrap) : (S.declaration, _) result =
  error node "Modules are not supported in JsLIGO." ~hint:"Try using namespaces."

(* Internal module declaration *)

and strip_D_internal_module (node : Ast.internal_module wrap) : (S.declaration, _) result =
  ignore node; Error "TODO: strip_D_internal_module_declaration"

(* Type alias declaration *)

and strip_D_type_alias_declaration (node : Ast.type_alias_declaration wrap) : (S.declaration, _) result =
  let type_decl, region = node#payload, node#region in
  let Ast.{kwd_type=_; name; type_parameters; sym_equal=_; type_expr} = type_decl in
  let  name = strip_type_identifier name in
  let* generics = strip_list_opt strip_type_parameters type_parameters in
  let* type_expr = strip_type_expr type_expr in
  let type_decl' = S.{name; generics; type_expr} in
  Ok (S.D_type (mk_reg region type_decl'))

and strip_type_identifier (node: Ast.type_identifier) : S.variable = node

and strip_type_parameters (node : Ast.type_parameters) : (S.variable list, _) result =
  let Ast.Chevrons type_params = node in
  let type_params = type_params#payload.contents in
  Result.all @@ (List.map ~f:strip_type_parameter type_params)

and strip_type_parameter (node : Ast.type_parameter wrap) : (S.variable, _) result =
  let Ast.{kwd_const=_; name; constraint_expr; default_type} = node#payload in
  match constraint_expr, default_type with
  | None, None ->
     Ok name
  | Some (_, type_expr), _ ->
     let region = Ast.region_of_type_expr type_expr in
     error_reg region "Constraints on type parameters are not supported in JsLIGO."
  | _, Some (_, type_expr) ->
     let region = Ast.region_of_type_expr type_expr in
     error_reg region "Defaults of type parameters are not supported in JsLIGO."

(* Enum declaration *)

and strip_D_enum_declaration (node : Ast.enum_declaration wrap) : (S.declaration, _) result =
  error node "Enumerated values are not supported in JsLIGO."

(* Interface declaration *)

and strip_D_interface_declaration (node : Ast.interface_declaration wrap) : (S.declaration, _) result =
  ignore node; Error "TODO: strip_D_interface_declaration"

(* Import alias *)

and strip_D_import_alias (node : Ast.import_alias wrap) : (S.declaration, _) result =
  ignore node; Error "TODO: strip_D_import_alias"

(* Ambient declaration *)

and strip_D_ambient_declaration (node : Ast.ambient_declaration wrap) : (S.declaration, _) result =
  error node "Ambient declarations are not supported in JsLIGO."

(* TYPES *)

and strip_type_expr (node : Ast.type_expr) : (S.type_expr, _) result =
  match node with
  | T_primary_type t -> strip_T_primary_type t
  | T_function_type t -> strip_T_function_type t
  | T_readonly_type t -> strip_T_readonly_type t
  | T_constructor_type t -> strip_T_constructor_type t
  | T_infer_type t -> strip_T_infer_type t
  | T_member_expression t -> strip_T_member_expression t
  | T_call_expression t -> strip_T_call_expression t

(* Primary type *)

and strip_T_primary_type (node : Ast.primary_type) : (S.type_expr, _) result =
  match node with
  | T_parenthesized_type t -> strip_T_parenthesized_type t
  | T_predefined_type t -> strip_T_predefined_type t
  | T_type_identifier t -> strip_T_type_identifier t
  | T_nested_type_identifier t -> strip_T_nested_type_identifier t
  | T_generic_type t -> strip_T_generic_type t
  | T_object_type t -> strip_T_object_type t
  | T_array_type t -> strip_T_array_type t
  | T_tuple_type t -> strip_T_tuple_type t
  | T_flow_maybe_type t -> strip_T_flow_maybe_type t
  | T_type_query t -> strip_T_type_query t
  | T_index_type_query t -> strip_T_index_type_query t
  | T_this t -> strip_T_this t
  | T_existential_type t -> strip_T_existential_type t
  | T_literal_type t -> strip_T_literal_type t
  | T_lookup_type t -> strip_T_lookup_type t
  | T_conditional_type t -> strip_T_conditional_type t
  | T_template_literal_type t -> strip_T_template_literal_type t
  | T_intersection_type t -> strip_T_intersection_type t
  | T_union_type t -> strip_T_union_type t

(* Parenthesized type *)

and strip_T_parenthesized_type (node : Ast.type_expr Ast.parens) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_parenthesized_type"

(* Predefined type *)

and strip_T_predefined_type (node : Ast.predefined_type) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_predefined_type"

(* Type identifier *)

and strip_T_type_identifier (node : Ast.type_identifier) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_type_identifier"

(* Nested type identifier *)

and strip_T_nested_type_identifier (node : Ast.nested_type_identifier wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_nested_type_identifier"

(* Generic type *)

and strip_T_generic_type (node : Ast.generic_type wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_generic_type"

(* Object type *)

and strip_T_object_type (node : Ast.object_type) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_object_type"

(* Array type *)

and strip_T_array_type (node : Ast.array_type wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_array_type"

(* Tuple type *)

and strip_T_tuple_type (node : Ast.tuple_type) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_tuple_type"

(* Flow maybe type *)

and strip_T_flow_maybe_type (node : (Ast.sym_qmark * Ast.primary_type) wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_flow_maybe_type"

(* Type query *)

and strip_T_type_query (node : (Ast.kwd_keyof * Ast.type_query) wrap) :
(S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_type_query"

(* Index type query *)

and strip_T_index_type_query (node : (Ast.kwd_keyof * Ast.primary_type) wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_index_type_query"

(* This as a type *)

and strip_T_this (node : Ast.kwd_this) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_this"

and strip_T_existential_type (node : Ast.sym_star) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_existential_type"

(* Literal type *)

and strip_T_literal_type (node : Ast.literal_type) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_literal_type"

(* Lookup type *)

and strip_T_lookup_type (node : Ast.lookup_type wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_lookup_type"

(* Conditional type *)

and strip_T_conditional_type (node : Ast.conditional_type wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_conditional_type"

(* Template literal type *)

and strip_T_template_literal_type (node : Ast.template_literal_type wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_template_literal_type"

(* Intersection type *)

and strip_T_intersection_type (node : Ast.intersection_type wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_intersection_type"

(* Union type *)

and strip_T_union_type (node : Ast.union_type wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_union_type"

(* Function type *)

and strip_T_function_type (node : Ast.function_type wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_function_type"

(* Readonly type *)

and strip_T_readonly_type (node : Ast.readonly_type wrap) : (S.type_expr, _) result =
  error node "Read-only types are not supported in JsLIGO."

(* Constructor type *)

and strip_T_constructor_type (node : Ast.constructor_type wrap) : (S.type_expr, _) result =
  error node "Constructor types are not supported in JsLIGO."

(* Infer type *)

and strip_T_infer_type (node : Ast.infer_type wrap) : (S.type_expr, _) result =
  error node "Infer types are not supported in JsLIGO."

(* Member expression (type expresion) *)

and strip_T_member_expression (node : Ast.member_expression wrap) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_member_expression"

(* Call expression *)

and strip_T_call_expression (node : Ast.call_expression) : (S.type_expr, _) result =
  ignore node; Error "TODO: strip_T_call_expression"

(* EXPRESSIONS *)

and strip_expression (node : Ast.expression) : (S.expr, _) result =
  match node with
  | E_as_expression e -> strip_E_as_expression e
  | E_assignment_expression e -> strip_E_assignment_expression e
  | E_augmented_assignment_expression e -> strip_E_augmented_assignment_expression e
  | E_await_expression e -> strip_E_await_expression e
  | E_binary_expression e -> strip_E_binary_expression e
  | E_instantiation_expression e -> strip_E_instantiation_expression e
  | E_internal_module e -> strip_E_internal_module e
  | E_new_expression e -> strip_E_new_expression e
  | E_primary_expression e -> strip_E_primary_expression e
  | E_satisfies_expression e -> strip_E_satisfies_expression e
  | E_ternary_expression e -> strip_E_ternary_expression e
  | E_type_assertion e -> strip_E_type_assertion e
  | E_unary_expression e -> strip_E_unary_expression e
  | E_update_expression e -> strip_E_update_expression e
  | E_yield_expression e -> strip_E_yield_expression e

(* As-expression *)

and strip_E_as_expression (node : Ast.as_expression wrap) : (S.expr, _) result =
  let as_expr, region = node#payload, node#region in
  let expr, _, as_what = as_expr in
  match as_what with
  | Ast.As_type type_expr ->
     let* expr = strip_expression expr in
     let* type_expr = strip_type_expr type_expr in
     Ok (S.E_typed (mk_reg region (expr, type_expr)))
  | As_const kwd_const -> error kwd_const "Const not supported here in JsLIGO."

(* Assignment expression *)

and strip_E_assignment_expression (node : Ast.assignment_expression wrap) : (S.expr, _) result =
  ignore node; Error "TODO: strip_E_assignment_expression"

(* Augmented assignment expression *)

and strip_E_augmented_assignment_expression (node : Ast.augmented_assignment_expression wrap) : (S.expr, _) result =
  ignore node; Error "TODO: strip_E_augmented_assignment_expression"

(* Await-expression *)

and strip_E_await_expression (node : Ast.await_expression wrap) : (S.expr, _) result =
  error node "Await-expressions are not supported in JsLIGO."

(* Binary expression *)

and strip_E_binary_expression (node : Ast.binary_expression wrap) : (S.expr, _) result =
  ignore node; Error "TODO: strip_E_binary_expression"

(* Instantiation expression *)

and strip_E_instantiation_expression (node : Ast.instantiation_expression wrap) : (S.expr, _) result =
  error node "Instantiation of type parameters is not supported in JsLIGO."

(* Internal module expression *)

and strip_E_internal_module (node : Ast.internal_module wrap) : (S.expr, _) result =
  ignore node; Error "TODO: strip_E_internal_module"

(* New-expression *)

and strip_E_new_expression (node : Ast.new_expression wrap) : (S.expr, _) result =
  error node "Instantiation of classes is not supported in JsLIGO."

(* Primary expression *)

and strip_E_primary_expression (node : Ast.primary_expression) : (S.expr, _) result =
  ignore node; Error "TODO: strip_E_primary_expression"

(* Statisfies-expression *)

and strip_E_satisfies_expression (node : Ast.satisfies_expression wrap) : (S.expr, _) result =
  error node "Type checks are not supported in JsLIGO."

(* Ternary expression *)

and strip_E_ternary_expression (node : Ast.ternary_expression wrap) : (S.expr, _) result =
  ignore node; Error "TODO: strip_E_ternary_expression"

(* Type assertion (expression) *)

and strip_E_type_assertion (node : Ast.type_assertion wrap) : (S.expr, _) result =
  error node "Type assertions are not supported in JsLIGO."

(* Unary expression *)

and strip_E_unary_expression (node : Ast.unary_expression wrap) : (S.expr, _) result =
  ignore node; Error "TODO: strip_E_unary_expression"

(* Update expression *)

and strip_E_update_expression (node : Ast.update_expression) : (S.expr, _) result =
  ignore node; Error "TODO: strip_E_update_expression"

(* Yield-expression *)

and strip_E_yield_expression (node : Ast.yield_expression) : (S.expr, _) result =
  let region = Ast.region_of_yield_expression node in
  error_reg region "Yield expressions are not supported in JsLIGO."

(* PATTERNS *)

and strip_pattern (node : Ast.pattern) : (S.pattern, _) result =
  match node with
  | P_member_expression p -> strip_P_member_expression p
  | P_subscript_expression p -> strip_P_subscript_expression p
  | P_identifier p -> strip_P_identifier p
  | P_undefined p -> strip_P_undefined p
  | P_destructuring_pattern p -> strip_P_destructuring_pattern p
  | P_non_null_expression p -> strip_P_non_null_expression p
  | P_rest_pattern p -> strip_P_rest_pattern p

(* Member expression (pattern) *)

and strip_P_member_expression (node : Ast.member_expression wrap) : (S.pattern, _) result =
  ignore node; Error "TODO: strip_P_member_expression"

(* Subscript expression (pattern) *)

and strip_P_subscript_expression (node : Ast.subscript_expression wrap) : (S.pattern, _) result =
  ignore node; Error "TODO: strip_P_subscript_expression"

(* Identifier (pattern) *)

and strip_P_identifier (node : Ast.identifier) : (S.pattern, _) result =
  ignore node; Error "TODO: strip_P_identifier"

(* Undefined (pattern) *)

and strip_P_undefined (node : Ast.kwd_undefined) : (S.pattern, _) result =
  error node "The undefined value is not supported in patterns in JsLIGO."

(* Destructuring pattern *)

and strip_P_destructuring_pattern (node : Ast.destructuring_pattern) : (S.pattern, _) result =
  ignore node; Error "TODO: strip_P_destructuring_pattern"

(* Non-null expression (pattern) *)

and strip_P_non_null_expression (node : Ast.expression) : (S.pattern, _) result =
  ignore node; Error "TODO: strip_P_non_null_expression"

(* Rest pattern *)

and strip_P_rest_pattern (node : Ast.rest_pattern wrap) : (S.pattern, _) result =
  ignore node; Error "TODO: strip_P_rest_pattern"
