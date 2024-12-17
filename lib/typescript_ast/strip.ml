(* Decoding the tree-sitter CST for TypeScript and stripping it *)

open Core

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

(* Local dependencies *)

module S = Ast_stripped

(* Utilities *)

type 'a reg = 'a Region.reg
type 'a wrap = 'a Wrap.wrap

let ( let* ) v f = Result.bind v ~f
let ( <@ ) f g x = f (g x)
let mk_reg region value = Region.{ region; value }
let error = Strip_err.make
let error_reg = Strip_err.of_region

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

let map_opt strip = function
  | None -> Ok None
  | Some node ->
    let* node = strip node in
    Ok (Some node)

(* Temporary data structures *)

type call_signature =
  { generics : S.variable list
  ; parameters : S.parameter list
  ; rhs_type : S.type_expr option
  }

type for_header =
  { index_kind : S.var_kind option
  ; index : S.key * S.value option
  ; expr : S.expr
  }

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

and strip_S_export_statement (node : Ast.export_statement wrap)
    : (S.statement option, _) result
  =
  ignore node;
  Error "TODO: strip_S_export_statement"

(* Import statement *)

and strip_S_import_statement (node : Ast.import_statement wrap)
    : (S.statement option, _) result
  =
  ignore node;
  Error "TODO: strip_S_import_statement"

(* Debugger statement *)

and strip_S_debugger_statement (node : Ast.kwd_debugger) : (S.statement option, _) result =
  error node "Debugger statements are not supported in JsLIGO."

(* Expression statement *)

and strip_S_expression_statement (node : Ast.expression_statement)
    : (S.statement option, _) result
  =
  let* expr = strip_expression_statement node in
  Ok (Option.map ~f:(fun e -> S.S_expr e) expr)

and strip_expression_statement (node : Ast.expression_statement)
    : (S.expr option, _) result
  =
  let* exprs = strip_expressions node in
  match exprs with
  | [] -> Ok None (* Should not happen *)
  | [ expr ] -> Ok (Some expr)
  | _ -> error node "Multiple values are not supported in JsLIGO."

(* Declaration statement *)

and strip_S_declaration_statement (node : Ast.declaration)
    : (S.statement option, _) result
  =
  let* declaration = strip_declaration node in
  Ok (Some (S.S_decl declaration))

(* Statement block *)

and strip_S_statement_block (node : Ast.statement_block) : (S.statement option, _) result =
  let* statements = strip_statement_block node in
  Ok (Some (S.S_block statements))

and strip_statement_block (node : Ast.statement_block) : (S.statement list reg, _) result =
  let (Braces statements) = node in
  let region = statements#region in
  let statements = statements#payload.contents in
  let* statements = strip_statements statements in
  Ok (mk_reg region statements)

(* If statement *)

and strip_S_if_statement (node : Ast.if_statement wrap) : (S.statement option, _) result =
  let if_stmt, region = node#payload, node#region in
  let Ast.{ kwd_if = _; condition; consequence; alternative } = if_stmt in
  let* test = strip_parenthesized_expression condition in
  let* test =
    match test with
    | [ test ] -> Ok test
    | _ ->
      let region = Ast.region_of_parens condition
      and msg = "Exactly one test expression is supported in JsLIGO." in
      error_reg region msg
  in
  let* if_so = strip_statement consequence in
  let* if_so =
    match if_so with
    | None ->
      let region = Ast.region_of_statement consequence
      and msg = "Empty consequence is not supported in JsLIGO." in
      error_reg region msg
    | Some if_so -> Ok if_so
  in
  let* if_not = strip_opt (strip_statement <@ snd) alternative in
  let if_stmt = S.{ test; if_so; if_not } in
  Ok (Some (S.S_if (mk_reg region if_stmt)))

and strip_parenthesized_expression (node : Ast.parenthesized_expression)
    : (S.expr list, _) result
  =
  let (Ast.Parens expressions) = node in
  let expressions = expressions#payload.contents in
  strip_expressions expressions

and strip_expressions (node : Ast.expressions) : (S.expr list, _) result =
  let expressions = Nonempty_list.to_list node#payload in
  Result.all @@ List.map ~f:strip_expression expressions

(* Switch statement *)

and strip_S_switch_statement (node : Ast.switch_statement wrap)
    : (S.statement option, _) result
  =
  ignore node;
  Error "TODO: strip_S_switch_statement"

(* For statement *)

and strip_S_for_statement (node : Ast.for_statement wrap) : (S.statement option, _) result
  =
  let Ast.
        { kwd_for = _
        ; sym_lpar = _
        ; initializer_
        ; condition
        ; increment
        ; sym_rpar = _
        ; body
        }
    =
    node#payload
  in
  let* initialiser = strip_for_initializer initializer_ in
  let* condition = strip_for_condition condition in
  let* afterthought =
    match increment with
    | None -> Ok []
    | Some increment -> strip_expressions increment
  in
  let* for_body = strip_statement body in
  let for_stmt = S.{ initialiser; condition; afterthought; for_body } in
  Ok (Some (S.S_for (mk_reg node#region for_stmt)))

and strip_for_initializer (node : Ast.for_initializer) : (S.statement option, _) result =
  match node with
  | For_lexical_declaration decl ->
    let* declaration = strip_lexical_declaration decl in
    Ok (Some (S.S_decl declaration))
  | For_variable_declaration decl ->
    let* declaration = strip_variable_declaration decl in
    Ok (Some (S.S_decl declaration))
  | For_expression_statement stmt -> strip_S_expression_statement stmt
  | For_empty_statement _ -> Ok None

and strip_for_condition (node : Ast.for_condition) : (S.expr option, _) result =
  match node with
  | For_condition_expression expr_stmt -> strip_expression_statement expr_stmt
  | For_condition_empty _ -> Ok None

(* For-in statement *)

and strip_S_for_in_statement (node : Ast.for_in_statement wrap)
    : (S.statement option, _) result
  =
  let Ast.{ kwd_for = _; kwd_await; sym_lpar = _; for_header; sym_rpar = _; body } =
    node#payload
  in
  let* () =
    match kwd_await with
    | None -> Ok ()
    | Some kwd_await ->
      error_reg kwd_await#region "Asynchronicity is not supported in JsLIGO."
  in
  let* { index_kind; index; expr } = strip_for_header for_header in
  let* for_of_body = strip_statement body in
  let for_of_stmt = S.{ index_kind; index; expr; for_of_body } in
  let for_of_stmt = mk_reg node#region for_of_stmt in
  Ok (Some (S.S_for_of for_of_stmt))

and strip_for_header (node : Ast.for_header) : (for_header, _) result =
  let Ast.{ range; operator; collection } = node in
  let* in_region =
    match operator with
    | In kwd_in -> Ok kwd_in#region
    | Of kwd_of ->
      error_reg
        kwd_of#region
        "Loops ranging with 'of' are not supported in JsLIGO."
        ~hint:"Try using 'in' instead."
  in
  let* index_kind, index = strip_for_range range in
  let* exprs = strip_expressions collection in
  let* expr =
    match exprs with
    | [ expr ] -> Ok expr
    | _ -> error_reg in_region "Iterated collections are one expression in JsLIGO."
  in
  Ok { index_kind; index; expr }

and strip_for_range (node : Ast.for_range)
    : (S.var_kind option * (S.key * S.value option), _) result
  =
  match node with
  | For_in_expression (Identifier v) ->
    let variable = strip_identifier v in
    Ok (None, (variable, None))
  | For_in_expression e ->
    let region = Ast.region_of_lhs_expression e in
    error_reg region "Only variables can range in JsLIGO loops. "
  | For_in_parenthesized e ->
    let region = Ast.region_of_parens e in
    error_reg region "Only variables can range in JsLIGO loops. "
  | For_in_var for_in_var -> strip_for_in_var for_in_var
  | For_in_let (kwd_let, for_in_variable) ->
    let var_kind = `Let kwd_let#region in
    let* index = strip_for_in_variable for_in_variable in
    Ok (Some var_kind, index)
  | For_in_const (kwd_const, for_in_variable) ->
    let var_kind = `Const kwd_const#region in
    let* index = strip_for_in_variable for_in_variable in
    Ok (Some var_kind, index)

and strip_for_in_variable (node : Ast.for_in_variable)
    : (S.key * S.value option, _) result
  =
  let region = Ast.region_of_for_in_variable node in
  match node with
  | For_in_ident ident -> Ok (strip_identifier ident, None)
  | For_in_pattern p ->
    let* pattern = strip_destructuring_pattern p in
    (match pattern with
    | S.P_array array ->
      (match array.value with
      | [ elem_1; elem_2 ] ->
        let* elem_1 = force_single_var elem_1 in
        let* elem_2 = force_single_var elem_2 in
        Ok (elem_1, Some elem_2)
      | _ ->
        error_reg
          region
          "Only a variable or an array of two variables (key, value of maps) can range \
           over collections in JsLIGO.")
    | _ ->
      error_reg
        region
        "Only a variable or an array of two variables (key, value of maps) can range \
         over collections in JsLIGO.")

and force_single_var (node : S.pattern S.element) : (S.variable, _) result =
  match node with
  | Element (P_var path as pattern) ->
    (match path.Region.value with
    | Nonempty_list.[ variable ] -> Ok variable
    | _ ->
      let region = S.region_of_pattern pattern in
      error_reg region "Expected a variable.")
  | Element pattern | Spread pattern ->
    let region = S.region_of_pattern pattern in
    error_reg region "Expected a variable."

and strip_for_in_var (node : Ast.for_in_var) =
  let Ast.{ kwd_var; variable = _; default = _ } = node in
  error_reg
    kwd_var#region
    "'var' variables are not supported in JsLIGO"
    ~hint:"Use 'let' or 'const'."

(* While statement *)

and strip_S_while_statement (node : Ast.while_statement wrap)
    : (S.statement option, _) result
  =
  let* stmt = strip_while_statement node in
  Ok (Some (S.S_while (mk_reg node#region stmt)))

and strip_while_statement (node : Ast.while_statement wrap) : (S.while_stmt, _) result =
  let (Ast.{ kwd_while; condition; body } : Ast.while_statement) = node#payload in
  let* exprs = strip_parenthesized_expression condition in
  let* expr =
    match exprs with
    | [ expr ] -> Ok expr
    | _ ->
      error_reg kwd_while#region "Only one expression as invariant in JsLIGO is valid."
  in
  let* statement = strip_statement body in
  Ok (expr, statement)

(* Do statement *)

and strip_S_do_statement (node : Ast.do_statement wrap) : (S.statement option, _) result =
  error node "Do-while loops are not supported in JsLIGO."

(* Try statement *)

and strip_S_try_statement (node : Ast.try_statement wrap) : (S.statement option, _) result
  =
  error node "Exceptions are not supported in JsLIGO."

(* With statement *)

and strip_S_with_statement (node : Ast.with_statement wrap)
    : (S.statement option, _) result
  =
  ignore node;
  Error "With-statements are not supported in JsLIGO."

(* Break statement *)

and strip_S_break_statement (node : Ast.break_statement wrap)
    : (S.statement option, _) result
  =
  let Ast.{ kwd_break; stmt_id } = node#payload in
  match stmt_id with
  | Some ident -> error ident "Labels in breaks are not supported in JsLIGO."
  | None -> Ok (Some (S.S_break kwd_break#region))

(* Continue statement *)

and strip_S_continue_statement (node : Ast.continue_statement wrap)
    : (S.statement option, _) result
  =
  error node "Continue statements are not supported in JsLIGO."

(* Return statement *)

and strip_S_return_statement (node : Ast.return_statement wrap)
    : (S.statement option, _) result
  =
  let Ast.{ kwd_return = _; expressions } = node#payload in
  match expressions with
  | None -> Ok (Some (S.S_return (mk_reg node#region None)))
  | Some exprs ->
    let* exprs = strip_expressions exprs in
    (match exprs with
    | [] -> Ok (Some (S.S_return (mk_reg node#region None)))
    | [ expr ] -> Ok (Some (S.S_return (mk_reg node#region (Some expr))))
    | _ -> error node "Multiple values in return are not supported in JsLIGO.")

(* Throw statement *)

and strip_S_throw_statement (node : Ast.throw_statement wrap)
    : (S.statement option, _) result
  =
  error node "Exceptions are not supported in JsLIGO."

(* Empty statement *)

and strip_S_empty_statement (node : Region.t) : (S.statement option, _) result =
  ignore node;
  Ok None

(* Labeled statement *)

and strip_S_labeled_statement (node : Ast.labeled_statement wrap)
    : (S.statement option, _) result
  =
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

and strip_D_function_declaration (node : Ast.function_declaration wrap)
    : (S.declaration, _) result
  =
  let Ast.{ fun_sig; body } = node#payload in
  let (Ast.{ kwd_async; kwd_function; name; call_sig } : Ast.function_signature) =
    fun_sig
  in
  let* () =
    match kwd_async with
    | None -> Ok ()
    | Some kwd_async ->
      error_reg kwd_async#region "Asynchronicity is not supported in JsLIGO."
  in
  let comments = kwd_function#comments in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let fun_name = strip_identifier name in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig in
  let* fun_body = strip_statement_block body in
  let fun_decl =
    S.{ decorators; comments; fun_name; generics; parameters; rhs_type; fun_body }
  in
  Ok (S.D_function (mk_reg node#region fun_decl))

and strip_comments (node : Wrap.comment list) : S.comment list =
  let f comment =
    let region = Wrap.comment_to_region comment
    and contents = Wrap.comment_to_string comment in
    Wrap.make contents region
  in
  List.map ~f node

and extract_decorators (node : S.comment list) : S.decorator list =
  let filter comment =
    comment#region, Decorator.scan (Lexing.from_string comment#payload)
  in
  let decorators = List.map ~f:filter node in
  let rec clean = function
    | [] -> []
    | (_, None) :: decorators -> clean decorators
    | (region, Some decorator) :: decorators ->
      Wrap.make decorator region :: clean decorators
  in
  clean decorators

and strip_call_signature (node : Ast.call_signature) : (call_signature, _) result =
  let (Ast.{ type_parameters; parameters; return_type } : Ast.call_signature) = node in
  let* generics = strip_list_opt strip_type_parameters type_parameters in
  let* parameters = strip_formal_parameters parameters in
  let parameters = format_parameters_into_patterns parameters in
  let* rhs_type = map_opt strip_call_return_type return_type in
  Ok { generics; parameters; rhs_type }

and format_parameters_into_patterns (node : (S.variable * S.type_expr option) list)
    : S.parameter list
  =
  let make_parameter (variable, opt) =
    let path = Nonempty_list.singleton variable in
    S.P_var (mk_reg variable#region path), opt
  in
  List.map ~f:make_parameter node

and strip_call_return_type (node : Ast.call_return_type) : (S.type_expr, _) result =
  match node with
  | Ast.Type_annotation (_, type_expr) ->
    let* type_expr = strip_type_expr type_expr in
    Ok type_expr
  | Asserts_annotation a ->
    let region = Ast.region_of_asserts a in
    error_reg region "Assertions in return types are not supported in JsLIGO."
  | Type_predicate_annotation w -> error w "Type predicates are not supported in JsLIGO."

(* Generator function declaration *)

and strip_D_generator_function_declaration
    (node : Ast.generator_function_declaration wrap)
    : (S.declaration, _) result
  =
  error node "Generator functions are not supported in JsLIGO."

(* Class declaration *)

and strip_D_class_declaration (node : Ast.class_declaration wrap)
    : (S.declaration, _) result
  =
  ignore node;
  Error "TODO: strip_D_class_declaration"

(* Lexical declaration *)

and strip_D_lexical_declaration (node : Ast.lexical_declaration wrap)
    : (S.declaration, _) result
  =
  ignore node;
  Error "TODO: strip_D_lexical_declaration"

and strip_lexical_declaration (node : Ast.lexical_declaration wrap)
    : (S.declaration, _) result
  =
  ignore node;
  Error "TODO: strip_lexical_declaration"

(* Variable declaration *)

and strip_D_variable_declaration (node : Ast.variable_declaration wrap)
    : (S.declaration, _) result
  =
  error
    node
    "Variable declared with 'var' are not supported in JsLIGO."
    ~hint:"Use the 'let' modifier."

and strip_variable_declaration (node : Ast.variable_declaration wrap)
    : (S.declaration, _) result
  =
  strip_D_variable_declaration node

(* Function signature *)

and strip_D_function_signature (node : Ast.function_signature wrap)
    : (S.declaration, _) result
  =
  ignore node;
  Error "TODO: strip_D_function_signature"

(* Abstract class declaration *)

and strip_D_abstract_class_declaration (node : Ast.abstract_class_declaration wrap)
    : (S.declaration, _) result
  =
  error node "Abstract classes are not supported in JsLIGO."

(* Module declaration *)

and strip_D_module_declaration (node : Ast.module_declaration wrap)
    : (S.declaration, _) result
  =
  error node "Modules are not supported in JsLIGO." ~hint:"Try using namespaces."

(* Internal module declaration *)

and strip_D_internal_module (node : Ast.internal_module wrap) : (S.declaration, _) result =
  ignore node;
  Error "TODO: strip_D_internal_module_declaration"

(* Type alias declaration *)

and strip_D_type_alias_declaration (node : Ast.type_alias_declaration wrap)
    : (S.declaration, _) result
  =
  let type_decl, region = node#payload, node#region in
  let Ast.{ kwd_type = _; name; type_parameters; sym_equal = _; type_expr } = type_decl in
  let name = strip_type_identifier name in
  let* generics = strip_list_opt strip_type_parameters type_parameters in
  let* type_expr = strip_type_expr type_expr in
  let type_decl' = S.{ name; generics; type_expr } in
  Ok (S.D_type (mk_reg region type_decl'))

and strip_type_identifier (node : Ast.type_identifier) : S.variable = node

and strip_type_parameters (node : Ast.type_parameters) : (S.variable list, _) result =
  let (Ast.Chevrons type_params) = node in
  let type_params = type_params#payload.contents in
  Result.all @@ List.map ~f:strip_type_parameter type_params

and strip_type_parameter (node : Ast.type_parameter wrap) : (S.variable, _) result =
  let Ast.{ kwd_const = _; name; constraint_expr; default_type } = node#payload in
  match constraint_expr, default_type with
  | None, None -> Ok name
  | Some (_, type_expr), _ ->
    let region = Ast.region_of_type_expr type_expr in
    error_reg region "Constraints on type parameters are not supported in JsLIGO."
  | _, Some (_, type_expr) ->
    let region = Ast.region_of_type_expr type_expr in
    error_reg region "Defaults of type parameters are not supported in JsLIGO."

(* Enum declaration *)

and strip_D_enum_declaration (node : Ast.enum_declaration wrap)
    : (S.declaration, _) result
  =
  error node "Enumerated values are not supported in JsLIGO."

(* Interface declaration *)

and strip_D_interface_declaration (node : Ast.interface_declaration wrap)
    : (S.declaration, _) result
  =
  ignore node;
  Error "TODO: strip_D_interface_declaration"

(* Import alias *)

and strip_D_import_alias (node : Ast.import_alias wrap) : (S.declaration, _) result =
  let Ast.{ kwd_import = _; alias; sym_equal = _; aliased } = node#payload
  and region = node#region in
  let alias = strip_identifier alias in
  let path = strip_aliased aliased in
  let import = alias, path in
  Ok S.(D_import (S.Import_alias (mk_reg region import)))

and strip_aliased (node : Ast.aliased) : S.path =
  match node with
  | Ident ident ->
    let singleton = Nonempty_list.singleton (strip_identifier ident) in
    mk_reg ident#region singleton
  | Nested nested -> strip_nested_identifier nested

and strip_nested_identifier (node : Ast.nested_identifier wrap) : S.path =
  let path, selected = node#payload
  and region = node#region in
  let path = Nonempty_list.map ~f:strip_type_identifier path
  and selected = strip_type_identifier selected in
  mk_reg region (Nonempty_list.cons selected path)

(* Ambient declaration *)

and strip_D_ambient_declaration (node : Ast.ambient_declaration wrap)
    : (S.declaration, _) result
  =
  error node "Ambient declarations are not supported in JsLIGO."

(* TYPES *)

and strip_type_expr (node : Ast.type_expr) : (S.type_expr, _) result =
  match node with
  | T_primary_type t -> strip_T_primary_type t
  | T_function_type t -> strip_T_function_type t
  | T_readonly_type t -> strip_T_readonly_type t
  | T_constructor_type t -> strip_T_constructor_type t
  | T_infer_type t -> strip_T_infer_type t
  | T_member_expression t -> strip_T_type_query_member_expression_in_type_annotation t
  | T_call_expression t -> strip_T_type_query_call_expression_in_type_annotation t

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

and strip_T_parenthesized_type (node : Ast.type_expr Ast.parens) : (S.type_expr, _) result
  =
  let (Parens parens) = node in
  strip_type_expr parens#payload.contents

(* Predefined type *)

and strip_T_predefined_type (node : Ast.predefined_type) : (S.type_expr, _) result =
  match node with
  | T_any kwd_any -> error_reg kwd_any#region "The type 'any' is not supported in JsLIGO."
  | T_number kwd_number ->
    error_reg
      kwd_number#region
      "The type 'number' is not supported in JsLIGO."
      ~hint:"Use 'bigint' or 'nat'."
  | T_boolean kwd_boolean ->
    (* The pipeline uses "bool" instead *)
    let region = kwd_boolean#region in
    let bool = Wrap.make "bool" region in
    let path = mk_reg region (Nonempty_list.singleton bool) in
    Ok (T_var (mk_reg region (path, [])))
  | T_string kwd_string ->
    let region = kwd_string#region in
    let path = mk_reg region (Nonempty_list.singleton kwd_string) in
    Ok (T_var (mk_reg region (path, [])))
  | T_symbol kwd_symbol ->
    error_reg kwd_symbol#region "The type 'symbol' is not supported in JsLIGO."
  | T_unique_symbol kwd_unique_symbol ->
    error_reg kwd_unique_symbol#region "Type 'unique symbol' is not supported in JsLIGO."
  | T_void kwd_void -> error_reg kwd_void#region "Type 'void' is not supported in JsLIGO."
  | T_unknown kwd_unknown ->
    error_reg kwd_unknown#region "Type 'unknown' is not supported in JsLIGO."
  | T_never kwd_never ->
    error_reg kwd_never#region "Type 'never' is not supported in JsLIGO."
  | T_object kwd_object ->
    error_reg kwd_object#region "Type 'object' is not supported in JsLIGO"

(* Type identifier *)

and strip_T_type_identifier (node : Ast.type_identifier) : (S.type_expr, _) result =
  let type_ident = strip_type_identifier node in
  let region = node#region in
  let path = Nonempty_list.singleton type_ident in
  Ok (T_var (mk_reg region (mk_reg region path, [])))

(* Nested type identifier (access path is reversed) *)

and strip_T_nested_type_identifier (node : Ast.nested_type_identifier wrap)
    : (S.type_expr, _) result
  =
  let region = node#region in
  let path = strip_nested_type_identifier node in
  Ok (T_var (mk_reg region (path, [])))

and strip_nested_type_identifier (node : Ast.nested_type_identifier wrap) : S.path =
  let path, selected = node#payload
  and region = node#region in
  let path = Nonempty_list.map ~f:strip_type_identifier path
  and selected = strip_type_identifier selected in
  mk_reg region (Nonempty_list.cons selected path)

(* Generic type

   TODO: Test the order of the type arguments.
*)

and strip_T_generic_type (node : Ast.generic_type wrap) : (S.type_expr, _) result =
  let region = node#region
  and name, type_args = node#payload in
  let path = strip_generic_name name in
  let* type_args = strip_type_arguments type_args in
  Ok (S.T_var (mk_reg region (path, type_args)))

and strip_generic_name (node : Ast.generic_name) : S.path =
  match node with
  | Generic_type type_identifier ->
    let region = type_identifier#region in
    let ident = strip_type_identifier type_identifier in
    mk_reg region (Nonempty_list.singleton ident)
  | Generic_nested nested -> strip_nested_type_identifier nested

and strip_type_arguments (node : Ast.type_arguments) : (S.type_expr list, _) result =
  let (Chevrons chevrons) = node in
  let type_args = chevrons#payload.contents in
  let type_args = Nonempty_list.to_list type_args in
  Result.all @@ List.map ~f:strip_type_expr type_args

(* Object type *)

and strip_T_object_type (node : Ast.object_type) : (S.type_expr, _) result =
  ignore node;
  Error "TODO: strip_T_object_type"

(* Array type *)

and strip_T_array_type (node : Ast.array_type wrap) : (S.type_expr, _) result =
  error node "Array types are not supported in JsLIGO."

(* Tuple type *)

and strip_T_tuple_type (node : Ast.tuple_type) : (S.type_expr, _) result =
  let (Brackets brackets) = node in
  let members = brackets#payload.contents in
  let* members = Result.all @@ List.map ~f:strip_tuple_type_member members in
  Ok (S.T_tuple (mk_reg brackets#region members))

and strip_tuple_type_member (node : Ast.tuple_type_member) : (S.type_expr, _) result =
  let region = Ast.region_of_tuple_type_member node in
  match node with
  | Ast.Tuple_parameter _
  | Tuple_optional_parameter _
  | Tuple_optional_type _
  | Tuple_rest_type _ ->
    error_reg
      region
      "This tuple type member is not supported in JsLIGO."
      ~hint:"Use a single type expression."
  | Tuple_type type_expr -> strip_type_expr type_expr

(* Flow maybe type *)

and strip_T_flow_maybe_type (node : (Ast.sym_qmark * Ast.primary_type) wrap)
    : (S.type_expr, _) result
  =
  error node "Maybe types are not supported in JsLIGO."

(* Type query *)

and strip_T_type_query (node : (Ast.kwd_keyof * Ast.type_query) wrap)
    : (S.type_expr, _) result
  =
  error node "Type queries are not supported in JsLIGO."

(* Index type query *)

and strip_T_index_type_query (node : (Ast.kwd_keyof * Ast.primary_type) wrap)
    : (S.type_expr, _) result
  =
  error node "Index type queries are not supported in JsLIGO."

(* "This" as a type *)

and strip_T_this (node : Ast.kwd_this) : (S.type_expr, _) result =
  error node "Type 'this' is not supported by JsLIGO."

and strip_T_existential_type (node : Ast.sym_star) : (S.type_expr, _) result =
  error node "Existential types are not supported by JsLIGO."

(* Literal type *)

and strip_T_literal_type (node : Ast.literal_type) : (S.type_expr, _) result =
  match node with
  | T_unary_type t -> strip_T_unary_type t
  | T_number t -> strip_T_number t
  | T_string t -> strip_T_string t
  | T_true t -> strip_T_true t
  | T_false t -> strip_T_false t
  | T_null t -> strip_T_null t
  | T_undefined t -> strip_T_undefined t

and strip_T_unary_type (node : Ast.unary_expression wrap) : (S.type_expr, _) result =
  error node "Unary type are not supported in JsLIGO."

and strip_T_number (node : Ast.number) : (S.type_expr, _) result =
  let region = Ast.region_of_number node in
  match node with
  | Hex _ | Bin _ | Oct _ ->
    error_reg region "This number literal as a type is not supported by JsLIGO."
  | Dec (literal, _) ->
    let lexeme, q = literal#payload in
    if Z.equal (Q.den q) Z.one
    then (
      let z = Q.to_bigint q in
      let literal = Wrap.make (lexeme, z) literal#region in
      Ok (T_int literal))
    else error_reg region "Non-integer numbers as types are not supported by JsLIGO."

and strip_T_string (node : Ast.string_literal) : (S.type_expr, _) result =
  Ok (S.T_string node)

and strip_T_true (node : Ast.kwd_true) : (S.type_expr, _) result =
  error node "The singleton type 'true' is not supported by JsLIGO."

and strip_T_false (node : Ast.kwd_false) : (S.type_expr, _) result =
  error node "The singleton type 'false' is not supported by JsLIGO."

and strip_T_null (node : Ast.kwd_null) : (S.type_expr, _) result =
  error node "The type 'null' is not supported by JsLIGO."

and strip_T_undefined (node : Ast.kwd_undefined) : (S.type_expr, _) result =
  error node "The type 'undefined' is not supported by JsLIGO."

(* Lookup type *)

and strip_T_lookup_type (node : Ast.lookup_type wrap) : (S.type_expr, _) result =
  error node "Lookup types are not supported in JsLIGO."

(* Conditional type *)

and strip_T_conditional_type (node : Ast.conditional_type wrap) : (S.type_expr, _) result =
  error node "Conditional types are not supported in JsLIGO."

(* Template literal type *)

and strip_T_template_literal_type (node : Ast.template_literal_type wrap)
    : (S.type_expr, _) result
  =
  error node "Template literal type are not supported in JsLIGO."

(* Intersection type *)

and strip_T_intersection_type (node : Ast.intersection_type wrap)
    : (S.type_expr, _) result
  =
  error node "Intersection types are not supported in JsLIGO."

(* Union type *)

and strip_T_union_type (node : Ast.union_type wrap) : (S.type_expr, _) result =
  let region = node#region in
  let type_1_opt, _, type_2 = node#payload in
  let* type_2 = strip_type_expr type_2 in
  let* union_type =
    match type_1_opt with
    | None -> Ok (Nonempty_list.singleton type_2)
    | Some type_1 ->
      let* type_1 = strip_type_expr type_1 in
      Ok Nonempty_list.(type_1 :: [ type_2 ])
  in
  Ok (S.T_union (mk_reg region union_type))

(* Function type *)

and strip_T_function_type (node : Ast.function_type wrap) : (S.type_expr, _) result =
  let Ast.{ type_parameters; parameters; sym_arrow = _; return_type } = node#payload in
  let* t_params = strip_list_opt strip_type_parameters type_parameters in
  let* v_params = strip_formal_parameters parameters in
  let* v_params = filter_type_annotations v_params in
  let* ret_type = strip_return_type return_type in
  let fun_type = v_params, ret_type in
  let fun_type_reg =
    let (Ast.Parens parens) = parameters in
    Region.cover parens#region (Ast.region_of_return_type return_type)
  in
  let fun_type = S.T_fun (mk_reg fun_type_reg fun_type) in
  match t_params with
  | [] -> Ok fun_type
  | _ -> Ok (S.T_for_all (mk_reg node#region (t_params, fun_type)))

and filter_type_annotations (node : (S.variable * S.type_expr option) list)
    : ((S.variable * S.type_expr) list, _) result
  =
  let check = function
    | variable, None ->
      error_reg
        variable#region
        "Type annotations in function types are mandatory in JsLIGO."
    | variable, Some type_expr -> Ok (variable, type_expr)
  in
  Result.all @@ List.map ~f:check node

and strip_formal_parameters (node : Ast.formal_parameters)
    : ((S.variable * S.type_expr option) list, _) result
  =
  let (Ast.Parens parens) = node in
  let parameters = parens#payload.contents in
  Result.all @@ List.map ~f:strip_formal_parameter parameters

and strip_formal_parameter (node : Ast.formal_parameter wrap)
    : (S.variable * S.type_expr option, _) result
  =
  let Ast.{ parameter_name; optional; type_opt; default } = node#payload in
  let* parameter = strip_parameter_name parameter_name in
  let* () =
    match optional with
    | None -> Ok ()
    | Some sym_qmark ->
      error_reg sym_qmark#region "Optional parameters are not supported in JsLIGO."
  in
  let* type_expr =
    match type_opt with
    | None -> Ok None
    | Some (_, type_expr) ->
      let* type_expr = strip_type_expr type_expr in
      Ok (Some type_expr)
  in
  let* () =
    match default with
    | None -> Ok ()
    | Some (_, expr) ->
      let region = Ast.region_of_expression expr in
      error_reg region "Default parameter values are not supported in JsLIGO."
  in
  Ok (parameter, type_expr)

and strip_parameter_name (node : Ast.parameter_name wrap) : (S.variable, _) result =
  let Ast.{ decorators; access; kwd_override; kwd_readonly; pattern } = node#payload in
  let* () =
    match decorators with
    | [] -> Ok ()
    | decorator :: _ ->
      let region = Ast.region_of_decorator decorator in
      error_reg region "Decorators on function parameters are not supported in JsLIGO."
  in
  let* () =
    match access with
    | None -> Ok ()
    | Some modifier ->
      let region = Ast.region_of_accessibility_modifier modifier in
      error_reg
        region
        "Accessibility modifiers on function parameters are not supported in JsLIGO."
  in
  let* () =
    match kwd_override with
    | None -> Ok ()
    | Some kwd_override ->
      error_reg
        kwd_override#region
        "Override modifier on function parameters not supported in JsLIGO."
  in
  let* () =
    match kwd_readonly with
    | None -> Ok ()
    | Some kwd_readonly ->
      error_reg
        kwd_readonly#region
        "Read-only modifier on function parameters not supported in JsLIGO."
  in
  strip_parameter_pattern pattern

and strip_parameter_pattern (node : Ast.parameter_pattern) : (S.variable, _) result =
  match node with
  | Parameter_pattern (P_identifier ident) -> Ok (strip_identifier ident)
  | Parameter_pattern pattern ->
    let region = Ast.region_of_pattern pattern in
    error_reg region "Only variables are supported as function parameters in JsLIGO."
  | Parameter_this kwd_this ->
    error_reg
      kwd_this#region
      "The `this` identifier is not supported in JsLIGO"
      ~hint:"Rename it."

and strip_identifier (node : Ast.identifier) : S.variable = node

and strip_return_type (node : Ast.return_type) : (S.type_expr, _) result =
  let region = Ast.region_of_return_type node in
  match node with
  | Return_type type_expr -> strip_type_expr type_expr
  | Return_asserts _ -> error_reg region "Type assertion not supported in JsLIGO."
  | Return_type_predicate _ -> error_reg region "Type predicate not supported in JsLIGO."

(* Readonly type *)

and strip_T_readonly_type (node : Ast.readonly_type wrap) : (S.type_expr, _) result =
  error node "Read-only types are not supported in JsLIGO."

(* Constructor type *)

and strip_T_constructor_type (node : Ast.constructor_type wrap) : (S.type_expr, _) result =
  error node "Constructor types are not supported in JsLIGO."

(* Infer type *)

and strip_T_infer_type (node : Ast.infer_type wrap) : (S.type_expr, _) result =
  error node "Infer types are not supported in JsLIGO."

(* Member expression (in type expressions) *)

and strip_T_type_query_member_expression_in_type_annotation
    (node : Ast.type_query_member_expression_in_type_annotation wrap)
    : (S.type_expr, _) result
  =
  error node "Member expressions in type queries are not supported by JsLIGO."

(* Call expression (in type expressions) *)

and strip_T_type_query_call_expression_in_type_annotation
    (node : Ast.type_query_call_expression_in_type_annotation wrap)
    : (S.type_expr, _) result
  =
  error node "Call expressions in type queries are not supported in JsLIGO."

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

and strip_E_assignment_expression (node : Ast.assignment_expression wrap)
    : (S.expr, _) result
  =
  ignore node;
  Error "TODO: strip_E_assignment_expression"

(* Augmented assignment expression *)

and strip_E_augmented_assignment_expression
    (node : Ast.augmented_assignment_expression wrap)
    : (S.expr, _) result
  =
  ignore node;
  Error "TODO: strip_E_augmented_assignment_expression"

(* Await-expression *)

and strip_E_await_expression (node : Ast.await_expression wrap) : (S.expr, _) result =
  error node "Await-expressions are not supported in JsLIGO."

(* Binary expression *)

and strip_E_binary_expression (node : Ast.binary_expression wrap) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_binary_expression"

(* Instantiation expression *)

and strip_E_instantiation_expression (node : Ast.instantiation_expression wrap)
    : (S.expr, _) result
  =
  error node "Instantiation of type parameters is not supported in JsLIGO."

(* Internal module expression *)

and strip_E_internal_module (node : Ast.internal_module wrap) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_internal_module"

(* New-expression *)

and strip_E_new_expression (node : Ast.new_expression wrap) : (S.expr, _) result =
  error node "Instantiation of classes is not supported in JsLIGO."

(* Primary expression *)

and strip_E_primary_expression (node : Ast.primary_expression) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_primary_expression"

(* Statisfies-expression *)

and strip_E_satisfies_expression (node : Ast.satisfies_expression wrap)
    : (S.expr, _) result
  =
  error node "Type checks are not supported in JsLIGO."

(* Ternary expression *)

and strip_E_ternary_expression (node : Ast.ternary_expression wrap) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_ternary_expression"

(* Type assertion (expression) *)

and strip_E_type_assertion (node : Ast.type_assertion wrap) : (S.expr, _) result =
  error node "Type assertions are not supported in JsLIGO."

(* Unary expression *)

and strip_E_unary_expression (node : Ast.unary_expression wrap) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_unary_expression"

(* Update expression *)

and strip_E_update_expression (node : Ast.update_expression) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_update_expression"

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
  ignore node;
  Error "TODO: strip_P_member_expression"

(* Subscript expression (pattern) *)

and strip_P_subscript_expression (node : Ast.subscript_expression wrap)
    : (S.pattern, _) result
  =
  ignore node;
  Error "TODO: strip_P_subscript_expression"

(* Identifier and booleans (pattern) *)

and strip_P_identifier (node : Ast.identifier) : (S.pattern, _) result =
  let region = node#region
  and identifier = strip_identifier node in
  match identifier#payload with
  | "false" -> Ok (S.P_false region)
  | "true" -> Ok (S.P_true region)
  | _ ->
    let path = mk_reg region (Nonempty_list.singleton identifier) in
    Ok (S.P_var path)

(* Undefined (pattern) *)

and strip_P_undefined (node : Ast.kwd_undefined) : (S.pattern, _) result =
  error node "The undefined value is not supported in patterns in JsLIGO."

(* Destructuring pattern *)

and strip_P_destructuring_pattern (node : Ast.destructuring_pattern)
    : (S.pattern, _) result
  =
  ignore node;
  Error "TODO: strip_P_destructuring_pattern"

and strip_destructuring_pattern (node : Ast.destructuring_pattern) : (S.pattern, _) result
  =
  match node with
  | Pattern_object p ->
    let* pattern = strip_object_pattern p in
    Ok (S.P_object pattern)
  | Pattern_array p ->
    let* pattern = strip_array_pattern p in
    Ok (S.P_array pattern)

and strip_object_pattern (node : Ast.object_pattern) : (S.pattern S._object, _) result =
  ignore node;
  Error "TODO: strip_object_pattern"

and strip_array_pattern (node : Ast.array_pattern) : (S.pattern S._array, _) result =
  ignore node;
  Error "TODO: strip_array_pattern"

(* Non-null expression (pattern) *)

and strip_P_non_null_expression (node : Ast.expression) : (S.pattern, _) result =
  let region = Ast.region_of_expression node in
  error_reg region "Non-null patterns are not supported in JsLIGO."

(* Rest pattern *)

and strip_P_rest_pattern (node : Ast.rest_pattern wrap) : (S.pattern, _) result =
  ignore node;
  Error "TODO: strip_P_rest_pattern"

(* Alias for external access by means of [Strip.statements] *)

let statements = strip_statements
