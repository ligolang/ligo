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

type parameters =
  | Parameter of S.variable
  | Call_signature of call_signature reg

type for_header =
  { index_kind : S.var_kind option
  ; index : S.key * S.value option
  ; expr : S.expr
  }

(* Filters *)

let filter_out_async (node : Ast.kwd_async option) : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some kwd_async -> Strip_err.(make kwd_async#region Asynchronicity)

let filter_out_await (node : Ast.kwd_await option) : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some kwd_await -> Strip_err.(make kwd_await#region Asynchronicity)

(* Stripping *)

let rec strip_statements (node : Ast.statements) : (S.t, _) result =
  match node with
  | None -> Ok []
  | Some stmts -> strip_statement_list @@ Nonempty_list.to_list stmts#payload

and strip_statement_list (node : Ast.statement list) : (S.statement list, _) result =
  let f acc stmt =
    let* stmt' = strip_statement stmt in
    Ok (stmt' :: acc)
  in
  let* stmts = List.fold_result ~f ~init:[] node in
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
  Strip_err.(make node#region Debugger_statement)

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
  | _ -> Strip_err.(make node#region Multiple_values)

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
      let region = Ast.region_of_parens condition in
      Strip_err.(make region Multiple_values)
  in
  let* if_so = strip_statement consequence in
  let* if_so =
    match if_so with
    | None ->
      let region = Ast.region_of_statement consequence in
      Strip_err.(make region Empty_consequence)
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
  let* stmt = strip_switch_statement node in
  Ok (Some (S.S_switch (mk_reg node#region stmt)))

and strip_switch_statement (node : Ast.switch_statement wrap) : (S.switch_stmt, _) result =
  let Ast.{ kwd_switch; value; body } = node#payload in
  let* exprs = strip_parenthesized_expression value in
  let* expr =
    match exprs with
    | [ expr ] -> Ok expr
    | _ -> Strip_err.(make kwd_switch#region Multiple_values)
  in
  let* cases = strip_switch_body body in
  Ok (expr, cases)

and strip_switch_body (node : Ast.switch_body) : (S.cases, _) result =
  let (Ast.Braces braces) = node in
  let entries = braces#payload.contents in
  let filter case (case_acc, default_acc) =
    match case with
    | Ast.Switch_case case -> case :: case_acc, default_acc
    | Switch_default default -> case_acc, default :: default_acc
  in
  let cases, defaults = List.fold_right entries ~init:([], []) ~f:filter in
  let* cases = Result.all @@ List.map ~f:strip_switch_case cases in
  match defaults with
  | [] -> Ok (cases, [])
  | [ default ] ->
    let* default = strip_switch_default default in
    Ok (cases, default)
  | _ :: default :: _ -> Strip_err.(make default#region Multiple_defaults)

and strip_switch_case (node : Ast.switch_case wrap) : (S.switch_case, _) result =
  let Ast.{ kwd_case = _; value; body } = node#payload in
  match value#payload with
  | Nonempty_list.[ expr ] ->
    let* expr = strip_expression expr in
    let* body = strip_statement_list body in
    Ok (expr, body)
  | _ :: expr :: _ ->
    let region = Ast.region_of_expression expr in
    Strip_err.(make region Multiple_values)

and strip_switch_default (node : Ast.switch_default wrap) : (S.switch_default, _) result =
  let Ast.{ kwd_default = _; statements } = node#payload in
  let* statements = strip_statement_list statements in
  Ok statements

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
    let* declaration = strip_D_lexical_declaration decl in
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
  let* () = filter_out_await kwd_await in
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
      Strip_err.(make kwd_of#region Range_over_keys ~hint:"Try using 'in' instead.")
  in
  let* index_kind, index = strip_for_range range in
  let* exprs = strip_expressions collection in
  let* expr =
    match exprs with
    | [ expr ] -> Ok expr
    | _ -> Strip_err.(make in_region Multiple_values)
  in
  Ok { index_kind; index; expr }

and strip_for_range (node : Ast.for_range)
    : (S.var_kind option * (S.key * S.value option), _) result
  =
  match node with
  | For_in_expression (Identifier v) -> Ok (None, (strip_identifier v, None))
  | For_in_expression e ->
    let region = Ast.region_of_lhs_expression e in
    Strip_err.(make region Invalid_loop_index)
  | For_in_parenthesized e ->
    let region = Ast.region_of_parens e in
    Strip_err.(make region Invalid_loop_index)
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
      | _ -> Strip_err.(make region Invalid_loop_index))
    | _ -> Strip_err.(make region Invalid_loop_index))

and force_single_var (node : S.pattern S.element) : (S.variable, _) result =
  match node with
  | Element (P_var path as pattern) ->
    (match path.Region.value with
    | Nonempty_list.[ variable ] -> Ok variable
    | _ ->
      let region = S.region_of_pattern pattern in
      Strip_err.(make region Not_a_variable))
  | Element pattern | Spread pattern ->
    let region = S.region_of_pattern pattern in
    Strip_err.(make region Not_a_variable)

and strip_for_in_var (node : Ast.for_in_var) =
  let Ast.{ kwd_var; variable = _; default = _ } = node in
  Strip_err.(make kwd_var#region Var_declaration ~hint:"Use 'let' or 'const'.")

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
    | _ -> Strip_err.(make kwd_while#region Multiple_values)
  in
  let* statement = strip_statement body in
  Ok (expr, statement)

(* Do statement *)

and strip_S_do_statement (node : Ast.do_statement wrap) : (S.statement option, _) result =
  Strip_err.(make node#region Do_while_loop)

(* Try statement *)

and strip_S_try_statement (node : Ast.try_statement wrap) : (S.statement option, _) result
  =
  Strip_err.(make node#region Exception)

(* With statement *)

and strip_S_with_statement (node : Ast.with_statement wrap)
    : (S.statement option, _) result
  =
  Strip_err.(make node#region With_statement)

(* Break statement *)

and strip_S_break_statement (node : Ast.break_statement wrap)
    : (S.statement option, _) result
  =
  let Ast.{ kwd_break; stmt_id } = node#payload in
  match stmt_id with
  | Some ident -> Strip_err.(make ident#region Label)
  | None -> Ok (Some (S.S_break kwd_break#region))

(* Continue statement *)

and strip_S_continue_statement (node : Ast.continue_statement wrap)
    : (S.statement option, _) result
  =
  Strip_err.(make node#region Continue)

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
    | _ -> Strip_err.(make node#region Multiple_values))

(* Throw statement *)

and strip_S_throw_statement (node : Ast.throw_statement wrap)
    : (S.statement option, _) result
  =
  Strip_err.(make node#region Exception)

(* Empty statement *)

and strip_S_empty_statement (node : Region.t) : (S.statement option, _) result =
  ignore node;
  Ok None

(* Labeled statement *)

and strip_S_labeled_statement (node : Ast.labeled_statement wrap)
    : (S.statement option, _) result
  =
  Strip_err.(make node#region Label)

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
  let* () = filter_out_async kwd_async in
  let comments = kwd_function#comments in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let fun_name = strip_identifier name in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
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

and strip_call_signature (node : Ast.call_signature wrap) : (call_signature reg, _) result
  =
  let (Ast.{ type_parameters; parameters; return_type } : Ast.call_signature) =
    node#payload
  in
  let* generics = strip_list_opt strip_type_parameters type_parameters in
  let* parameters = strip_formal_parameters parameters in
  let parameters = format_parameters_into_patterns parameters in
  let* rhs_type = map_opt strip_call_return_type return_type in
  let call_sig = { generics; parameters; rhs_type } in
  Ok (mk_reg node#region call_sig)

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
    Strip_err.(make region Type_assertion)
  | Type_predicate_annotation w -> Strip_err.(make w#region Type_predicate)

(* Generator function declaration *)

and strip_D_generator_function_declaration
    (node : Ast.generator_function_declaration wrap)
    : (S.declaration, _) result
  =
  Strip_err.(make node#region Generator)

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
  let* decl = strip_lexical_declaration node in
  Ok (S.D_value decl)

and strip_lexical_declaration (node : Ast.lexical_declaration wrap)
    : (S.value_decl reg, _) result
  =
  let Ast.{ kind; decls } = node#payload in
  let kind, comments =
    match kind with
    | Ast.Let kwd_let -> `Let kwd_let#region, kwd_let#comments
    | Const kwd_const -> `Const kwd_const#region, kwd_const#comments
  in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let* bindings = strip_variable_declarators decls in
  let value_decl = S.{ decorators; comments; kind; bindings } in
  Ok (mk_reg node#region value_decl)

and strip_variable_declarators (node : Ast.variable_declarator Ast.ne_list)
    : (S.val_binding reg Nonempty_list.t, _) result
  =
  let (var_decl :: var_decls) = node in
  let* var_decl = strip_variable_declarator var_decl in
  let* var_decls = Result.all @@ List.map ~f:strip_variable_declarator var_decls in
  Ok Nonempty_list.(var_decl :: var_decls)

and strip_variable_declarator (node : Ast.variable_declarator)
    : (S.val_binding reg, _) result
  =
  match node with
  | Var_decl lhs -> strip_var_decl_lhs lhs
  | Var_decl_assertion (_, sym_qmark, _) ->
    Strip_err.(make sym_qmark#region Definite_asgmt_assertion)

and strip_var_decl_lhs (node : Ast.var_decl_lhs wrap) : (S.val_binding reg, _) result =
  let Ast.{ var_names; var_type; default } = node#payload in
  let* pattern =
    match var_names with
    | Decl_ident ident ->
      let path = Nonempty_list.singleton (strip_identifier ident) in
      Ok (S.P_var (mk_reg ident#region path))
    | Decl_pattern p -> strip_destructuring_pattern p
  in
  let* rhs_type = map_opt strip_type_annotation var_type in
  let* rhs_expr =
    match default with
    | None ->
      let region = Ast.region_of_lhs_pattern var_names in
      Strip_err.(make region Unitialised_variable)
    | Some (_, expr) ->
      let* expr = strip_expression expr in
      Ok expr
  in
  let val_binding = S.{ pattern; rhs_type; rhs_expr } in
  Ok (mk_reg node#region val_binding)

and strip_type_annotation (node : Ast.type_annotation) : (S.type_expr, _) result =
  strip_type_expr @@ snd node

(* Variable declaration *)

and strip_D_variable_declaration (node : Ast.variable_declaration wrap)
    : (S.declaration, _) result
  =
  Strip_err.(make node#region Var_declaration ~hint:"Use the 'let' modifier.")

and strip_variable_declaration (node : Ast.variable_declaration wrap)
    : (S.declaration, _) result
  =
  strip_D_variable_declaration node

(* Function signature

   The function signature

   {@js[function f <T>(x: T) : T;]}

   is transformed internally here into the transform of the following
   type declaration:

   {@js[type f = <T>(x: T) => T;]}
 *)

and strip_D_function_signature (node : Ast.function_signature wrap)
    : (S.declaration, _) result
  =
  let (Ast.{ kwd_async; kwd_function = _; name; call_sig } : Ast.function_signature) =
    node#payload
  in
  let* () = filter_out_async kwd_async in
  let name = strip_identifier name in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* v_params = Result.all @@ List.map ~f:filter_parameter parameters in
  let* v_params = filter_type_annotations v_params in
  let* rhs_type =
    match rhs_type with
    | None -> Strip_err.(make node#region Return_type_absent)
    | Some rhs_type -> Ok rhs_type
  in
  let fun_type = v_params, rhs_type in
  let type_expr = S.T_fun (mk_reg call_sig.region fun_type) in
  let type_expr =
    match generics with
    | [] -> type_expr
    | _ -> S.T_for_all (mk_reg call_sig.region (generics, type_expr))
  in
  let type_decl = S.{ name; generics; type_expr } in
  Ok (S.D_type (mk_reg node#region type_decl))

and filter_parameter (node : S.parameter) : (S.variable * S.type_expr option, _) result =
  let pattern, type_expr = node in
  match pattern with
  | S.P_var path ->
    (match path.Region.value with
    | Nonempty_list.[ variable ] -> Ok (variable, type_expr)
    | _ ->
      let region = S.region_of_pattern pattern in
      Strip_err.(make region Not_a_variable))
  | _ -> Strip_err.(make (S.region_of_pattern pattern) Not_a_variable)

(* Abstract class declaration *)

and strip_D_abstract_class_declaration (node : Ast.abstract_class_declaration wrap)
    : (S.declaration, _) result
  =
  Strip_err.(make node#region Abstract_class)

(* Module declaration *)

and strip_D_module_declaration (node : Ast.module_declaration wrap)
    : (S.declaration, _) result
  =
  Strip_err.(make node#region Module ~hint:"Try using namespaces.")

(* Namespace declaration *)

and strip_D_internal_module (node : Ast.internal_module wrap) : (S.declaration, _) result =
  let* namespace_decl = strip_internal_module node in
  Ok (S.D_namespace namespace_decl)

and strip_internal_module (node : Ast.internal_module wrap)
    : (S.namespace_decl reg, _) result
  =
  let Ast.{ kwd_namespace = _; module_name; module_body } = node#payload in
  let* (namespace_name : S.variable) = strip_module_name module_name in
  let* (statements : S.statement list) =
    match module_body with
    | None -> Ok []
    | Some block ->
      let* stmts = strip_statement_block block in
      Ok (stmts.Region.value : S.statement list)
  in
  let decl = namespace_name, statements in
  Ok (mk_reg node#region decl)

and strip_module_name (node : Ast.module_name) : (S.variable, _) result =
  match node with
  | Module_string str -> Strip_err.(make str#region Namespace_string)
  | Module_ident ident -> Ok ident
  | Module_nested nested -> Strip_err.(make nested#region Namespace_nested)

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
    Strip_err.(make region Type_constraint)
  | _, Some (_, type_expr) ->
    let region = Ast.region_of_type_expr type_expr in
    Strip_err.(make region Default_type_parameter)

(* Enum declaration *)

and strip_D_enum_declaration (node : Ast.enum_declaration wrap)
    : (S.declaration, _) result
  =
  Strip_err.(make node#region Enumerated)

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
  Strip_err.(make node#region Ambient_declaration)

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
  | T_any kwd_any -> Strip_err.(make kwd_any#region Any_type)
  | T_number kwd_number ->
    Strip_err.(make kwd_number#region Number_type ~hint:"Use 'bigint' or 'nat'.")
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
  | T_symbol kwd_symbol -> Strip_err.(make kwd_symbol#region Symbol_type)
  | T_unique_symbol kwd_unique_symbol ->
    Strip_err.(make kwd_unique_symbol#region Unique_symbol_type)
  | T_void kwd_void -> Strip_err.(make kwd_void#region Void_type)
  | T_unknown kwd_unknown -> Strip_err.(make kwd_unknown#region Unknown_type)
  | T_never kwd_never -> Strip_err.(make kwd_never#region Never_type)
  | T_object kwd_object -> Strip_err.(make kwd_object#region Object_type)

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
  Strip_err.(make node#region Array_type)

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
    Strip_err.(make region Unsupported_tuple_member ~hint:"Use a single type expression.")
  | Tuple_type type_expr -> strip_type_expr type_expr

(* Flow maybe type *)

and strip_T_flow_maybe_type (node : (Ast.sym_qmark * Ast.primary_type) wrap)
    : (S.type_expr, _) result
  =
  Strip_err.(make node#region Maybe_type)

(* Type query *)

and strip_T_type_query (node : (Ast.kwd_keyof * Ast.type_query) wrap)
    : (S.type_expr, _) result
  =
  Strip_err.(make node#region Type_query)

(* Index type query *)

and strip_T_index_type_query (node : (Ast.kwd_keyof * Ast.primary_type) wrap)
    : (S.type_expr, _) result
  =
  Strip_err.(make node#region Index_type_query)

(* "This" as a type *)

and strip_T_this (node : Ast.kwd_this) : (S.type_expr, _) result =
  Strip_err.(make node#region This)

and strip_T_existential_type (node : Ast.sym_star) : (S.type_expr, _) result =
  Strip_err.(make node#region Existential_type)

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
  Strip_err.(make node#region Unary_type)

and strip_T_number (node : Ast.number) : (S.type_expr, _) result =
  let region = Ast.region_of_number node in
  match node with
  | Hex _ | Bin _ | Oct _ ->
    Strip_err.(make region Unsupported_number ~hint:"Use a decimal.")
  | Dec (literal, _) ->
    let lexeme, q = literal#payload in
    if Z.equal (Q.den q) Z.one
    then (
      let literal = Wrap.make (lexeme, Q.to_bigint q) literal#region in
      Ok (T_int literal))
    else Strip_err.(make region Non_integer_as_type)

and strip_T_string (node : Ast.string_literal) : (S.type_expr, _) result =
  Ok (S.T_string node)

and strip_T_true (node : Ast.kwd_true) : (S.type_expr, _) result =
  Strip_err.(make node#region Singleton_type_true)

and strip_T_false (node : Ast.kwd_false) : (S.type_expr, _) result =
  Strip_err.(make node#region Singleton_type_false)

and strip_T_null (node : Ast.kwd_null) : (S.type_expr, _) result =
  Strip_err.(make node#region Null_type)

and strip_T_undefined (node : Ast.kwd_undefined) : (S.type_expr, _) result =
  Strip_err.(make node#region Undefined_type)

(* Lookup type *)

and strip_T_lookup_type (node : Ast.lookup_type wrap) : (S.type_expr, _) result =
  Strip_err.(make node#region Lookup_type)

(* Conditional type *)

and strip_T_conditional_type (node : Ast.conditional_type wrap) : (S.type_expr, _) result =
  Strip_err.(make node#region Conditional_type)

(* Template literal type *)

and strip_T_template_literal_type (node : Ast.template_literal_type wrap)
    : (S.type_expr, _) result
  =
  Strip_err.(make node#region Template_literal_type)

(* Intersection type *)

and strip_T_intersection_type (node : Ast.intersection_type wrap)
    : (S.type_expr, _) result
  =
  Strip_err.(make node#region Intersection_type)

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
    | variable, None -> Strip_err.(make variable#region Missing_type)
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
    | Some sym_qmark -> Strip_err.(make sym_qmark#region Optional_parameter)
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
      Strip_err.(make region Default_argument)
  in
  Ok (parameter, type_expr)

and strip_parameter_name (node : Ast.parameter_name wrap) : (S.variable, _) result =
  let Ast.{ decorators; access; kwd_override; kwd_readonly; pattern } = node#payload in
  let* () =
    match decorators with
    | [] -> Ok ()
    | decorator :: _ ->
      let region = Ast.region_of_decorator decorator in
      Strip_err.(make region Decorated_parameter)
  in
  let* () =
    match access with
    | None -> Ok ()
    | Some modifier ->
      let region = Ast.region_of_accessibility_modifier modifier in
      Strip_err.(make region Access_parameter)
  in
  let* () =
    match kwd_override with
    | None -> Ok ()
    | Some kwd_override -> Strip_err.(make kwd_override#region Override_parameter)
  in
  let* () =
    match kwd_readonly with
    | None -> Ok ()
    | Some kwd_readonly -> Strip_err.(make kwd_readonly#region Readonly_parameter)
  in
  strip_parameter_pattern pattern

and strip_parameter_pattern (node : Ast.parameter_pattern) : (S.variable, _) result =
  match node with
  | Parameter_pattern (P_identifier ident) -> Ok (strip_identifier ident)
  | Parameter_pattern pattern ->
    let region = Ast.region_of_pattern pattern in
    Strip_err.(make region Non_variable_parameter)
  | Parameter_this kwd_this ->
    Strip_err.(make kwd_this#region Non_variable_parameter ~hint:"Rename 'this'.")

and strip_identifier (node : Ast.identifier) : S.variable = node

and strip_return_type (node : Ast.return_type) : (S.type_expr, _) result =
  let region = Ast.region_of_return_type node in
  match node with
  | Return_type type_expr -> strip_type_expr type_expr
  | Return_asserts _ -> Strip_err.(make region Type_assertion)
  | Return_type_predicate _ -> Strip_err.(make region Type_predicate)

(* Readonly type *)

and strip_T_readonly_type (node : Ast.readonly_type wrap) : (S.type_expr, _) result =
  Strip_err.(make node#region Readonly_type)

(* Constructor type *)

and strip_T_constructor_type (node : Ast.constructor_type wrap) : (S.type_expr, _) result =
  Strip_err.(make node#region Constructor_type)

(* Infer type *)

and strip_T_infer_type (node : Ast.infer_type wrap) : (S.type_expr, _) result =
  Strip_err.(make node#region Conditional_type)

(* Member expression (in type expressions) *)

and strip_T_type_query_member_expression_in_type_annotation
    (node : Ast.type_query_member_expression_in_type_annotation wrap)
    : (S.type_expr, _) result
  =
  Strip_err.(make node#region Type_query)

(* Call expression (in type expressions) *)

and strip_T_type_query_call_expression_in_type_annotation
    (node : Ast.type_query_call_expression_in_type_annotation wrap)
    : (S.type_expr, _) result
  =
  Strip_err.(make node#region Type_query)

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
  | As_const kwd_const -> Strip_err.(make kwd_const#region Constant_type)

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
  let Ast.{ left; operator; right } = node#payload in
  let* lhs = strip_augmented_assignment_lhs left in
  let* rhs = strip_expression right in
  let* op = strip_assignment_operator operator in
  Ok (op (mk_reg node#region (lhs, rhs)))

and strip_augmented_assignment_lhs (node : Ast.augmented_assignment_lhs)
    : (S.expr, _) result
  =
  match node with
  | Member_expression w -> Strip_err.(make w#region Complex_lhs ~hint:"Use a variable.")
  | Subscript_expression w ->
    Strip_err.(make w#region Complex_lhs ~hint:"Use a variable.")
  | Identifier ident ->
    let path = Nonempty_list.singleton (strip_identifier ident) in
    Ok (S.E_var (mk_reg ident#region path))
  | Parenthesized_expression expr ->
    let* exprs = strip_parenthesized_expression expr in
    let* expr =
      match exprs with
      | [ expr ] -> Ok expr
      | _ ->
        let region = Ast.region_of_augmented_assignment_lhs node in
        Strip_err.(make region Multiple_values)
    in
    Ok expr

and strip_assignment_operator (node : Ast.assignment_operator)
    : ((S.expr * S.expr) reg -> S.expr, _) result
  =
  match node with
  | Add_eq _ -> Ok (fun args -> S.E_add_eq args) (* += *)
  | Sub_eq _ -> Ok (fun args -> S.E_sub_eq args) (* -= *)
  | Mult_eq _ -> Ok (fun args -> S.E_mult_eq args) (* *= *)
  | Div_eq _ -> Ok (fun args -> S.E_div_eq args) (* /= *)
  | Rem_eq _ -> Ok (fun args -> S.E_rem_eq args) (* %= *)
  | Bit_xor_eq _ -> Ok (fun args -> S.E_bit_xor_eq args) (* ^= *)
  | Bit_and_eq _ -> Ok (fun args -> S.E_bit_and_eq args) (* &= *)
  | Bit_or_eq _ -> Ok (fun args -> S.E_bit_or_eq args) (* |= *)
  | Bit_sr_eq _ -> Ok (fun args -> S.E_bit_sr_eq args) (* >>= *)
  | Bit_usr_eq sym -> Strip_err.(make sym#region Bit_usr_eq) (* >>>= *)
  | Bit_sl_eq _ -> Ok (fun args -> S.E_bit_sl_seq args) (* <<= *)
  | Exp_eq sym -> Strip_err.(make sym#region Exp_eq) (* **= *)
  | Log_and_eq sym ->
    (* &&= *)
    Strip_err.(make sym#region Log_and_eq ~hint:"Use \"=\" and \"&&\" separately.")
  | Log_or_eq sym ->
    (* ||= *)
    Strip_err.(make sym#region Log_or_eq ~hint:"Use \"=\" and \"||\" separately.")
  | Non_null_eq sym ->
    (* ??= *)
    Strip_err.(make sym#region Non_null_eq)

(* Await-expression *)

and strip_E_await_expression (node : Ast.await_expression wrap) : (S.expr, _) result =
  Strip_err.(make node#region Asynchronicity)

(* Binary expression *)

and strip_E_binary_expression (node : Ast.binary_expression wrap) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_binary_expression"

(* Instantiation expression *)

and strip_E_instantiation_expression (node : Ast.instantiation_expression wrap)
    : (S.expr, _) result
  =
  Strip_err.(make node#region Type_parameter_instantiation)

(* Internal module expression *)

and strip_E_internal_module (node : Ast.internal_module wrap) : (S.expr, _) result =
  Strip_err.(make node#region Namespace_expression)

(* New-expression *)

and strip_E_new_expression (node : Ast.new_expression wrap) : (S.expr, _) result =
  Strip_err.(make node#region Class_instantiation)

(* Primary expression *)

and strip_E_primary_expression (node : Ast.primary_expression) : (S.expr, _) result =
  match node with
  | E_array expr -> strip_E_array expr
  | E_arrow_function expr -> strip_E_arrow_function expr
  | E_call_expression expr -> strip_E_call_expression expr
  | E_class expr -> strip_E_class expr
  | E_false expr -> strip_E_false expr
  | E_function_expression expr -> strip_E_function_expression expr
  | E_generator_function expr -> strip_E_generator_function expr
  | E_identifier expr -> strip_E_identifier expr
  | E_member_expression expr -> strip_E_member_expression expr
  | E_meta_property expr -> strip_E_meta_property expr
  | E_non_null_expression expr -> strip_E_non_null_expression expr
  | E_null expr -> strip_E_null expr
  | E_number expr -> strip_E_number expr
  | E_object expr -> strip_E_object expr
  | E_parenthesized_expression expr -> strip_E_parenthesized_expression expr
  | E_regex expr -> strip_E_regex expr
  | E_string expr -> strip_E_string expr
  | E_subscript_expression expr -> strip_E_subscript_expression expr
  | E_super expr -> strip_E_super expr
  | E_template_string expr -> strip_E_template_string expr
  | E_this expr -> strip_E_this expr
  | E_true expr -> strip_E_true expr
  | E_undefined expr -> strip_E_undefined expr

(* Array expression *)

and strip_E_array (node : Ast.array) : (S.expr, _) result =
  let (Ast.Brackets brackets) = node in
  let list = brackets#payload.contents in
  let* array = Result.all @@ List.map ~f:strip_argument list in
  Ok (S.E_array (mk_reg brackets#region array))

and strip_argument (node : Ast.argument) : (S.expr S.element, _) result =
  match node with
  | Expression expr ->
    let* expr = strip_expression expr in
    Ok (S.Element expr)
  | Spread_element spread ->
    let _, expr = spread#payload in
    let* expr = strip_expression expr in
    Ok (S.Spread expr)

(* Arrow function (expression) *)

and strip_E_arrow_function (node : Ast.arrow_function wrap) : (S.expr, _) result =
  let* arrow_fun_expr = strip_arrow_function node in
  Ok (S.E_arrow_fun (mk_reg node#region arrow_fun_expr))

and strip_arrow_function (node : Ast.arrow_function wrap) : (S.arrow_fun_expr, _) result =
  let Ast.{ kwd_async; parameters; sym_arrow = _; body } = node#payload in
  let* () = filter_out_async kwd_async in
  let* parameters = strip_parameters parameters in
  let* generics = get_generics parameters in
  let* rhs_type = get_rhs_type parameters in
  let* parameters = get_parameters parameters in
  let* fun_body = strip_function_body body in
  Ok S.{ generics; parameters; rhs_type; fun_body }

and get_parameters (node : parameters) : (S.parameter list, _) result =
  match node with
  | Parameter variable ->
    let path = Nonempty_list.singleton variable in
    Ok [ S.P_var (mk_reg variable#region path), None ]
  | Call_signature call_sig ->
    let { generics = _; parameters; rhs_type = _ } = call_sig.value in
    Ok parameters

and get_generics (node : parameters) : (S.variable list, _) result =
  match node with
  | Parameter _ -> Ok []
  | Call_signature call_sig ->
    let { generics; parameters = _; rhs_type = _ } = call_sig.value in
    Ok generics

and get_rhs_type (node : parameters) : (S.type_expr option, _) result =
  match node with
  | Parameter _ -> Ok None
  | Call_signature call_sig ->
    let { generics = _; parameters = _; rhs_type } = call_sig.value in
    Ok rhs_type

and strip_function_body (node : Ast.function_body) : (S.fun_body, _) result =
  match node with
  | Expression expr ->
    let* expr = strip_expression expr in
    Ok (S.Expr_body expr)
  | Statement_block block ->
    let* statements = strip_statement_block block in
    Ok (S.Stmt_body statements)

and strip_parameters (node : Ast.parameters) : (parameters, _) result =
  match node with
  | Parameter ident -> Ok (Parameter (strip_identifier ident))
  | Call_signature call_sig ->
    let* call_sig = strip_call_signature call_sig in
    Ok (Call_signature call_sig)

(* Call expression *)

and strip_E_call_expression (node : Ast.call_expression) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_call_expression"

(* Class (expression) *)

and strip_E_class (node : Ast.class_expression wrap) : (S.expr, _) result =
  Strip_err.(make node#region Class_expression)

(* False expression *)

and strip_E_false (node : Ast.kwd_false) : (S.expr, _) result = Ok (S.E_false node#region)

(* Function expression *)

and strip_E_function_expression (node : Ast.function_expression wrap) : (S.expr, _) result
  =
  let* function_expr = strip_function_expression node in
  Ok (S.E_function (mk_reg node#region function_expr))

and strip_function_expression (node : Ast.function_expression wrap)
    : (S.function_expr, _) result
  =
  let Ast.{ kwd_async; kwd_function = _; name; call_sig; body } = node#payload in
  let* () = filter_out_async kwd_async in
  let* () =
    match name with
    | None -> Ok ()
    | Some name -> Strip_err.(make name#region Named_lambda ~hint:"Declare a function.")
  in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* fun_body = strip_statement_block body in
  let fun_body = S.Stmt_body fun_body in
  Ok S.{ generics; parameters; rhs_type; fun_body }

(* Generator function (expression) *)

and strip_E_generator_function (node : Ast.generator_function wrap) : (S.expr, _) result =
  Strip_err.(make node#region Generator)

(* Identifier (expression) *)

and strip_E_identifier (node : Ast.identifier) : (S.expr, _) result =
  let path = Nonempty_list.singleton (strip_identifier node) in
  Ok (S.E_var (mk_reg node#region path))

(* Member expression *)

and strip_E_member_expression (node : Ast.member_expression wrap) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_member_expression"

(* Meta-property *)

and strip_E_meta_property (node : Ast.meta_property) : (S.expr, _) result =
  Strip_err.(make (Ast.region_of_meta_property node) Metaproperty)

(* Non-null expression *)

and strip_E_non_null_expression (node : Ast.expression) : (S.expr, _) result =
  Strip_err.(make (Ast.region_of_expression node) Non_null)

(* Null (expression) *)

and strip_E_null (node : Ast.kwd_null) : (S.expr, _) result =
  Strip_err.(make node#region Null_value)

(* Number (expression) *)

and strip_E_number (node : Ast.number) : (S.expr, _) result =
  match node with
  | Hex (hex, _) -> strip_hex hex
  | Bin (bin, _) -> Strip_err.(make bin#region Binary_octal)
  | Oct (oct, _) -> Strip_err.(make oct#region Binary_octal)
  | Dec (dec, _) -> strip_dec dec

and strip_hex (node : Ast.hex_literal) : (S.expr, _) result = Ok (S.E_bytes node)

and strip_dec (node : Ast.dec_literal) : (S.expr, _) result =
  let lexeme, q = node#payload in
  if Z.equal (Q.den q) Z.one
  then (
    let int = Wrap.make (lexeme, Q.to_bigint q) node#region in
    Ok (S.E_int int))
  else Strip_err.(make node#region Non_integer)

(* Object (expression) *)

and strip_E_object (node : Ast.object_expr) : (S.expr, _) result =
  ignore node;
  Error "TODO: strip_E_object"

(* Parenthesized expression *)

and strip_E_parenthesized_expression (node : Ast.parenthesized_expression)
    : (S.expr, _) result
  =
  let* exprs = strip_parenthesized_expression node in
  let* expr =
    match exprs with
    | [ expr ] -> Ok expr
    | _ -> Strip_err.(make (Ast.region_of_parens node) Multiple_values)
  in
  Ok expr

(* Regex *)

and strip_E_regex (node : Ast.string_literal) : (S.expr, _) result =
  Strip_err.(make node#region Regex)

(* String (expression) *)

and strip_E_string (node : Ast.string_literal) : (S.expr, _) result = Ok (S.E_string node)

(* Subscript expression *)

and strip_E_subscript_expression (node : Ast.subscript_expression wrap)
    : (S.expr, _) result
  =
  ignore node;
  Error "TODO: strip_E_subscript_expression"

(* Super (expression) *)

and strip_E_super (node : Ast.kwd_super) : (S.expr, _) result =
  Strip_err.(make node#region Super)

(* Template string *)

and strip_E_template_string (node : Ast.template_string wrap) : (S.expr, _) result =
  Strip_err.(make node#region Template_string)

(* This (expression) *)

and strip_E_this (node : Ast.kwd_this) : (S.expr, _) result =
  Strip_err.(make node#region This)

(* True (expression) *)

and strip_E_true (node : Ast.kwd_true) : (S.expr, _) result = Ok (S.E_true node#region)

(* Undefined (expression) *)

and strip_E_undefined (node : Ast.kwd_undefined) : (S.expr, _) result =
  Strip_err.(make node#region Undefined_value)

(* Statisfies-expression *)

and strip_E_satisfies_expression (node : Ast.satisfies_expression wrap)
    : (S.expr, _) result
  =
  Strip_err.(make node#region Type_check)

(* Ternary expression *)

and strip_E_ternary_expression (node : Ast.ternary_expression wrap) : (S.expr, _) result =
  let Ast.{ condition; sym_qmark = _; consequence; sym_colon = _; alternative } =
    node#payload
  in
  let* condition = strip_expression condition in
  let* truthy = strip_expression consequence in
  let* falsy = strip_expression alternative in
  Ok (S.E_ternary (mk_reg node#region S.{ condition; truthy; falsy }))

(* Type assertion (expression) *)

and strip_E_type_assertion (node : Ast.type_assertion wrap) : (S.expr, _) result =
  Strip_err.(make node#region Type_assertion)

(* Unary expression *)

and strip_E_unary_expression (node : Ast.unary_expression wrap) : (S.expr, _) result =
  let (Ast.{ operator; argument } : Ast.unary_expression) = node#payload in
  let* expr = strip_expression argument in
  let expr = mk_reg node#region expr in
  let* op = strip_unary_operator operator in
  Ok (op expr)

and strip_unary_operator (node : Ast.unary_operator) : (S.expr reg -> S.expr, _) result =
  match node with
  | Bang _ -> Ok (fun arg -> S.E_not arg) (* !x *)
  | Not _ -> Ok (fun arg -> S.E_bit_neg arg) (* ~x *)
  | Unary_sub _ -> Ok (fun arg -> S.E_neg arg) (* -x *)
  | Unary_add sym -> Strip_err.(make sym#region Unary_add) (* +x *)
  | Typeof kwd_typeof ->
    (* typeof x *)
    Strip_err.(make kwd_typeof#region Typeof_void_delete)
  | Void kwd_void ->
    (* void *)
    Strip_err.(make kwd_void#region Typeof_void_delete)
  | Delete kwd_delete ->
    (* delete *)
    Strip_err.(make kwd_delete#region Typeof_void_delete)

(* Update expression *)

and strip_E_update_expression (node : Ast.update_expression) : (S.expr, _) result =
  match node with
  | Update_postfix update -> strip_update `Post update
  | Update_prefix update -> strip_update `Pre update

and strip_update (kind : [ `Pre | `Post ]) (node : Ast.update wrap) : (S.expr, _) result =
  let (Ast.{ argument; operator } : Ast.update) = node#payload in
  let* expr = strip_expression argument in
  let* var =
    match expr with
    | S.E_var path ->
      (match path.value with
      | Nonempty_list.[ variable ] -> Ok (mk_reg path.region variable)
      | _ ->
        Strip_err.(make node#region Not_a_variable ~hint:"Define a temporary variable."))
    | _ ->
      Strip_err.(make node#region Not_a_variable ~hint:"Define a temporary variable.")
  in
  match kind, operator with
  | `Pre, Increment _ -> Ok (S.E_pre_incr var)
  | `Pre, Decrement _ -> Ok (S.E_pre_decr var)
  | `Post, Increment _ -> Ok (S.E_post_incr var)
  | `Post, Decrement _ -> Ok (S.E_post_decr var)

(* Yield-expression *)

and strip_E_yield_expression (node : Ast.yield_expression) : (S.expr, _) result =
  let region = Ast.region_of_yield_expression node in
  Strip_err.(make region Generator)

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
  Strip_err.(make node#region Member_pattern ~hint:"Use a variable.")

(* Subscript expression (pattern) *)

and strip_P_subscript_expression (node : Ast.subscript_expression wrap)
    : (S.pattern, _) result
  =
  Strip_err.(make node#region Subscript_pattern)

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
  Strip_err.(make node#region Undefined_value)

(* Destructuring pattern *)

and strip_P_destructuring_pattern (node : Ast.destructuring_pattern)
    : (S.pattern, _) result
  =
  strip_destructuring_pattern node

and strip_destructuring_pattern (node : Ast.destructuring_pattern) : (S.pattern, _) result
  =
  match node with
  | Pattern_object p ->
    let* pattern = strip_object_pattern p in
    Ok (S.P_object pattern)
  | Pattern_array p ->
    let* pattern = strip_array_pattern p in
    Ok (S.P_array pattern)

(* Object pattern *)

and strip_object_pattern (node : Ast.object_pattern) : (S.pattern S._object, _) result =
  ignore node;
  Error "TODO: strip_object_pattern"

(* Array pattern *)

and strip_array_pattern (node : Ast.array_pattern) : (S.pattern S.array, _) result =
  let (Ast.Brackets brackets) = node in
  let list = brackets#payload.contents in
  let* array = Result.all @@ List.map ~f:strip_array_cell_pattern list in
  Ok (mk_reg brackets#region array)

and strip_array_cell_pattern (node : Ast.array_cell_pattern)
    : (S.pattern S.element, _) result
  =
  match node with
  | Cell_pattern (P_rest_pattern rest) ->
    let* pattern = strip_rest_pattern rest in
    Ok (S.Spread pattern)
  | Cell_pattern pattern ->
    let* pattern = strip_pattern pattern in
    Ok (S.Element pattern)
  | Cell_assignment pattern -> Strip_err.(make pattern#region Asgmt_pattern_in_array)

(* Non-null expression (pattern) *)

and strip_P_non_null_expression (node : Ast.expression) : (S.pattern, _) result =
  let region = Ast.region_of_expression node in
  Strip_err.(make region Non_null)

(* Rest pattern *)

and strip_P_rest_pattern (node : Ast.rest_pattern wrap) : (S.pattern, _) result =
  Strip_err.(make node#region Top_rest_pattern)

and strip_rest_pattern (node : Ast.rest_pattern wrap) : (S.pattern, _) result =
  let Ast.{ sym_ellipsis = _; expression } = node#payload in
  match expression with
  | Member_expression expr -> strip_P_member_expression expr
  | Pattern (Pattern_object pattern) ->
    let* pattern = strip_object_pattern pattern in
    Ok (S.P_object pattern)
  | Pattern (Pattern_array pattern) ->
    let* pattern = strip_array_pattern pattern in
    Ok (S.P_array pattern)
  | Identifier ident ->
    let path = Nonempty_list.singleton (strip_identifier ident) in
    Ok (S.P_var (mk_reg ident#region path))
  | _ ->
    Strip_err.(
      make
        node#region
        Complex_rest_pattern
        ~hint:"Use variables or array/object patterns.")

(* Alias for external access by means of [Strip.statements] *)

let statements = strip_statements
