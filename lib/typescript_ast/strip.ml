(* Decoding the tree-sitter CST for TypeScript and stripping it *)

open Core

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Ne_list = Nonempty_list

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
  ; parameters : S.parameter reg list
  ; rhs_type : S.type_expr option
  }

type parameters =
  | Parameter of S.variable
  | Call_signature of call_signature reg

type for_header =
  { index_kind : S.var_kind option
  ; index : (S.key * S.value option) reg
  ; expr : S.expr
  }

(* Filters *)

let filter_decorator_argument (node : S.expr) : (string, _) result =
  match node with
  | S.E_var variable -> Ok variable#payload
  | E_string literal -> Ok literal#payload
  | _ ->
    let region = S.region_of_expr node in
    Strip_err.(make region Invalid_decorator_argument)

let filter_async (node : Ast.kwd_async option) : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some kwd_async -> Strip_err.(make kwd_async#region Asynchronicity)

let filter_await (node : Ast.kwd_await option) : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some kwd_await -> Strip_err.(make kwd_await#region Asynchronicity)

let filter_spread (node : Ast.arguments) : (Ast.expression list, _) result =
  let (Ast.Parens args) = node in
  let args = args#payload.contents in
  let filter (arg : Ast.argument) acc =
    match arg with
    | Ast.Expression expr -> Ok expr :: acc
    | Ast.Spread_element spread -> Strip_err.(make spread#region Spread_expression) :: acc
  in
  let* exprs = Result.all @@ List.fold_right args ~init:[] ~f:filter in
  Ok exprs

let filter_static (node : Ast.method_scope) : (Region.t option, _) result =
  match node with
  | { kwd_static = None; kwd_override = None; kwd_readonly = None } -> Ok None
  | { kwd_static = Some kwd_static; _ } -> Ok (Some kwd_static#region)
  | { kwd_override = Some kwd; _ } | { kwd_readonly = Some kwd; _ } ->
    Strip_err.(make kwd#region Property_scope)

let filter_method_scope (node : Ast.method_scope) : (unit, _) result =
  match node with
  | { kwd_static = None; kwd_override = None; kwd_readonly = None } -> Ok ()
  | { kwd_static = Some kwd; _ }
  | { kwd_override = Some kwd; _ }
  | { kwd_readonly = Some kwd; _ } -> Strip_err.(make kwd#region Property_scope)

let filter_field_scope (node : Ast.field_scope) : (Region.t option, _) result =
  match node with
  | { kwd_static
    ; kwd_override = None
    ; kwd_readonly = None
    ; kwd_abstract = None
    ; kwd_accessor = None
    } ->
    (match kwd_static with
    | None -> Ok None
    | Some kwd_static -> Ok (Some kwd_static#region))
  | { kwd_override = Some kwd; _ }
  | { kwd_readonly = Some kwd; _ }
  | { kwd_abstract = Some kwd; _ }
  | { kwd_accessor = Some kwd; _ } -> Strip_err.(make kwd#region Public_field_scope)

let filter_access (node : Ast.accessibility_modifier option) : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some (Public kwd) | Some (Private kwd) | Some (Protected kwd) ->
    Strip_err.(make kwd#region Property_access)

let filter_optional (node : Ast.sym_qmark option) error : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some sym_qmark -> Strip_err.(make sym_qmark#region error)

let rec filter_path (expr : S.expr) : (S.simple_path reg, _) result =
  match expr with
  | S.E_member { value = e, v; region } ->
    let* path = filter_path e in
    let S.{ path; selected } = path.value in
    Ok (mk_reg region S.{ path = selected :: path; selected = v })
  | S.E_var v -> Ok (mk_reg v#region S.{ path = []; selected = v })
  | _ -> Strip_err.(make (S.region_of_expr expr) Complex_path)

let filter_path (expr : S.expr) : (S.simple_path reg, _) result =
  let* { value; region } = filter_path expr in
  let S.{ path; selected } = value in
  Ok (mk_reg region S.{ path = List.rev path; selected })

(* Stripping *)

let rec strip_statements (node : Ast.statements) : (S.statements reg option, _) result =
  match node with
  | None -> Ok None
  | Some stmts ->
    let f acc stmt =
      let* stmt' = strip_statement stmt in
      Ok (stmt' :: acc)
    in
    let* stmts' = Nonempty_list.fold_result ~f ~init:[] stmts#payload in
    let stmts' = rev_erase_options stmts' in
    (match stmts' with
    | [] -> Ok None
    | fst_stmt :: more_stmts ->
      let stmts' = Ne_list.(fst_stmt :: more_stmts) in
      Ok (Some (mk_reg stmts#region stmts')))

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
  let Ast.{ kwd_export; export_kind } = node#payload in
  match export_kind with
  | Export_from _
  | Export_as _
  | Export_clause _
  | Export_default_declaration _
  | Export_default_expression _
  | Export_type _
  | Export_equal _
  | Export_as_namespace _ -> Strip_err.(make kwd_export#region Invalid_export)
  | Export_declaration decl ->
    let* declaration = strip_decorated_declaration decl in
    Ok (Some (S.S_export declaration))

and strip_decorated_declaration (node : Ast.declaration Ast.decorated)
    : (S.declaration, _) result
  =
  let (Ast.{ decorators; decorated } : _ Ast.decorated) = node in
  let* decorators = strip_decorators decorators in
  let* decl = strip_declaration decorated in
  let f dec decl = S.D_decorated (dec, decl) in
  Ok (List.fold_right ~f ~init:decl decorators)

(* Import statement *)

and strip_S_import_statement (node : Ast.import_statement wrap)
    : (S.statement option, _) result
  =
  let* statement = strip_import_statement node in
  Ok (Some statement)

and strip_import_statement (node : Ast.import_statement wrap) : (S.statement, _) result =
  let Ast.{ kwd_import = _; import_kind; import; import_attribute } = node#payload in
  let* () =
    match import_kind with
    | None -> Ok ()
    | Some (Import_type kwd) | Some (Import_typeof kwd) ->
      Strip_err.(make kwd#region Invalid_import)
  in
  let* () =
    match import_attribute with
    | None -> Ok ()
    | Some (Import_with (kwd, _)) | Some (Import_assert (kwd, _)) ->
      Strip_err.(make kwd#region Invalid_import)
  in
  let* import_decl = strip_import node#region import in
  Ok (S.S_decl import_decl)

and strip_import region (node : Ast.import) : (S.declaration, _) result =
  match node with
  | Import_clause clause -> strip_Import_clause region clause
  | Import_require_clause clause -> strip_Import_require_clause clause
  | Import_source string -> strip_Import_source string

and strip_Import_clause region (node : Ast.import_clause * Ast.from_clause)
    : (S.declaration, _) result
  =
  let import_clause, from_clause = node in
  let _, file_path = from_clause in
  let* import_decl = strip_import_clause region file_path import_clause in
  Ok (S.D_import import_decl)

and strip_import_clause region file_path (node : Ast.import_clause)
    : (S.import_decl, _) result
  =
  match node with
  | Import_namespace import -> strip_namespace_import region file_path import
  | Import_named import -> strip_named_imports region file_path import
  | Import_ident (ident, _) ->
    Strip_err.(make ident#region Invalid_import ~hint:"Use named imports.")

and strip_namespace_import region file_path (node : Ast.namespace_import wrap)
    : (S.import_decl, _) result
  =
  let Ast.{ sym_star = _; kwd_as = _; identifier } = node#payload in
  let import_alias = strip_identifier identifier, file_path in
  let import_alias = mk_reg region import_alias in
  Ok (S.Import_all_as import_alias)

and strip_named_imports region file_path (node : Ast.named_imports)
    : (S.import_decl, _) result
  =
  let Ast.(Braces braces) = node in
  match braces#payload.contents with
  | [] -> Strip_err.(make region Empty_import_list)
  | fst_import :: more_imports ->
    let* fst_import = strip_import_specifier fst_import in
    let* more_imports = Result.all @@ List.map ~f:strip_import_specifier more_imports in
    let imported_vars = Ne_list.(fst_import :: more_imports) in
    let import_from = mk_reg region (imported_vars, file_path) in
    Ok (S.Import_from import_from)

and strip_import_specifier (node : Ast.import_specifier) : (S.variable, _) result =
  let import_kind, spec = node in
  let* () =
    match import_kind with
    | None -> Ok ()
    | Some (Import_type kwd) | Some (Import_typeof kwd) ->
      Strip_err.(make kwd#region Invalid_import)
  in
  strip_import_specifier' spec

and strip_import_specifier' (node : Ast.import_specifier') : (S.variable, _) result =
  match node with
  | Import_spec_name ident -> Ok (strip_identifier ident)
  | Import_spec_alias alias ->
    Strip_err.(
      make
        alias.Ast.kwd_as#region
        Import_and_rename
        ~hint:"Declare a new name after the import.")

and strip_Import_require_clause (node : Ast.import_require_clause wrap)
    : (S.declaration, _) result
  =
  let Ast.
        { ident = _; sym_equal = _; kwd_require; sym_lpar = _; source = _; sym_rpar = _ }
    =
    node#payload
  in
  Strip_err.(make kwd_require#region Invalid_import)

and strip_Import_source (node : Ast.string_literal) : (S.declaration, _) result =
  Strip_err.(make node#region Invalid_import)

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

and strip_statement_block (node : Ast.statement_block) : (S.statements reg, _) result =
  let (Braces statements) = node in
  let statements' = statements#payload.contents in
  let* stmts = strip_statements statements' in
  match stmts with
  | None -> Strip_err.(make statements#region No_statements)
  | Some stmts -> Ok stmts

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
  let expressions = Ne_list.to_list node#payload in
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
  let* cases =
    match cases with
    | [] -> Strip_err.(make braces#region Empty_switch)
    | fst_case :: more_cases -> Ok Nonempty_list.(fst_case :: more_cases)
  in
  match defaults with
  | [] -> Ok (cases, None)
  | [ default ] ->
    let* default = strip_switch_default default in
    Ok (cases, Some default)
  | _ :: default :: _ -> Strip_err.(make default#region Multiple_defaults)

and strip_switch_case (node : Ast.switch_case wrap) : (S.switch_case, _) result =
  let Ast.{ kwd_case = _; value; body } = node#payload in
  match value#payload with
  | Ne_list.[ expr ] ->
    let* expr = strip_expression expr in
    let* body = strip_statements body in
    let body = Option.map ~f:(fun stmt -> stmt.value) body in
    Ok (expr, body)
  | _ :: expr :: _ ->
    let region = Ast.region_of_expression expr in
    Strip_err.(make region Multiple_values)

and strip_switch_default (node : Ast.switch_default wrap) : (S.switch_default, _) result =
  let Ast.{ kwd_default = _; statements } = node#payload in
  let* statements = strip_statements statements in
  Ok (Option.map ~f:(fun stmt -> stmt.value) statements)

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
  let* () = filter_await kwd_await in
  let* { index_kind; index; expr } = strip_for_header for_header in
  let* for_of_body = strip_statement body in
  let* for_of_body =
    match for_of_body with
    | None -> Strip_err.(make node#region No_statements)
    | Some statement -> Ok statement
  in
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
    : (S.var_kind option * (S.key * S.value option) reg, _) result
  =
  match node with
  | For_in_expression (Identifier v) ->
    Ok (None, mk_reg v#region (strip_identifier v, None))
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
    : ((S.key * S.value option) reg, _) result
  =
  let region = Ast.region_of_for_in_variable node in
  match node with
  | For_in_ident ident -> Ok (mk_reg ident#region (strip_identifier ident, None))
  | For_in_pattern p ->
    let* pattern = strip_destructuring_pattern p in
    (match pattern with
    | S.P_array array ->
      (match array.value with
      | [ elem_1; elem_2 ] ->
        let* elem_1 = force_single_var elem_1 in
        let* elem_2 = force_single_var elem_2 in
        let region = Ast.region_of_destructuring_pattern p in
        Ok (mk_reg region (elem_1, Some elem_2))
      | _ -> Strip_err.(make region Invalid_loop_index))
    | _ -> Strip_err.(make region Invalid_loop_index))

and force_single_var (node : S.pattern S.element) : (S.variable, _) result =
  match node with
  | Element (P_var path as pattern) ->
    let S.{ path; selected } = path.value in
    (match path with
    | [] -> Ok selected
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
  match statement with
  | None -> Strip_err.(make node#region Empty_while)
  | Some statement -> Ok (expr, statement)

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
  let* () = filter_async kwd_async in
  let comments = kwd_function#comments in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let fun_name = strip_identifier name in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* fun_body = strip_statement_block body in
  let fun_decl = S.{ comments; fun_name; generics; parameters; rhs_type; fun_body } in
  let decl = S.D_function (mk_reg node#region fun_decl) in
  let f dec decl = S.D_decorated (dec, decl) in
  Ok (List.fold_right ~f ~init:decl decorators)

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

and format_parameters_into_patterns (node : (S.variable * S.type_expr option) reg list)
    : S.parameter reg list
  =
  let make_parameter Region.{ value = variable, opt; region } =
    let path = S.{ path = []; selected = variable } in
    Region.{ value = S.P_var (mk_reg variable#region path), opt; region }
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
  let (Ast.{ decorators; kwd_class; name; type_parameters; class_heritage; body }
        : Ast.class_declaration)
    =
    node#payload
  in
  let* decorators = strip_decorators decorators in
  let comments = strip_comments kwd_class#comments in
  let class_name = strip_identifier name in
  let* generics = strip_list_opt strip_type_parameters type_parameters in
  let* () =
    match generics with
    | [] -> Ok ()
    | type_var :: _ -> Strip_err.(make type_var#region Generic_class)
  in
  let* implements = strip_class_heritage class_heritage in
  let* class_body = strip_class_body body in
  let class_decl = S.{ comments; class_name; implements; class_body } in
  let decl = S.D_class (mk_reg node#region class_decl) in
  let f dec decl = S.D_decorated (dec, decl) in
  Ok (List.fold_right ~f ~init:decl decorators)

and strip_class_body (node : Ast.class_body)
    : (S.class_member Nonempty_list.t reg, _) result
  =
  let Ast.(Braces braces) = node in
  match braces#payload.contents with
  | [] -> Strip_err.(make braces#region Empty_class)
  | fst_memb :: more_memb ->
    let* fst_memb = strip_class_member fst_memb in
    let* more_memb = Result.all @@ List.map ~f:strip_class_member more_memb in
    let members = Nonempty_list.(fst_memb :: more_memb) in
    Ok (mk_reg braces#region members)

and strip_class_member (node : Ast.class_member) : (S.class_member, _) result =
  match node with
  | Method_definition (decorators, definition) ->
    let* def = strip_method_definition decorators definition in
    Ok (S.Method_definition def)
  | Method_signature signature ->
    Strip_err.(
      make signature#region Method_signature_in_class ~hint:"Provide a method body.")
  | Call_static_block (kwd_static, _) ->
    Strip_err.(make kwd_static#region Call_static_block)
  | Abstract_method_signature signature ->
    Strip_err.(make signature#region Abstract_method)
  | Index_signature signature -> Strip_err.(make signature#region Index_signature)
  | Public_field_definition definition ->
    let* def = strip_public_field_definition definition in
    Ok (S.Public_field_definition def)

and strip_method_definition
    (decorators : Ast.decorators)
    (node : Ast.method_definition wrap)
    : (S.method_definition reg, _) result
  =
  let Ast.{ signature; body } = node#payload in
  let* method_sig = strip_method_signature decorators signature in
  let* method_body = strip_statement_block body in
  let region = node#region in
  Ok (mk_reg region S.{ method_sig; method_body })

and strip_public_field_definition (node : Ast.public_field_definition wrap)
    : (S.public_field_definition reg, _) result
  =
  let Ast.{ decorators; access; kwd_declare; scope; name; mode; type_; default } =
    node#payload
  in
  let* decorators = strip_decorators decorators in
  let* () = filter_access access in
  let* () =
    match kwd_declare with
    | None -> Ok ()
    | Some kwd_declare -> Strip_err.(make kwd_declare#region Declare_definition)
  in
  let* static = filter_field_scope scope in
  let* name = strip_property_name name in
  let* () =
    match mode with
    | None -> Ok ()
    | Some (Optional sym | Definite_assert sym) -> Strip_err.(make sym#region Field_mode)
  in
  let* field_type = map_opt strip_type_annotation type_ in
  let* field_type =
    match field_type with
    | None -> Strip_err.(make node#region Missing_type)
    | Some field_type -> Ok field_type
  in
  let* field_value =
    match default with
    | None -> Strip_err.(make name#region No_default)
    | Some (_, expr) -> strip_expression expr
  in
  let def = S.{ decorators; static; name; field_type; field_value } in
  Ok (mk_reg node#region def)

and strip_class_heritage (node : Ast.class_heritage option)
    : (S.simple_path reg list, _) result
  =
  match node with
  | None -> Ok []
  | Some (Extends_clause ((kwd_extends, _), _)) ->
    Strip_err.(make kwd_extends#region Extends_clause)
  | Some (Implements_clause (_, type_exprs)) ->
    let type_exprs = Ne_list.to_list type_exprs in
    let* type_exprs = Result.all @@ List.map ~f:strip_type_expr type_exprs in
    let filter type_expr =
      match type_expr with
      | S.T_path path -> Ok path
      | _ -> Strip_err.(make (S.region_of_type_expr type_expr) Invalid_implements)
    in
    Result.all @@ List.map ~f:filter type_exprs

(* DECORATORS *)

and strip_decorators (node : Ast.decorators) : (S.decorator list, _) result =
  Result.all @@ List.map ~f:strip_decorator node

and strip_decorator (node : Ast.decorator) : (S.decorator, _) result =
  let region = Ast.region_of_decorator node in
  match node with
  | Decorator_identifier ident ->
    let name = strip_identifier ident in
    Ok (Wrap.make (name#payload, None) region)
  | Decorator_member_expression _ -> Strip_err.(make region Member_decorator)
  | Decorator_call_expression call -> strip_decorator_call_expression call
  | Decorator_parenthesized_expression parens ->
    strip_decorator_parenthesized_expression parens

and strip_decorator_parenthesized_expression
    (node : Ast.decorator_parenthesized_expression Ast.parens)
    : (S.decorator, _) result
  =
  let Ast.(Parens parens) = node in
  let par_expr = parens#payload.contents in
  match par_expr with
  | Parenthesized_ident ident ->
    let dec_name = strip_identifier ident in
    Ok (Wrap.make (dec_name#payload, None) parens#region)
  | Parenthesized_member _ -> Strip_err.(make parens#region Member_decorator)
  | Parenthesized_call call -> strip_decorator_call_expression call

and strip_decorator_call_expression (node : Ast.decorator_call_expression wrap)
    : (S.decorator, _) result
  =
  let Ast.{ function_; type_arguments; arguments } = node#payload in
  let* dec_name = strip_function_or_property function_ in
  let* () =
    match type_arguments with
    | None -> Ok ()
    | Some type_args ->
      let Ast.(Chevrons chevrons) = type_args in
      Strip_err.(make chevrons#region Type_arguments_in_decorator)
  in
  let Ast.(Parens parens) = arguments in
  let arguments = parens#payload.contents in
  match arguments with
  | [] -> Ok (Wrap.make (dec_name, None) node#region)
  | [ argument ] ->
    let* expr = strip_argument argument in
    (match expr with
    | S.Element expr ->
      let* dec_param = filter_decorator_argument expr in
      Ok (Wrap.make (dec_name, Some dec_param) node#region)
    | Spread expr ->
      let region = S.region_of_expr expr in
      Strip_err.(make region Spread_expression))
  | _ :: snd_arg :: _ ->
    let region = Ast.region_of_argument snd_arg in
    Strip_err.(make region Multiple_arguments_in_decorator)

and strip_function_or_property (node : Ast.function_or_property) : (string, _) result =
  match node with
  | Function_name ident ->
    let variable = strip_identifier ident in
    Ok variable#payload
  | Qualified_member_expression _ ->
    let region = Ast.region_of_function_or_property node in
    Strip_err.(make region Member_decorator)

(* Lexical declaration *)

and strip_D_lexical_declaration (node : Ast.lexical_declaration wrap)
    : (S.declaration, _) result
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
  let value_decl = S.{ comments; kind; bindings } in
  let decl = S.D_value (mk_reg node#region value_decl) in
  let f dec decl = S.D_decorated (dec, decl) in
  Ok (List.fold_right ~f ~init:decl decorators)

and strip_variable_declarators (node : Ast.variable_declarator Ast.ne_list)
    : (S.val_binding reg Ne_list.t, _) result
  =
  let (var_decl :: var_decls) = node in
  let* var_decl = strip_variable_declarator var_decl in
  let* var_decls = Result.all @@ List.map ~f:strip_variable_declarator var_decls in
  Ok Ne_list.(var_decl :: var_decls)

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
      let path = S.{ path = []; selected = strip_identifier ident } in
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
  let* () = filter_async kwd_async in
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

and filter_parameter (node : S.parameter reg)
    : ((S.variable * S.type_expr option) reg, _) result
  =
  let pattern, type_expr = node.value in
  match pattern with
  | S.P_var path ->
    let S.{ path; selected } = path.value in
    (match path with
    | [] -> Ok (mk_reg node.region (selected, type_expr))
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
  let Ast.{ kwd_namespace = _; module_name; module_body } = node#payload in
  let* (namespace_name : S.variable) = strip_module_name module_name in
  let* (namespace_body : S.statements reg) =
    match module_body with
    | None -> Strip_err.(make node#region No_statements)
    | Some block -> strip_statement_block block
  in
  let decl = S.{ namespace_name; namespace_type = []; namespace_body } in
  Ok (S.D_namespace (mk_reg node#region decl))

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
  let type_decl = S.{ name; generics; type_expr } in
  Ok (S.D_type (mk_reg region type_decl))

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
  let Ast.{ kwd_interface = _; name; type_parameters; extends; body } = node#payload in
  let intf_name = strip_identifier name in
  let* () =
    match type_parameters with
    | None -> Ok ()
    | Some chevrons ->
      let region = Ast.region_of_chevrons chevrons in
      Strip_err.(make region Interface_with_type_parameters)
  in
  let* intf_extends =
    match extends with
    | None -> Ok []
    | Some extensions -> strip_extends extensions
  in
  let* intf_body = strip_interface_body body in
  let intf_decl = S.{ intf_name; intf_extends; intf_body } in
  Ok (S.D_interface (mk_reg node#region intf_decl))

and strip_interface_body (node : Ast.object_type) : (S.intf_entry reg list reg, _) result =
  let Ast.(Braces braces) = node in
  let member_types = braces#payload.contents in
  let* entries = Result.all @@ List.map ~f:strip_intf_entry member_types in
  Ok (mk_reg braces#region entries)

and strip_intf_entry (node : Ast.member_type) : (S.intf_entry reg, _) result =
  match node with
  | Export_statement stmt -> Strip_err.(make stmt#region Export_member)
  | Property_signature signature -> strip_property_signature_as_intf_entry signature
  | Call_signature signature -> Strip_err.(make signature#region Call_signature)
  | Construct_signature signature -> Strip_err.(make signature#region Constructor)
  | Index_signature signature -> Strip_err.(make signature#region Index_signature)
  | Method_signature signature -> strip_method_signature_as_intf_entry signature

and strip_property_signature_as_intf_entry (node : Ast.property_signature wrap)
    : (S.intf_entry reg, _) result
  =
  let Ast.{ access; scope; name; sym_qmark = _; type_ } = node#payload in
  let* () = filter_access access in
  let* () = filter_method_scope scope in
  let* entry_name = strip_property_name name in
  let entry_optional = None in
  let* entry_type = map_opt strip_type_annotation type_ in
  match entry_type with
  | None -> Strip_err.(make node#region Missing_type)
  | Some entry_type ->
    let comments = entry_name#comments in
    let comments = strip_comments comments in
    let decorators = extract_decorators comments in
    let entry = S.{ decorators; comments; entry_name; entry_optional; entry_type } in
    Ok (mk_reg node#region entry)

and strip_method_signature_as_intf_entry (node : Ast.method_signature wrap)
    : (S.intf_entry reg, _) result
  =
  let Ast.{ access; scope; kwd_async; set_get_all; name; optional; call_sig } =
    node#payload
  in
  let* () = filter_access access in
  let* () = filter_method_scope scope in
  let* () = filter_async kwd_async in
  let* () =
    match set_get_all with
    | None -> Ok ()
    | Some (Set kwd | Get kwd) -> Strip_err.(make kwd#region Set_get_all)
    | Some (All sym) -> Strip_err.(make sym#region Set_get_all)
  in
  let* entry_name = strip_property_name name in
  let entry_optional =
    match optional with
    | None -> None
    | Some sym_qmark -> Some sym_qmark#region
  in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* parameters = Result.all @@ List.map ~f:filter_parameter parameters in
  let* parameters = filter_type_annotations parameters in
  let* rhs_type =
    match rhs_type with
    | None -> Strip_err.(make node#region Return_type_absent)
    | Some rhs_type -> Ok rhs_type
  in
  let rhs_type = S.T_fun (mk_reg call_sig.region (parameters, rhs_type)) in
  let entry_type =
    match generics with
    | [] -> rhs_type
    | _ -> S.T_for_all (mk_reg call_sig.region (generics, rhs_type))
  in
  let comments = entry_name#comments in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let entry = S.{ decorators; comments; entry_name; entry_optional; entry_type } in
  Ok (mk_reg node#region entry)

and strip_extends (node : Ast.extends_type_clause) : (S.simple_path reg list, _) result =
  let Ast.{ kwd_extends = _; extensions } = node in
  let extensions = Ne_list.to_list extensions in
  Result.all @@ List.map ~f:strip_type_extension extensions

and strip_type_extension (node : Ast.type_extension) : (S.simple_path reg, _) result =
  match node with
  | Extends_type ident ->
    let path = S.{ path = []; selected = strip_type_identifier ident } in
    Ok (mk_reg ident#region path)
  | Extends_nested nested -> Ok (strip_nested_type_identifier nested)
  | Extends_generic gen_type -> Strip_err.(make gen_type#region Generic_class_extension)

(* Import alias *)

and strip_D_import_alias (node : Ast.import_alias wrap) : (S.declaration, _) result =
  let Ast.{ kwd_import = _; alias; sym_equal = _; aliased } = node#payload
  and region = node#region in
  let alias = strip_identifier alias in
  let path = strip_aliased aliased in
  let import = alias, path in
  Ok S.(D_import (S.Import_alias (mk_reg region import)))

and strip_aliased (node : Ast.aliased) : S.simple_path reg =
  match node with
  | Ident ident ->
    let path = S.{ path = []; selected = strip_identifier ident } in
    mk_reg ident#region path
  | Nested nested -> strip_nested_identifier nested

and strip_nested_identifier (node : Ast.nested_identifier wrap) : S.simple_path reg =
  let path, selected = node#payload in
  let path = List.rev (Ne_list.to_list path) in
  let path = List.map ~f:strip_type_identifier path
  and selected = strip_type_identifier selected in
  mk_reg node#region S.{ path; selected }

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
    let path = mk_reg region S.{ path = []; selected = bool } in
    Ok (S.T_path path)
  | T_string kwd_string ->
    let region = kwd_string#region in
    let path = mk_reg region S.{ path = []; selected = kwd_string } in
    Ok (S.T_path path)
  | T_symbol kwd_symbol -> Strip_err.(make kwd_symbol#region Symbol_type)
  | T_unique_symbol kwd_unique_symbol ->
    Strip_err.(make kwd_unique_symbol#region Unique_symbol_type)
  | T_void kwd_void -> Strip_err.(make kwd_void#region Void_type)
  | T_unknown kwd_unknown -> Strip_err.(make kwd_unknown#region Unknown_type)
  | T_never kwd_never -> Strip_err.(make kwd_never#region Never_type)
  | T_object kwd_object -> Strip_err.(make kwd_object#region Object_type)

(* Type identifier *)

and strip_T_type_identifier (node : Ast.type_identifier) : (S.type_expr, _) result =
  let selected = strip_type_identifier node in
  let region = node#region in
  let path = S.{ path = []; selected } in
  Ok (S.T_path (mk_reg region path))

(* Nested type identifier (access path is reversed) *)

and strip_T_nested_type_identifier (node : Ast.nested_type_identifier wrap)
    : (S.type_expr, _) result
  =
  Ok (S.T_path (strip_nested_type_identifier node))

and strip_nested_type_identifier (node : Ast.nested_type_identifier wrap)
    : S.simple_path reg
  =
  let path, selected = node#payload in
  let path = List.rev (Ne_list.to_list path) in
  let path = List.map ~f:strip_type_identifier path
  and selected = strip_type_identifier selected in
  mk_reg node#region S.{ path; selected }

(* Generic type

   TODO: Test the order of the type arguments.
*)

and strip_T_generic_type (node : Ast.generic_type wrap) : (S.type_expr, _) result =
  let name, type_args = node#payload in
  let path = S.T_path (strip_generic_name name) in
  let* type_args = strip_type_arguments type_args in
  let ok = Ok (S.T_apply (mk_reg node#region (path, type_args))) in
  let error = Strip_err.(make node#region Invalid_parameter_of) in
  match name with
  | Ast.Generic_type type_ident ->
    (match type_ident#payload with
    | "parameter_of" ->
      (match type_args with
      | [ type_arg ] ->
        (match type_arg with
        | S.T_path path -> Ok (S.T_parameter_of (mk_reg node#region path))
        | _ -> error)
      | _ -> error)
    | _ -> ok)
  | _ -> ok

and strip_generic_name (node : Ast.generic_name) : S.simple_path reg =
  match node with
  | Generic_type type_identifier ->
    let region = type_identifier#region in
    let selected = strip_type_identifier type_identifier in
    mk_reg region S.{ path = []; selected }
  | Generic_nested nested -> strip_nested_type_identifier nested

and strip_type_arguments (node : Ast.type_arguments) : (S.type_expr list, _) result =
  let (Chevrons chevrons) = node in
  let type_args = chevrons#payload.contents in
  let type_args = Ne_list.to_list type_args in
  Result.all @@ List.map ~f:strip_type_expr type_args

(* Object type *)

and strip_T_object_type (node : Ast.object_type) : (S.type_expr, _) result =
  let* object_type = strip_object_type node in
  let object_type = { object_type with value = object_type.value } in
  Ok (S.T_object object_type)

and strip_object_type (node : Ast.object_type) : (S.member_type reg list reg, _) result =
  let Ast.(Braces braces) = node in
  let member_types = braces#payload.contents in
  let* members = Result.all @@ List.map ~f:strip_member_type member_types in
  Ok (mk_reg braces#region members)

and strip_member_type (node : Ast.member_type) : (S.member_type reg, _) result =
  match node with
  | Export_statement stmt -> Strip_err.(make stmt#region Export_member)
  | Property_signature signature -> strip_property_signature signature
  | Call_signature signature -> Strip_err.(make signature#region Call_signature)
  | Construct_signature signature -> Strip_err.(make signature#region Constructor)
  | Index_signature signature -> Strip_err.(make signature#region Index_signature)
  | Method_signature signature -> strip_method_signature_as_property signature

and strip_property_signature (node : Ast.property_signature wrap)
    : (S.member_type reg, _) result
  =
  let Ast.{ access; scope; name; sym_qmark = _; type_ } = node#payload in
  let* () = filter_access access in
  let* () = filter_method_scope scope in
  let* property_name = strip_property_name name in
  let* rhs_type = map_opt strip_type_annotation type_ in
  match rhs_type with
  | None -> Strip_err.(make node#region Missing_type)
  | Some rhs_type ->
    let comments = property_name#comments in
    let comments = strip_comments comments in
    let decorators = extract_decorators comments in
    let signature = S.{ decorators; comments; property_name; rhs_type } in
    Ok (mk_reg node#region signature)

and strip_method_signature_as_property (node : Ast.method_signature wrap)
    : (S.member_type reg, _) result
  =
  let Ast.{ access; scope; kwd_async; set_get_all; name; optional; call_sig } =
    node#payload
  in
  let* () = filter_access access in
  let* () = filter_method_scope scope in
  let* () = filter_async kwd_async in
  let* () =
    match set_get_all with
    | None -> Ok ()
    | Some (Set kwd | Get kwd) -> Strip_err.(make kwd#region Set_get_all)
    | Some (All sym) -> Strip_err.(make sym#region Set_get_all)
  in
  let* property_name = strip_property_name name in
  let* () = filter_optional optional Optional_member in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* parameters = Result.all @@ List.map ~f:filter_parameter parameters in
  let* parameters = filter_type_annotations parameters in
  let* rhs_type =
    match rhs_type with
    | None -> Strip_err.(make node#region Return_type_absent)
    | Some rhs_type -> Ok rhs_type
  in
  let rhs_type = S.T_fun (mk_reg call_sig.region (parameters, rhs_type)) in
  let rhs_type =
    match generics with
    | [] -> rhs_type
    | _ -> S.T_for_all (mk_reg call_sig.region (generics, rhs_type))
  in
  let comments = property_name#comments in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let signature : S.member_type = S.{ decorators; comments; property_name; rhs_type } in
  Ok (mk_reg node#region signature)

and strip_method_signature decorators (node : Ast.method_signature wrap)
    : (S.method_signature reg, _) result
  =
  let Ast.{ access; scope; kwd_async; set_get_all; name; optional; call_sig } =
    node#payload
  in
  let* () = filter_access access in
  let* static = filter_static scope in
  let* () = filter_async kwd_async in
  let* () =
    match set_get_all with
    | None -> Ok ()
    | Some (Set kwd | Get kwd) -> Strip_err.(make kwd#region Set_get_all)
    | Some (All sym) -> Strip_err.(make sym#region Set_get_all)
  in
  let* method_name = strip_property_name name in
  let* () = filter_optional optional Optional_member in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* parameters = Result.all @@ List.map ~f:filter_parameter parameters in
  let* parameters = filter_type_annotations parameters in
  let* rhs_type =
    match rhs_type with
    | None -> Strip_err.(make node#region Return_type_absent)
    | Some rhs_type -> Ok rhs_type
  in
  let rhs_type =
    match generics with
    | [] -> rhs_type
    | _ -> S.T_for_all (mk_reg call_sig.region (generics, rhs_type))
  in
  let comments = method_name#comments in
  let comments = strip_comments comments in
  let* decorators = strip_decorators decorators in
  let decorators = decorators @ extract_decorators comments in
  let signature =
    S.{ decorators; comments; static; method_name; generics; parameters; rhs_type }
  in
  Ok (mk_reg node#region signature)

(* Array type *)

and strip_T_array_type (node : Ast.array_type wrap) : (S.type_expr, _) result =
  Strip_err.(make node#region Array_type)

(* Tuple type *)

and strip_T_tuple_type (node : Ast.tuple_type) : (S.type_expr, _) result =
  let (Brackets brackets) = node in
  let members = brackets#payload.contents in
  let* members = Result.all @@ List.map ~f:strip_tuple_type_member members in
  match members with
  | [] -> Strip_err.(make brackets#region Empty_tuple_type)
  | fst_comp :: components ->
    let members = Ne_list.(fst_comp :: components) in
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
      Ok (S.T_int literal))
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
    | None -> Ok (Ne_list.singleton type_2)
    | Some type_1 ->
      let* type_1 = strip_type_expr type_1 in
      Ok Ne_list.(type_1 :: [ type_2 ])
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

and filter_type_annotations (node : (S.variable * S.type_expr option) reg list)
    : ((S.variable * S.type_expr) reg list, _) result
  =
  let check Region.{ value; region } =
    match value with
    | variable, None -> Strip_err.(make variable#region Missing_type)
    | variable, Some type_expr -> Ok Region.{ value = variable, type_expr; region }
  in
  Result.all @@ List.map ~f:check node

and strip_formal_parameters (node : Ast.formal_parameters)
    : ((S.variable * S.type_expr option) reg list, _) result
  =
  let (Ast.Parens parens) = node in
  let parameters = parens#payload.contents in
  Result.all @@ List.map ~f:strip_formal_parameter parameters

and strip_formal_parameter (node : Ast.formal_parameter wrap)
    : ((S.variable * S.type_expr option) reg, _) result
  =
  let Ast.{ parameter_name; optional; type_opt; default } = node#payload in
  let* parameter = strip_parameter_name parameter_name in
  let* () = filter_optional optional Optional_parameter in
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
  let region =
    match default with
    | Some (_, expr) -> Ast.region_of_expression expr
    | _ ->
      (match type_opt with
      | Some (_, type_expr) -> Ast.region_of_type_expr type_expr
      | None ->
        (match optional with
        | Some sym -> sym#region
        | None -> parameter_name#region))
  in
  Ok (mk_reg region (parameter, type_expr))

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
    (* Filtering Michelson code injections *)
    let* expr' = strip_expression expr in
    let* type_expr = strip_type_expr type_expr in
    let ok = Ok (S.E_typed (mk_reg region (expr', type_expr))) in
    (match expr with
    | Ast.E_primary_expression (E_call_expression (Call call)) ->
      let Ast.{ lambda; type_arguments; arguments } = call#payload in
      (match lambda with
      | Ast.Fun_call (E_primary_expression (E_identifier fun_name))
        when String.(fun_name#payload = "michelson" || fun_name#payload = "Michelson") ->
        (match type_arguments with
        | None ->
          (match arguments with
          | Ast.Template_string w ->
            (match w#payload with
            | _, [ String_fragment literal ], _ ->
              let code_inj = fun_name, literal, type_expr in
              let code_inj = mk_reg node#region code_inj in
              (* [NOTE][TEMPORARY]: The wrapping of a S.E_typed
                 constructor (with duplication of the type annotation)
                 is not strictly necessary, but it helps with the
                 compilation to the unified AST. *)
              let typed_expr = S.E_michelson code_inj, type_expr in
              Ok S.(E_typed (mk_reg node#region typed_expr))
            | _ -> ok)
          | _ -> ok)
        | _ -> ok)
      | _ -> ok)
    | _ -> ok)
  | As_const kwd_const -> Strip_err.(make kwd_const#region Constant_type)

(* Assignment expression *)

and strip_E_assignment_expression (node : Ast.assignment_expression wrap)
    : (S.expr, _) result
  =
  let Ast.{ kwd_using; left; sym_equal = _; right } = node#payload in
  let* () =
    match kwd_using with
    | None -> Ok ()
    | Some kwd -> Strip_err.(make kwd#region Finalised_const)
  in
  let* left = strip_assignment_lhs left in
  let* right = strip_expression right in
  Ok (S.E_assign (mk_reg node#region (left, right)))

and strip_assignment_lhs (node : Ast.assignment_lhs) : (S.expr, _) result =
  match node with
  | Ast.Assign_lhs expr -> strip_lhs_expression expr
  | Assign_lhs_parens expr ->
    let* exprs = strip_parenthesized_expression expr in
    (match exprs with
    | [ expr ] -> Ok expr
    | _ ->
      let region = Ast.region_of_assignment_lhs node in
      Strip_err.(make region Multiple_values))

and strip_lhs_expression (node : Ast.lhs_expression) : (S.expr, _) result =
  let* (lhs : Ast.expression) =
    match node with
    | Member_expression expr -> Ok (Ast.E_primary_expression (E_member_expression expr))
    | Subscript_expression expr -> Ok (E_primary_expression (E_subscript_expression expr))
    | Identifier ident -> Ok (E_primary_expression (E_identifier ident))
    | Undefined kwd_undefined -> Ok (E_primary_expression (E_undefined kwd_undefined))
    | Pattern pattern -> Ast.destructuring_pattern_to_expression pattern
    | Non_null_expression expr -> Ok (E_primary_expression (E_non_null_expression expr))
  in
  strip_expression lhs

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
  | Identifier ident -> Ok (S.E_var (strip_identifier ident))
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
  | Bit_sl_eq _ -> Ok (fun args -> S.E_bit_sl_eq args) (* <<= *)
  | Exp_eq sym -> Strip_err.(make sym#region Exp_eq) (* **= *)
  | Log_and_eq sym ->
    (* &&= *)
    Strip_err.(make sym#region Log_and_eq ~hint:"Use \"=\" and \"&&\" separately.")
  | Log_or_eq sym ->
    (* ||= *)
    Strip_err.(make sym#region Log_or_eq ~hint:"Use \"=\" and \"||\" separately.")
  | Non_null_eq sym ->
    (* ??= *)
    Strip_err.(make sym#region Non_null)

(* Await-expression *)

and strip_E_await_expression (node : Ast.await_expression wrap) : (S.expr, _) result =
  Strip_err.(make node#region Asynchronicity)

(* Binary expression *)

and strip_E_binary_expression (node : Ast.binary_expression wrap) : (S.expr, _) result =
  let Ast.{ lhs_expr; operator; rhs_expr } = node#payload in
  let* lhs_expr = strip_lhs_bin_expression lhs_expr in
  let* rhs_expr = strip_expression rhs_expr in
  let* op = strip_binary_operator operator in
  let arg = mk_reg node#region (lhs_expr, rhs_expr) in
  Ok (op arg)

and strip_binary_operator (node : Ast.binary_operator)
    : ((S.expr * S.expr) reg -> S.expr, _) result
  =
  match node with
  | Log_and _ -> Ok (fun arg -> S.E_and arg) (* && *)
  | Log_or _ -> Ok (fun arg -> S.E_or arg) (* || *)
  | Bit_sr _ -> Ok (fun arg -> S.E_bit_sr arg) (* >> *)
  | Bit_usr sym -> Strip_err.(make sym#region Bit_usr_eq) (* >>> *)
  | Bit_sl _ -> Ok (fun arg -> S.E_bit_sl arg) (* << *)
  | Bit_and _ -> Ok (fun arg -> S.E_bit_and arg) (* & *)
  | Bit_xor _ -> Ok (fun arg -> S.E_bit_xor arg) (* ^ *)
  | Bit_or _ -> Ok (fun arg -> S.E_bit_or arg) (* | *)
  | Add _ -> Ok (fun arg -> S.E_add arg) (* + *)
  | Sub _ -> Ok (fun arg -> S.E_sub arg) (* - *)
  | Mult _ -> Ok (fun arg -> S.E_mult arg) (* * *)
  | Div _ -> Ok (fun arg -> S.E_div arg) (* / *)
  | Rem _ -> Ok (fun arg -> S.E_rem arg) (* % *)
  | Exp sym -> Strip_err.(make sym#region Exp_eq) (* * *)
  | Lt _ -> Ok (fun arg -> S.E_lt arg) (* < *)
  | Leq _ -> Ok (fun arg -> S.E_leq arg) (* <= *)
  | Equal _ -> Ok (fun arg -> S.E_equal arg) (* == *)
  | Strict_eq sym ->
    (* === *)
    Strip_err.(make sym#region Strict_equality ~hint:"Use '=='")
  | Neq _ -> Ok (fun arg -> S.E_neq arg) (* != *)
  | Strict_neq sym ->
    (* !== *)
    Strip_err.(make sym#region Strict_equality ~hint:"Use '!='")
  | Geq _ -> Ok (fun arg -> S.E_geq arg) (* >= *)
  | Gt _ -> Ok (fun arg -> S.E_gt arg) (* > *)
  | Non_null sym -> Strip_err.(make sym#region Non_null) (* ?? *)
  | Instance_of kwd_instanceof ->
    (* instanceof *)
    Strip_err.(make kwd_instanceof#region Instanceof)
  | In kwd_in ->
    (* in *)
    Strip_err.(make kwd_in#region In)

and strip_lhs_bin_expression (node : Ast.lhs_bin_expression) : (S.expr, _) result =
  match node with
  | Lhs_bin_expression expr ->
    let* expr = strip_expression expr in
    Ok expr
  | Lhs_bin_hash hash -> Strip_err.(make hash#region Private_property)

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
  let* () = filter_async kwd_async in
  let* parameters = strip_parameters parameters in
  let* generics = get_generics parameters in
  let* rhs_type = get_rhs_type parameters in
  let* parameters = get_parameters parameters in
  let* fun_body = strip_function_body body in
  Ok S.{ generics; parameters; rhs_type; fun_body }

and get_parameters (node : parameters) : (S.parameter reg list, _) result =
  match node with
  | Parameter variable ->
    let path = S.{ path = []; selected = variable } in
    let param = S.P_var (mk_reg variable#region path), None in
    Ok [ mk_reg variable#region param ]
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
  match node with
  | Call fun_call -> strip_call_fun fun_call
  | Member expr_call -> Strip_err.(make expr_call#region Optional_chaining)

and strip_call_fun (node : (Ast.fun_call, Ast.arguments_to_call) Ast.call wrap)
    : (S.expr, _) result
  =
  let Ast.{ lambda; type_arguments; arguments } = node#payload in
  let* (lambda : S.expr) = strip_fun_call lambda in
  let* () =
    match type_arguments with
    | None -> Ok ()
    | Some type_args ->
      let region = Ast.region_of_chevrons type_args in
      Strip_err.(make region Type_parameters_on_args)
  in
  let* (arguments : S.expr list) = strip_arguments_to_call arguments in
  let app = mk_reg node#region (lambda, arguments) in
  let ok = Ok (S.E_app app) in
  let error = Strip_err.(make node#region Invalid_contract_of) in
  match lambda with
  | S.E_var var ->
    (match var#payload with
    | "contract_of" ->
      (match arguments with
      | [ expr ] ->
        let* path = filter_path expr in
        Ok (S.E_contract_of (mk_reg node#region path))
      | _ -> error)
    | _ -> ok)
  | _ -> ok

and strip_fun_call (node : Ast.fun_call) : (S.expr, _) result =
  match node with
  | Fun_call expr -> strip_expression expr
  | Import kwd_import -> Strip_err.(make kwd_import#region Import)

and strip_arguments_to_call (node : Ast.arguments_to_call) : (S.expr list, _) result =
  match node with
  | Arguments arguments ->
    let* arguments = filter_spread arguments in
    let* arguments = Result.all @@ List.map ~f:strip_expression arguments in
    Ok arguments
  | Template_string string ->
    let* expr = strip_template_string string in
    Ok [ expr ]

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
  let* () = filter_async kwd_async in
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
  Ok (S.E_var (strip_identifier node))

(* Member expression *)

and strip_E_member_expression (node : Ast.member_expression wrap) : (S.expr, _) result =
  let (Ast.{ object_expr; selector; property } : Ast.member_expression) = node#payload in
  let* expr = strip_object_member object_expr in
  let* () =
    match selector with
    | Ast.Dot _ -> Ok ()
    | Optional_chain sym -> Strip_err.(make sym#region Optional_chaining)
  in
  let* property = strip_property_ident property in
  Ok (S.E_member (mk_reg node#region (expr, property)))

and strip_object_member (node : Ast.object_member) : (S.expr, _) result =
  match node with
  | Object_member_expression expr -> strip_expression expr
  | Object_member_import kwd_import -> Strip_err.(make kwd_import#region Import)

and strip_property_ident (node : Ast.property_ident) : (S.variable, _) result =
  match node with
  | Private_property_identifier hash -> Strip_err.(make hash#region Private_property)
  | Property_identifier ident -> Ok (strip_identifier ident)

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
  let Ast.(Braces braces) = node in
  let entries = braces#payload.contents in
  let spreads : Ast.spread_element wrap list =
    let app entry acc =
      match entry with
      | Ast.Object_entry_spread spread -> spread :: acc
      | _ -> acc
    in
    List.fold_right ~f:app ~init:[] entries
  in
  let* properties = Result.all @@ List.map ~f:strip_object_entry entries in
  let properties = Option.all properties in
  let properties =
    match properties with
    | None -> []
    | Some list -> list
  in
  match spreads with
  | [] -> Ok (S.E_object (mk_reg braces#region properties))
  | [ spread ] ->
    let _sym_ellipsis, expr = spread#payload in
    let* obj_expr = strip_expression expr in
    let update_expr = S.{ obj_expr; updates = properties } in
    Ok (S.E_update (mk_reg braces#region update_expr))
  | _ :: snd_spread :: _ ->
    Strip_err.(
      make
        snd_spread#region
        Multiple_spreads_in_object
        ~hint:"Expand in place on of them.")

and strip_object_entry (node : Ast.object_entry)
    : (S.expr S.property reg option, _) result
  =
  match node with
  | Object_entry_pair pair ->
    let* pair = strip_pair pair in
    Ok (Some pair)
  | Object_entry_spread _ -> Ok None
  | Object_entry_method definition ->
    let make_parameter (node : (S.variable * S.type_expr) reg) : S.parameter reg =
      let variable, type_expr = node.value in
      let path = mk_reg variable#region S.{ path = []; selected = variable } in
      mk_reg node.region (S.P_var path, Some type_expr)
    in
    let* def = strip_method_definition [] definition in
    let S.{ method_sig; method_body } = def.value in
    let S.{ decorators; comments; static; method_name; generics; parameters; rhs_type } =
      method_sig.value
    in
    let property_name = method_name in
    let fun_body = S.Stmt_body method_body in
    let parameters = List.map ~f:make_parameter parameters in
    let rhs_type = Some rhs_type in
    let property_rhs : S.function_expr = S.{ generics; parameters; rhs_type; fun_body } in
    let property_rhs : S.expr =
      (* [method_body.region] is an approximation *)
      S.E_function (mk_reg method_body.region property_rhs)
    in
    let property : S.expr S.property =
      { decorators; comments; property_name; static; property_rhs }
    in
    Ok (Some (mk_reg definition#region property))
  | Object_entry_shorthand ident ->
    let comments = ident#comments in
    let comments = strip_comments comments in
    let decorators = extract_decorators comments in
    let property_name = strip_identifier ident in
    let property_rhs = S.E_var property_name in
    let static = None in
    let property : S.expr S.property =
      { decorators; comments; property_name; static; property_rhs }
    in
    Ok (Some (mk_reg ident#region property))

and strip_pair (node : Ast.pair wrap) : (S.expr S.property reg, _) result =
  let Ast.{ key; sym_colon = _; value } = node#payload in
  let comments = Ast.comments_of_property_name key in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let* property_name = strip_property_name key in
  let* property_rhs = strip_expression value in
  let static = None in
  let property : S.expr S.property =
    { decorators; comments; property_name; static; property_rhs }
  in
  Ok (mk_reg node#region property)

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
  let Ast.{ object_expr; optional_chain; index } = node#payload in
  let* () =
    match optional_chain with
    | None -> Ok ()
    | Some Ast.(Optional_chain sym) -> Strip_err.(make sym#region Optional_chaining)
  in
  let Ast.(Brackets brackets) = index in
  let exprs = brackets#payload.contents in
  let* exprs = strip_expressions exprs in
  let* expr =
    match exprs with
    | [ expr ] -> Ok expr
    | _ -> Strip_err.(make (Ast.region_of_brackets index) Multiple_values)
  in
  match expr with
  | E_int nat ->
    let* obj = strip_expression object_expr in
    Ok (S.E_subscript (mk_reg node#region (obj, nat)))
  | _ ->
    Strip_err.(
      make node#region Invalid_subscript ~hint:"Use a natural number as an index.")

(* Super (expression) *)

and strip_E_super (node : Ast.kwd_super) : (S.expr, _) result =
  Strip_err.(make node#region Super)

(* Template string *)

and strip_E_template_string (node : Ast.template_string wrap) : (S.expr, _) result =
  strip_template_string node

and strip_template_string (node : Ast.template_string wrap) : (S.expr, _) result =
  match node#payload with
  | _, [ String_fragment literal ], _ -> Ok (S.E_template literal)
  | _ -> Strip_err.(make node#region Template_string)

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
    | S.E_var v -> Ok (mk_reg node#region v)
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
    let path = mk_reg region S.{ path = []; selected = identifier } in
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

(* Object pattern

   {@js[const {x, y:alias} = {x:0, y:1};]}
 *)

and strip_object_pattern (node : Ast.object_pattern) : (S.pattern S._object, _) result =
  let Ast.(Braces braces) = node in
  let member_patterns = braces#payload.contents in
  let* object_pattern = Result.all @@ List.map ~f:strip_member_pattern member_patterns in
  Ok (mk_reg braces#region object_pattern)

and strip_member_pattern (node : Ast.member_pattern)
    : (S.pattern S.property reg, _) result
  =
  match node with
  | Member_pair_pattern pattern -> strip_pair_pattern pattern
  | Member_rest_pattern rest -> Strip_err.(make rest#region Rest_in_object_pattern)
  | Member_object_assignment asgmt ->
    Strip_err.(make asgmt#region Asgmt_in_object_pattern)
  | Member_shorthand_property ident ->
    let comments = ident#comments in
    let comments = strip_comments comments in
    let decorators = extract_decorators comments in
    let property_name = strip_identifier ident in
    let path = S.{ path = []; selected = property_name } in
    let property_rhs = S.P_var (mk_reg ident#region path) in
    let static = None in
    let property : S.pattern S.property =
      { decorators; comments; property_name; static; property_rhs }
    in
    let region = Ast.region_of_member_pattern node in
    Ok (mk_reg region property)

and strip_property_name (node : Ast.property_name) : (S.variable, _) result =
  match node with
  | Property_identifier ident -> Ok (strip_identifier ident)
  | Private_property_identifier hash -> Strip_err.(make hash#region Private_property)
  | String str_literal -> Strip_err.(make str_literal#region Property_as_string)
  | Number n -> Strip_err.(make (Ast.region_of_number n) Property_as_number)
  | Computed_property_name brackets ->
    Strip_err.(make (Ast.region_of_brackets brackets) Computed_property_name)

and strip_pair_pattern (node : Ast.pair_pattern wrap)
    : (S.pattern S.property reg, _) result
  =
  let Ast.{ key; sym_colon = _; value } = node#payload in
  let comments = comments_of_property_name key in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let* property_name = strip_property_name key in
  let static = None in
  let* property_rhs = strip_pair_value_pattern value in
  let property : S.pattern S.property =
    { decorators; comments; property_name; static; property_rhs }
  in
  Ok (mk_reg node#region property)

and strip_pair_value_pattern (node : Ast.pair_value_pattern) : (S.pattern, _) result =
  match node with
  | Pair_value pattern -> strip_pattern pattern
  | Pair_value_assignment asgmt -> Strip_err.(make asgmt#region Asgmt_in_object_pattern)

and comments_of_property_name (node : Ast.property_name) : Wrap.comment list =
  match node with
  | Property_identifier ident -> ident#comments
  | Private_property_identifier _ | String _ | Number _ | Computed_property_name _ -> []

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
    let path = S.{ path = []; selected = strip_identifier ident } in
    Ok (S.P_var (mk_reg ident#region path))
  | _ ->
    Strip_err.(
      make
        node#region
        Complex_rest_pattern
        ~hint:"Use variables or array/object patterns.")

(* Alias for external access by means of [Strip.statements] *)

let statements = strip_statements
