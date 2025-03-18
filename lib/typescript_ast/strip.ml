(* Decoding the tree-sitter CST for TypeScript and stripping it *)

open Core

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Snippet = Simple_utils.Snippet
module Ne_list = Simple_utils.Ne_list

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

(* Local dependencies *)

module Ast = Typescript_ast.Ast
module S = Ast_stripped
open Strip_err

exception Declaration of S.declaration

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

(* Formatting error messages *)

let pack_err ?(hint : string option) err region =
  let hint =
    match hint with
    | None | Some "" -> ""
    | Some msg -> "\nHint: " ^ msg
  in
  let value = Strip_err.to_string err ^ hint in
  Region.{ region; value }

let mk_err ?hint err region = Error (pack_err ?hint err region)

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
    mk_err Invalid_decorator_argument region

let filter_async (node : Ast.kwd_async option) : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some kwd_async -> mk_err Asynchronicity kwd_async#region

let filter_await (node : Ast.kwd_await option) : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some kwd_await -> mk_err Asynchronicity kwd_await#region

let filter_spread (node : Ast.arguments) : (Ast.expression list, _) result =
  let (Ast.Parens args) = node in
  let args = args#payload.contents in
  let filter (arg : Ast.argument) acc =
    match arg with
    | Ast.Expression expr -> Ok expr :: acc
    | Ast.Spread_element spread -> mk_err Spread_expression spread#region :: acc
  in
  let* exprs = Result.all @@ List.fold_right args ~init:[] ~f:filter in
  Ok exprs

let filter_static (node : Ast.method_scope) : (Region.t option, _) result =
  match node with
  | { kwd_static = None; kwd_override = None; kwd_readonly = None } -> Ok None
  | { kwd_static = Some kwd_static; _ } -> Ok (Some kwd_static#region)
  | { kwd_override = Some kwd; _ } | { kwd_readonly = Some kwd; _ } ->
    mk_err Property_scope kwd#region

let filter_method_scope (node : Ast.method_scope) : (unit, _) result =
  match node with
  | { kwd_static = None; kwd_override = None; kwd_readonly = None } -> Ok ()
  | { kwd_static = Some kwd; _ }
  | { kwd_override = Some kwd; _ }
  | { kwd_readonly = Some kwd; _ } -> mk_err Property_scope kwd#region

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
  | { kwd_accessor = Some kwd; _ } -> mk_err Public_field_scope kwd#region

let filter_access (node : Ast.accessibility_modifier option) : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some (Public kwd) | Some (Private kwd) | Some (Protected kwd) ->
    mk_err Property_access kwd#region

let filter_optional (node : Ast.sym_qmark option) error : (unit, _) result =
  match node with
  | None -> Ok ()
  | Some sym_qmark -> mk_err error sym_qmark#region

let rec filter_path (expr : S.expr) : (S.simple_path reg, _) result =
  match expr with
  | S.E_member { value = e, v; region } ->
    let* path = filter_path e in
    let S.{ path; selected } = path.value in
    Ok (mk_reg region S.{ path = selected :: path; selected = v })
  | S.E_var v -> Ok (mk_reg v#region S.{ path = []; selected = v })
  | _ -> mk_err Complex_path (S.region_of_expr expr)

let filter_path (expr : S.expr) : (S.simple_path reg, _) result =
  let* { value; region } = filter_path expr in
  let S.{ path; selected } = value in
  Ok (mk_reg region S.{ path = List.rev path; selected })

(* From some patterns in assignments to expressions *)

let rec destructuring_pattern_to_expression (node : Ast.destructuring_pattern)
    : (Ast.expression, _) result
  =
  match node with
  | Pattern_object obj -> mk_err Object_pattern_in_lhs (Ast.region_of_braces obj)
  | Pattern_array array -> array_pattern_to_expression array

and pattern_to_argument (node : Ast.pattern) : (Ast.argument, _) result =
  match node with
  | P_member_expression expr ->
    Ok (Expression (E_primary_expression (E_member_expression expr)))
  | P_subscript_expression expr ->
    Ok (Expression (E_primary_expression (E_subscript_expression expr)))
  | P_identifier ident -> Ok (Expression (E_primary_expression (E_identifier ident)))
  | P_undefined kwd_undefined ->
    Ok (Expression (E_primary_expression (E_undefined kwd_undefined)))
  | P_destructuring_pattern pattern ->
    let* expr = destructuring_pattern_to_expression pattern in
    Ok (Expression expr : Ast.argument)
  | P_non_null_expression expr ->
    Ok (Expression (E_primary_expression (E_non_null_expression expr)))
  | P_rest_pattern rest -> mk_err Rest_pattern_in_lhs rest#region

and array_cell_pattern_to_argument (node : Ast.array_cell_pattern)
    : (Ast.argument, _) result
  =
  match node with
  | Cell_pattern p -> pattern_to_argument p
  | Cell_assignment asgnmt -> mk_err Assignment_in_pattern asgnmt#region

and array_pattern_to_expression (node : Ast.array_pattern) : (Ast.expression, _) result =
  let (Brackets brackets) = node in
  let enclosed = brackets#payload in
  let cells = enclosed.contents in
  let* arguments = Result.all @@ List.map ~f:array_cell_pattern_to_argument cells in
  let enclosed = { enclosed with contents = arguments } in
  let arguments = Wrap.make enclosed brackets#region in
  let array = Ast.Brackets arguments in
  Ok Ast.(E_primary_expression (E_array array))

(* Stripping *)

let rec strip_statements (node : Ast.statements) : (S.statements option, _) result =
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
      let stmts' = Nonempty_list.(fst_stmt :: more_stmts) in
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
  | Export_as_namespace _ -> mk_err Invalid_export kwd_export#region
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
      mk_err Invalid_import kwd#region
  in
  let* () =
    match import_attribute with
    | None -> Ok ()
    | Some (Import_with (kwd, _)) | Some (Import_assert (kwd, _)) ->
      mk_err Invalid_import kwd#region
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
    mk_err Invalid_import ident#region ~hint:"Use named imports."

and strip_namespace_import region file_path (node : Ast.namespace_import wrap)
    : (S.import_decl, _) result
  =
  let Ast.{ sym_asterisk = _; kwd_as = _; identifier } = node#payload in
  let import_alias = strip_identifier identifier, file_path in
  let import_alias = mk_reg region import_alias in
  Ok (S.Import_all_as import_alias)

and strip_named_imports region file_path (node : Ast.named_imports)
    : (S.import_decl, _) result
  =
  let Ast.(Braces braces) = node in
  match braces#payload.contents with
  | [] -> mk_err Empty_import_list region
  | fst_import :: more_imports ->
    let* fst_import = strip_import_specifier fst_import in
    let* more_imports = Result.all @@ List.map ~f:strip_import_specifier more_imports in
    let imported_vars = Nonempty_list.(fst_import :: more_imports) in
    let import_from = mk_reg region (imported_vars, file_path) in
    Ok (S.Import_from import_from)

and strip_import_specifier (node : Ast.import_specifier) : (S.variable, _) result =
  let import_kind, spec = node in
  let* () =
    match import_kind with
    | None -> Ok ()
    | Some (Import_type kwd) | Some (Import_typeof kwd) ->
      mk_err Invalid_import kwd#region
  in
  strip_import_specifier' spec

and strip_import_specifier' (node : Ast.import_specifier') : (S.variable, _) result =
  match node with
  | Import_spec_name ident -> Ok (strip_identifier ident)
  | Import_spec_alias alias ->
    mk_err
      Import_and_rename
      alias.Ast.kwd_as#region
      ~hint:"Declare a new name after the import."

and strip_Import_require_clause (node : Ast.import_require_clause wrap)
    : (S.declaration, _) result
  =
  let Ast.
        { ident = _
        ; sym_equal = _
        ; kwd_require
        ; sym_lparen = _
        ; source = _
        ; sym_rparen = _
        }
    =
    node#payload
  in
  mk_err Invalid_import kwd_require#region

and strip_Import_source (node : Ast.string_literal) : (S.declaration, _) result =
  mk_err Invalid_import node#region

(* Debugger statement *)

and strip_S_debugger_statement (node : Ast.kwd_debugger) : (S.statement option, _) result =
  mk_err Debugger_statement node#region

(* Expression statement *)

and strip_S_expression_statement (node : Ast.expression_statement)
    : (S.statement option, _) result
  =
  try
    let* expr = strip_expression_statement ~is_stmt:true node in
    Ok (Option.map ~f:(fun e -> S.S_expr e) expr)
  with
  | Declaration decl -> Ok (Some (S.S_decl decl))

and strip_expression_statement ?(is_stmt = false) (node : Ast.expression_statement)
    : (S.expr option, _) result
  =
  let* exprs = strip_expressions ~is_stmt node in
  match exprs with
  | [] -> Ok None (* Should not happen *)
  | [ expr ] -> Ok (Some expr)
  | _ -> mk_err Multiple_values node#region

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

and strip_statement_block (node : Ast.statement_block) : (S.statements, _) result =
  let (Braces statements) = node in
  let statements' = statements#payload.contents in
  let* stmts = strip_statements statements' in
  match stmts with
  | None -> mk_err No_statements statements#region
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
      mk_err Multiple_values region
  in
  let* if_so = strip_statement consequence in
  let* if_so =
    match if_so with
    | None ->
      let region = Ast.region_of_statement consequence in
      mk_err Empty_consequence region
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
  strip_in_expressions expressions

and strip_in_expressions (node : Ast.in_expressions) : (S.expr list, _) result =
  match node with
  | Typed_expression (expression, type_annotation) ->
    let* expr = strip_expression expression in
    let* type_expr = strip_type_annotation type_annotation in
    let region = Ast.region_of_in_expressions node in
    Ok [ S.E_typed (mk_reg region (expr, type_expr)) ]
  | Sequence_expression expressions -> strip_expressions expressions

and strip_expressions ?is_stmt (node : Ast.expressions) : (S.expr list, _) result =
  let expressions = Nonempty_list.to_list node#payload in
  Result.all @@ List.map ~f:(strip_expression ?is_stmt) expressions

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
    | _ -> mk_err Multiple_values kwd_switch#region
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
    | [] -> mk_err Empty_switch braces#region
    | fst_case :: more_cases -> Ok Nonempty_list.(fst_case :: more_cases)
  in
  match defaults with
  | [] -> Ok (cases, None)
  | [ default ] ->
    let* default = strip_switch_default default in
    Ok (cases, Some default)
  | _ :: default :: _ -> mk_err Multiple_defaults default#region

and strip_switch_case (node : Ast.switch_case wrap) : (S.switch_case, _) result =
  let Ast.{ kwd_case = _; value; body } = node#payload in
  match value#payload with
  | Nonempty_list.[ expr ] ->
    let* expr = strip_expression expr in
    let* body = strip_statements body in
    Ok (expr, body)
  | _ :: expr :: _ ->
    let region = Ast.region_of_expression expr in
    mk_err Multiple_values region

and strip_switch_default (node : Ast.switch_default wrap) : (S.switch_default, _) result =
  let Ast.{ kwd_default = _; statements } = node#payload in
  strip_statements statements

(* For statement *)

and strip_S_for_statement (node : Ast.for_statement wrap) : (S.statement option, _) result
  =
  let Ast.
        { kwd_for = _
        ; sym_lparen = _
        ; initializer_
        ; condition
        ; increment
        ; sym_rparen = _
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
  let Ast.{ kwd_for = _; kwd_await; sym_lparen = _; for_header; sym_rparen = _; body } =
    node#payload
  in
  let* () = filter_await kwd_await in
  let* { index_kind; index; expr } = strip_for_header for_header in
  let* for_of_body = strip_statement body in
  let* for_of_body =
    match for_of_body with
    | None -> mk_err No_statements node#region
    | Some statement -> Ok statement
  in
  let for_of_stmt = S.{ index_kind; index; expr; for_of_body } in
  let for_of_stmt = mk_reg node#region for_of_stmt in
  Ok (Some (S.S_for_of for_of_stmt))

and strip_for_header (node : Ast.for_header) : (for_header, _) result =
  let Ast.{ range; operator; collection } = node in
  let* in_region =
    match operator with
    | In kwd_in ->
      mk_err
        Range_over_keys
        kwd_in#region
        ~hint:"Iterate over key and values using 'of' instead."
    | Of kwd_of -> Ok kwd_of#region
  in
  let* index_kind, index = strip_for_range range in
  let* exprs = strip_expressions collection in
  let* expr =
    match exprs with
    | [ expr ] -> Ok expr
    | _ -> mk_err Multiple_values in_region
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
    mk_err Invalid_loop_index region
  | For_in_parenthesized e ->
    let region = Ast.region_of_parens e in
    mk_err Invalid_loop_index region
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
      | _ -> mk_err Invalid_loop_index region)
    | _ -> mk_err Invalid_loop_index region)

and force_single_var (node : S.pattern S.element) : (S.variable, _) result =
  match node with
  | Element (P_var path as pattern) ->
    let S.{ path; selected } = path.value in
    (match path with
    | [] -> Ok selected
    | _ ->
      let region = S.region_of_pattern pattern in
      mk_err Not_a_variable region)
  | Element pattern | Spread pattern ->
    let region = S.region_of_pattern pattern in
    mk_err Not_a_variable region

and strip_for_in_var (node : Ast.for_in_var) =
  let Ast.{ kwd_var; variable = _; default = _ } = node in
  mk_err Var_declaration kwd_var#region ~hint:"Use 'let' or 'const'."

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
    | _ -> mk_err Multiple_values kwd_while#region
  in
  let* statement = strip_statement body in
  match statement with
  | None -> mk_err Empty_while node#region
  | Some statement -> Ok (expr, statement)

(* Do statement *)

and strip_S_do_statement (node : Ast.do_statement wrap) : (S.statement option, _) result =
  mk_err Do_while_loop node#region

(* Try statement *)

and strip_S_try_statement (node : Ast.try_statement wrap) : (S.statement option, _) result
  =
  mk_err Exception node#region

(* With statement *)

and strip_S_with_statement (node : Ast.with_statement wrap)
    : (S.statement option, _) result
  =
  mk_err With_statement node#region

(* Break statement *)

and strip_S_break_statement (node : Ast.break_statement wrap)
    : (S.statement option, _) result
  =
  let Ast.{ kwd_break; stmt_id } = node#payload in
  match stmt_id with
  | Some ident -> mk_err Label ident#region
  | None -> Ok (Some (S.S_break kwd_break#region))

(* Continue statement *)

and strip_S_continue_statement (node : Ast.continue_statement wrap)
    : (S.statement option, _) result
  =
  mk_err Continue node#region

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
    | _ -> mk_err Multiple_values node#region)

(* Throw statement *)

and strip_S_throw_statement (node : Ast.throw_statement wrap)
    : (S.statement option, _) result
  =
  mk_err Exception node#region

(* Empty statement *)

and strip_S_empty_statement (node : Region.t) : (S.statement option, _) result =
  ignore node;
  Ok None

(* Labeled statement *)

and strip_S_labeled_statement (node : Ast.labeled_statement wrap)
    : (S.statement option, _) result
  =
  mk_err Label node#region

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
  let* rhs_type = map_opt strip_call_return_type return_type in
  let call_sig = { generics; parameters; rhs_type } in
  Ok (mk_reg node#region call_sig)

and strip_call_return_type (node : Ast.call_return_type) : (S.type_expr, _) result =
  match node with
  | Ast.Type_annotation (_, type_expr) ->
    let* type_expr = strip_type_expr type_expr in
    Ok type_expr
  | Asserts_annotation a ->
    let region = Ast.region_of_asserts a in
    mk_err Type_assertion region
  | Type_predicate_annotation w -> mk_err Type_predicate w#region

(* Generator function declaration *)

and strip_D_generator_function_declaration
    (node : Ast.generator_function_declaration wrap)
    : (S.declaration, _) result
  =
  mk_err Generator node#region

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
    | type_var :: _ -> mk_err Generic_class type_var#region
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
  | [] -> mk_err Empty_class braces#region
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
    mk_err Method_signature_in_class signature#region ~hint:"Provide a method body."
  | Call_static_block (kwd_static, _) -> mk_err Call_static_block kwd_static#region
  | Abstract_method_signature signature -> mk_err Abstract_method signature#region
  | Index_signature signature -> mk_err Index_signature signature#region
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
    | Some kwd_declare -> mk_err Declare_definition kwd_declare#region
  in
  let* static = filter_field_scope scope in
  let* name = strip_property_name name in
  let* () =
    match mode with
    | None -> Ok ()
    | Some (Optional sym | Definite_assert sym) -> mk_err Field_mode sym#region
  in
  let* field_type = map_opt strip_type_annotation type_ in
  let* field_value =
    match default with
    | None -> mk_err No_default (S.region_of_property_name name)
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
    mk_err Extends_clause kwd_extends#region
  | Some (Implements_clause (_, type_exprs)) ->
    let type_exprs = Nonempty_list.to_list type_exprs in
    let* type_exprs = Result.all @@ List.map ~f:strip_type_expr type_exprs in
    let filter type_expr =
      match type_expr with
      | S.T_path path -> Ok path
      | _ -> mk_err Invalid_implements (S.region_of_type_expr type_expr)
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
  | Decorator_member_expression _ -> mk_err Member_decorator region
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
  | Parenthesized_member _ -> mk_err Member_decorator parens#region
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
      mk_err Type_arguments_in_decorator chevrons#region
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
      mk_err Spread_expression region)
  | _ :: snd_arg :: _ ->
    let region = Ast.region_of_argument snd_arg in
    mk_err Multiple_arguments_in_decorator region

and strip_function_or_property (node : Ast.function_or_property) : (string, _) result =
  match node with
  | Function_name ident ->
    let variable = strip_identifier ident in
    Ok variable#payload
  | Qualified_member_expression _ ->
    let region = Ast.region_of_function_or_property node in
    mk_err Member_decorator region

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
    mk_err Definite_asgmt_assertion sym_qmark#region

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
      mk_err Unitialised_variable region
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
  mk_err Var_declaration node#region ~hint:"Use the 'let' modifier."

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
    | None -> mk_err Return_type_absent node#region
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
      mk_err Not_a_variable region)
  | _ -> mk_err Not_a_variable (S.region_of_pattern pattern)

(* Abstract class declaration *)

and strip_D_abstract_class_declaration (node : Ast.abstract_class_declaration wrap)
    : (S.declaration, _) result
  =
  mk_err Abstract_class node#region

(* Module declaration *)

and strip_D_module_declaration (node : Ast.module_declaration wrap)
    : (S.declaration, _) result
  =
  mk_err Module node#region ~hint:"Try using namespaces."

(* Namespace declaration *)

and strip_D_internal_module (node : Ast.internal_module wrap) : (S.declaration, _) result =
  let Ast.{ kwd_namespace = _; module_name; module_body } = node#payload in
  let* (namespace_name : S.variable) = strip_module_name module_name in
  let* (namespace_body : S.statements) =
    match module_body with
    | None -> mk_err No_statements node#region
    | Some block -> strip_statement_block block
  in
  let decl = S.{ namespace_name; namespace_type = []; namespace_body } in
  Ok (S.D_namespace (mk_reg node#region decl))

and strip_module_name (node : Ast.module_name) : (S.variable, _) result =
  match node with
  | Module_string str -> mk_err Namespace_string str#region
  | Module_ident ident -> Ok ident
  | Module_nested nested -> mk_err Namespace_nested nested#region

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
    mk_err Type_constraint region
  | _, Some (_, type_expr) ->
    let region = Ast.region_of_type_expr type_expr in
    mk_err Default_type_parameter region

(* Enum declaration *)

and strip_D_enum_declaration (node : Ast.enum_declaration wrap)
    : (S.declaration, _) result
  =
  mk_err Enumerated node#region

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
      mk_err Interface_with_type_parameters region
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
  | Export_statement stmt -> mk_err Export_member stmt#region
  | Property_signature signature -> strip_property_signature_as_intf_entry signature
  | Call_signature signature -> mk_err Call_signature signature#region
  | Construct_signature signature -> mk_err Constructor signature#region
  | Index_signature signature -> mk_err Index_signature signature#region
  | Method_signature signature -> strip_method_signature_as_intf_entry signature

and strip_property_signature_as_intf_entry (node : Ast.property_signature wrap)
    : (S.intf_entry reg, _) result
  =
  let Ast.{ access; scope; name; sym_qmark; type_ } = node#payload in
  let* () = filter_access access in
  let* () = filter_method_scope scope in
  let* entry_name = strip_property_name name in
  let entry_optional = Option.map ~f:(fun sym_qmark -> sym_qmark#region) sym_qmark in
  let* entry_type = map_opt strip_type_annotation type_ in
  match entry_type with
  | None -> mk_err Missing_type node#region
  | Some entry_type ->
    let comments = S.comments_of_property_name entry_name in
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
    | Some (Set kwd | Get kwd) -> mk_err Set_get_all kwd#region
    | Some (All sym) -> mk_err Set_get_all sym#region
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
    | None -> mk_err Return_type_absent node#region
    | Some rhs_type -> Ok rhs_type
  in
  let rhs_type = S.T_fun (mk_reg call_sig.region (parameters, rhs_type)) in
  let entry_type =
    match generics with
    | [] -> rhs_type
    | _ -> S.T_for_all (mk_reg call_sig.region (generics, rhs_type))
  in
  let comments = S.comments_of_property_name entry_name in
  let comments = strip_comments comments in
  let decorators = extract_decorators comments in
  let entry = S.{ decorators; comments; entry_name; entry_optional; entry_type } in
  Ok (mk_reg node#region entry)

and strip_extends (node : Ast.extends_type_clause) : (S.simple_path reg list, _) result =
  let Ast.{ kwd_extends = _; extensions } = node in
  let extensions = Nonempty_list.to_list extensions in
  Result.all @@ List.map ~f:strip_type_extension extensions

and strip_type_extension (node : Ast.type_extension) : (S.simple_path reg, _) result =
  match node with
  | Extends_type ident ->
    let path = S.{ path = []; selected = strip_type_identifier ident } in
    Ok (mk_reg ident#region path)
  | Extends_nested nested -> Ok (strip_nested_type_identifier nested)
  | Extends_generic gen_type -> mk_err Generic_class_extension gen_type#region

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
  let path = List.rev (Nonempty_list.to_list path) in
  let path = List.map ~f:strip_type_identifier path
  and selected = strip_type_identifier selected in
  mk_reg node#region S.{ path; selected }

(* Ambient declaration *)

and strip_D_ambient_declaration (node : Ast.ambient_declaration wrap)
    : (S.declaration, _) result
  =
  mk_err Ambient_declaration node#region

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
  | T_any kwd_any -> mk_err Any_type kwd_any#region
  | T_number kwd_number ->
    mk_err Number_type kwd_number#region ~hint:"Use 'bigint' or 'nat'."
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
  | T_symbol kwd_symbol -> mk_err Symbol_type kwd_symbol#region
  | T_unique_symbol kwd_unique_symbol ->
    mk_err Unique_symbol_type kwd_unique_symbol#region
  | T_void kwd_void -> mk_err Void_type kwd_void#region
  | T_unknown kwd_unknown -> mk_err Unknown_type kwd_unknown#region
  | T_never kwd_never ->
    let region = kwd_never#region in
    let path = mk_reg region S.{ path = []; selected = kwd_never } in
    Ok (S.T_path path)
  | T_object kwd_object -> mk_err Object_type kwd_object#region

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
  let path = List.rev (Nonempty_list.to_list path) in
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
  let error = mk_err Invalid_parameter_of node#region in
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
  let type_args = Nonempty_list.to_list type_args in
  Result.all @@ List.map ~f:strip_type_expr type_args

(* Object type *)

and strip_T_object_type (node : Ast.object_type) : (S.type_expr, _) result =
  let Ast.(Braces braces) = node in
  let decorate = spool @@ extract_decorators @@ strip_comments braces#comments in
  let member_types = braces#payload.contents in
  let* members = Result.all @@ List.map ~f:strip_member_type member_types in
  let object_type = mk_reg braces#region members in
  let object_type = { object_type with value = object_type.value } in
  Ok (decorate @@ S.T_object object_type)

and strip_member_type (node : Ast.member_type) : (S.member_type reg, _) result =
  match node with
  | Export_statement stmt -> mk_err Export_member stmt#region
  | Property_signature signature -> strip_property_signature signature
  | Call_signature signature -> mk_err Call_signature signature#region
  | Construct_signature signature -> mk_err Constructor signature#region
  | Index_signature signature -> mk_err Index_signature signature#region
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
  | None -> mk_err Missing_type node#region
  | Some rhs_type ->
    let comments = S.comments_of_property_name property_name in
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
    | Some (Set kwd | Get kwd) -> mk_err Set_get_all kwd#region
    | Some (All sym) -> mk_err Set_get_all sym#region
  in
  let* property_name = strip_property_name name in
  let* () = filter_optional optional Optional_member in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* parameters = Result.all @@ List.map ~f:filter_parameter parameters in
  let* parameters = filter_type_annotations parameters in
  let* rhs_type =
    match rhs_type with
    | None -> mk_err Return_type_absent node#region
    | Some rhs_type -> Ok rhs_type
  in
  let rhs_type = S.T_fun (mk_reg call_sig.region (parameters, rhs_type)) in
  let rhs_type =
    match generics with
    | [] -> rhs_type
    | _ -> S.T_for_all (mk_reg call_sig.region (generics, rhs_type))
  in
  let comments = S.comments_of_property_name property_name in
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
    | Some (Set kwd | Get kwd) -> mk_err Set_get_all kwd#region
    | Some (All sym) -> mk_err Set_get_all sym#region
  in
  let* method_name = strip_property_name name in
  let* () = filter_optional optional Optional_member in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* parameters = Result.all @@ List.map ~f:filter_parameter parameters in
  let* parameters = filter_type_annotations parameters in
  let* rhs_type =
    match rhs_type with
    | None -> mk_err Return_type_absent node#region
    | Some rhs_type -> Ok rhs_type
  in
  let rhs_type =
    match generics with
    | [] -> rhs_type
    | _ -> S.T_for_all (mk_reg call_sig.region (generics, rhs_type))
  in
  let comments = S.comments_of_property_name method_name in
  let comments = strip_comments comments in
  let* decorators = strip_decorators decorators in
  let decorators = decorators @ extract_decorators comments in
  let signature =
    S.{ decorators; comments; static; method_name; generics; parameters; rhs_type }
  in
  Ok (mk_reg node#region signature)

(* Array type *)

and strip_T_array_type (node : Ast.array_type wrap) : (S.type_expr, _) result =
  mk_err Array_type node#region

(* Tuple type *)

and strip_T_tuple_type (node : Ast.tuple_type) : (S.type_expr, _) result =
  let (Brackets brackets) = node in
  let members = brackets#payload.contents in
  let* members = Result.all @@ List.map ~f:strip_tuple_type_member members in
  match members with
  | [] -> mk_err Empty_tuple_type brackets#region
  | fst_comp :: components ->
    let members = Nonempty_list.(fst_comp :: components) in
    Ok (S.T_tuple (mk_reg brackets#region members))

and strip_tuple_type_member (node : Ast.tuple_type_member) : (S.type_expr, _) result =
  let region = Ast.region_of_tuple_type_member node in
  match node with
  | Ast.Tuple_parameter _
  | Tuple_optional_parameter _
  | Tuple_optional_type _
  | Tuple_rest_type _ ->
    mk_err Unsupported_tuple_member region ~hint:"Use a single type expression."
  | Tuple_type type_expr -> strip_type_expr type_expr

(* Flow maybe type *)

and strip_T_flow_maybe_type (node : (Ast.sym_qmark * Ast.primary_type) wrap)
    : (S.type_expr, _) result
  =
  mk_err Maybe_type node#region

(* Type query *)

and strip_T_type_query (node : (Ast.kwd_keyof * Ast.type_query) wrap)
    : (S.type_expr, _) result
  =
  mk_err Type_query node#region

(* Index type query *)

and strip_T_index_type_query (node : (Ast.kwd_keyof * Ast.primary_type) wrap)
    : (S.type_expr, _) result
  =
  mk_err Index_type_query node#region

(* "This" as a type *)

and strip_T_this (node : Ast.kwd_this) : (S.type_expr, _) result = mk_err This node#region

and strip_T_existential_type (node : Ast.sym_asterisk) : (S.type_expr, _) result =
  mk_err Existential_type node#region

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
  mk_err Unary_type node#region

and strip_T_number (node : Ast.number) : (S.type_expr, _) result =
  let region = Ast.region_of_number node in
  match node with
  | Hex _ | Bin _ | Oct _ -> mk_err Unsupported_number region ~hint:"Use a decimal."
  | Dec (literal, _) ->
    let lexeme, q = literal#payload in
    if Z.equal (Q.den q) Z.one
    then (
      let literal = Wrap.make (lexeme, Q.to_bigint q) literal#region in
      Ok (S.T_int literal))
    else mk_err Non_integer_as_type region

and strip_T_string (node : Ast.string_literal) : (S.type_expr, _) result =
  Ok (S.T_string node)

and strip_T_true (node : Ast.kwd_true) : (S.type_expr, _) result =
  mk_err Singleton_type_true node#region

and strip_T_false (node : Ast.kwd_false) : (S.type_expr, _) result =
  mk_err Singleton_type_false node#region

and strip_T_null (node : Ast.kwd_null) : (S.type_expr, _) result =
  mk_err Null_type node#region

and strip_T_undefined (node : Ast.kwd_undefined) : (S.type_expr, _) result =
  mk_err Undefined_type node#region

(* Lookup type *)

and strip_T_lookup_type (node : Ast.lookup_type wrap) : (S.type_expr, _) result =
  mk_err Lookup_type node#region

(* Conditional type *)

and strip_T_conditional_type (node : Ast.conditional_type wrap) : (S.type_expr, _) result =
  mk_err Conditional_type node#region

(* Template literal type *)

and strip_T_template_literal_type (node : Ast.template_literal_type wrap)
    : (S.type_expr, _) result
  =
  mk_err Template_literal_type node#region

(* Intersection type *)

and strip_T_intersection_type (node : Ast.intersection_type wrap)
    : (S.type_expr, _) result
  =
  mk_err Intersection_type node#region

(* Union type and sum type (see comment about the latter in [Ast_stripped])

   There are three stages to the stripping of union types.

     1. We transform the AST node for a union type into Disjunctive
     Normal Form (DNF), as expected by the rest of the pipeline, by
     means of a call to the function [flatten]. Note that the function
     [flatten_type_expr] is quadratic in the number of summands
     because of the use of [Ne_list.append] instead of folding with an
     accumulator. We do not expect this to be an issue, as the number
     of operands is always small.

     2. We try to build a sum type out of the union type, as they are
     a special case handled apart by the type-checker, by calling the
     function [filter_sum]. For example, the declaration

       type parameter = ["Increment", int] | ["Decrement", int] | ["Reset"];

     will be filtered as a sum type, not a general union type.

     3. We look a vertical bar starting the union type. If none, then
     there are no decorators; otherwise we extract and decorator in
     them and create a [S.T_decorated] node wrapping the resulting
     union or sum type. *)

and strip_T_union_type (node : Ast.union_type wrap) : (S.type_expr, _) result =
  let decorate = decoration_of_union_type node in
  let types =
    match node#payload with
    | None, _, type_2 -> Nonempty_list.[ type_2 ]
    | Some type_1, _, type_2 -> Nonempty_list.[ type_1; type_2 ]
  in
  let Nonempty_list.(head :: tail) = flatten types in
  let* stripped_head = strip_type_expr head in
  let* stripped_tail = Result.all @@ List.map ~f:strip_type_expr tail in
  let union_type = Nonempty_list.(stripped_head :: stripped_tail) in
  Ok (decorate @@ filter_sum union_type node#region)

and decoration_of_union_type (node : Ast.union_type wrap) =
  match node#payload with
  | None, sym_vbar, _ -> spool @@ extract_decorators @@ strip_comments sym_vbar#comments
  | Some (Ast.T_primary_type (T_union_type type_1)), _, _ ->
    decoration_of_union_type type_1
  | _ -> fun t -> t

and spool (decorators : S.decorator list) (t_expr : S.type_expr) : S.type_expr =
  match decorators with
  | [] -> t_expr
  | decorator :: decorators -> S.T_decorated (decorator, spool decorators t_expr)

and filter_sum (node : S.type_expr Nonempty_list.t) region : S.type_expr =
  let variant_of_type_expr : S.type_expr -> S.variant reg option = function
    | T_tuple members ->
      let Nonempty_list.(first_memb :: rest) = members.value in
      (match first_memb with
      | T_string literal -> Some Region.{ value = literal, rest; region }
      | _ -> None)
    | _ -> None
  in
  let Nonempty_list.(first_member :: more_members) = node in
  let first_member = variant_of_type_expr first_member in
  let more_members = List.map ~f:variant_of_type_expr more_members in
  let more_members = Option.all more_members in
  match first_member, more_members with
  | None, _ | _, None -> S.T_union (mk_reg region node)
  | Some first, Some more ->
    let members = Nonempty_list.(first :: more) in
    S.T_sum (mk_reg region members)

and flatten (node : Ast.type_expr Nonempty_list.t) : Ast.type_expr Nonempty_list.t =
  Nonempty_list.concat_map ~f:flatten_type_expr node

and flatten_type_expr : Ast.type_expr -> Ast.type_expr Nonempty_list.t = function
  | Ast.T_primary_type (T_union_type t) ->
    (match t#payload with
    | None, _, type_2 -> flatten_type_expr type_2
    | Some type_1, _, type_2 ->
      Ne_list.append (flatten_type_expr type_1) (flatten_type_expr type_2))
  | type_expr -> Nonempty_list.[ type_expr ]

(* Function type *)

and strip_T_function_type (node : Ast.function_type wrap) : (S.type_expr, _) result =
  let Ast.{ type_parameters; parameters; sym_arrow = _; return_type } = node#payload in
  let* t_params = strip_list_opt strip_type_parameters type_parameters in
  let* v_params = strip_formal_parameters parameters in
  let* v_params = Result.all @@ List.map ~f:filter_parameter v_params in
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
    | variable, None -> mk_err Missing_type variable#region
    | variable, Some type_expr -> Ok Region.{ value = variable, type_expr; region }
  in
  Result.all @@ List.map ~f:check node

and strip_formal_parameters (node : Ast.formal_parameters)
    : ((S.pattern * S.type_expr option) reg list, _) result
  =
  let (Ast.Parens parens) = node in
  let parameters = parens#payload.contents in
  Result.all @@ List.map ~f:strip_formal_parameter parameters

and strip_formal_parameter (node : Ast.formal_parameter wrap)
    : ((S.pattern * S.type_expr option) reg, _) result
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
      mk_err Default_argument region
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

and strip_parameter_name (node : Ast.parameter_name wrap) : (S.pattern, _) result =
  let Ast.{ decorators; access; kwd_override; kwd_readonly; pattern } = node#payload in
  let* () =
    match decorators with
    | [] -> Ok ()
    | decorator :: _ ->
      let region = Ast.region_of_decorator decorator in
      mk_err Decorated_parameter region
  in
  let* () =
    match access with
    | None -> Ok ()
    | Some modifier ->
      let region = Ast.region_of_accessibility_modifier modifier in
      mk_err Access_parameter region
  in
  let* () =
    match kwd_override with
    | None -> Ok ()
    | Some kwd_override -> mk_err Override_parameter kwd_override#region
  in
  let* () =
    match kwd_readonly with
    | None -> Ok ()
    | Some kwd_readonly -> mk_err Readonly_parameter kwd_readonly#region
  in
  strip_parameter_pattern pattern

and strip_parameter_pattern (node : Ast.parameter_pattern) : (S.pattern, _) result =
  match node with
  | Parameter_pattern pattern -> strip_pattern pattern
  | Parameter_this kwd_this ->
    mk_err Non_variable_parameter kwd_this#region ~hint:"Rename 'this'."

and strip_identifier (node : Ast.identifier) : S.variable = node

and strip_return_type (node : Ast.return_type) : (S.type_expr, _) result =
  let region = Ast.region_of_return_type node in
  match node with
  | Return_type type_expr -> strip_type_expr type_expr
  | Return_asserts _ -> mk_err Type_assertion region
  | Return_type_predicate _ -> mk_err Type_predicate region

(* Readonly type *)

and strip_T_readonly_type (node : Ast.readonly_type wrap) : (S.type_expr, _) result =
  mk_err Readonly_type node#region

(* Constructor type *)

and strip_T_constructor_type (node : Ast.constructor_type wrap) : (S.type_expr, _) result =
  mk_err Constructor_type node#region

(* Infer type *)

and strip_T_infer_type (node : Ast.infer_type wrap) : (S.type_expr, _) result =
  mk_err Conditional_type node#region

(* Member expression (in type expressions) *)

and strip_T_type_query_member_expression_in_type_annotation
    (node : Ast.type_query_member_expression_in_type_annotation wrap)
    : (S.type_expr, _) result
  =
  mk_err Type_query node#region

(* Call expression (in type expressions) *)

and strip_T_type_query_call_expression_in_type_annotation
    (node : Ast.type_query_call_expression_in_type_annotation wrap)
    : (S.type_expr, _) result
  =
  mk_err Type_query node#region

(* EXPRESSIONS *)

and strip_expression ?is_stmt (node : Ast.expression) : (S.expr, _) result =
  match node with
  | E_as_expression e -> strip_E_as_expression e
  | E_assignment_expression e -> strip_E_assignment_expression e
  | E_augmented_assignment_expression e -> strip_E_augmented_assignment_expression e
  | E_await_expression e -> strip_E_await_expression e
  | E_binary_expression e -> strip_E_binary_expression e
  | E_instantiation_expression e -> strip_E_instantiation_expression e
  | E_internal_module e -> strip_E_internal_module ?is_stmt e
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
  | As_const kwd_const -> mk_err Constant_type kwd_const#region

(* Assignment expression *)

and strip_E_assignment_expression (node : Ast.assignment_expression wrap)
    : (S.expr, _) result
  =
  let Ast.{ kwd_using; left; sym_equal = _; right } = node#payload in
  let* () =
    match kwd_using with
    | None -> Ok ()
    | Some kwd -> mk_err Finalised_const kwd#region
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
      mk_err Multiple_values region)

and strip_lhs_expression (node : Ast.lhs_expression) : (S.expr, _) result =
  let* (lhs : Ast.expression) =
    match node with
    | Member_expression expr -> Ok (Ast.E_primary_expression (E_member_expression expr))
    | Subscript_expression expr -> Ok (E_primary_expression (E_subscript_expression expr))
    | Identifier ident -> Ok (E_primary_expression (E_identifier ident))
    | Undefined kwd_undefined -> Ok (E_primary_expression (E_undefined kwd_undefined))
    | Pattern pattern -> destructuring_pattern_to_expression pattern
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
  | Member_expression w -> mk_err Complex_lhs w#region ~hint:"Use a variable."
  | Subscript_expression w -> mk_err Complex_lhs w#region ~hint:"Use a variable."
  | Identifier ident -> Ok (S.E_var (strip_identifier ident))
  | Parenthesized_expression expr ->
    let* exprs = strip_parenthesized_expression expr in
    let* expr =
      match exprs with
      | [ expr ] -> Ok expr
      | _ ->
        let region = Ast.region_of_augmented_assignment_lhs node in
        mk_err Multiple_values region
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
  | Bitwise_xor_eq _ -> Ok (fun args -> S.E_bit_xor_eq args) (* ^= *)
  | Bitwise_and_eq _ -> Ok (fun args -> S.E_bit_and_eq args) (* &= *)
  | Bitwise_or_eq _ -> Ok (fun args -> S.E_bit_or_eq args) (* |= *)
  | Bitwise_sr_eq _ -> Ok (fun args -> S.E_bit_sr_eq args) (* >>= *)
  | Bitwise_usr_eq sym -> mk_err Bitwise_usr_eq sym#region (* >>>= *)
  | Bitwise_sl_eq _ -> Ok (fun args -> S.E_bit_sl_eq args) (* <<= *)
  | Exp_eq sym -> mk_err Exp_eq sym#region (* **= *)
  | Logical_and_eq sym ->
    (* &&= *)
    mk_err Logical_and_eq sym#region ~hint:"Use \"=\" and \"&&\" separately."
  | Logical_or_eq sym ->
    (* ||= *)
    mk_err Logical_or_eq sym#region ~hint:"Use \"=\" and \"||\" separately."
  | Non_null_eq sym ->
    (* ??= *)
    mk_err Non_null sym#region

(* Await-expression *)

and strip_E_await_expression (node : Ast.await_expression wrap) : (S.expr, _) result =
  mk_err Asynchronicity node#region

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
  | Logical_and _ -> Ok (fun arg -> S.E_and arg) (* && *)
  | Logical_or _ -> Ok (fun arg -> S.E_or arg) (* || *)
  | Bitwise_sr _ -> Ok (fun arg -> S.E_bit_sr arg) (* >> *)
  | Bitwise_usr sym -> mk_err Bitwise_usr_eq sym#region (* >>> *)
  | Bitwise_sl _ -> Ok (fun arg -> S.E_bit_sl arg) (* << *)
  | Bitwise_and _ -> Ok (fun arg -> S.E_bit_and arg) (* & *)
  | Bitwise_xor _ -> Ok (fun arg -> S.E_bit_xor arg) (* ^ *)
  | Bitwise_or _ -> Ok (fun arg -> S.E_bit_or arg) (* | *)
  | Add _ -> Ok (fun arg -> S.E_add arg) (* + *)
  | Sub _ -> Ok (fun arg -> S.E_sub arg) (* - *)
  | Mult _ -> Ok (fun arg -> S.E_mult arg) (* * *)
  | Div _ -> Ok (fun arg -> S.E_div arg) (* / *)
  | Rem _ -> Ok (fun arg -> S.E_rem arg) (* % *)
  | Exp sym -> mk_err Exp sym#region (* * *)
  | Lt _ -> Ok (fun arg -> S.E_lt arg) (* < *)
  | Leq _ -> Ok (fun arg -> S.E_leq arg) (* <= *)
  | Equal _ -> Ok (fun arg -> S.E_equal arg) (* == *)
  | Strict_eq sym ->
    (* === *)
    mk_err Strict_equality sym#region ~hint:"Use '=='"
  | Neq _ -> Ok (fun arg -> S.E_neq arg) (* != *)
  | Strict_neq sym ->
    (* !== *)
    mk_err Strict_equality sym#region ~hint:"Use '!='"
  | Geq _ -> Ok (fun arg -> S.E_geq arg) (* >= *)
  | Gt _ -> Ok (fun arg -> S.E_gt arg) (* > *)
  | Non_null sym -> mk_err Non_null sym#region (* ?? *)
  | Instance_of kwd_instanceof ->
    (* instanceof *)
    mk_err Instanceof kwd_instanceof#region
  | In kwd_in ->
    (* in *)
    mk_err In kwd_in#region

and strip_lhs_bin_expression (node : Ast.lhs_bin_expression) : (S.expr, _) result =
  match node with
  | Lhs_bin_expression expr ->
    let* expr = strip_expression expr in
    Ok expr
  | Lhs_bin_hash hash -> mk_err Private_property hash#region

(* Instantiation expression *)

and strip_E_instantiation_expression (node : Ast.instantiation_expression wrap)
    : (S.expr, _) result
  =
  mk_err Type_parameter_instantiation node#region

(* Internal module expression *)

and strip_E_internal_module ?(is_stmt = false) (node : Ast.internal_module wrap)
    : (S.expr, _) result
  =
  if is_stmt
  then
    let* decl = strip_D_internal_module node in
    raise (Declaration decl)
  else mk_err Namespace_expression node#region

(* New-expression *)

and strip_E_new_expression (node : Ast.new_expression wrap) : (S.expr, _) result =
  mk_err Class_instantiation node#region

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
  Ok (filter_constructor_application array brackets#region)

and strip_argument (node : Ast.argument) : (S.expr S.element, _) result =
  match node with
  | Expression expr ->
    let* expr = strip_expression expr in
    Ok (S.Element expr)
  | Spread_element spread ->
    let _, expr = spread#payload in
    let* expr = strip_expression expr in
    Ok (S.Spread expr)

(* Application of data constructors

   The convention is that the application of data constructors is
   syntactically distinguished from an array by having the following
   form:

     ["constructor" as "constructor", expression_1, ..., expression_n]

   where "constructor" is a data constructor, and "expression_1"
   etc. are its arguments. (See function [strip_T_union_type] and its
   comment.)

   Note how we always return an expression, that is, the function is
   complete. This because our convention is idiosyncrasic to JsLIGO,
   so any deviation from it is not considered an error. (We might
   revisit this after feedback from users.)
 *)

and filter_constructor_application (node : S.expr S.element list) region : S.expr =
  let array = S.E_array (mk_reg region node) in
  match node with
  | [] -> array
  | first :: more ->
    (match first with
    | Element (S.E_typed as_expr) ->
      (match as_expr.value with
      | S.E_string literal_1, S.T_string literal_2
        when String.equal literal_1#payload literal_2#payload ->
        let ctor = literal_1 in
        (match filter_constructor_arguments more with
        | None -> array
        | Some args -> S.E_ctor_app (mk_reg region (ctor, args)))
      | _ -> array)
    | _ -> array)

and filter_constructor_arguments (node : S.expr S.element list) : S.expr list option =
  Option.all @@ List.map ~f:filter_constructor_argument node

and filter_constructor_argument (node : S.expr S.element) : S.expr option =
  match node with
  | Spread _ -> None
  | Element expr -> Some expr

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

(* Pattern matching

   We assume the existence of a predefined function "$match" taking
   two arguments: the first is the subject expression, that is, the
   expression to be matched; the second is an object whose contents is
   used to filter and handle all the cases of the matching.

   For example, given the following declaration of a sum type:

     type parameter = ["Increment", int] | ["Decrement", int] | ["Reset"];

   the following expression of type "int" is a pattern matching each
   constructor:

      $match(p, {
        Increment: (n) => storage + n,
        Decrement: (n) => storage - n,
        Reset: () => 0,
      })

   assuming that "p" is of type "parameter", and "storage" of type "int".
 *)

and filter_match_clauses (node : S.expr) : (S.match_clause Ne_list.t, _) result =
  match node with
  | E_object obj ->
    (match obj.value with
    | [] -> mk_err Empty_match obj.region
    | first_property :: more_properties ->
      let* head = filter_match_clause first_property in
      let* tail = Result.all @@ List.map ~f:filter_match_clause more_properties in
      Ok Nonempty_list.(head :: tail))
  | _ ->
    mk_err
      Pattern_matching
      (S.region_of_expr node)
      ~hint:"The object contains arrow functions for each case."

and filter_match_clause (node : S.expr S.property reg) : (S.match_clause, _) result =
  let S.{ decorators = _; comments = _; property_name; static = _; property_rhs } =
    node.value
  in
  let constructor = property_name in
  match property_rhs with
  | Some (E_arrow_fun arrow_fun) ->
    let S.{ generics; parameters; rhs_type = _; fun_body } = arrow_fun.value in
    let* () =
      match generics with
      | [] -> Ok ()
      | _ -> mk_err Match_clause_rhs node.region
    in
    let* filter =
      match parameters with
      | [] -> Ok None
      | [ parameter ] -> Ok (Some parameter)
      | _ :: param_2 :: _ -> mk_err Match_filter param_2.region
    in
    let* clause_expr =
      match fun_body with
      | S.Expr_body expr -> Ok expr
      | Stmt_body stmts -> mk_err Match_clause_rhs stmts.region
    in
    Ok S.{ constructor; filter; clause_expr }
  | _ -> mk_err Match_clause_rhs node.region

(* Call expression *)

and strip_E_call_expression (node : Ast.call_expression) : (S.expr, _) result =
  match node with
  | Call fun_call -> strip_fun_call fun_call
  | Member expr_call -> mk_err Optional_chaining expr_call#region

and strip_fun_call (node : (Ast.fun_call, Ast.arguments_to_call) Ast.call wrap)
    : (S.expr, _) result
  =
  let Ast.{ lambda; type_arguments; arguments } = node#payload in
  let* (lambda : S.expr) =
    match lambda with
    | Fun_call expr -> strip_expression expr
    | Import kwd_import -> mk_err Import kwd_import#region
  in
  let* () =
    match type_arguments with
    | None -> Ok ()
    | Some type_args ->
      let region = Ast.region_of_chevrons type_args in
      mk_err Type_parameters_on_args region
  in
  let* (arguments : S.expr list) = strip_arguments_to_call arguments in
  let app = mk_reg node#region (lambda, arguments) in
  let ok = Ok (S.E_app app) in
  match lambda with
  | S.E_var var ->
    (match var#payload with
    | "$match" ->
      (match arguments with
      | [ subject_expr; object_expr ] ->
        let* match_clauses = filter_match_clauses object_expr in
        let matching = subject_expr, match_clauses in
        Ok (S.E_match (mk_reg node#region matching))
      | _ -> mk_err Pattern_matching node#region)
    | "contract_of" ->
      (match arguments with
      | [ expr ] ->
        let* path = filter_path expr in
        Ok (S.E_contract_of (mk_reg node#region path))
      | _ -> mk_err Invalid_contract_of node#region)
    | "michelson" | "Michelson" | "create_contract_of_file" | "bytes" ->
      (match arguments with
      | [ S.E_template string_literal ] ->
        let code_inj = mk_reg node#region (var, string_literal) in
        Ok (S.E_michelson code_inj)
      | _ -> ok)
    | _ -> ok)
  | _ -> ok

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
  mk_err Class_expression node#region

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
    | Some name -> mk_err Named_lambda name#region ~hint:"Declare a function."
  in
  let* call_sig = strip_call_signature call_sig in
  let { generics; parameters; rhs_type } = call_sig.value in
  let* fun_body = strip_statement_block body in
  let fun_body = S.Stmt_body fun_body in
  Ok S.{ generics; parameters; rhs_type; fun_body }

(* Generator function (expression) *)

and strip_E_generator_function (node : Ast.generator_function wrap) : (S.expr, _) result =
  mk_err Generator node#region

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
    | Optional_chain sym -> mk_err Optional_chaining sym#region
  in
  let* property = strip_property_ident property in
  Ok (S.E_member (mk_reg node#region (expr, property)))

and strip_object_member (node : Ast.object_member) : (S.expr, _) result =
  match node with
  | Object_member_expression expr -> strip_expression expr
  | Object_member_import kwd_import -> mk_err Import kwd_import#region

and strip_property_ident (node : Ast.property_ident) : (S.variable, _) result =
  match node with
  | Private_property_identifier hash -> mk_err Private_property hash#region
  | Property_identifier ident -> Ok (strip_identifier ident)

(* Meta-property *)

and strip_E_meta_property (node : Ast.meta_property) : (S.expr, _) result =
  mk_err Metaproperty (Ast.region_of_meta_property node)

(* Non-null expression *)

and strip_E_non_null_expression (node : Ast.expression) : (S.expr, _) result =
  mk_err Non_null (Ast.region_of_expression node)

(* Null (expression) *)

and strip_E_null (node : Ast.kwd_null) : (S.expr, _) result =
  mk_err Null_value node#region

(* Number (expression) *)

and strip_E_number (node : Ast.number) : (S.expr, _) result =
  match node with
  | Hex (hex, _) -> strip_hex hex
  | Bin (bin, _) -> mk_err Binary_octal bin#region
  | Oct (oct, _) -> mk_err Binary_octal oct#region
  | Dec (dec, _) -> strip_dec dec

and strip_hex (node : Ast.hex_literal) : (S.expr, _) result = Ok (S.E_bytes node)

and strip_dec (node : Ast.dec_literal) : (S.expr, _) result =
  let lexeme, q = node#payload in
  if Z.equal (Q.den q) Z.one
  then (
    let int = Wrap.make (lexeme, Q.to_bigint q) node#region in
    Ok (S.E_int int))
  else mk_err Non_integer node#region

(* Object (expression) *)

and strip_E_object (node : Ast.object_expr) : (S.expr, _) result =
  let Ast.(Braces braces) = node in
  let entries = braces#payload.contents in
  let* properties = Result.all @@ List.map ~f:strip_object_entry entries in
  let properties =
    let f entry acc =
      match entry with
      | None -> acc
      | Some entry -> entry :: acc
    in
    List.fold_right ~f ~init:[] properties
  in
  let spreads : Ast.spread_element wrap list =
    let app entry acc =
      match entry with
      | Ast.Object_entry_spread spread -> spread :: acc
      | _ -> acc
    in
    List.fold_right ~f:app ~init:[] entries
  in
  match spreads with
  | [] -> Ok (S.E_object (mk_reg braces#region properties))
  | [ spread ] ->
    let _sym_ellipsis, expr = spread#payload in
    let* obj_expr = strip_expression expr in
    let update_expr = S.{ obj_expr; updates = properties } in
    Ok (S.E_update (mk_reg braces#region update_expr))
  | _ :: snd_spread :: _ ->
    mk_err
      Multiple_spreads_in_object
      snd_spread#region
      ~hint:"Expand in place one of them."

and strip_object_entry (node : Ast.object_entry)
    : (S.expr S.property reg option, _) result
  =
  match node with
  | Object_entry_pair pair ->
    let* pair = strip_pair pair in
    Ok (Some pair)
  | Object_entry_spread _ -> Ok None (* See [strip_E_object] *)
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
    (* [method_body.region] is an approximation *)
    let property_rhs = Some (S.E_function (mk_reg method_body.region property_rhs)) in
    let property : S.expr S.property =
      { decorators; comments; property_name; static; property_rhs }
    in
    Ok (Some (mk_reg definition#region property))
  | Object_entry_shorthand ident ->
    let comments = ident#comments in
    let comments = strip_comments comments in
    let decorators = extract_decorators comments in
    let property_name = strip_identifier ident in
    let property_name = S.Property_ident property_name in
    let static = None in
    let property_rhs = None in
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
  let property_rhs = Some property_rhs in
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
    | _ -> mk_err Multiple_values (Ast.region_of_parens node)
  in
  Ok expr

(* Regex *)

and strip_E_regex (node : Ast.string_literal) : (S.expr, _) result =
  mk_err Regex node#region

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
    | Some Ast.(Optional_chain sym) -> mk_err Optional_chaining sym#region
  in
  let Ast.(Brackets brackets) = index in
  let exprs = brackets#payload.contents in
  let* exprs = strip_expressions exprs in
  let* expr =
    match exprs with
    | [ expr ] -> Ok expr
    | _ -> mk_err Multiple_values (Ast.region_of_brackets index)
  in
  match expr with
  | E_int nat ->
    let* obj = strip_expression object_expr in
    Ok (S.E_subscript (mk_reg node#region (obj, nat)))
  | _ -> mk_err Invalid_subscript node#region ~hint:"Use a natural number as an index."

(* Super (expression) *)

and strip_E_super (node : Ast.kwd_super) : (S.expr, _) result = mk_err Super node#region

(* Template string *)

and strip_E_template_string (node : Ast.template_string wrap) : (S.expr, _) result =
  strip_template_string node

and strip_template_string (node : Ast.template_string wrap) : (S.expr, _) result =
  match node#payload with
  | _, [ String_fragment literal ], _ -> Ok (S.E_template literal)
  | _ -> mk_err Template_string node#region

(* This (expression) *)

and strip_E_this (node : Ast.kwd_this) : (S.expr, _) result = mk_err This node#region

(* True (expression) *)

and strip_E_true (node : Ast.kwd_true) : (S.expr, _) result = Ok (S.E_true node#region)

(* Undefined (expression) *)

and strip_E_undefined (node : Ast.kwd_undefined) : (S.expr, _) result =
  mk_err Undefined_value node#region

(* Statisfies-expression *)

and strip_E_satisfies_expression (node : Ast.satisfies_expression wrap)
    : (S.expr, _) result
  =
  mk_err Type_check node#region

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
  mk_err Type_assertion node#region

(* Unary expression *)

and strip_E_unary_expression (node : Ast.unary_expression wrap) : (S.expr, _) result =
  let (Ast.{ operator; argument } : Ast.unary_expression) = node#payload in
  let* expr = strip_expression argument in
  let expr = mk_reg node#region expr in
  let* op = strip_unary_operator operator in
  Ok (op expr)

and strip_unary_operator (node : Ast.unary_operator) : (S.expr reg -> S.expr, _) result =
  match node with
  | Logical_neg _ -> Ok (fun arg -> S.E_not arg) (* !x *)
  | Bitwise_not _ -> Ok (fun arg -> S.E_bit_neg arg) (* ~x *)
  | Neg _ -> Ok (fun arg -> S.E_neg arg) (* -x *)
  | Plus_zero sym -> mk_err Plus_zero sym#region (* +x *)
  | Typeof kwd_typeof ->
    (* typeof x *)
    mk_err Typeof_void_delete kwd_typeof#region
  | Void kwd_void ->
    (* void *)
    mk_err Typeof_void_delete kwd_void#region
  | Delete kwd_delete ->
    (* delete *)
    mk_err Typeof_void_delete kwd_delete#region

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
    | _ -> mk_err Not_a_variable node#region ~hint:"Define a temporary variable."
  in
  match kind, operator with
  | `Pre, Increment _ -> Ok (S.E_pre_incr var)
  | `Pre, Decrement _ -> Ok (S.E_pre_decr var)
  | `Post, Increment _ -> Ok (S.E_post_incr var)
  | `Post, Decrement _ -> Ok (S.E_post_decr var)

(* Yield-expression *)

and strip_E_yield_expression (node : Ast.yield_expression) : (S.expr, _) result =
  let region = Ast.region_of_yield_expression node in
  mk_err Generator region

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
  mk_err Member_pattern node#region ~hint:"Use a variable."

(* Subscript expression (pattern) *)

and strip_P_subscript_expression (node : Ast.subscript_expression wrap)
    : (S.pattern, _) result
  =
  mk_err Subscript_pattern node#region

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
  mk_err Undefined_value node#region

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
  | Member_rest_pattern rest -> mk_err Rest_in_object_pattern rest#region
  | Member_object_assignment asgmt -> mk_err Asgmt_in_object_pattern asgmt#region
  | Member_shorthand_property ident ->
    let comments = ident#comments in
    let comments = strip_comments comments in
    let decorators = extract_decorators comments in
    let property_name = strip_identifier ident in
    let property_name = S.Property_ident property_name in
    let property_rhs = None in
    let static = None in
    let property : S.pattern S.property =
      { decorators; comments; property_name; static; property_rhs }
    in
    let region = Ast.region_of_member_pattern node in
    Ok (mk_reg region property)

and strip_property_name (node : Ast.property_name) : (S.property_name, _) result =
  match node with
  | Property_identifier ident -> Ok (S.Property_ident (strip_identifier ident))
  | Private_property_identifier hash -> mk_err Private_property hash#region
  | String literal -> Ok (S.Property_string literal)
  | Number n -> mk_err Property_as_number (Ast.region_of_number n)
  | Computed_property_name brackets ->
    mk_err Computed_property_name (Ast.region_of_brackets brackets)

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
  let property_rhs = Some property_rhs in
  let property : S.pattern S.property =
    { decorators; comments; property_name; static; property_rhs }
  in
  Ok (mk_reg node#region property)

and strip_pair_value_pattern (node : Ast.pair_value_pattern) : (S.pattern, _) result =
  match node with
  | Pair_value pattern -> strip_pattern pattern
  | Pair_value_assignment asgmt -> mk_err Asgmt_in_object_pattern asgmt#region

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
  | Cell_assignment pattern -> mk_err Asgmt_pattern_in_array pattern#region

(* Non-null expression (pattern) *)

and strip_P_non_null_expression (node : Ast.expression) : (S.pattern, _) result =
  let region = Ast.region_of_expression node in
  mk_err Non_null region

(* Rest pattern *)

and strip_P_rest_pattern (node : Ast.rest_pattern wrap) : (S.pattern, _) result =
  mk_err Top_rest_pattern node#region

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
    mk_err
      Complex_rest_pattern
      node#region
      ~hint:"Use variables or array/object patterns."

(* Alias for external access by means of [Strip.statements] *)

let statements (node : Ast.t) : (Ast_stripped.t, _) result =
  let* stmts = strip_statements node in
  match stmts with
  | None -> mk_err No_statements (Region.min ~file:"")
  | Some stmts -> Ok stmts
