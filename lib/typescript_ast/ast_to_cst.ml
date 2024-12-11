(* Mapping the TypeScript AST to the JsLIGO CST *)

(* Dependencies *)

module Cst = Cst_jsligo.CST
open Core
module Wrap = Lexing_shared.Wrap
module Region = Simple_utils.Region

type 'a wrap = 'a Wrap.t

(* Errors (temporary) *)

let error_reg (region : Region.t) (msg : string) =
  Error (Printf.sprintf "%s:\n%s" (region#to_string `Byte) msg)

let error (wrap : 'a wrap) (msg : string) = error_reg wrap#region msg

(* Monadic let-binder for result values *)

let ( let* ) v f = Result.bind v ~f

(* List of options to reversed list without options *)

let rev_erase_options =
  let f acc = function
    | None -> acc
    | Some elt -> elt :: acc
  in
  List.fold_left ~f ~init:[]

(* Mapping the AST *)

let rec map_statements : Ast.statements -> (Cst.statements, _) result = function
  | None -> Error "A JsLIGO contract expects at least one statement."
  | Some stmts ->
    let stmts = Nonempty_list.to_list stmts#payload in
    let f acc stmt =
      let* stmt' = map_statement stmt in
      Ok (stmt' :: acc)
    in
    let* stmts = List.fold_result ~f ~init:[] stmts in
    (match rev_erase_options stmts with
    | [] -> Error "A JsLIGO contract expects at least one statement."
    | fst_stmt :: other_stmts ->
      let stmts = Nonempty_list.(fst_stmt :: other_stmts) in
      Ok (Nonempty_list.map ~f:(fun stmt -> stmt, None) stmts))

and map_statement : Ast.statement -> (Cst.statement option, _) result = function
  | S_export_statement s -> map_export_statement s
  | S_import_statement s -> map_import_statement s
  | S_debugger_statement s -> map_debugger_statement s
  | S_expression_statement s -> map_expression_statement s
  | S_declaration_statement s -> map_declaration_statement s
  | S_statement_block s -> map_statement_block s
  | S_if_statement s -> map_if_statement s
  | S_switch_statement s -> map_switch_statement s
  | S_for_statement s -> map_for_statement s
  | S_for_in_statement s -> map_for_in_statement s
  | S_while_statement s -> map_while_statement s
  | S_do_statement s -> map_do_statement s
  | S_try_statement s -> map_try_statement s
  | S_with_statement s -> map_with_statement s
  | S_break_statement s -> map_break_statement s
  | S_continue_statement s -> map_continue_statement s
  | S_return_statement s -> map_return_statement s
  | S_throw_statement s -> map_throw_statement s
  | S_empty_statement s -> map_empty_statement s
  | S_labeled_statement s -> map_labeled_statement s

(* Export statement *)

and map_export_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_export_statement"

(* Import statement *)

and map_import_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_import_statement"

(* Debugger statement *)

and map_debugger_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_debugger_statement"

(* Expression statement *)

and map_expression_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_expression_statement"

(* Statement block *)

and map_statement_block node : (Cst.statement option, _) result =
  let (Braces node) = node in
  ignore node;
  error node "TODO: map_statement_block"

(* If statement *)

and map_if_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_if_statement"

(* Switch statement *)

and map_switch_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_switch_statement"

(* For statement *)

and map_for_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_for_statement"

(* For-in statement *)

and map_for_in_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_for_in_statement"

(* While statement *)

and map_while_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_while_statement"

(* Do statement *)

and map_do_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_do_statement"

(* Try statement *)

and map_try_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_try_statement"

(* With statement *)

and map_with_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_with_statement"

(* Break statement *)

and map_break_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_break_statement"

(* Continue statement *)

and map_continue_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_continue_statement"

(* Return statement *)

and map_return_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_return_statement"

(* Throw statement *)

and map_throw_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_throw_statement"

(* Empty statement *)

and map_empty_statement node : (Cst.statement option, _) result =
  ignore node;
  Ok None

(* Labeled statement *)

and map_labeled_statement node : (Cst.statement option, _) result =
  ignore node;
  error node "TODO: map_labeled_statement "

(* DECLARATION *)

and map_declaration_statement node : (Cst.statement option, _) result =
  let region = Ast.region_of_declaration node in
  ignore node;
  error_reg region "TODO: map_declaration_statement"
