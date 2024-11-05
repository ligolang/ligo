(* Decoding the tree-sitter CST for TypeScript *)

module Region = Simple_utils.Region
module Wrap = Lexing_shared.Wrap
open Core
open Ts_wrap

(* Monadic let for result values *)

let ( let* ) v f = Result.bind v ~f

(* Source map for converting vertical and horizontal offset ranges
   into regions *)

let get_region : (Ts_wrap.ts_tree -> Region.t) ref =
  ref (fun _ -> failwith "Internal error: Decoder.get_region")

(* Decoding literals *)

let make_node ?(comments = []) node : string Wrap.t =
  let region = !get_region node in
  let root = Lexeme.read region
  and comments = comments @ prev_comments node in
  let f node =
    let region = !get_region node in
    let value = Lexeme.read region in
    Wrap.Block Region.{ value; region }
  in
  let comments = List.map ~f comments in
  Wrap.make ~comments root region

let make_kwd ?comments node : Ast.keyword = make_node ?comments node
let dec_identifier ?comments node : Ast.identifier = make_node ?comments node

(* Optional nodes *)

let make_opt decoder node = Option.map ~f:decoder node

(* Decoding a list of nodes of the same type *)

let wrap_of_list ?(comments = []) decoder node =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match collect_named_children node with
  | [] -> None
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    let stmts = Nonempty_list.(fst_child :: List.fold_right ~f ~init:[] siblings) in
    let region = !get_region node in
    Some (Wrap.make stmts region)

(* Decoding the CST *)

open Ast

let ensure_Ok node = function
  | Result.Ok ok -> ok
  | Error msg -> failwith ((!get_region node)#compact `Byte ^ "\n" ^ msg)

let rec dec_program file map node =
  (* Opening a read channel for lexemes *)
  let () = Lexeme.open_input ~file in
  (* Setting up the extracting of source regions *)
  let () = get_region := Ts_wrap.get_region file map in
  (* Decoding the CST into an AST *)
  let ast = dec_statements node in
  (* Closing the input channel for reading lexemes *)
  let () = Lexeme.close_input () in
  ast

(* STATEMENTS

   The JavaScript tree-sitter grammar has the non-terminal
   "statement" be a supertype, that is, a hidden rule. *)

and dec_statements ?(comments = []) node : statements =
  wrap_of_list ~comments dec_statement node

and dec_statement ?(comments = []) node : statement =
  match get_name node with
  | "export_statement" -> dec_export_statement ~comments node
  | "import_statement" -> dec_import_statement ~comments node
  | "debugger_statement" -> dec_debugger_statement ~comments node
  | "expression_statement" -> dec_expression_statement ~comments node
  | "statement_block" -> dec_statement_block ~comments node
  | "if_statement" -> dec_if_statement ~comments node
  | "switch_statement" -> dec_switch_statement node
  | "for_statement" -> dec_for_statement node
  | "for_in_statement" -> dec_for_in_statement node
  | "while_statement" -> dec_while_statement node
  | "do_statement" -> dec_do_statement ~comments node
  | "try_statement" -> dec_try_statement node
  | "with_statement" -> dec_with_statement node
  | "break_statement" -> dec_break_statement node
  | "continue_statement" -> dec_continue_statement node
  | "return_statement" -> dec_return_statement node
  | "throw_statement" -> dec_throw_statement node
  | "empty_statement" -> S_empty_statement (!get_region node)
  (* Inlining declarations cases (hidden rule) *)
  | "function_declaration" -> dec_function_declaration ~comments node
  | "generator_function_declaration" -> dec_generator_function_declaration node
  | "class_declaration" -> dec_class_declaration ~comments node
  | "lexical_declaration" -> dec_lexical_declaration ~comments node
  | "variable_declaration" -> dec_variable_declaration ~comments node
  | "function_signature" -> dec_function_signature node
  | "abstract_class_declaration" -> dec_abstract_class_declaration node
  | "module" -> dec_module node
  | "internal_module" -> dec_internal_module ~comments node
  | "type_alias_declaration" -> dec_type_alias_declaration node
  | "enum_declaration" -> dec_enum_declaration node
  | "interface_declaration" -> dec_interface_declaration node
  | "import_alias" -> dec_import_alias node
  | "ambient_declaration" -> dec_ambient_declaration node
  | _ -> failwith "dec_statement"

(* Export statement *)

and dec_export_statement ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "TODO: dec_export_statement"

(* Import statement *)

and dec_import_statement ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "TODO: dec_import_statement"

(* Debugger statement *)

and dec_debugger_statement ?(comments = []) node =
  S_debugger_statement (make_kwd ~comments node)

(* Expression statements

   {@js[
    expression_statement: $ => seq($._expressions, $._semicolon),
    _expressions: $ => choice($.expression, $.sequence_expression),
    sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [doc_expression]. *)

and dec_expression_statement ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "TODO: dec_expression_statement"

(* Statement blocks *)

and dec_statement_block ?(comments = []) node =
  S_statement_block (dec_statements ~comments node)

(* If statement *)

and dec_if_statement ?(comments = []) node =
  ensure_Ok node
  @@ let* kwd_if = first_child_named "if" node in
     let* condition_field = child_with_field "condition" node in
     let* consequence_field = child_with_field "consequence" node in
     let alternative_field = child_with_field_opt "alternative" node in
     let stmt : if_statement =
       { kwd_if = make_kwd ~comments kwd_if
       ; condition = dec_expression condition_field
       ; consequence = dec_statement consequence_field
       ; alternative = make_opt dec_else_clause alternative_field
       }
     in
     Ok (S_if_statement stmt)

and dec_else_clause ?(comments = []) node : keyword * statement =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_else = first_child_named "else" node in
  let* statement = next_sibling kwd_else in
  Ok (make_kwd ~comments kwd_else, dec_statement statement)

(* Switch statement *)

and dec_switch_statement node =
  ignore node;
  failwith "TODO: dec_switch_statement"

(* For statement *)

and dec_for_statement node =
  ignore node;
  failwith "TODO: dec_for_statement"

(* For-in statement *)

and dec_for_in_statement node =
  ignore node;
  failwith "TODO: dec_for_in_statement"

(* While statement *)

and dec_while_statement node =
  ensure_Ok node
  @@ let* kwd_while = first_child_named "while" node in
     let* condition_field = child_with_field "condition" node in
     let* body_field = child_with_field "body" node in
     let stmt : while_statement =
       { kwd_while = make_kwd kwd_while
       ; condition = dec_expression condition_field
       ; body = dec_statement body_field
       }
     in
     Ok (S_while_statement stmt)

(* Do statement *)

and dec_do_statement ?(comments = []) node : statement =
  ensure_Ok node
  @@ let* kwd_do = first_child_named "do" node in
     let* body_field = child_with_field "body" node in
     let* kwd_while = first_child_named "while" node in
     let* condition_field = child_with_field "condition" node in
     let stmt : do_statement =
       { kwd_do = make_kwd ~comments kwd_do
       ; body = dec_statement body_field
       ; kwd_while = make_kwd kwd_while
       ; condition = dec_parenthesized_expression condition_field
       }
     in
     Ok (S_do_statement stmt)

(* Try statement *)

and dec_try_statement node =
  ignore node;
  failwith "dec_try_statement"

(* With statement *)

and dec_with_statement node =
  ignore node;
  failwith "dec_with_statement"

(* Break statement *)

and dec_break_statement node =
  ensure_Ok node
  @@ let* kwd_break = first_child_named "break" node in
     let label_field = child_with_field_opt "label" node in
     let stmt =
       { kwd_break = make_kwd kwd_break; stmt_id = make_opt dec_identifier label_field }
     in
     Ok (S_break_statement stmt)

(* Continue statement *)

and dec_continue_statement node =
  ensure_Ok node
  @@ let* kwd_continue = first_child_named "continue" node in
     let label_field = child_with_field_opt "label" node in
     let stmt =
       { kwd_continue = make_kwd kwd_continue
       ; stmt_id = make_opt dec_identifier label_field
       }
     in
     Ok (S_continue_statement stmt)

(* Return statement *)

and dec_return_statement node =
  ensure_Ok node @@
  let* kwd_return = first_child_named "return" node in
  let expr = child_ranked_opt 1 node
  and decode node =
    match get_name node with
    | "sequence_expression" -> Sequence_expression (dec_sequence_expression node)
    | _ -> General_expression (dec_expression node)
  in
  let stmt = {
    kwd_return = make_kwd kwd_return
  ; expressions = make_opt decode expr
  }
  in
  Ok (S_return_statement stmt)

(* Throw statement *)

and dec_throw_statement node =
  ignore node;
  failwith "dec_throw_statement"

(* DECLARATION

   The JavaScript tree-sitter grammar has the non-terminal
   "declaration" be a supertype, that is, a hidden rule. *)

(* Function declaration (see [dec_function_signature]) *)

and dec_function_declaration ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "dec_function_declaration"

(* Generator function declaration (see function declaration) *)

and dec_generator_function_declaration node =
  ignore node;
  failwith "dec_generator_function_declaration"

(* Class declaration (see [dec_class]) *)

and dec_class_declaration ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "dec_class_declaration"

(* Lexical declaration (see [dec_variable_declaration]) *)

and dec_lexical_declaration ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "dec_lexical_declaration"

(* Variable declaration (see [dec_lexical_declaration]) *)

and dec_variable_declaration ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "dec_variable_declaration"

(* Function signature (See [dec_function_declaration]) *)

and dec_function_signature node =
  ignore node;
  failwith "dec_function_signature"

(* Abstract class declaration ( see [dec_class_declaration]) *)

and dec_abstract_class_declaration node =
  ignore node;
  failwith "dec_abstract_class_declaration"

(* Module *)

and dec_module ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "dec_module"

(* Internal module (a.k.a. namespaces) *)

and dec_internal_module ?comments node =
  ignore comments;
  ignore node;
  failwith "dec_internal_module"

(* Type alias declaration *)

and dec_type_alias_declaration ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "dec_type_alias_declaration"

(* Enum declaration *)

and dec_enum_declaration node =
  ignore node;
  failwith "dec_enum_declaration"

(* Interface declaration *)

and dec_interface_declaration node =
  ignore node;
  failwith "dec_interface_declaration"

(* Import alias *)

and dec_import_alias node =
  ignore node;
  failwith "dec_import_alias"

(* Ambient declaration *)

and dec_ambient_declaration node =
  ignore node;
  failwith "dec_ambient_declaration"

(* EXPRESSIONS *)

and dec_expression ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "dec_expression"

and dec_parenthesized_expression ?(comments = []) node : expression =
  ignore comments;
  ignore node;
  failwith "dec_parenthesized_expression"

(* Sequence expression *)

and dec_sequence_expression ?(comments = []) node =
  ignore comments;
  ignore node;
  failwith "dec_sequence_expression"
