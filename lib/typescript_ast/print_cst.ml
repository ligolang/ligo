(* Printing the tree-sitter CST for TypeScript *)

open Core
open Typescript_ast.Ts_wrap
module Lexeme = Typescript_ast.Lexeme
module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Ast = Typescript_ast.Ast (* Only for numbers *)
module Number = Typescript_ast.Number
module Wrap = Lexing_shared.Wrap

(* Source map for converting vertical and horizontal offset ranges
   into regions *)

let get_region : (ts_tree -> Region.t) ref =
  ref (fun _ -> failwith "Internal error: Print_cst.get_region")

(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Making trees and nodes with labels (name + location) *)

let mk_child_opt = Tree.mk_child_opt
let mk_child = Tree.mk_child
let mk_children_list = Tree.mk_children_list

let tree_of_list ?(comments = []) state node printer raw_children =
  let region = !get_region node
  and label = get_name node
  and f raw_child nodes = mk_child (printer ?comments:None) raw_child :: nodes in
  let children =
    match raw_children with
    | [] -> []
    | fst_raw_child :: siblings ->
      let printer = printer ?comments:(Some comments) in
      let fst_child = mk_child printer fst_raw_child in
      fst_child :: List.fold_right ~f ~init:[] siblings
  in
  Tree.make_tree ~region state label children

let tree_of_named_children ?(comments = []) state node printer =
  let raw_children = collect_named_children node in
  tree_of_list ~comments state node printer raw_children

let make_unary state root printer child =
  let region = !get_region root
  and label = get_name root in
  Tree.make_unary ~region state label printer child

let make_node state node =
  let region = !get_region node in
  let lexeme = Lexeme.read region in
  make_unary state node Tree.make_node lexeme

let print_comment state node = make_node state node

let print_error_node state node =
  if arity node = 0
  then make_node state node
  else make_unary state node Tree.make_node "UNMATCHED"

let mk_error_children node =
  mk_children_list print_error_node @@ collect_error_children node

let make_tree state node children =
  let region = !get_region node
  and label = get_name node in
  Tree.make ~region state label (mk_error_children node @ children)

(* We shadow [make_node] above *)

let make_node ?(comments = []) state node =
  let region = !get_region node in
  let lexeme = Lexeme.read region in
  let comments = comments @ prev_comments node in
  let children =
    mk_children_list print_comment comments @ [ mk_child Tree.make_node lexeme ]
  in
  make_tree state node children

let make_kwd ?(comments = []) state node =
  let region = !get_region node in
  let root = Lexeme.read region ^ " [keyword]" in
  let comments = comments @ prev_comments node in
  Tree.of_list ~region state root print_comment comments

let make_sym ?(comments = []) state node =
  let region = !get_region node in
  let root = Lexeme.read region in
  let comments = comments @ prev_comments node in
  Tree.of_list ~region state root print_comment comments

let mk_child_res print = function
  | Result.Ok child -> mk_child print child
  | Error name -> mk_child Tree.make_node name

let internal_error_child child_name parent_node =
  let child_name = if String.(child_name = "") then child_name else " " ^ child_name in
  let parent_name = get_name parent_node
  and suffix = Printf.sprintf "Child%s is missing." child_name in
  let msg = Printf.sprintf "INTERNAL: [%s] %s" parent_name suffix in
  [ mk_child Tree.make_node msg ]

let make_unary_res state node print = function
  | Result.Ok child -> make_unary state node print child
  | Error child_name -> make_unary state node Tree.make_node child_name

let print_missing_node state node = make_node state node

let print_unexpected_node state node =
  let region = !get_region node
  and label = get_name node in
  Tree.make_node ~region state ("UNKNOWN: " ^ label)

(* Some literals *)

let print_identifier ?comments state node = make_node ?comments state node
let print_string ?comments state node = make_node ?comments state node
let print_regex ?comments state node = make_node ?comments state node

let decode_comments ?(comments = []) node : Wrap.comment list =
  let f node =
    let region = !get_region node in
    let value = Lexeme.read region in
    Wrap.Block Region.{ value; region }
  in
  List.map ~f (comments @ prev_comments node)

let print_number ?(comments = []) state node =
  let region = !get_region node in
  let lexeme = Lexeme.read region in
  let lexbuf = Lexing.from_string lexeme in
  let w_comments = decode_comments ~comments node in
  let num = Number.scan w_comments region lexbuf in
  let print_hex w = Hex.show (snd w#payload)
  and print_dec w = Q.to_string (snd w#payload) in
  let open Ast in
  let print_kind state = function
    | Hex (w, false) -> Tree.make_node state ("hex (" ^ print_hex w ^ ")")
    | Hex (w, true) -> Tree.make_node state ("bigint/hex (" ^ print_hex w ^ ")")
    | Bin (w, false) -> Tree.make_node state ("bin (" ^ print_hex w ^ ")")
    | Bin (w, true) -> Tree.make_node state ("bigint/bin (" ^ print_hex w ^ ")")
    | Oct (w, false) -> Tree.make_node state ("oct (" ^ print_hex w ^ ")")
    | Oct (w, true) -> Tree.make_node state ("bigint/oct (" ^ print_hex w ^ ")")
    | Dec (w, false) -> Tree.make_node state ("dec (" ^ print_dec w ^ ")")
    | Dec (w, true) -> Tree.make_node state ("bigint/dec (" ^ print_dec w ^ ")")
  in
  let comments = comments @ prev_comments node in
  let children =
    mk_children_list print_comment comments
    @ [ mk_child print_kind num; mk_child Tree.make_node lexeme ]
  in
  make_tree state node children

(* Printing enclosed constructs *)

let print_enclosed ?(comments = []) state node printer opening closing =
  let comments = comments @ prev_comments node in
  let opening = first_child_named opening node
  and closing = first_child_named closing node
  and clauses = collect_named_children node in
  let children =
    (mk_child_res (make_sym ~comments) opening :: mk_children_list printer clauses)
    @ [ mk_child_res make_sym closing ]
  in
  make_tree state node children

let print_braces ?(comments = []) state node printer =
  print_enclosed ~comments state node printer "{" "}"

let print_chevrons ?(comments = []) state node printer =
  print_enclosed ~comments state node printer "<" ">"

let print_brackets ?(comments = []) state node printer =
  print_enclosed ~comments state node printer "[" "]"

let print_parens ?(comments = []) state node printer =
  print_enclosed ~comments state node printer "(" ")"

(* Concluding a pattern matching with a default printer. Dropping comments. *)

let match_rest state node print_default =
  match get_name node with
  (* Comments are ignored *)
  | "comment" -> ()
  (* Errors *)
  | "ERROR" -> print_error_node state node
  | "MISSING" -> print_missing_node state node
  (* Default case *)
  | _ -> print_default state node

(* Printing the CST *)

let rec print_program file (map : Loc_map.t) node =
  (* Opening a read channel for lexemes *)
  let () = Lexeme.open_input ~file in
  (* Setting up the extracting of source regions *)
  let () = get_region := Ts_wrap.get_region file map in
  (* Empty state for building the AST *)
  let buffer = Buffer.create 1023 in
  let state = Tree.mk_state ~buffer ~regions:true ~layout:true ~offsets:true `Byte in
  (* Printing the CST into a string buffer in [state] *)
  let () = print_statements state node in
  (* Closing the input channel for reading lexemes *)
  let () = Lexeme.close_input () in
  (* Making the output string *)
  Buffer.contents @@ Tree.to_buffer state

(* STATEMENTS

   The JavaScript tree-sitter grammar has the non-terminal
   "statement" be a supertype, that is, a hidden rule. *)

and print_statements state node = tree_of_named_children state node print_statement

and print_statement ?(comments = []) state node =
  match get_name node with
  | "export_statement" -> print_export_statement ~comments state node
  | "import_statement" -> print_import_statement ~comments state node
  | "debugger_statement" -> print_debugger_statement ~comments state node
  | "expression_statement" -> print_expression_statement ~comments state node
  | "statement_block" -> print_statement_block ~comments state node
  | "if_statement" -> print_if_statement ~comments state node
  | "switch_statement" -> print_switch_statement state node
  | "for_statement" -> print_for_statement state node
  | "for_in_statement" -> print_for_in_statement state node
  | "while_statement" -> print_while_statement state node
  | "do_statement" -> print_do_statement state node
  | "try_statement" -> print_try_statement state node
  | "with_statement" -> print_with_statement state node
  | "break_statement" -> print_break_statement state node
  | "continue_statement" -> print_continue_statement state node
  | "return_statement" -> print_return_statement state node
  | "throw_statement" -> print_throw_statement state node
  | "empty_statement" -> print_empty_statement state node
  | "labeled_statement" -> print_labeled_statement state node
  (* Inlining declarations cases (hidden rule) *)
  | "function_declaration" -> print_function_declaration ~comments state node
  | "generator_function_declaration" -> print_generator_function_declaration state node
  | "class_declaration" -> print_class_declaration ~comments state node
  | "lexical_declaration" -> print_lexical_declaration ~comments state node
  | "variable_declaration" -> print_variable_declaration ~comments state node
  | "function_signature" -> print_function_signature state node
  | "abstract_class_declaration" -> print_abstract_class_declaration state node
  | "module" -> print_module state node
  | "internal_module" -> print_internal_module ~comments state node
  | "type_alias_declaration" -> print_type_alias_declaration state node
  | "enum_declaration" -> print_enum_declaration state node
  | "interface_declaration" -> print_interface_declaration state node
  | "import_alias" -> print_import_alias state node
  | "ambient_declaration" -> print_ambient_declaration state node
  | _ -> match_rest state node print_unexpected_node

(* Export statement *)

and print_export_statement ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and decorators = children_named "decorator" node
  and kwd_export = first_child_named_opt "export" node in
  let decorators = mk_children_list print_decorator decorators in
  let children =
    match kwd_export with
    | None -> internal_error_child "export" node
    | Some kwd_export ->
      (* Previous comments are hooked to the keyword "export" *)
      mk_child (make_kwd ~comments) kwd_export
      ::
      (match next_sibling kwd_export with
      | Error _ -> internal_error_child "after \"export\"" node
      | Ok after_export ->
        (match get_name after_export with
        | "*" ->
          let kwd_from = first_child_named "from" node in
          [ mk_child make_sym after_export; mk_child_from_clause kwd_from node ]
        | "namespace_export" ->
          let kwd_from = first_child_named "from" node in
          [ mk_child print_namespace_export after_export
          ; mk_child_from_clause kwd_from node
          ]
        | "export_clause" ->
          mk_child print_export_clause after_export :: mk_child_from_clause_opt node
        | "default" ->
          let declaration_field = child_with_field_opt "declaration" node in
          decorators
          @ [ mk_child make_kwd after_export ] (* keyword "default" *)
          @
          (match declaration_field with
          | Some declaration_field -> [ mk_child print_declaration declaration_field ]
          | None ->
            let value_field = child_with_field "value" node in
            [ mk_child_res print_expression value_field ])
        | "type" ->
          (match next_sibling after_export with
          | Error _ -> internal_error_child "export_clause" node
          | Ok export_clause ->
            mk_child make_kwd after_export
            :: mk_child print_export_clause export_clause
            :: mk_child_from_clause_opt node)
        | "=" ->
          (match next_sibling after_export with
          | Error _ -> internal_error_child "expression" node
          | Ok expression ->
            [ mk_child make_sym after_export; mk_child print_expression expression ])
        | "as" ->
          let kwd_namespace = first_child_named "namespace" node
          and identifier = first_child_named "identifier" node in
          [ mk_child make_kwd after_export (* keyword "as" *)
          ; mk_child_res make_kwd kwd_namespace
          ; mk_child_res print_identifier identifier
          ]
        | _ -> decorators @ [ mk_child print_declaration after_export ]))
  in
  make_tree state node children

and print_namespace_export ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and sym_star = first_child_named "*" node
  and kwd_as = first_child_named "as" node in
  let module_export_name = next_sibling_res kwd_as in
  let children =
    [ mk_child_res (make_sym ~comments) sym_star
    ; mk_child_res make_kwd kwd_as
    ; mk_child_res print_module_export_name module_export_name
    ]
  in
  make_tree state node children

and mk_child_from_clause kwd_from node =
  let source_field = child_with_field "source" node in
  let children =
    [ mk_child_res make_kwd kwd_from; mk_child_res print_string source_field ]
  in
  Some (fun state -> Tree.make_tree state "from_clause" children)

and mk_child_from_clause_opt node =
  match first_child_named_opt "from" node with
  | None -> []
  | Some kwd_from -> [ mk_child_from_clause (Ok kwd_from) node ]

and print_export_clause state node = print_braces state node print_export_specifier

and print_module_export_name ?(comments = []) state node =
  match get_name node with
  | "identifier" -> print_identifier ~comments state node
  | "string" -> print_string ~comments state node
  | _ -> match_rest state node print_unexpected_node

and print_export_specifier ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  let name_field = child_with_field "name" node in
  let children =
    mk_child_res (print_module_export_name ~comments) name_field
    ::
    (match child_with_field_opt "alias" node with
    | None -> []
    | Some alias_field ->
      let kwd_as = first_child_named "as" node in
      [ mk_child_res make_kwd kwd_as; mk_child print_module_export_name alias_field ])
  in
  make_tree state node children

(* Import statement *)

and print_import_statement ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and kwd_import = first_child_named "import" node
  and kind_node =
    match first_child_named_opt "type" node with
    | None -> first_child_named_opt "typeof" node
    | some -> some
  and import_attribute = first_child_named_opt "import_attribute" node in
  let middle_children =
    match first_child_named_opt "import_clause" node with
    | Some import_clause ->
      let kwd_from = first_child_named "from" node in
      [ mk_child print_import_clause import_clause; mk_child_from_clause kwd_from node ]
    | None ->
      (match first_child_named_opt "import_require_clause" node with
      | Some clause -> [ mk_child print_import_require_clause clause ]
      | None ->
        let source_field = child_with_field "source" node in
        [ mk_child_res print_string source_field ])
  in
  let children =
    (* Previous comments are hooked to the keyword "import" *)
    (mk_child_res (make_kwd ~comments) kwd_import
    :: mk_child_opt make_kwd kind_node
    :: middle_children)
    @ [ mk_child_opt print_import_attribute import_attribute ]
  in
  make_tree state node children

and print_import_clause ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  let print_rest state node =
    match get_name node with
    | "namespace_import" -> print_namespace_import state node
    | "named_imports" -> print_named_imports state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    match child_ranked_opt 0 node with
    | None -> internal_error_child "\"first child\"" node
    | Some fst_child ->
      (match get_name fst_child with
      | "namespace_import" -> [ mk_child (print_namespace_import ~comments) fst_child ]
      | "named_imports" -> [ mk_child (print_named_imports ~comments) fst_child ]
      | "identifier" ->
        mk_child (print_identifier ~comments) fst_child
        ::
        (match next_sibling_opt fst_child with
        | None -> []
        | Some comma ->
          (match next_sibling comma with
          | Error _ -> internal_error_child "namespace_import/named_imports" node
          | Ok next -> [ mk_child print_rest next ]))
      | _ -> [ mk_child print_unexpected_node fst_child ])
  in
  make_tree state node children

and print_namespace_import ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and sym_star = first_child_named "*" node
  and kwd_as = first_child_named "as" node in
  let identifier = next_sibling_res kwd_as in
  let children =
    [ mk_child_res (make_sym ~comments) sym_star
    ; mk_child_res make_kwd kwd_as
    ; mk_child_res print_identifier identifier
    ]
  in
  make_tree state node children

and print_named_imports ?(comments = []) state node =
  print_braces ~comments state node print_import_specifier

and print_import_specifier ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  let kind_node =
    match first_child_named_opt "type" node with
    | None -> first_child_named_opt "typeof" node
    | some -> some
  in
  let fst_child_comments, snd_child_comments =
    match kind_node with
    | None -> [], comments
    | Some _ -> comments, []
  and name_field = child_with_field "name" node
  and alias_field = child_with_field_opt "alias" node in
  let children =
    mk_child_opt (make_kwd ~comments:fst_child_comments) kind_node
    ::
    (match alias_field with
    | None -> [ mk_child_res (print_identifier ~comments:snd_child_comments) name_field ]
    | Some alias_field ->
      let kwd_as = first_child_named "as" node in
      [ mk_child_res (print_module_export_name ~comments:snd_child_comments) name_field
      ; mk_child_res make_kwd kwd_as
      ; mk_child print_identifier alias_field
      ])
  in
  make_tree state node children

and print_import_require_clause ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and identifier = child_ranked 0 node
  and sym_equal = first_child_named "=" node
  and id_require = first_child_named "require" node
  and sym_lparen = first_child_named "(" node
  and source_field = child_with_field "source" node
  and sym_rparen = first_child_named ")" node in
  let children =
    [ mk_child_res (print_identifier ~comments) identifier
    ; mk_child_res make_sym sym_equal
    ; mk_child_res print_identifier id_require
    ; mk_child_res make_sym sym_lparen
    ; mk_child_res print_string source_field
    ; mk_child_res make_sym sym_rparen
    ]
  in
  make_tree state node children

and print_import_attribute state node =
  let kind_node = child_ranked 0 node
  and object_node = child_ranked 1 node
  and print_kind state node =
    match get_name node with
    | "with" -> make_kwd state node
    | "assert" -> make_kwd state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_kind kind_node; mk_child_res print_object object_node ]
  in
  make_tree state node children

(* Debugger statement *)

and print_debugger_statement ?(comments = []) state node =
  let kwd_debugger = first_child_named "debugger" node in
  let children = [ mk_child_res (make_kwd ~comments) kwd_debugger ] in
  make_tree state node children

(* Expression statements

   {@js[
    expression_statement: $ => seq($._expressions, $._semicolon),
    _expressions: $ => choice($.expression, $.sequence_expression),
    sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [print_expression]. *)

and print_expression_statement ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and child = named_child_ranked 0 node in
  make_unary_res state node (print_expressions ~comments) child

and print_expressions ?(comments = []) state (node : ts_tree) =
  match get_name node with
  | "sequence_expression" -> print_sequence_expression ~comments state node
  | _ -> print_expression ~comments state node

(* Statement blocks *)

and print_statement_block ?(comments = []) state node =
  print_braces ~comments state node print_statement

(* If statement *)

and print_if_statement ?(comments = []) state node =
  let kwd_if = first_child_named "if" node
  and condition_field = child_with_field "condition" node
  and consequence_field = child_with_field "consequence" node
  and alternative_field = child_with_field_opt "alternative" node in
  let children =
    [ mk_child_res (make_kwd ~comments) kwd_if
    ; mk_child_res print_parenthesized_expression condition_field
    ; mk_child_res print_statement consequence_field
    ; mk_child_opt print_else_clause alternative_field
    ]
  in
  make_tree state node children

and print_else_clause ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  let kwd_else = first_child_named "else" node in
  let statement = next_sibling_res kwd_else in
  let children =
    [ mk_child_res (make_kwd ~comments) kwd_else; mk_child_res print_statement statement ]
  in
  make_tree state node children

(* Switch statement *)

and print_switch_statement state node =
  let kwd_switch = first_child_named "switch" node
  and value_field = child_with_field "value" node
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_res make_kwd kwd_switch
    ; mk_child_res print_parenthesized_expression value_field
    ; mk_child_res print_switch_body body_field
    ]
  in
  make_tree state node children

and print_switch_body state node =
  let print state node =
    match get_name node with
    | "switch_case" -> print_switch_case state node
    | _ -> match_rest state node print_switch_default
  in
  print_braces state node print

and print_switch_case state node =
  let kwd_case = first_child_named "case" node
  and children = collect_children node in
  let rec skip_until_colon = function
    | [] -> []
    | node :: nodes ->
      (match get_name node with
      | ":" -> nodes
      | _ -> skip_until_colon nodes)
  in
  let stmt_children = skip_until_colon children
  and value_field = child_with_field "value" node in
  let children =
    mk_child_res make_kwd kwd_case
    :: mk_child_res print_expressions value_field
    :: mk_children_list print_statement stmt_children
  in
  make_tree state node children

and print_switch_default state node =
  let kwd_default = first_child_named "default" node
  and statements = collect_named_children node in
  let children =
    mk_child_res make_kwd kwd_default :: mk_children_list print_statement statements
  in
  make_tree state node children

(* For statement *)

and print_for_statement state node =
  let kwd_for = first_child_named "for" node
  and sym_lparen = first_child_named "(" node
  and initializer_field = child_with_field "initializer" node
  and condition_field = child_with_field "condition" node
  and increment_field = child_with_field_opt "increment" node
  and sym_rparen = first_child_named ")" node
  and body_field = child_with_field "body" node
  and print_initializer state node =
    match get_name node with
    | "lexical_declaration" -> print_lexical_declaration state node
    | "variable_declaration" -> print_variable_declaration state node
    | "expression_statement" -> print_expression_statement state node
    | "empty_statement" -> print_empty_statement state node
    | _ -> match_rest state node print_unexpected_node
  and print_condition state node =
    match get_name node with
    | "expression_statement" -> print_expression_statement state node
    | "empty_statement" -> print_empty_statement state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res make_kwd kwd_for
    ; mk_child_res make_sym sym_lparen
    ; mk_child_res print_initializer initializer_field
    ; mk_child_res print_condition condition_field
    ; mk_child_opt print_expressions increment_field
    ; mk_child_res make_sym sym_rparen
    ; mk_child_res print_statement body_field
    ]
  in
  make_tree state node children

(* For-in statement *)

and print_for_in_statement state node =
  let kwd_await = first_child_named_opt "await" node
  and kwd_for = first_child_named "for" node
  and sym_lparen = first_child_named "(" node
  and left_field = child_with_field "left" node
  and sym_rparen = first_child_named ")" node
  and body_field = child_with_field "body" node
  and operator_field = child_with_field "operator" node
  and right_field = child_with_field "right" node
  and kind_field = child_with_field_opt "kind" node in
  let print_operator state node =
    match get_name node with
    | "in" -> make_kwd state node
    | "of" -> make_kwd state node
    | _ -> match_rest state node print_unexpected_node
  in
  let header_children =
    match kind_field with
    | None ->
      let print_left state node =
        match get_name node with
        | "parenthesized_expression" -> print_parenthesized_expression state node
        | _ -> match_rest state node print_lhs_expression
      in
      [ mk_child_res print_left left_field ]
    | Some kind_field ->
      let print_left state node =
        match get_name node with
        | "identifier" -> print_identifier state node
        | _ -> match_rest state node print_destructuring_pattern
      in
      (match get_name kind_field with
      | "var" ->
        let value_field = child_with_field_opt "value" node in
        [ mk_child make_kwd kind_field
        ; mk_child_res print_left left_field
        ; mk_child_opt print_expression value_field
        ]
      | "let" | "const" ->
        [ mk_child make_kwd kind_field; mk_child_res print_left left_field ]
      | _ -> [ mk_child print_unexpected_node kind_field ])
  in
  let children =
    (mk_child_res make_kwd kwd_for
    :: mk_child_opt make_kwd kwd_await
    :: mk_child_res make_sym sym_lparen
    :: header_children)
    @ [ mk_child_res print_operator operator_field
      ; mk_child_res print_expressions right_field
      ; mk_child_res make_sym sym_rparen
      ; mk_child_res print_statement body_field
      ]
  in
  make_tree state node children

(* While statement *)

and print_while_statement state node =
  let kwd_while = first_child_named "while" node
  and condition_field = child_with_field "condition" node
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_res make_kwd kwd_while
    ; mk_child_res print_parenthesized_expression condition_field
    ; mk_child_res print_statement body_field
    ]
  in
  make_tree state node children

(* Do statement *)

and print_do_statement state node =
  let kwd_do = first_child_named "do" node
  and body_field = child_with_field "body" node
  and kwd_while = first_child_named "while" node
  and condition_field = child_with_field "condition" node in
  let children =
    [ mk_child_res make_kwd kwd_do
    ; mk_child_res print_statement body_field
    ; mk_child_res make_kwd kwd_while
    ; mk_child_res print_parenthesized_expression condition_field
    ]
  in
  make_tree state node children

(* Try statement *)

and print_try_statement state node =
  let kwd_try = first_child_named "try" node
  and body_field = child_with_field "body" node
  and handler_field = child_with_field_opt "handler" node
  and finalizer_field = child_with_field_opt "finalizer" node in
  let children =
    [ mk_child_res make_kwd kwd_try
    ; mk_child_res print_statement_block body_field
    ; mk_child_opt print_catch_clause handler_field
    ; mk_child_opt print_finally_clause finalizer_field
    ]
  in
  make_tree state node children

and print_catch_clause state node =
  let kwd_catch = first_child_named "catch" node
  and body_field = child_with_field "body" node
  and parameter_field = child_with_field_opt "parameter" node
  and print_parameter state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | _ -> match_rest state node print_destructuring_pattern
  in
  let children =
    match parameter_field with
    | Some parameter_field ->
      let sym_lparen = first_child_named "(" node
      and type_field = child_with_field_opt "type" node
      and sym_rparen = first_child_named ")" node in
      [ mk_child_res make_sym sym_lparen
      ; mk_child print_parameter parameter_field
      ; mk_child_opt print_type_annotation type_field
      ; mk_child_res make_sym sym_rparen
      ]
    | None -> []
  in
  let children = mk_child_res make_kwd kwd_catch :: children in
  let children = children @ [ mk_child_res print_statement_block body_field ] in
  make_tree state node children

and print_finally_clause state node =
  let kwd_finally = first_child_named "finally" node
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_res make_kwd kwd_finally; mk_child_res print_statement_block body_field ]
  in
  make_tree state node children

(* With statement *)

and print_with_statement state node =
  let kwd_with = first_child_named "with" node
  and object_field = child_with_field "object" node
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_res make_kwd kwd_with
    ; mk_child_res print_parenthesized_expression object_field
    ; mk_child_res print_statement body_field
    ]
  in
  make_tree state node children

(* Break statement *)

and print_break_statement state node =
  let kwd_break = first_child_named "break" node
  and label_field = child_with_field_opt "label" node in
  let children =
    [ mk_child_res make_kwd kwd_break; mk_child_opt print_identifier label_field ]
  in
  make_tree state node children

(* Continue statement *)

and print_continue_statement state node =
  let kwd_continue = first_child_named "continue" node
  and label_field = child_with_field_opt "label" node in
  let children =
    [ mk_child_res make_kwd kwd_continue; mk_child_opt print_identifier label_field ]
  in
  make_tree state node children

(* Return statement *)

and print_return_statement state node =
  let kwd_return = first_child_named "return" node
  and expr = child_ranked_opt 1 node in
  let children =
    [ mk_child_res make_kwd kwd_return; mk_child_opt print_expressions expr ]
  in
  make_tree state node children

(* Throw statement *)

and print_throw_statement state node =
  let kwd_throw = first_child_named "throw" node
  and expr = child_ranked 1 node in
  let children =
    [ mk_child_res make_kwd kwd_throw; mk_child_res print_expressions expr ]
  in
  make_tree state node children

(* Empty statement *)

and print_empty_statement state node =
  let region = !get_region node
  and label = get_name node in
  Tree.make ~region state label []

(* Labeled statement *)

and print_labeled_statement state node =
  let label_field = child_with_field "label" node
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_res print_identifier label_field; mk_child_res print_statement body_field ]
  in
  make_tree state node children

(* DECLARATION

   The JavaScript tree-sitter grammar has the non-terminal
   "declaration" be a supertype, that is, a hidden rule. *)

and print_declaration ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  match get_name node with
  | "function_declaration" -> print_function_declaration state node
  | "generator_function_declaration" -> print_generator_function_declaration state node
  | "class_declaration" -> print_class_declaration ~comments state node
  | "lexical_declaration" -> print_lexical_declaration ~comments state node
  | "variable_declaration" -> print_variable_declaration state node
  | "function_signature" -> print_function_signature state node
  | "abstract_class_declaration" -> print_abstract_class_declaration state node
  | "module" -> print_module state node
  | "internal_module" -> print_internal_module ~comments state node
  | "type_alias_declaration" -> print_type_alias_declaration ~comments state node
  | "enum_declaration" -> print_enum_declaration state node
  | "interface_declaration" -> print_interface_declaration state node
  | "import_alias" -> print_import_alias state node
  | "ambient_declaration" -> print_ambient_declaration state node
  | _ -> match_rest state node print_unexpected_node

(* Function declaration (see [print_function_signature]) *)

and print_function_declaration ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and kwd_async = first_child_named_opt "async" node
  and kwd_function = first_child_named "function" node
  and name_field = child_with_field "name" node
  (* "_call_signature" inlined: *)
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node
  (* "statement_block" *)
  and body_field = child_with_field "body" node in
  let async_comments, function_comments =
    match kwd_async with
    | None -> [], comments
    | Some _ -> comments, []
  in
  let children =
    [ mk_child_opt (make_kwd ~comments:async_comments) kwd_async
    ; mk_child_res (make_kwd ~comments:function_comments) kwd_function
    ; mk_child_res print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ; mk_child_res print_statement_block body_field
    ]
  in
  make_tree state node children

and print_return_type state node =
  match get_name node with
  | "type_annotation" -> print_type_annotation state node
  | "asserts_annotation" -> print_asserts_annotation state node
  | _ -> match_rest state node print_type_predicate_annotation

(* Generator function declaration (see function declaration) *)

and print_generator_function_declaration state node =
  let kwd_async = first_child_named_opt "async" node
  and kwd_function = first_child_named "function" node
  and sym_star = first_child_named "*" node
  and name_field = child_with_field "name" node
  (* "_call_signature" inlined: *)
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node
  (* "statement_block" *)
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_opt make_kwd kwd_async
    ; mk_child_res make_kwd kwd_function
    ; mk_child_res make_sym sym_star
    ; mk_child_res print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ; mk_child_res print_statement_block body_field
    ]
  in
  make_tree state node children

(* Class declaration (see [print_class]) *)

and print_class_declaration ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and decorators = children_named "decorator" node
  and kwd_class = first_child_named "class" node
  and name_field = child_with_field "name" node
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and heritage_child = first_child_named_opt "class_heritage" node
  and body_field = child_with_field "body" node in
  let children =
    mk_children_list print_decorator decorators
    @ [ mk_child_res (make_kwd ~comments) kwd_class (* Comments on "class" *)
      ; mk_child_res print_type_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_opt print_class_heritage heritage_child
      ; mk_child_res print_class_body body_field
      ]
  in
  make_tree state node children

(* Lexical declaration (see [print_variable_declaration]) *)

and print_lexical_declaration ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and kind_field = child_with_field "kind" node
  and var_decls = children_named "variable_declarator" node in
  let print_set_or_const state node =
    match get_name node with
    | "let" -> make_kwd ~comments state node
    | "const" -> make_kwd ~comments state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    mk_child_res print_set_or_const kind_field
    :: mk_children_list print_variable_declarator var_decls
  in
  make_tree state node children

and print_variable_declarator state node =
  let name_field = child_with_field "name" node
  and print_name_field state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | _ -> match_rest state node print_destructuring_pattern
  in
  let children =
    mk_child_res print_name_field name_field
    :: mk_child_initializer_opt node (* "_initializer" inlined *)
  in
  make_tree state node children

(* Variable declaration (see [print_lexical_declaration]) *)

and print_variable_declaration ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and kwd_var = first_child_named "var" node
  and var_decls = children_named "variable_declarator" node in
  let children =
    mk_child_res (make_kwd ~comments) kwd_var
    :: mk_children_list print_variable_declarator var_decls
  in
  make_tree state node children

(* Function signature (See [print_function_declaration]) *)

and print_function_signature state node =
  let kwd_async = first_child_named_opt "async" node
  and kwd_function = first_child_named "function" node
  and name_field = child_with_field "name" node
  (* "_call_signature" inlined: *)
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node in
  (* "statement_block" *)
  let children =
    [ mk_child_opt make_kwd kwd_async
    ; mk_child_res make_kwd kwd_function
    ; mk_child_res print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ]
  in
  make_tree state node children

(* Abstract class declaration ( see [print_class_declaration]) *)

and print_abstract_class_declaration state node =
  let decorators = children_named "decorator" node
  and kwd_abstract = first_child_named "abstract" node
  and kwd_class = first_child_named "class" node
  and name_field = child_with_field "name" node
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and heritage_child = first_child_named_opt "class_heritage" node
  and body_field = child_with_field "body" node in
  let children =
    mk_children_list print_decorator decorators
    @ [ mk_child_res make_kwd kwd_abstract
      ; mk_child_res make_kwd kwd_class
      ; mk_child_res print_type_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_opt print_class_heritage heritage_child
      ; mk_child_res print_class_body body_field
      ]
  in
  make_tree state node children

(* Module *)

and print_module ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and kwd_module = first_child_named "module" node
  and name_field = child_with_field "name" node
  and body_field = child_with_field_opt "body" node
  and print_name state node =
    match get_name node with
    | "string" -> print_string state node
    | "identifier" -> print_identifier state node
    | "nested_identifier" -> print_nested_identifier state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res (make_kwd ~comments) kwd_module
    ; mk_child_res print_name name_field
    ; mk_child_opt print_statement_block body_field
    ]
  in
  make_tree state node children

(* Internal module (a.k.a. namespaces) *)

and print_internal_module ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and kwd_namespace = first_child_named "namespace" node
  and name_field = child_with_field "name" node
  and body_field = child_with_field_opt "body" node
  and print_name state node =
    match get_name node with
    | "string" -> print_string state node
    | "identifier" -> print_identifier state node
    | "nested_identifier" -> print_nested_identifier state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res (make_kwd ~comments) kwd_namespace
    ; mk_child_res print_name name_field
    ; mk_child_opt print_statement_block body_field
    ]
  in
  make_tree state node children

(* Type alias declaration *)

and print_type_alias_declaration ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and kwd_type = first_child_named "type" node
  and name_field = child_with_field "name" node
  and sym_equal = first_child_named "=" node
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and value_field = child_with_field "value" node in
  let children =
    [ mk_child_res (make_kwd ~comments) kwd_type
    ; mk_child_res print_identifier name_field
    ; mk_child_res make_sym sym_equal
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_type value_field
    ]
  in
  make_tree state node children

(* Type parameters *)

and print_type_parameters state node = print_chevrons state node print_type_parameter

and print_type_parameter state node =
  let kwd_const = first_child_named_opt "const" node
  and name_field = child_with_field "name" node
  and constraint_field = child_with_field_opt "constraint" node
  and value_field = child_with_field_opt "value" node in
  let children =
    [ mk_child_opt make_kwd kwd_const
    ; mk_child_res print_identifier name_field
    ; mk_child_opt print_constraint constraint_field
    ; mk_child_opt print_default_type value_field
    ]
  in
  make_tree state node children

and print_constraint state node =
  let kwd_extends = first_child_named "extends" node
  and type_child = child_ranked 1 node in
  let children =
    [ mk_child_res make_kwd kwd_extends; mk_child_res print_type type_child ]
  in
  make_tree state node children

and print_default_type state node =
  let sym_equal = first_child_named "=" node
  and type_node = child_ranked 1 node in
  let children = [ mk_child_res make_sym sym_equal; mk_child_res print_type type_node ] in
  make_tree state node children

(* Enum declaration *)

and print_enum_declaration state node =
  let kwd_const = first_child_named_opt "const" node
  and kwd_enum = first_child_named "enum" node
  and name_field = child_with_field "name" node
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_opt make_kwd kwd_const
    ; mk_child_res make_kwd kwd_enum
    ; mk_child_res print_identifier name_field
    ; mk_child_res print_enum_body body_field
    ]
  in
  make_tree state node children

and print_enum_body state node =
  let print state node =
    match get_name node with
    | "enum_assignment" -> print_enum_assignment state node
    | _ -> match_rest state node print_property_name
  in
  print_braces state node print

and print_enum_assignment state node =
  let name_field = child_with_field "name" node in
  let children =
    mk_child_res print_property_name name_field
    :: mk_child_initializer_opt node (* "_initializer" inlined *)
  in
  make_tree state node children

(* Interface declaration *)

and print_interface_declaration state node =
  let kwd_interface = first_child_named "interface" node
  and name_field = child_with_field "name" node
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and extends_type_clause = first_child_named_opt "extends_type_clause" node
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_res make_kwd kwd_interface
    ; mk_child_res print_type_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_opt print_extends_type_clause extends_type_clause
    ; mk_child_res print_interface_body body_field
    ]
  in
  make_tree state node children

and print_interface_body state node = print_object_type state node

and print_extends_type_clause state node =
  let kwd_extends = first_child_named "extends" node
  and print state node =
    match get_name node with
    | "type_identifier" -> print_type_identifier state node
    | "nested_type_identifier" -> print_nested_type_identifier state node
    | "generic_type" -> print_generic_type state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    mk_child_res make_kwd kwd_extends
    :: mk_children_list print (collect_named_children node)
  in
  make_tree state node children

(* Import alias *)

and print_import_alias state node =
  let kwd_import = first_child_named "import" node
  and lhs = child_ranked 1 node
  and rhs = child_ranked 3 node
  and sym_equal = first_child_named "=" node
  and print_rhs state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | "nested_identifier" -> print_nested_identifier state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res make_kwd kwd_import
    ; mk_child_res print_identifier lhs
    ; mk_child_res make_sym sym_equal
    ; mk_child_res print_rhs rhs
    ]
  in
  make_tree state node children

(* Ambient declaration *)

and print_ambient_declaration state node =
  let kwd_declare = first_child_named "declare" node
  and fst_child = named_child_ranked 0 node in
  let children =
    mk_child_res make_kwd kwd_declare
    ::
    (match get_name_res fst_child with
    | "statement_block" ->
      let kwd_global = first_child_named "global" node in
      [ mk_child_res make_kwd kwd_global; mk_child_res print_statement_block fst_child ]
    | "property_identifier" ->
      let kwd_module = first_child_named "module" node
      and type_child = child_ranked 5 node in
      [ mk_child_res make_kwd kwd_module
      ; mk_child_res print_identifier fst_child
      ; mk_child_res print_type type_child
      ]
    | _ -> [ mk_child_res print_declaration fst_child ])
  in
  make_tree state node children

(* EXPRESSION

   The JavasScript tree-sitter grammar have the non-terminals
   "expression" and "primary_expression" be supertypes, that is,
   hidden rules. Therefore we have to match all the RHS of those
   non-terminals in [print_expression]. *)

and print_expression ?(comments = []) state (node : ts_tree) =
  match get_name node with
  (* Rest of "expression": *)
  | "glimmer_template" -> print_glimmer_template state node
  | "assignment_expression" -> print_assignment_expression state node
  | "augmented_assignment_expression" -> print_augmented_assignment_expression state node
  | "await_expression" -> print_await_expression state node
  | "unary_expression" -> print_unary_expression state node
  | "binary_expression" -> print_binary_expression ~comments state node
  | "ternary_expression" -> print_ternary_expression state node
  | "update_expression" -> print_update_expression state node
  | "new_expression" -> print_new_expression state node
  | "yield_expression" -> print_yield_expression state node
  | "as_expression" -> print_as_expression state node
  | "satisfies_expression" -> print_satisfies_expression state node
  | "instantiation_expression" -> print_instantiation_expression state node
  | "internal_module" -> print_internal_module ~comments state node
  | "type_assertion" -> print_type_assertion state node
  | _ -> print_primary_expression ~comments state node

and print_primary_expression ?(comments = []) state node =
  match get_name node with
  | "subscript_expression" -> print_subscript_expression state node
  | "member_expression" -> print_member_expression state node
  | "parenthesized_expression" -> print_parenthesized_expression state node
  | "identifier" -> print_identifier ~comments state node
  | "undefined" -> make_kwd state node
  | "this" -> make_kwd state node
  | "super" -> make_kwd state node
  | "number" -> print_number ~comments state node
  | "string" -> print_string state node
  | "template_string" -> print_template_string state node
  | "regex" -> print_regex state node
  | "true" -> make_kwd state node
  | "false" -> make_kwd state node
  | "null" -> make_kwd state node
  | "object" -> print_object state node
  | "array" -> print_array state node
  | "function_expression" -> print_function_expression state node
  | "arrow_function" -> print_arrow_function state node
  | "generator_function" -> print_generator_function state node
  | "class" -> print_class state node
  | "meta_property" -> print_meta_property state node
  | "call_expression" -> print_call_expression state node
  | "non_null_expression" -> print_non_null_expression state node
  | _ -> match_rest state node print_unexpected_node

(* Glimmer template (not supported) *)

and print_glimmer_template state node = make_node state node

(* Assignment expression *)

and print_assignment_expression state node =
  let kwd_using = first_child_named_opt "using" node
  and left_field = child_with_field "left" node
  and sym_equal = first_child_named "=" node
  and right_field = child_with_field "right" node
  and print_left state node =
    match get_name node with
    | "parenthesized_expression" -> print_parenthesized_expression state node
    | _ -> match_rest state node print_lhs_expression
  in
  let children =
    [ mk_child_opt make_kwd kwd_using
    ; mk_child_res print_left left_field
    ; mk_child_res make_sym sym_equal
    ; mk_child_res print_expression right_field
    ]
  in
  make_tree state node children

(* Augmented assignment expression *)

and print_augmented_assignment_expression state node =
  let left_field = child_with_field "left" node
  and right_field = child_with_field "right" node
  and operator = child_with_field "operator" node
  and print_left state node =
    (* "_augmented_assignment_lhs" is inlined here (hidden rule): *)
    match get_name node with
    | "member_expression" -> print_member_expression state node
    | "subscript_expression" -> print_subscript_expression state node
    | "identifier" -> print_identifier state node
    | "parenthesized_expression" -> print_parenthesized_expression state node
    | _ -> match_rest state node print_unexpected_node
  and print_assignment state node =
    match get_name node with
    | "+=" -> make_sym state node
    | "-=" -> make_sym state node
    | "*=" -> make_sym state node
    | "/=" -> make_sym state node
    | "%=" -> make_sym state node
    | "^=" -> make_sym state node
    | "&=" -> make_sym state node
    | "|=" -> make_sym state node
    | ">>=" -> make_sym state node
    | ">>>=" -> make_sym state node
    | "<<=" -> make_sym state node
    | "**=" -> make_sym state node
    | "&&=" -> make_sym state node
    | "||=" -> make_sym state node
    | "??=" -> make_sym state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_assignment operator
    ; mk_child_res print_left left_field
    ; mk_child_res print_expression right_field
    ]
  in
  make_tree state node children

(* Await expression *)

and print_await_expression state node =
  let kwd_await = first_child_named "await" node
  and expression = child_ranked 1 node in
  let children =
    [ mk_child_res make_kwd kwd_await; mk_child_res print_expression expression ]
  in
  make_tree state node children

(* Unary expression *)

and print_unary_expression state node =
  let operator_field = child_with_field "operator" node
  and argument_field = child_with_field "argument" node
  and print_unary_operator state node =
    match get_name node with
    | "!" -> make_sym state node
    | "~" -> make_sym state node
    | "-" -> make_sym state node
    | "+" -> make_sym state node
    | "typeof" -> make_kwd state node
    | "void" -> make_kwd state node
    | "delete" -> make_kwd state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_unary_operator operator_field
    ; mk_child_res print_number argument_field
    ]
  in
  make_tree state node children

(* Binary expression *)

and print_binary_expression ?(comments = []) state node =
  let comments = comments @ prev_comments node
  and left_field = child_with_field "left" node
  and right_field = child_with_field "right" node
  and operator = child_with_field "operator" node in
  let print_left state node =
    match get_name node with
    | "private_property_identifier" -> print_identifier ~comments state node
    | _ -> match_rest state node (print_expression ~comments)
  and print_bin_operator state node =
    match get_name node with
    | "&&" -> make_sym state node
    | "||" -> make_sym state node
    | ">>" -> make_sym state node
    | ">>>" -> make_sym state node
    | "<<" -> make_sym state node
    | "&" -> make_sym state node
    | "^" -> make_sym state node
    | "|" -> make_sym state node
    | "+" -> make_sym state node
    | "-" -> make_sym state node
    | "*" -> make_sym state node
    | "/" -> make_sym state node
    | "%" -> make_sym state node
    | "**" -> make_sym state node
    | "<" -> make_sym state node
    | "<=" -> make_sym state node
    | "==" -> make_sym state node
    | "===" -> make_sym state node
    | "!=" -> make_sym state node
    | "!==" -> make_sym state node
    | ">=" -> make_sym state node
    | ">" -> make_sym state node
    | "??" -> make_sym state node
    | "instanceof" -> make_sym state node
    | "in" -> make_sym state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_left left_field
    ; mk_child_res print_bin_operator operator
    ; mk_child_res print_expression right_field
    ]
  in
  make_tree state node children

(* Ternary expression *)

and print_ternary_expression state node =
  let condition_field = child_with_field "condition" node
  and consequence_field = child_with_field "consequence" node
  and alternative_field = child_with_field "alternative" node in
  let children =
    [ mk_child_res print_expression condition_field
    ; mk_child_res print_expression consequence_field
    ; mk_child_res print_expression alternative_field
    ]
  in
  make_tree state node children

(* Update expression *)

and print_update_expression state node =
  let argument_field = child_with_field "argument" node
  and first_child = child_ranked 0 node in
  let children =
    match get_name_res first_child with
    | "++" ->
      (* Prefix *)
      [ mk_child_res make_sym first_child; mk_child_res print_expression argument_field ]
    | "--" ->
      (* Prefix *)
      [ mk_child_res make_sym first_child; mk_child_res print_expression argument_field ]
    | _ ->
      let snd_child = child_ranked 1 node in
      (match get_name_res snd_child with
      | "++" ->
        (* Postfix *)
        [ mk_child_res print_expression argument_field; mk_child_res make_sym snd_child ]
      | "--" ->
        (* Postfix *)
        [ mk_child_res print_expression argument_field; mk_child_res make_sym snd_child ]
      | _ -> [] (* Should not happen. *))
  in
  make_tree state node children

(* New expression

   Note that the constructor field is a primary expression, but
   "primary_expression" is a supertype, that is, a hidden rule. We
   assume it is an "expression", since primary expressions are a subset
   of them. *)

and print_new_expression state node =
  let kwd_new = first_child_named "new" node
  and constructor_field = child_with_field "constructor" node
  and type_arguments_field = child_with_field_opt "type_arguments" node
  and arguments_field = child_with_field_opt "arguments" node in
  let children =
    [ mk_child_res make_kwd kwd_new
    ; mk_child_res print_expression constructor_field
    ; mk_child_opt print_type_arguments type_arguments_field
    ; mk_child_opt print_arguments arguments_field
    ]
  in
  make_tree state node children

(* Yield expression *)

and print_yield_expression state node =
  let kwd_yield = first_child_named "yield" node in
  match child_ranked_opt 1 node with
  | None -> make_unary_res state node make_kwd kwd_yield
  | Some snd_child ->
    let snd_child =
      match get_name snd_child with
      | "*" -> child_ranked 2 node
      | _ -> Result.Ok snd_child
    in
    let children =
      [ mk_child_res make_kwd kwd_yield; mk_child_res print_expression snd_child ]
    in
    make_tree state node children

(* As-expression *)

and print_as_expression state node =
  let expression = child_ranked 0 node
  and kwd_as = first_child_named "as" node
  and as_what = child_ranked 2 node
  and print_as state node =
    match get_name node with
    | "const" -> make_kwd state node
    | _ -> match_rest state node print_type
  in
  let children =
    [ mk_child_res print_expression expression
    ; mk_child_res make_kwd kwd_as
    ; mk_child_res print_as as_what
    ]
  in
  make_tree state node children

(* Statisfies-expression *)

and print_satisfies_expression state node =
  let expression = child_ranked 0 node
  and kwd_satisfies = first_child_named "satisfies" node
  and type_child = child_ranked 2 node in
  let children =
    [ mk_child_res print_expression expression
    ; mk_child_res make_kwd kwd_satisfies
    ; mk_child_res print_type type_child
    ]
  in
  make_tree state node children

(* Instantiation expression *)

and print_instantiation_expression state node =
  let expression = named_child_ranked 0 node
  and type_arguments_field = child_with_field "type_arguments" node in
  let children =
    [ mk_child_res print_expression expression
    ; mk_child_res print_type_arguments type_arguments_field
    ]
  in
  make_tree state node children

(* Type assertion *)

and print_type_assertion state node =
  let type_arguments = named_child_ranked 0 node
  and expression = named_child_ranked 1 node in
  let children =
    [ mk_child_res print_type_arguments type_arguments
    ; mk_child_res print_expression expression
    ]
  in
  make_tree state node children

(* Subscript expression (see [print_member_expression]) *)

and print_subscript_expression state node =
  let object_field = child_with_field "object" node
  and optional_chain_field = child_with_field_opt "optional_chain" node
  and index_field = child_with_field "index" node
  and sym_lbracket = first_child_named "[" node
  and sym_rbracket = first_child_named "]" node
  and print_chain state node =
    match get_name node with
    | "optional_chain" -> make_node state node
    | _ -> match_rest state node print_unexpected_node
  and print_index state node =
    match get_name node with
    | "sequence_expression" -> print_sequence_expression state node
    | _ -> print_expression state node
  in
  let children =
    [ mk_child_res print_expression object_field
    ; mk_child_opt print_chain optional_chain_field
    ; mk_child_res make_sym sym_lbracket
    ; mk_child_res print_index index_field
    ; mk_child_res make_sym sym_rbracket
    ]
  in
  make_tree state node children

(* Member expression *)

and print_member_expression state node =
  let object_field = child_with_field "object" node
  and optional_chain_field = child_with_field_opt "optional_chain" node
  and property_field = child_with_field "property" node
  and print_object state node =
    match get_name node with
    | "import" -> make_kwd state node
    | _ -> match_rest state node print_expression
  and print_selector state = function
    | None -> () (* "." *)
    | Some node ->
      (* "?." *)
      make_sym state node
  in
  let children =
    [ mk_child_res print_object object_field
    ; mk_child print_selector optional_chain_field
    ; mk_child_res print_property_field property_field
    ]
  in
  make_tree state node children

(* Parenthesised expression *)

and print_parenthesized_expression ?(comments = []) state node =
  let print state node =
    match get_name node with
    | "sequence_expression" -> print_sequence_expression state node
    | _ -> print_expression state node
  in
  print_parens ~comments state node print

(* Template strings *)

and print_template_string ?(comments = []) state node =
  let opening = child_ranked 0 node in
  let closing = last_child node in
  let raw_children = collect_named_children node in
  let print ?comments state node =
    match get_name node with
    | "string_fragment" -> make_node ?comments state node
    | "escape_sequence" -> make_node ?comments state node
    | "template_substitution" -> make_node ?comments state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    (mk_child_res (make_sym ~comments) opening :: mk_children_list print raw_children)
    @ [ mk_child_res make_sym closing ]
  in
  make_tree state node children

(* Object *)

and print_object state node =
  let print state node =
    match get_name node with
    | "pair" -> print_pair state node
    | "spread_element" -> print_spread_element state node
    | "method_definition" -> print_method_definition state node
    | "shorthand_property_identifier" ->
      print_shorthand_property_identifier_pattern state node
    | _ -> match_rest state node print_unexpected_node
  in
  print_braces state node print

(* Pairs *)

and print_pair state node =
  let key_field = child_with_field "key" node
  and value_field = child_with_field "value" node
  and sym_colon = first_child_named ":" node in
  let children =
    [ mk_child_res print_property_name key_field
    ; mk_child_res make_sym sym_colon
    ; mk_child_res print_expression value_field
    ]
  in
  make_tree state node children

(* Array (expression) *)

and print_array state node = print_brackets state node print_array_cell

and print_array_cell state node =
  match get_name node with
  | "spread_element" -> print_spread_element state node
  | _ -> match_rest state node print_expression

and print_spread_element state node =
  let sym_ellipsis = first_child_named "..." node
  and expr_node = named_child_ranked 0 node in
  let children =
    [ mk_child_res make_sym sym_ellipsis; mk_child_res print_expression expr_node ]
  in
  make_tree state node children

(* Function (expression) *)

and print_function_expression state node =
  let kwd_async = first_child_named_opt "async" node
  and kwd_function = first_child_named "function" node
  and name_field = child_with_field_opt "name" node
  (* "_call_signature" inlined: *)
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node
  (* "statement_block" *)
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_opt make_kwd kwd_async
    ; mk_child_res make_kwd kwd_function
    ; mk_child_opt print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ; mk_child_res print_statement_block body_field
    ]
  in
  make_tree state node children

(* Arrow function *)

and print_arrow_function state node =
  let kwd_async = first_child_named_opt "async" node
  and parameter_field = child_with_field_opt "parameter" node
  and sym_arrow = first_child_named "=>" node
  and body_field = child_with_field "body" node in
  let children =
    match parameter_field with
    | Some parameter_field ->
      [ mk_child_opt make_kwd kwd_async
      ; mk_child print_identifier parameter_field
      ; mk_child_res make_sym sym_arrow
      ; mk_child_res print_arrow_function_body body_field
      ]
    | None ->
      (* "_call_signature" inlined: *)
      let type_parameters_field = child_with_field_opt "type_parameters" node
      and parameters_field = child_with_field "parameters" node
      and return_type_field = child_with_field_opt "return_type" node in
      [ mk_child_opt make_kwd kwd_async
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ; mk_child_res make_sym sym_arrow
      ; mk_child_res print_arrow_function_body body_field
      ]
  in
  make_tree state node children

and print_arrow_function_body state node =
  match get_name node with
  | "statement_block" -> print_statement_block state node
  | _ -> match_rest state node print_expression

(* Generator function *)

and print_generator_function state node =
  let kwd_async = first_child_named_opt "async" node
  and kwd_function = first_child_named "function" node
  and sym_star = first_child_named "*" node
  and name_field = child_with_field_opt "name" node
  (* "_call_signature" inlined: *)
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node
  (* "statement_block" *)
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_opt make_kwd kwd_async
    ; mk_child_res make_kwd kwd_function
    ; mk_child_res make_sym sym_star
    ; mk_child_opt print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ; mk_child_res print_statement_block body_field
    ]
  in
  make_tree state node children

(* Class *)

and print_class state node =
  let decorators = children_named "decorator" node
  and kwd_class = first_child_named "class" node
  and name_field = child_with_field_opt "name" node
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and heritage_child = first_child_named_opt "class_heritage" node
  and body_field = child_with_field "body" node in
  let children =
    mk_children_list print_decorator decorators
    @ [ mk_child_res make_kwd kwd_class
      ; mk_child_opt print_type_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_opt print_class_heritage heritage_child
      ; mk_child_res print_class_body body_field
      ]
  in
  make_tree state node children

and print_class_heritage state node =
  let children =
    match first_child_named_opt "extends_clause" node with
    | Some extends_clause ->
      let implements_clause = first_child_named_opt "implements_clause" node in
      [ mk_child print_extends_clause extends_clause
      ; mk_child_opt print_implements_clause implements_clause
      ]
    | None ->
      (* [implements_clause] is never [None]. *)
      let implements_clause = first_child_named_opt "implements_clause" node in
      [ mk_child_opt print_implements_clause implements_clause ]
  in
  make_tree state node children

and print_implements_clause state node =
  let kwd_implements = first_child_named "implements" node
  and named_children = collect_named_children node in
  let children =
    mk_child_res make_kwd kwd_implements :: mk_children_list print_type named_children
  in
  make_tree state node children

and print_extends_clause state node =
  let kwd_extends = first_child_named "extends" node
  and children =
    match collect_children node with
    | [] -> []
    | _extends :: clauses -> clauses
  in
  let not_comma child = String.(get_name child <> ",") in
  let children = List.filter children ~f:not_comma in
  let rec pair_up acc = function
    | value :: snd :: nodes ->
      if String.equal (get_name snd) "type_arguments"
      then pair_up ((value, Some snd) :: acc) nodes
      else pair_up ((value, None) :: acc) (snd :: nodes)
    | [ value ] -> List.rev ((value, None) :: acc)
    | [] -> List.rev acc
  in
  let pairs = pair_up [] children in
  let mk_children (value, type_arguments_opt) acc =
    let value_child = mk_child print_expression value in
    match type_arguments_opt with
    | None -> value_child :: acc
    | Some type_arguments ->
      value_child :: mk_child print_type_arguments type_arguments :: acc
  in
  let children =
    mk_child_res make_kwd kwd_extends :: List.fold_right ~f:mk_children pairs ~init:[]
  in
  make_tree state node children

and print_class_body ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  let opening = first_child_named "{" node
  and closing = first_child_named "}" node
  and named_children = collect_named_children node in
  let pair (decorators, acc) child =
    match get_name child with
    | "decorator" -> child :: decorators, acc
    | _ -> [], (List.rev decorators, child) :: acc
  in
  let _, pairs = List.fold_left ~f:pair ~init:([], []) named_children in
  let pairs = List.rev pairs in
  let children =
    (mk_child_res (make_sym ~comments) opening
    :: mk_children_list print_class_member pairs)
    @ [ mk_child_res make_sym closing ]
  in
  make_tree state node children

and print_class_member state (decorators, node) =
  match get_name node with
  | "method_definition" ->
    List.iter ~f:(print_decorator state) decorators;
    print_method_definition state node
  | "method_signature" -> print_method_signature state node
  | "class_static_block" -> print_class_static_block state node
  | "abstract_method_signature" -> print_abstract_method_signature state node
  | "index_signature" -> print_index_signature state node
  | "public_field_definition" -> print_public_field_definition state node
  | _ -> match_rest state node print_unexpected_node

and print_method_definition state node =
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node
  and kwd_static = first_child_named_opt "static" node
  and override_modifier = first_child_named_opt "override_modifier" node
  and kwd_readonly = first_child_named_opt "readonly" node
  and kwd_async = first_child_named_opt "async" node
  and kwd_set = first_child_named_opt "set" node
  and kwd_get = first_child_named_opt "get" node
  and sym_star = first_child_named_opt "*" node
  and name_field = child_with_field "name" node
  and qmark = first_child_named_opt "?" node
  (* "_call_signature" inlined: *)
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node
  (* "statement_block" *)
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_opt print_accessibility_modifier accessibility_modifier
    ; mk_child_opt make_kwd kwd_static
    ; mk_child_opt print_override_modifier override_modifier
    ; mk_child_opt make_kwd kwd_readonly
    ; mk_child_opt make_kwd kwd_async
    ; mk_child_opt make_kwd kwd_set
    ; mk_child_opt make_kwd kwd_get
    ; mk_child_opt make_sym sym_star
    ; mk_child_res print_property_name name_field
    ; mk_child_opt make_node qmark
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ; mk_child_res print_statement_block body_field
    ]
  in
  make_tree state node children

and print_class_static_block state node =
  let kwd_static = first_child_named "static" node
  and body_field = child_with_field "body" node in
  let children =
    [ mk_child_res make_kwd kwd_static; mk_child_res print_statement_block body_field ]
  in
  make_tree state node children

and print_abstract_method_signature state node =
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node
  and kwd_abstract = first_child_named_opt "abstract" node
  and override_modifier = first_child_named_opt "override_modifier" node
  and kwd_set = first_child_named_opt "set" node
  and kwd_get = first_child_named_opt "get" node
  and sym_star = first_child_named_opt "*" node
  and name_field = child_with_field "name" node
  and sym_qmark = first_child_named_opt "?" node
  (* "_call_signature" inlined: *)
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node in
  let children =
    [ mk_child_opt print_accessibility_modifier accessibility_modifier
    ; mk_child_opt make_kwd kwd_abstract
    ; mk_child_opt print_override_modifier override_modifier
    ; mk_child_opt make_kwd kwd_set
    ; mk_child_opt make_kwd kwd_get
    ; mk_child_opt make_node sym_star
    ; mk_child_res print_property_name name_field
    ; mk_child_opt make_sym sym_qmark
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ]
  in
  make_tree state node children

and print_public_field_definition state node =
  let decorators = children_named "decorator" node
  and accessibility_modifier = first_child_named_opt "accessibility_modifier" node
  and override_modifier = first_child_named_opt "override_modifier" node
  and kwd_declare = first_child_named_opt "declare" node
  and kwd_static = first_child_named_opt "static" node
  and kwd_readonly = first_child_named_opt "readonly" node
  and kwd_accessor = first_child_named_opt "accessor" node
  and kwd_abstract = first_child_named_opt "abstract" node
  and name_field = child_with_field "name" node
  and type_field = child_with_field_opt "type" node
  and sym_qmark = first_child_named_opt "?" node
  and sym_emark = first_child_named_opt "!" node in
  let children =
    mk_children_list print_decorator decorators
    @ [ mk_child_opt make_kwd kwd_declare
      ; mk_child_opt print_accessibility_modifier accessibility_modifier
      ; mk_child_opt print_override_modifier override_modifier
      ; mk_child_opt make_kwd kwd_static
      ; mk_child_opt make_kwd kwd_readonly
      ; mk_child_opt make_kwd kwd_accessor
      ; mk_child_opt make_kwd kwd_abstract
      ; mk_child_res print_property_name name_field
      ; mk_child_opt make_sym sym_qmark
      ; mk_child_opt make_sym sym_emark
      ; mk_child_opt print_type_annotation type_field
      ]
    @ mk_child_initializer_opt node (* "_initializer" inlined *)
  in
  make_tree state node children

(* Meta-property *)

and print_meta_property state node =
  let fst_child = child_ranked 0 node
  and snd_child = child_ranked 2 node in
  let children = [ mk_child_res make_kwd fst_child; mk_child_res make_kwd snd_child ] in
  make_tree state node children

(* Call expression *)

and print_call_expression state node =
  let function_field = child_with_field "function" node
  and member_selection = first_child_named_opt "?." node
  and type_arguments_field = child_with_field_opt "type_arguments" node
  and arguments_field = child_with_field "arguments" node in
  let children =
    match member_selection with
    | None ->
      let print_function state node =
        match get_name node with
        | "import" -> make_kwd state node
        | _ -> match_rest state node print_expression
      and print_arguments state node =
        match get_name node with
        | "template_string" -> print_template_string state node
        | _ -> match_rest state node print_arguments
      in
      [ mk_child_res print_function function_field
      ; mk_child_opt print_type_arguments type_arguments_field
      ; mk_child_res print_arguments arguments_field
      ]
    | Some _ ->
      [ mk_child_res print_primary_expression function_field
      ; mk_child_opt print_type_arguments type_arguments_field
      ; mk_child_res print_arguments arguments_field
      ]
  in
  make_tree state node children

and print_type_arguments state node = print_chevrons state node print_type
and print_arguments state node = print_parens state node print_argument

and print_argument state node =
  match get_name node with
  | "spread_element" -> print_spread_element state node
  | _ -> match_rest state node print_expression

(* Non-null expression *)

and print_non_null_expression state node =
  let child = named_child_ranked 0 node in
  make_unary_res state node print_expression child

(* Sequence expression *)

and print_sequence_expression ?(comments = []) state node =
  tree_of_named_children ~comments state node print_expression

(* TYPE

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and print_type state node =
  match get_name node with
  | "function_type" -> print_function_type state node
  | "readonly_type" -> print_readonly_type state node
  | "constructor_type" -> print_constructor_type state node
  | "infer_type" -> print_infer_type state node
  (* A couple of aliases *)
  | "member_expression" -> print_member_expression state node
  | "call_expression" -> print_call_expression state node
  (* "primary_type" is hidden *)
  | _ -> match_rest state node print_primary_type

and print_primary_type state node =
  match get_name node with
  | "parenthesized_type" -> print_parenthesized_type state node
  | "predefined_type" -> print_predefined_type state node
  | "type_identifier" -> print_type_identifier state node
  | "nested_type_identifier" -> print_nested_type_identifier state node
  | "generic_type" -> print_generic_type state node
  | "object_type" -> print_object_type state node
  | "array_type" -> print_array_type state node
  | "tuple_type" -> print_tuple_type state node
  | "flow_maybe_type" -> print_flow_maybe_type state node
  | "type_query" -> print_type_query state node
  | "index_type_query" -> print_index_type_query state node
  | "this_type" -> make_kwd state node
  | "existential_type" -> print_existential_type state node
  | "literal_type" -> print_literal_type state node
  | "lookup_type" -> print_lookup_type state node
  | "conditional_type" -> print_conditional_type state node
  | "template_literal_type" -> print_template_literal_type state node
  | "intersection_type" -> print_intersection_type state node
  | "union_type" -> print_union_type state node
  | _ -> match_rest state node print_unexpected_node

(* Flow maybe type

   flow_maybe_type: $ => prec.right(seq('?', $.primary_type))
*)

and print_flow_maybe_type state node =
  let child = named_child_ranked 0 node in
  make_unary_res state node print_primary_type child

(* Type identifier *)

and print_type_identifier ?comments state node = print_identifier ?comments state node

(* Parenthesized type *)

and print_parenthesized_type state node = print_parens state node print_type

(* Predefined type *)

and print_predefined_type ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  match collect_children node with
  | [] -> () (* Should not happen *)
  | child :: _ ->
    (* The tree-sitter parser for TypeScript has a bug: a child node
       "unique symbol" occurs repeated, for some mysterious
       reason. This case of the pattern matching is a hack to work
       around the issue. For reference, here is the production:

       predefined_type: _ => choice(
         ...
         alias(seq('unique', 'symbol'), 'unique symbol')
         ...)
    *)
    let print state node =
      match get_name node with
      | "any" -> make_kwd ~comments state node
      | "number" -> make_kwd ~comments state node
      | "boolean" -> make_kwd ~comments state node
      | "string" -> make_kwd ~comments state node
      | "symbol" -> make_kwd ~comments state node
      | "unique symbol" -> make_kwd ~comments state node
      | "void" -> make_kwd ~comments state node
      | "unknown" -> make_kwd ~comments state node
      | "never" -> make_kwd ~comments state node
      | "object" -> make_kwd ~comments state node
      | _ -> match_rest state node print_unexpected_node
    in
    make_unary state node print child

(* Nested type identifier *)

and print_nested_type_identifier ?comments state node =
  let module_field = child_with_field "module" node
  and name_field = child_with_field "name" node
  and print_module_field state node =
    match get_name node with
    | "identifier" -> print_identifier ?comments state node
    | "nested_identifier" -> print_nested_identifier ?comments state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_module_field module_field
    ; mk_child_res print_type_identifier name_field
    ]
  in
  make_tree state node children

(* Nested identifier *)

and print_nested_identifier ?comments state node =
  let object_field = child_with_field "object" node
  and property_field = child_with_field "property" node
  and print_object_field state node =
    match get_name node with
    | "identifier" -> print_identifier ?comments state node
    | "member_expression" -> print_nested_identifier ?comments state node
    | _ -> match_rest state node print_unexpected_node
  and print_property_field state node =
    match get_name node with
    | "property_identifier" -> print_identifier state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_object_field object_field
    ; mk_child_res print_property_field property_field
    ]
  in
  make_tree state node children

(* Generic type *)

and print_generic_type ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  let name_field = child_with_field "name" node
  and type_arguments_field = child_with_field "type_arguments" node
  and print_name_field state node =
    match get_name node with
    | "type_identifier" -> print_type_identifier ~comments state node
    | "nested_type_identifier" -> print_nested_type_identifier ~comments state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_name_field name_field
    ; mk_child_res print_type_arguments type_arguments_field
    ]
  in
  make_tree state node children

(* Object type *)

and print_object_type state node = print_braces state node print_object_type_field

and print_object_type_field state node =
  match get_name node with
  | "export_statement" -> print_export_statement state node
  | "property_signature" -> print_property_signature state node
  | "call_signature" -> print_call_signature state node
  | "construct_signature" -> print_construct_signature state node
  | "index_signature" -> print_index_signature state node
  | "method_signature" -> print_method_signature state node
  | _ -> match_rest state node print_unexpected_node

and print_property_signature state node =
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node
  and kwd_static = first_child_named_opt "static" node
  and override_modifier = first_child_named_opt "override_modifier" node
  and kwd_readonly = first_child_named_opt "readonly" node
  and name_field = child_with_field "name" node
  and sym_qmark = first_child_named_opt "?" node
  and type_field = child_with_field_opt "type" node in
  let children =
    [ mk_child_opt print_accessibility_modifier accessibility_modifier
    ; mk_child_opt make_kwd kwd_static
    ; mk_child_opt print_override_modifier override_modifier
    ; mk_child_opt make_kwd kwd_readonly
    ; mk_child_res print_identifier name_field
    ; mk_child_opt make_sym sym_qmark
    ; mk_child_opt print_type_annotation type_field
    ]
  in
  make_tree state node children

(* Call signature *)

and print_call_signature state node =
  let type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node in
  let children =
    [ mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ]
  in
  make_tree state node children

(* Asserts annotation *)

and print_asserts_annotation state node =
  let asserts = first_child_named "asserts" node in
  make_unary_res state node print_asserts asserts

and print_asserts state node =
  let kwd_asserts = first_child_named "asserts" node
  and child = child_ranked 1 node
  and print state node =
    match get_name node with
    | "type_predicate" -> print_type_predicate state node
    | "identifier" -> print_identifier state node
    | "this" -> make_kwd state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children = [ mk_child_res make_kwd kwd_asserts; mk_child_res print child ] in
  make_tree state node children

(* Type predicate annotation *)

and print_type_predicate_annotation state node =
  let predicate = child_ranked 1 node in
  make_unary_res state node print_type_predicate predicate

(* Construct signature *)

and print_construct_signature state node =
  let kwd_abstract = first_child_named_opt "abstract" node
  and kwd_new = first_child_named "new" node
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and type_field = child_with_field_opt "type" node in
  let children =
    [ mk_child_opt make_kwd kwd_abstract
    ; mk_child_res make_kwd kwd_new
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_type_annotation type_field
    ]
  in
  make_tree state node children

(* Index signature *)

and print_index_signature state node =
  let kwd_readonly = first_child_named_opt "readonly" node
  and sign_field = child_with_field_opt "sign" node
  and name_field = child_with_field_opt "name" node
  and type_field = child_with_field "type" node
  and sym_lbracket = first_child_named "[" node
  and sym_rbracket = first_child_named "]" node
  and print_type_field state node =
    match get_name node with
    | "type_annotation" -> print_type_annotation state node
    | "omitting_type_annotation" -> print_omitting_type_annotation state node
    | "adding_type_annotation" -> print_adding_type_annotation state node
    | "opting_type_annotation" -> print_opting_type_annotation state node
    | _ -> match_rest state node print_unexpected_node
  in
  let prefix =
    [ mk_child_opt print_plus_minus sign_field; mk_child_opt make_kwd kwd_readonly ]
  in
  let children =
    prefix
    @ [ mk_child_res make_sym sym_lbracket ]
    @ (match name_field with
      | Some name_field ->
        let sym_colon = first_child_named ":" node
        and index_type_field = child_with_field "index_type" node in
        [ mk_child print_identifier name_field
        ; mk_child_res make_sym sym_colon
        ; mk_child_res print_type index_type_field
        ; mk_child_res print_type_field type_field
        ]
      | None ->
        let mapped_type_clause = named_child_ranked 0 node in
        [ mk_child_res print_mapped_type_clause mapped_type_clause
        ; mk_child_res print_type_field type_field
        ])
    @ [ mk_child_res make_sym sym_rbracket ]
  in
  make_tree state node children

and print_plus_minus state node =
  match get_name node with
  | "+" -> make_sym state node
  | "-" -> make_sym state node
  | _ -> match_rest state node print_unexpected_node

and print_mapped_type_clause state node =
  let name_field = child_with_field "name" node
  and kwd_in = first_child_named "in" node
  and type_field = child_with_field "type" node
  and alias_field = child_with_field_opt "alias" node in
  let alias_children =
    match alias_field with
    | None -> []
    | Some alias ->
      let kwd_as = first_child_named "as" node in
      [ mk_child_res make_kwd kwd_as; mk_child print_type alias ]
  in
  let children =
    [ mk_child_res print_type_identifier name_field
    ; mk_child_res make_kwd kwd_in
    ; mk_child_res print_type type_field
    ]
    @ alias_children
  in
  make_tree state node children

and print_omitting_type_annotation state node =
  let sym_kind = first_child_named "-?:" node
  and type_child = named_child_ranked 0 node in
  let children = [ mk_child_res make_sym sym_kind; mk_child_res print_type type_child ] in
  make_tree state node children

and print_adding_type_annotation state node =
  let sym_kind = first_child_named "+?:" node
  and type_child = named_child_ranked 0 node in
  let children = [ mk_child_res make_sym sym_kind; mk_child_res print_type type_child ] in
  make_tree state node children

and print_opting_type_annotation state node =
  let sym_kind = first_child_named "?:" node
  and type_child = named_child_ranked 0 node in
  let children = [ mk_child_res make_sym sym_kind; mk_child_res print_type type_child ] in
  make_tree state node children

(* Method signature *)

and print_method_signature state node =
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node
  and kwd_static = first_child_named_opt "static" node
  and override_modifier = first_child_named_opt "override_modifier" node
  and kwd_readonly = first_child_named_opt "readonly" node
  and kwd_async = first_child_named_opt "async" node
  and kwd_set = first_child_named_opt "set" node
  and kwd_get = first_child_named_opt "get" node
  and sym_star = first_child_named_opt "*" node
  and name_field = child_with_field "name" node
  and sym_qmark = first_child_named_opt "?" node
  (* "_call_signature" inlined: *)
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field_opt "return_type" node in
  let children =
    [ mk_child_opt print_accessibility_modifier accessibility_modifier
    ; mk_child_opt make_kwd kwd_static
    ; mk_child_opt print_override_modifier override_modifier
    ; mk_child_opt make_kwd kwd_readonly
    ; mk_child_opt make_kwd kwd_async
    ; mk_child_opt make_kwd kwd_set
    ; mk_child_opt make_kwd kwd_get
    ; mk_child_opt make_sym sym_star
    ; mk_child_res print_property_name name_field
    ; mk_child_opt make_sym sym_qmark
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ]
  in
  make_tree state node children

(* Array type *)

and print_array_type state node =
  let type_child = child_ranked 0 node
  and sym_lbracket = first_child_named "[" node
  and sym_rbracket = first_child_named "]" node in
  let children =
    [ mk_child_res print_type type_child
    ; mk_child_res make_sym sym_lbracket
    ; mk_child_res make_sym sym_rbracket
    ]
  in
  make_tree state node children

(* Tuple type *)

and print_tuple_type state node = print_brackets state node print_tuple_type_member

and print_tuple_type_member state node =
  match get_name node with
  | "required_parameter" -> print_tuple_parameter state node (* Alias *)
  | "optional_parameter" -> print_optional_tuple_parameter state node (* Alias *)
  | "optional_type" -> print_optional_type state node
  | "rest_type" -> print_rest_type state node
  | _ -> match_rest state node print_type (* "type" is a hidden rule *)

and print_tuple_parameter state node =
  let name_field = child_with_field "name" node
  and type_field = child_with_field "type" node
  and print_name_field state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | "rest_pattern" -> print_rest_pattern state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_name_field name_field
    ; mk_child_res print_type_annotation type_field
    ]
  in
  make_tree state node children

and print_optional_tuple_parameter state node =
  let name_field = child_with_field "name" node
  and type_field = child_with_field "type" node in
  let children =
    [ mk_child_res print_identifier name_field
    ; mk_child_res print_type_annotation type_field
    ]
  in
  make_tree state node children

(* Type annotation *)

and print_type_annotation state node =
  let sym_colon = first_child_named ":" node
  and type_child = named_child_ranked 0 node in
  let children =
    [ mk_child_res make_sym sym_colon; mk_child_res print_type type_child ]
  in
  make_tree state node children

(* Rest pattern *)

and print_rest_pattern state node =
  let sym_ellipsis = first_child_named "..." node
  and expr_child = named_child_ranked 0 node in
  let children =
    [ mk_child_res make_sym sym_ellipsis; mk_child_res print_lhs_expression expr_child ]
  in
  make_tree state node children

(* LHS expression *)

and print_lhs_expression state node =
  match get_name node with
  | "member_expression" -> print_member_expression state node
  | "subscript_expression" -> print_subscript_expression state node
  | "identifier" -> print_identifier state node
  | "undefined" -> make_kwd state node
  | "object_pattern" -> print_object_pattern state node
  | "array_pattern" -> print_array_pattern state node
  | "non_null_expression" -> print_non_null_expression state node
  | _ -> match_rest state node print_unexpected_node

and print_optional_type state node =
  let child = named_child_ranked 0 node in
  make_unary_res state node print_type child

and print_rest_type state node =
  let sym_ellipsis = first_child_named "..." node
  and type_child = named_child_ranked 0 node in
  let children =
    [ mk_child_res make_sym sym_ellipsis; mk_child_res print_type type_child ]
  in
  make_tree state node children

(* Type query *)

and print_type_query state node =
  let kwd_typeof = first_child_named "typeof" node
  and child = child_ranked 1 node
  and print state node =
    match get_name node with
    | "subscript_expression" -> print_type_query_subscript_expression state node
    | "member_expression" -> print_type_query_member_expression state node
    | "call_expression" -> print_type_query_call_expression state node
    | "instantiation_expression" -> print_type_query_instantiation_expression state node
    | "identifier" -> print_identifier state node
    | "this" -> make_kwd state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children = [ mk_child_res make_kwd kwd_typeof; mk_child_res print child ] in
  make_tree state node children

and print_type_query_subscript_expression state node =
  let object_field = child_with_field "object" node
  and index_field = child_with_field "index" node
  and sym_lbracket = first_child_named "[" node
  and sym_rbracket = first_child_named "]" node
  and print_index_field state node =
    match get_name node with
    | "predefined_type" -> print_predefined_type state node
    | "string" -> make_node state node
    | "number" -> print_number state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_object_field object_field
    ; mk_child_res make_sym sym_lbracket
    ; mk_child_res print_index_field index_field
    ; mk_child_res make_sym sym_rbracket
    ]
  in
  make_tree state node children

and print_type_query_member_expression state node =
  let object_field = child_with_field "object" node
  and property_field = child_with_field "property" node in
  let children =
    [ mk_child_res print_object_field object_field
    ; mk_child_res print_property_field property_field
    ]
  in
  make_tree state node children

and print_object_field state node =
  match get_name node with
  | "identifier" -> print_identifier state node
  | "this" -> make_kwd state node
  | "member_expression" -> print_type_query_member_expression state node
  | "subscript_expression" -> print_type_query_subscript_expression state node
  | "call_expression" -> print_type_query_call_expression state node
  | _ -> match_rest state node print_unexpected_node

and print_property_field state node =
  match get_name node with
  | "private_property_identifier" -> print_identifier state node
  | "property_identifier" -> print_identifier state node
  | _ -> match_rest state node print_unexpected_node

and print_type_query_instantiation_expression state node =
  let function_field = child_with_field "function" node
  and type_arguments_field = child_with_field "type_arguments" node in
  let children =
    [ mk_child_res print_function_field function_field
    ; mk_child_res print_type_arguments type_arguments_field
    ]
  in
  make_tree state node children

and print_function_field state node =
  match get_name node with
  | "import" -> make_kwd state node
  | "identifier" -> print_identifier state node
  | "member_expression" -> print_type_query_member_expression state node
  | "subscript_expression" -> print_type_query_subscript_expression state node
  | _ -> match_rest state node print_unexpected_node

and print_type_query_call_expression state node =
  let function_field = child_with_field "function" node
  and arguments_field = child_with_field "arguments" node in
  let children =
    [ mk_child_res print_function_field function_field
    ; mk_child_res print_arguments arguments_field
    ]
  in
  make_tree state node children

(* Index type query *)

and print_index_type_query state node =
  let child = named_child_ranked 0 node in
  make_unary_res state node print_primary_type child

(* Existential type *)

and print_existential_type state node = make_node state node

(* Literal type *)

and print_literal_type state node =
  let child = named_child_ranked 0 node
  and print state node =
    match get_name node with
    | "unary_expression" -> print_unary_expression state node
    | "number" -> print_number state node
    | "string" -> print_string state node
    | "true" -> make_kwd state node
    | "false" -> make_kwd state node
    | "null" -> make_kwd state node
    | "undefined" -> make_kwd state node
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print child

(* Look up type

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and print_lookup_type state node =
  let primary_type_child = named_child_ranked 0 node
  and sym_lbracket = first_child_named "[" node
  and sym_rbracket = first_child_named "]" node
  and type_child = named_child_ranked 1 node in
  let children =
    [ mk_child_res print_primary_type primary_type_child
    ; mk_child_res make_sym sym_lbracket
    ; mk_child_res print_type type_child
    ; mk_child_res make_sym sym_rbracket
    ]
  in
  make_tree state node children

(* Conditional type *)

and print_conditional_type state node =
  let left_field = child_with_field "left" node
  and kwd_extends = first_child_named "extends" node
  and right_field = child_with_field "right" node
  and consequence_field = child_with_field "consequence" node
  and alternative_field = child_with_field "alternative" node
  and sym_qmark = first_child_named "?" node
  and sym_colon = first_child_named ":" node in
  let children =
    [ mk_child_res print_type left_field
    ; mk_child_res make_kwd kwd_extends
    ; mk_child_res print_type right_field
    ; mk_child_res make_sym sym_qmark
    ; mk_child_res print_type consequence_field
    ; mk_child_res make_sym sym_colon
    ; mk_child_res print_type alternative_field
    ]
  in
  make_tree state node children

(* Template literal type *)

and print_template_literal_type state node = make_node state node

(* Intersection type *)

and print_intersection_type state node =
  let first_child = child_ranked_opt 0 node
  and sym_ampersand = first_child_named "&" node in
  let children =
    match first_child with
    | None -> internal_error_child "" node
    | Some left_type ->
      (match get_name left_type with
      | "&" ->
        let type_node = child_ranked 1 node in
        [ mk_child_res make_sym sym_ampersand; mk_child_res print_type type_node ]
      | _ ->
        (* "type" is a supertype, therefore a hidden rule *)
        let right_type = child_ranked 2 node in
        [ mk_child print_type left_type
        ; mk_child_res make_sym sym_ampersand
        ; mk_child_res print_type right_type
        ])
  in
  make_tree state node children

(* Union type *)

and print_union_type state node =
  let first_child = child_ranked_opt 0 node
  and sym_vbar = first_child_named "|" node in
  let children =
    match first_child with
    | None -> internal_error_child "" node
    | Some left_type ->
      (match get_name left_type with
      | "|" ->
        let type_node = child_ranked 1 node in
        [ mk_child_res make_sym sym_vbar; mk_child_res print_type type_node ]
      | _ ->
        (* "type" is a supertype, therefore a hidden rule *)
        let right_type = child_ranked 2 node in
        [ mk_child print_type left_type
        ; mk_child_res make_sym sym_vbar
        ; mk_child_res print_type right_type
        ])
  in
  make_tree state node children

(* Function type *)

and print_function_type state node =
  let type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and return_type_field = child_with_field "return_type" node
  and sym_arrow = first_child_named "=>" node
  and print_return_type state node =
    match get_name node with
    | "asserts" -> print_asserts state node
    | "type_predicate" -> print_type_predicate state node
    | _ -> match_rest state node print_type
  in
  let children =
    [ mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_res make_sym sym_arrow
    ; mk_child_res print_return_type return_type_field
    ]
  in
  make_tree state node children

and print_type_predicate state node =
  let name_field = child_with_field "name" node
  and kwd_is = first_child_named "is" node
  and type_field = child_with_field "type" node in
  let print_name_field state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | "this" -> make_kwd state node
    | _ -> match_rest state node print_predefined_type
  in
  let children =
    [ mk_child_res print_name_field name_field
    ; mk_child_res make_kwd kwd_is
    ; mk_child_res print_type type_field
    ]
  in
  make_tree state node children

(* Readonly type *)

and print_readonly_type state node =
  let kwd_readonly = first_child_named "readonly" node
  and type_child = child_ranked 1 node in
  let children =
    [ mk_child_res make_kwd kwd_readonly; mk_child_res print_type type_child ]
  in
  make_tree state node children

(* Constructor type *)

and print_constructor_type state node =
  let kwd_abstract = first_child_named_opt "abstract" node
  and kwd_new = first_child_named "new" node
  and type_parameters_field = child_with_field_opt "type_parameters" node
  and parameters_field = child_with_field "parameters" node
  and sym_arrow = first_child_named "=>" node
  and type_field = child_with_field "type" node in
  let children =
    [ mk_child_opt make_kwd kwd_abstract
    ; mk_child_res make_kwd kwd_new
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_res make_sym sym_arrow
    ; mk_child_res print_type type_field
    ]
  in
  make_tree state node children

and print_formal_parameters state node = print_parens state node print_formal_parameter

and print_formal_parameter state node =
  match get_name node with
  | "required_parameter" -> print_required_parameter state node
  | "optional_parameter" -> print_optional_parameter state node
  | _ -> match_rest state node print_unexpected_node

and print_optional_parameter state node = print_required_parameter state node

and print_required_parameter state node =
  (* "_parameter_name" inlined: *)
  let decorators = children_named "decorator" node
  and accessibility_modifier = first_child_named_opt "accessibility_modifier" node
  and override_modifier = first_child_named_opt "override_modifier" node
  and kwd_readonly = first_child_named_opt "readonly" node
  and pattern_field = child_with_field "pattern" node
  (* *)
  and type_field = child_with_field_opt "type" node
  and print_pattern_field state node =
    match get_name node with
    | "this" -> make_kwd state node
    | _ -> print_pattern state node
  in
  let children =
    mk_children_list print_decorator decorators
    @ [ mk_child_opt print_accessibility_modifier accessibility_modifier
      ; mk_child_opt print_override_modifier override_modifier
      ; mk_child_opt make_kwd kwd_readonly
      ; mk_child_res print_pattern_field pattern_field
      ; mk_child_opt print_type_annotation type_field
      ]
    @ mk_child_initializer_opt node (* "_initializer" inlined *)
  in
  make_tree state node children

and mk_child_initializer sym_equal node =
  let value_field = child_with_field "value" node in
  let children =
    [ mk_child_res make_sym sym_equal; mk_child_res print_expression value_field ]
  in
  Some (fun state -> Tree.make_tree state "initializer" children)

and mk_child_initializer_opt node =
  match first_child_named_opt "=" node with
  | None -> []
  | Some sym_equal -> [ mk_child_initializer (Ok sym_equal) node ]

(* Decorator *)

and print_decorator state node =
  let child = named_child_ranked 0 node
  and print state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | "member_expression" -> print_decorator_member_expression state node
    | "call_expression" -> print_decorator_call_expression state node
    | "parenthesized_expression" -> print_decorator_parenthesized_expression state node
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print child

and print_decorator_member_expression state node =
  let object_field = child_with_field "object" node
  and selector = first_child_named "." node
  and property_field = child_with_field "property" node
  and print_object state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | _ -> match_rest state node print_decorator_member_expression
  in
  let children =
    [ mk_child_res print_object object_field
    ; mk_child_res make_sym selector
    ; mk_child_res print_identifier property_field
    ]
  in
  make_tree state node children

and print_decorator_call_expression state node =
  let function_field = child_with_field "function" node
  and type_arguments_field = child_with_field_opt "type_arguments" node
  and arguments_field = child_with_field "arguments" node
  and print_function state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | "member_expression" -> print_decorator_member_expression state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_function function_field
    ; mk_child_opt print_type_arguments type_arguments_field
    ; mk_child_res print_arguments arguments_field
    ]
  in
  make_tree state node children

and print_decorator_parenthesized_expression ?comments state node =
  let print state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | "member_expression" -> print_decorator_member_expression state node
    | _ -> match_rest state node print_call_expression
  in
  print_parens ?comments state node print

(* Accessibility modifier *)

and print_accessibility_modifier state node =
  let child = child_ranked 0 node
  and print state node =
    match get_name node with
    | "public" -> make_kwd state node
    | "private" -> make_kwd state node
    | "protected" -> make_kwd state node
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print child

(* Override modifier *)

and print_override_modifier state node =
  let child = child_ranked 0 node in
  make_unary_res state node make_kwd child

(* Infer type *)

and print_infer_type state node =
  let kwd_infer = first_child_named "infer" node
  and type_identifier_child = child_ranked 1 node (* name "type_identifier"? *)
  and kwd_extends = first_child_named_opt "extends" node
  and type_child = child_ranked_opt 3 node in
  let children =
    [ mk_child_res make_kwd kwd_infer
    ; mk_child_res print_identifier type_identifier_child
    ; mk_child_opt make_kwd kwd_extends
    ; mk_child_opt print_type type_child
    ]
  in
  make_tree state node children

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

(* Object pattern *)

and print_object_pattern state node = print_braces state node print_object_pattern_field

and print_object_pattern_field state node =
  match get_name node with
  | "pair_pattern" -> print_pair_pattern state node
  | "rest_pattern" -> print_rest_pattern state node
  | "object_assignment_pattern" -> print_object_assignment_pattern state node
  | "shorthand_property_identifier_pattern" ->
    print_shorthand_property_identifier_pattern state node
  | _ -> match_rest state node print_unexpected_node

(* Pair pattern *)

and print_pair_pattern state node =
  let key_field = child_with_field "key" node
  and sym_colon = first_child_named ":" node
  and value_field = child_with_field "value" node
  and print_value state node =
    match get_name node with
    | "assignment_pattern" -> print_assignment_pattern state node
    | _ -> match_rest state node print_pattern (* Hidden rule *)
  in
  let children =
    [ mk_child_res print_property_name key_field
    ; mk_child_res make_sym sym_colon
    ; mk_child_res print_value value_field
    ]
  in
  make_tree state node children

(* Assignment pattern *)

and print_assignment_pattern state node =
  let left_field = child_with_field "left" node
  and right_field = child_with_field "right" node
  and sym_equal = first_child_named "=" node in
  let children =
    [ mk_child_res print_pattern left_field
    ; mk_child_res make_sym sym_equal
    ; mk_child_res print_expression right_field
    ]
  in
  make_tree state node children

(* Property names *)

and print_property_name state node =
  match get_name node with
  | "property_identifier" -> print_identifier state node
  | "private_property_identifier" -> print_identifier state node
  | "string" -> print_string state node
  | "number" -> print_number state node
  | "computed_property_name" -> print_computed_property_name state node
  | _ -> match_rest state node print_unexpected_node

and print_computed_property_name state node = print_brackets state node print_expression
and print_shorthand_property_identifier_pattern state node = make_node state node

(* Object assignment pattern *)

and print_object_assignment_pattern state node =
  let left_field = child_with_field "left" node
  and sym_equal = first_child_named "=" node
  and right_field = child_with_field "right" node in
  let children =
    [ mk_child_res print_object_lhs_pattern left_field
    ; mk_child_res make_sym sym_equal
    ; mk_child_res print_expression right_field
    ]
  in
  make_tree state node children

and print_object_lhs_pattern state node = print_lhs_pattern state node

and print_lhs_pattern state node =
  match get_name node with
  | "shorthand_property_identifier_pattern" ->
    print_shorthand_property_identifier_pattern state node
  | _ -> match_rest state node print_destructuring_pattern

(* Rule "_destructuring_pattern" is inlined. *)

and print_destructuring_pattern state node =
  match get_name node with
  | "object_pattern" -> print_object_pattern state node
  | "array_pattern" -> print_array_pattern state node
  | _ -> match_rest state node print_unexpected_node

(* Array pattern *)

and print_array_pattern state node = print_brackets state node print_array_pattern_cell

and print_array_pattern_cell state node =
  match get_name node with
  | "assignment_pattern" -> print_assignment_pattern state node
  | _ -> match_rest state node print_pattern (* hidden rule *)

(* General patterns (hidden rule) *)

and print_pattern state node =
  match get_name node with
  | "rest_pattern" -> print_rest_pattern state node
  | _ -> print_lhs_expression state node
