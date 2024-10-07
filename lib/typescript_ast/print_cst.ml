(* Printing the tree-sitter CST for TypeScript *)

open Core
open Ts_wrap

(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Printing the ERROR and MISSING nodes *)

let print_error_node state node = Tree.make_node state (get_label node)
let print_missing_node state node = Tree.make_node state (get_label node)

(* Concluding a pattern matching with the remaining cases *)

let match_rest state node print_default =
  let name = get_name node in
  match name with
  (* Comments are ignored *)
  | "comment" -> ()
  (* Errors *)
  | "ERROR" -> print_error_node state node
  | "MISSING" -> print_missing_node state node
  (* Default case *)
  | _ -> print_default state node

(* Making trees and nodes *)

let make_tree state node children = Tree.make state (get_label node) children

let tree_of_list state node printer children =
  Tree.of_list state (get_label node) printer children

let make_node state node = Tree.make_node state @@ get_label node

let make_unary state root printer child =
  Tree.make_unary state (get_label root) printer child

let mk_child_opt = Tree.mk_child_opt
let mk_child = Tree.mk_child

(* Unexpected and TODO nodes *)

let print_unexpected_node state node = Tree.make_node state ("UNKNOWN: " ^ get_label node)
let print_todo_node state node = Tree.make_node state ("TODO: " ^ get_label node)

(* Wrappers for making trees and children, possibly invalid ones. *)

let mk_child_res print = function
  | Result.Ok child -> Tree.mk_child print child
  | Error name -> Tree.(mk_child make_node name)

(* Error/Invalid child *)

let internal_error_child parent_name child_name =
  let suffix = Printf.sprintf "Child %s is missing." child_name in
  let msg = Printf.sprintf "INTERNAL: [%s] %s" parent_name suffix in
  Tree.(mk_child make_node msg)

let make_unary_res state node print = function
  | Result.Ok child -> make_unary state node print child
  | Error child_name -> make_unary state node Tree.make_node child_name

(* Printing the CST *)

let rec print_program node =
  (* Empty state for building the AST *)
  let buffer = Buffer.create 1023 in
  let state = Tree.mk_state ~buffer ~regions:false ~layout:true ~offsets:true `Byte in
  (* Decoding the CST into an AST in [state] *)
  let children = collect_named_children node in
  let () = tree_of_list state node print_statement children in
  Buffer.contents @@ Tree.to_buffer state

(* Statements

   The JavasScript tree-sitter grammar have the non-terminals
   "statement" and "declaration" be supertypes, that is, hidden
   rules. *)

and print_statement state node =
  let name = get_name node in
  match name with
  | "export_statement" -> print_export_statement state node
  | "import_statement" -> print_import_statement state node
  | "debugger_statement" -> print_debugger_statement state node
  | "expression_statement" -> print_expression_statement state node
  | "statement_block" -> print_statement_block state node
  | "if_statement" -> print_if_statement state node
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
  | "function_declaration" -> print_function_declaration state node
  | "generator_function_declaration" -> print_generator_function_declaration state node
  | "class_declaration" -> print_class_declaration state node
  | "lexical_declaration" -> print_lexical_declaration state node
  | "variable_declaration" -> print_variable_declaration state node
  | "function_signature" -> print_function_signature state node
  | "abstract_class_declaration" -> print_abstract_class_declaration state node
  | "module" -> print_module state node
  | "internal_module" -> print_internal_module state node
  | "type_alias_declaration" -> print_type_alias_declaration state node
  | "enum_declaration" -> print_enum_declaration state node
  | "interface_declaration" -> print_interface_declaration state node
  | "import_alias" -> print_import_alias state node
  | "ambient_declaration" -> print_ambient_declaration state node
  | _ -> match_rest state node print_unexpected_node

(* Export statement *)

and print_export_statement state node =
  let name = get_name node
  and children = collect_children node in
  let decorators = filter_by_name "decorator" children
  and export_node = filter_first_by_name_opt "export" children in
  let decorators = Tree.mk_children_list print_decorator decorators in
  let children =
    match export_node with
    | None -> [ internal_error_child name "export" ]
    | Some export_node ->
      let after_export = TS_fun.ts_node_next_sibling export_node in
      if TS_fun.ts_node_is_null after_export
      then [ internal_error_child name "after \"export\"" ]
      else (
        match get_name after_export with
        | "*" ->
          mk_child make_node after_export
          ::
          (let source_field = ts_node_child_by_field_name_opt node "source" in
           [ mk_child_opt print_from_clause source_field ])
        | "namespace_export" ->
          Tree.mk_child print_namespace_export after_export
          ::
          (let source_field = ts_node_child_by_field_name_opt node "source" in
           [ mk_child_opt print_from_clause source_field ])
        | "export_clause" ->
          mk_child print_export_clause after_export
          ::
          (let source_field = ts_node_child_by_field_name_opt node "source" in
           [ mk_child_opt print_from_clause source_field ])
        | "default" ->
          let declaration_field = ts_node_child_by_field_name_opt node "declaration" in
          decorators
          @ [ mk_child make_node after_export ]
          @
          (match declaration_field with
          | Some declaration_field -> [ mk_child print_declaration declaration_field ]
          | None ->
            let value_field = ts_node_child_by_field_name_res node "value" in
            [ mk_child_res print_expression value_field ])
        | "type" ->
          let export_clause = TS_fun.ts_node_next_sibling after_export in
          if TS_fun.ts_node_is_null export_clause
          then [ internal_error_child name "export_clause" ]
          else (
            let source_field = ts_node_child_by_field_name_opt node "source" in
            [ mk_child make_node after_export
            ; mk_child print_export_clause export_clause
            ; mk_child_opt print_from_clause source_field
            ])
        | "=" ->
          let expression = TS_fun.ts_node_next_sibling after_export in
          if TS_fun.ts_node_is_null expression
          then [ internal_error_child name "expression" ]
          else [ mk_child make_node after_export; mk_child print_expression expression ]
        | "as" ->
          let identifier = filter_first_by_name_opt "identifier" children in
          (match identifier with
          | None -> [ internal_error_child name "identifier" ]
          | Some identifier ->
            [ mk_child make_node after_export; mk_child print_identifier identifier ])
        | _ -> decorators @ [ mk_child print_declaration after_export ])
  in
  make_tree state node children

and print_namespace_export state node =
  let module_export_name = ts_node_named_child_res node 0 in
  make_unary_res state node print_module_export_name module_export_name

(* The rule "_from_clause" is hidden *)

and print_from_clause state node = Tree.make_unary state "from_clause" print_string node

and print_export_clause state node =
  let children = collect_named_children node in
  tree_of_list state node print_export_specifier children

and print_module_export_name state node =
  let name = get_name node in
  match name with
  | "identifier" -> print_identifier state node
  | "string" -> print_string state node
  | _ -> match_rest state node print_unexpected_node

and print_export_specifier state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and alias_field = ts_node_child_by_field_name_opt node "alias" in
  let children =
    [ mk_child_res print_module_export_name name_field
    ; mk_child_opt print_module_export_name alias_field
    ]
  in
  make_tree state node children

(* Import statement *)

and print_import_statement state node =
  let children = collect_children node in
  let kind_node =
    match has_node_named_opt "type" children with
    | None -> has_node_named_opt "typeof" children
    | some -> some
  and import_attribute = filter_first_by_name_opt "import_attribute" children in
  let middle_children =
    match filter_first_by_name_opt "import_clause" children with
    | Some import_clause ->
      let source_field = ts_node_child_by_field_name_res node "source" in
      [ mk_child print_import_clause import_clause
      ; mk_child_res print_from_clause source_field
      ]
    | None ->
      (match filter_first_by_name_opt "import_require_clause" children with
      | Some clause -> [ mk_child print_import_require_clause clause ]
      | None ->
        let source_field = ts_node_child_by_field_name_res node "source" in
        [ mk_child_res print_string source_field ])
  in
  let children =
    (mk_child_opt make_node kind_node :: middle_children)
    @ [ mk_child_opt print_import_attribute import_attribute ]
  in
  make_tree state node children

and print_import_clause state node =
  let name = get_name node
  and fst_child = ts_node_child_opt node 0
  and print_rest state node =
    let name = get_name node in
    match name with
    | "namespace_import" -> print_namespace_import state node
    | "named_imports" -> print_named_imports state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    match fst_child with
    | None -> [ internal_error_child name "\"first child\"" ]
    | Some fst_child ->
      (match get_name fst_child with
      | "namespace_import" -> [ mk_child print_namespace_import fst_child ]
      | "named_imports" -> [ mk_child print_named_imports fst_child ]
      | "identifier" ->
        mk_child print_identifier fst_child
        ::
        (match ts_node_next_sibling_opt fst_child with
        | None -> []
        | Some comma ->
          (match ts_node_next_sibling_opt comma with
          | None -> [ internal_error_child name "namespace_import/named_imports" ]
          | Some next -> [ mk_child print_rest next ]))
      | _ -> [ mk_child print_unexpected_node fst_child ])
  in
  make_tree state node children

and print_namespace_import state node =
  let identifier = ts_node_named_child_res node 0 in
  make_unary_res state node print_identifier identifier

and print_named_imports state node =
  let children = collect_named_children node in
  tree_of_list state node print_import_specifier children

and print_import_specifier state node =
  let children = collect_children node in
  let kind_node =
    match has_node_named_opt "type" children with
    | None -> has_node_named_opt "typeof" children
    | some -> some
  and name_field = ts_node_child_by_field_name_res node "name"
  and alias_field = ts_node_child_by_field_name_opt node "alias" in
  let children =
    mk_child_opt make_node kind_node
    ::
    (match alias_field with
    | None -> [ mk_child_res print_identifier name_field ]
    | Some alias_field ->
      [ mk_child_res print_module_export_name name_field
      ; mk_child print_identifier alias_field
      ])
  in
  make_tree state node children

and print_import_require_clause state node =
  let identifier = ts_node_named_child_res node 0
  and source_field = ts_node_child_by_field_name_res node "source" in
  let children =
    [ mk_child_res print_identifier identifier; mk_child_res print_string source_field ]
  in
  make_tree state node children

and print_import_attribute state node =
  let kind_node = ts_node_child_res node 0
  and object_node = ts_node_child_res node 1
  and print_kind state node =
    let name = get_name node in
    match name with
    | "with" -> make_node state node
    | "assert" -> make_node state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_kind kind_node; mk_child_res print_object object_node ]
  in
  make_tree state node children

(* Debugger statement *)

and print_debugger_statement state node = make_node state node

(* Expression statements

   {@js[
    expression_statement: $ => seq($._expressions, $._semicolon),
    _expressions: $ => choice($.expression, $.sequence_expression),
    sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [print_expression]. *)

and print_expression_statement state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_expressions child

and print_expressions state node =
  let name = get_name node in
  match name with
  | "sequence_expression" -> print_sequence_expression state node
  | _ -> print_expression state node

(* Statement blocks *)

and print_statement_block state node =
  let children = collect_named_children node in
  tree_of_list state node print_statement children

(* If statement *)

and print_if_statement state node =
  let condition_field = ts_node_child_by_field_name_res node "condition"
  and consequence_field = ts_node_child_by_field_name_res node "consequence"
  and alternative_field = ts_node_child_by_field_name_opt node "alternative" in
  let children =
    [ mk_child_res print_parenthesized_expression condition_field
    ; mk_child_res print_statement consequence_field
    ; mk_child_opt print_else_clause alternative_field
    ]
  in
  make_tree state node children

and print_else_clause state node =
  let child = ts_node_child_res node 1 in
  make_unary_res state node print_statement child

(* Switch statement *)

and print_switch_statement state node =
  let value_field = ts_node_child_by_field_name_res node "value"
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_res print_parenthesized_expression value_field
    ; mk_child_res print_switch_body body_field
    ]
  in
  make_tree state node children

and print_switch_body state node =
  let children = collect_named_children node
  and print state node =
    let name = get_name node in
    match name with
    | "switch_case" -> print_switch_case state node
    | _ -> match_rest state node print_switch_default
  in
  tree_of_list state node print children

and print_switch_case state node =
  let children = collect_children node in
  let rec skip_until_colon = function
    | [] -> []
    | node :: nodes ->
      (match get_name node with
      | ":" -> nodes
      | _ -> skip_until_colon nodes)
  in
  let stmt_children = skip_until_colon children
  and value_field = ts_node_child_by_field_name_res node "value"
  and print_value state node =
    let name = get_name node in
    match name with
    | "sequence_expression" -> print_sequence_expression state node
    | _ -> print_expression state node
  in
  let children =
    mk_child_res print_value value_field
    :: Tree.mk_children_list print_statement stmt_children
  in
  make_tree state node children

and print_switch_default state node =
  let children = collect_named_children node in
  tree_of_list state node print_statement children

(* For statement *)

and print_for_statement state node =
  let initializer_field = ts_node_child_by_field_name_res node "initializer"
  and condition_field = ts_node_child_by_field_name_res node "condition"
  and increment_field = ts_node_child_by_field_name_opt node "increment"
  and body_field = ts_node_child_by_field_name_res node "body"
  and print_initializer state node =
    let name = get_name node in
    match name with
    | "lexical_declaration" -> print_lexical_declaration state node
    | "variable_declaration" -> print_variable_declaration state node
    | "expression_statement" -> print_expression_statement state node
    | "empty_statement" -> print_empty_statement state node
    | _ -> match_rest state node print_unexpected_node
  and print_condition state node =
    let name = get_name node in
    match name with
    | "expression_statement" -> print_expression_statement state node
    | "empty_statement" -> print_empty_statement state node
    | _ -> match_rest state node print_unexpected_node
  and print_increment state node =
    let name = get_name node in
    match name with
    | "sequence_expression" -> print_sequence_expression state node
    | _ -> print_expression state node
  in
  let children =
    [ mk_child_res print_initializer initializer_field
    ; mk_child_res print_condition condition_field
    ; mk_child_opt print_increment increment_field
    ; mk_child_res print_statement body_field
    ]
  in
  make_tree state node children

(* For-in statement *)

and print_for_in_statement state node =
  let children = collect_children node in
  let await = has_node_named_opt "await" children
  and left_field = ts_node_child_by_field_name_res node "left"
  and body_field = ts_node_child_by_field_name_res node "body"
  and operator_field = ts_node_child_by_field_name_res node "operator"
  and right_field = ts_node_child_by_field_name_res node "right"
  and kind_field = ts_node_child_by_field_name_opt node "kind" in
  let print_operator state node =
    let name = get_name node in
    match name with
    | "in" -> make_node state node
    | "of" -> make_node state node
    | _ -> match_rest state node print_unexpected_node
  in
  let header_children =
    match kind_field with
    | None ->
      let print_left state node =
        let name = get_name node in
        match name with
        | "parenthesized_expression" -> print_parenthesized_expression state node
        | _ -> match_rest state node print_lhs_expression
      in
      [ mk_child_res print_left left_field ]
    | Some kind_field ->
      let print_left state node =
        let name = get_name node in
        match name with
        | "identifier" -> print_identifier state node
        | _ -> match_rest state node print_destructuring_pattern
      in
      (match get_name kind_field with
      | "var" ->
        let value_field = ts_node_child_by_field_name_opt node "value" in
        [ mk_child make_node kind_field
        ; mk_child_res print_left left_field
        ; mk_child_opt print_expression value_field
        ]
      | "let" | "const" ->
        [ mk_child make_node kind_field; mk_child_res print_left left_field ]
      | _ -> [ mk_child print_unexpected_node kind_field ])
  in
  let children =
    (mk_child_opt make_node await :: header_children)
    @ [ mk_child_res print_operator operator_field
      ; mk_child_res print_expressions right_field
      ; mk_child_res print_statement body_field
      ]
  in
  make_tree state node children

(* While statement *)

and print_while_statement state node =
  let condition_field = ts_node_child_by_field_name_res node "condition"
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_res print_parenthesized_expression condition_field
    ; mk_child_res print_statement body_field
    ]
  in
  make_tree state node children

(* Do statement *)

and print_do_statement state node =
  let body_field = ts_node_child_by_field_name_res node "body"
  and condition_field = ts_node_child_by_field_name_res node "condition" in
  let children =
    [ mk_child_res print_statement body_field
    ; mk_child_res print_parenthesized_expression condition_field
    ]
  in
  make_tree state node children

(* Try statement *)

and print_try_statement state node =
  let body_field = ts_node_child_by_field_name_res node "body"
  and handler_field = ts_node_child_by_field_name_opt node "handler"
  and finalizer_field = ts_node_child_by_field_name_opt node "finalizer" in
  let children =
    [ mk_child_res print_statement_block body_field
    ; mk_child_opt print_catch_clause handler_field
    ; mk_child_opt print_finally_clause finalizer_field
    ]
  in
  make_tree state node children

and print_catch_clause state node =
  let body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    match ts_node_child_by_field_name_opt node "parameter" with
    | Some parameter_field ->
      let print_parameter state node =
        let name = get_name node in
        match name with
        | "identifier" -> print_identifier state node
        | _ -> match_rest state node print_destructuring_pattern
      in
      let type_field = ts_node_child_by_field_name_opt node "type" in
      [ mk_child print_parameter parameter_field
      ; mk_child_opt print_type_annotation type_field
      ; mk_child_res print_statement_block body_field
      ]
    | None -> [ mk_child_res print_statement_block body_field ]
  in
  make_tree state node children

and print_finally_clause state node =
  let body_field = ts_node_child_by_field_name_res node "body" in
  make_unary_res state node print_statement_block body_field

(* With statement *)

and print_with_statement state node =
  let object_field = ts_node_child_by_field_name_res node "object"
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_res print_parenthesized_expression object_field
    ; mk_child_res print_statement body_field
    ]
  in
  make_tree state node children

(* Break statement *)

and print_break_statement state node =
  let label_field = ts_node_child_by_field_name_opt node "label" in
  let children = [ mk_child_opt print_identifier label_field ] in
  make_tree state node children

(* Continue statement *)

and print_continue_statement state node =
  let label_field = ts_node_child_by_field_name_opt node "label" in
  let children = [ mk_child_opt print_identifier label_field ] in
  make_tree state node children

(* Return statement *)

and print_return_statement state node =
  let child = ts_node_named_child_opt node 0
  and print state node =
    let name = get_name node in
    match name with
    | "sequence_expression" -> print_sequence_expression state node
    | _ -> print_expression state node
  in
  let children = [ mk_child_opt print child ] in
  make_tree state node children

(* Throw statement *)

and print_throw_statement state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_expressions child

(* Empty statement *)

and print_empty_statement state node = make_node state node

(* Labeled statement *)

and print_labeled_statement state node =
  let label_field = ts_node_child_by_field_name_res node "label"
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_res print_identifier label_field; mk_child_res print_statement body_field ]
  in
  make_tree state node children

(* DECLARATION *)

and print_declaration state node =
  let name = get_name node in
  match name with
  | "function_declaration" -> print_function_declaration state node
  | "generator_function_declaration" -> print_generator_function_declaration state node
  | "class_declaration" -> print_class_declaration state node
  | "lexical_declaration" -> print_lexical_declaration state node
  | "variable_declaration" -> print_variable_declaration state node
  | "function_signature" -> print_function_signature state node
  | "abstract_class_declaration" -> print_abstract_class_declaration state node
  | "module" -> print_module state node
  | "internal_module" -> print_internal_module state node
  | "type_alias_declaration" -> print_type_alias_declaration state node
  | "enum_declaration" -> print_enum_declaration state node
  | "interface_declaration" -> print_interface_declaration state node
  | "import_alias" -> print_import_alias state node
  | "ambient_declaration" -> print_ambient_declaration state node
  | _ -> match_rest state node print_unexpected_node

(* Function declaration (see [print_function_signature]) *)

and print_function_declaration state node =
  let children = collect_children node in
  let async = has_node_named_opt "async" children
  and name_field = ts_node_child_by_field_name_res node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_opt make_node async
    ; mk_child_res print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ; mk_child_res print_statement_block body_field
    ]
  in
  make_tree state node children

(* Generator function declaration (see function declaration) *)

and print_generator_function_declaration state node =
  let children = collect_children node in
  let async = has_node_named_opt "async" children
  and name_field = ts_node_child_by_field_name_res node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_opt make_node async
    ; mk_child_res print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ; mk_child_res print_statement_block body_field
    ]
  in
  make_tree state node children

(* Class declaration (see [print_class] *)

and print_class_declaration state node =
  let children = collect_children node in
  let decorators = filter_by_name "decorator" children
  and name_field = ts_node_child_by_field_name_res node "name"
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and heritage_child = filter_first_by_name_opt "class_heritage" children
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    Tree.mk_children_list print_decorator decorators
    @ [ mk_child_res print_type_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_opt print_class_heritage heritage_child
      ; mk_child_res print_class_body body_field
      ]
  in
  make_tree state node children

(* Lexical declaration (see [print_variable_declaration]) *)

and print_lexical_declaration state node =
  let children = collect_named_children node
  and kind_field = ts_node_child_by_field_name_res node "kind" in
  let var_decls = filter_by_name "variable_declarator" children
  and print_set_or_const state node =
    let name = get_name node in
    match name with
    | "let" -> make_node state node
    | "const" -> make_node state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    mk_child_res print_set_or_const kind_field
    :: Tree.mk_children_list print_variable_declarator var_decls
  in
  make_tree state node children

and print_variable_declarator state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and value_field = ts_node_child_by_field_name_opt node "value"
  and print_name_field state node =
    let name = get_name node in
    match name with
    | "identifier" -> print_identifier state node
    | _ -> match_rest state node print_destructuring_pattern
  in
  let children =
    [ mk_child_res print_name_field name_field
    ; mk_child_opt print_expression value_field
    ]
  in
  make_tree state node children

(* Variable declaration (see [print_lexical_declaration]) *)

and print_variable_declaration state node =
  let children = collect_named_children node in
  let var_decls = filter_by_name "variable_declarator" children in
  let children = Tree.mk_children_list print_variable_declarator var_decls in
  make_tree state node children

(* Function signature (See [print_function_declaration]) *)

and print_function_signature state node =
  let children = collect_children node in
  let async = has_node_named_opt "async" children
  and name_field = ts_node_child_by_field_name_res node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type" in
  (* "statement_block" *)
  let children =
    [ mk_child_opt make_node async
    ; mk_child_res print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ]
  in
  make_tree state node children

(* Abstract class declaration ( see [print_class_declaration]) *)

and print_abstract_class_declaration state node = print_class_declaration state node

(* Module *)

and print_module state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and body_field = ts_node_child_by_field_name_opt node "body"
  and print_name state node =
    let name = get_name node in
    match name with
    | "string" -> print_string state node
    | "identifier" -> print_identifier state node
    | "nested_identifier" -> print_nested_identifier state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_name name_field; mk_child_opt print_statement_block body_field ]
  in
  make_tree state node children

(* Internal module (a.k.a. namespaces) *)

and print_internal_module state node = print_module state node

(* Type alias declaration *)

and print_type_alias_declaration state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and value_field = ts_node_child_by_field_name_res node "value" in
  let children =
    [ mk_child_res print_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_type value_field
    ]
  in
  make_tree state node children

(* Type parameters *)

and print_type_parameters state node =
  let children = collect_named_children node in
  tree_of_list state node print_type_parameter children

and print_type_parameter state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and constraint_field = ts_node_child_by_field_name_opt node "constraint"
  and value_field = ts_node_child_by_field_name_opt node "value" in
  let children =
    [ mk_child_res print_identifier name_field
    ; mk_child_opt print_constraint constraint_field
    ; mk_child_opt print_default_type value_field
    ]
  in
  make_tree state node children

and print_constraint state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

and print_default_type state node =
  let children = collect_named_children node in
  tree_of_list state node print_type children

(* Enum declaration *)

and print_enum_declaration state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_res print_identifier name_field; mk_child_res print_enum_body body_field ]
  in
  make_tree state node children

and print_enum_body state node =
  let children = collect_named_children node
  and print state node =
    let name = get_name node in
    match name with
    | "enum_assignment" -> print_enum_assignment state node
    | _ -> match_rest state node print_property_name
  in
  tree_of_list state node print children

and print_enum_assignment state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and value_field = ts_node_child_by_field_name_opt node "value" in
  let children =
    [ mk_child_res print_property_name name_field
    ; mk_child_opt print_expression value_field
    ]
  in
  make_tree state node children

(* Interface declaration *)

and print_interface_declaration state node =
  let children = collect_children node
  and name_field = ts_node_child_by_field_name_res node "name"
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and body_field = ts_node_child_by_field_name_res node "body" in
  let extends_type_clause = filter_first_by_name_opt "extends_type_clause" children in
  let children =
    [ mk_child_res print_type_identifier name_field
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_opt print_extends_type_clause extends_type_clause
    ; mk_child_res print_interface_body body_field
    ]
  in
  make_tree state node children

and print_interface_body state node = print_object_type state node

and print_extends_type_clause state node =
  let children = collect_named_children node
  and print state node =
    let name = get_name node in
    match name with
    | "type_identifier" -> print_type_identifier state node
    | "nested_type_identifier" -> print_nested_type_identifier state node
    | "generic_type" -> print_generic_type state node
    | _ -> match_rest state node print_unexpected_node
  in
  tree_of_list state node print children

(* Import alias *)

and print_import_alias state node =
  let lhs = ts_node_child_res node 1
  and rhs = ts_node_child_res node 3
  and print_rhs state node =
    let name = get_name node in
    match name with
    | "identifier" -> print_identifier state node
    | "nested_identifier" -> print_nested_identifier state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children = [ mk_child_res print_identifier lhs; mk_child_res print_rhs rhs ] in
  make_tree state node children

(* Ambient declaration *)

and print_ambient_declaration state node =
  let fst_child = ts_node_named_child_res node 0 in
  let child_name = get_name_res fst_child in
  let children =
    match child_name with
    | "statement_block" -> [ mk_child_res print_statement_block fst_child ]
    | "property_identifier" ->
      let type_child = ts_node_child_res node 5 in
      [ mk_child_res print_identifier fst_child; mk_child_res print_type type_child ]
    | _ -> [ mk_child_res print_declaration fst_child ]
  in
  make_tree state node children

(* EXPRESSION

   The JavasScript tree-sitter grammar have the non-terminals
   "expression" and "primary_expression" be supertypes, that is,
   hidden rules. Therefore we have to match all the RHS of those
   non-terminals in [print_expression]. *)

and print_expression state node =
  let name = get_name node in
  match name with
  (* "primary_expression" inlined: *)
  | "subscript_expression" -> print_subscript_expression state node
  | "member_expression" -> print_member_expression state node
  | "parenthesized_expression" -> print_parenthesized_expression state node
  | "identifier" -> print_identifier state node
  | "undefined" -> print_undefined state node
  | "this" -> print_this state node
  | "super" -> print_super state node
  | "number" -> print_number state node
  | "string" -> print_string state node
  | "template_string" -> print_template_string state node
  | "regex" -> print_regex state node
  | "true" -> print_true state node
  | "false" -> print_false state node
  | "null" -> print_null state node
  | "object" -> print_object state node
  | "array" -> print_array state node
  | "function_expression" -> print_function_expression state node
  | "arrow_function" -> print_arrow_function state node
  | "generator_function" -> print_generator_function state node
  | "class" -> print_class state node
  | "meta_property" -> print_meta_property state node
  | "call_expression" -> print_call_expression state node
  | "non_null_expression" -> print_non_null_expression state node
  (* Rest of "expression": *)
  | "glimmer_template" -> print_glimmer_template state node
  | "assignment_expression" -> print_assignment_expression state node
  | "augmented_assignment_expression" -> print_augmented_assignment_expression state node
  | "await_expression" -> print_await_expression state node
  | "unary_expression" -> print_unary_expression state node
  | "binary_expression" -> print_binary_expression state node
  | "ternary_expression" -> print_ternary_expression state node
  | "update_expression" -> print_update_expression state node
  | "new_expression" -> print_new_expression state node
  | "yield_expression" -> print_yield_expression state node
  | "as_expression" -> print_as_expression state node
  | "satisfies_expression" -> print_satisfies_expression state node
  | "instantiation_expression" -> print_instantiation_expression state node
  | "internal_module" -> print_internal_module state node
  | "type_assertion" -> print_type_assertion state node
  | _ -> match_rest state node print_unexpected_node

(* Glimmer template (not supported) *)

and print_glimmer_template state node = make_node state node

(* Assignment expression *)

and print_assignment_expression state node =
  let using = ts_node_child_opt node 0
  and left_field = ts_node_child_by_field_name_res node "left"
  and right_field = ts_node_child_by_field_name_res node "right"
  and print_left state node =
    let name = get_name node in
    match name with
    | "parenthesized_expression" -> print_parenthesized_expression state node
    | _ -> match_rest state node print_lhs_expression
  in
  let children =
    [ mk_child_opt make_node using
    ; mk_child_res print_left left_field
    ; mk_child_res print_expression right_field
    ]
  in
  make_tree state node children

(* Augmented assignment expression *)

and print_augmented_assignment_expression state node =
  let left_field = ts_node_child_by_field_name_res node "left"
  and right_field = ts_node_child_by_field_name_res node "right"
  and operator = ts_node_child_by_field_name_res node "operator"
  and print_left state node =
    let name = get_name node in
    (* "_augmented_assignment_lhs" is inlined here (hidden rule): *)
    match name with
    | "member_expression" -> print_member_expression state node
    | "subscript_expression" -> print_subscript_expression state node
    | "identifier" -> print_identifier state node
    | "parenthesized_expression" -> print_parenthesized_expression state node
    | _ -> match_rest state node print_unexpected_node
  and print_assignment state node =
    let name = get_name node in
    match name with
    | "+=" -> make_node state node
    | "-=" -> make_node state node
    | "*=" -> make_node state node
    | "/=" -> make_node state node
    | "%=" -> make_node state node
    | "^=" -> make_node state node
    | "&=" -> make_node state node
    | "|=" -> make_node state node
    | ">>=" -> make_node state node
    | ">>>=" -> make_node state node
    | "<<=" -> make_node state node
    | "**=" -> make_node state node
    | "&&=" -> make_node state node
    | "||=" -> make_node state node
    | "??=" -> make_node state node
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
  let expression = ts_node_child_res node 1 in
  make_unary_res state node print_expression expression

(* Binary expression *)

and print_binary_expression state node =
  let left_field = ts_node_child_by_field_name_res node "left"
  and right_field = ts_node_child_by_field_name_res node "right"
  and operator = ts_node_child_by_field_name_res node "operator"
  and print_left state node =
    let name = get_name node in
    match name with
    | "private_property_identifier" -> print_identifier state node
    | _ -> match_rest state node print_expression
  and print_bin_operator state node =
    let name = get_name node in
    match name with
    | "&&" -> make_node state node
    | "||" -> make_node state node
    | ">>" -> make_node state node
    | ">>>" -> make_node state node
    | "<<" -> make_node state node
    | "&" -> make_node state node
    | "^" -> make_node state node
    | "|" -> make_node state node
    | "+" -> make_node state node
    | "-" -> make_node state node
    | "*" -> make_node state node
    | "/" -> make_node state node
    | "%" -> make_node state node
    | "**" -> make_node state node
    | "<" -> make_node state node
    | "<=" -> make_node state node
    | "==" -> make_node state node
    | "===" -> make_node state node
    | "!=" -> make_node state node
    | "!==" -> make_node state node
    | ">=" -> make_node state node
    | ">" -> make_node state node
    | "??" -> make_node state node
    | "instanceof" -> make_node state node
    | "in" -> make_node state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_bin_operator operator
    ; mk_child_res print_left left_field
    ; mk_child_res print_expression right_field
    ]
  in
  make_tree state node children

(* Ternary expression *)

and print_ternary_expression state node =
  let condition_field = ts_node_child_by_field_name_res node "condition"
  and consequence_field = ts_node_child_by_field_name_res node "consequence"
  and alternative_field = ts_node_child_by_field_name_res node "alternative" in
  let children =
    [ mk_child_res print_expression condition_field
    ; mk_child_res print_expression consequence_field
    ; mk_child_res print_expression alternative_field
    ]
  in
  make_tree state node children

(* Update expression *)

and print_update_expression state node =
  let argument_field = ts_node_child_by_field_name_res node "argument"
  and first_child = ts_node_child_res node 0 in
  let children =
    match get_name_res first_child with
    | "++" ->
      [ mk_child_res make_node first_child; mk_child_res print_expression argument_field ]
    | "--" ->
      [ mk_child_res make_node first_child; mk_child_res print_expression argument_field ]
    | _ ->
      let snd_child = ts_node_child_res node 1 in
      (match get_name_res snd_child with
      | "++" ->
        [ mk_child_res print_expression argument_field; mk_child_res make_node snd_child ]
      | "--" ->
        [ mk_child_res print_expression argument_field; mk_child_res make_node snd_child ]
      | _ -> [] (* Should not happen. *))
  in
  make_tree state node children

(* New expression

   Note that the constructor field is a primary expression, but
   "primary_expression" is a supertype, that is, a hidden rule. We
   assume it is a "expression", since primary expressions are a subset
   of them. *)

and print_new_expression state node =
  let constructor_field = ts_node_child_by_field_name_res node "constructor"
  and type_arguments_field = ts_node_child_by_field_name_opt node "type_arguments"
  and arguments_field = ts_node_child_by_field_name_opt node "arguments" in
  let children =
    [ mk_child_res print_expression constructor_field
    ; mk_child_opt print_type_arguments type_arguments_field
    ; mk_child_opt print_arguments arguments_field
    ]
  in
  make_tree state node children

(* Yield expression *)

and print_yield_expression state node =
  match ts_node_child_opt node 1 with
  | None -> make_node state node
  | Some child ->
    let child =
      match get_name child with
      | "*" -> ts_node_child_res node 2
      | _ -> Result.Ok child
    in
    make_unary_res state node print_expression child

(* As-expression *)

and print_as_expression state node =
  let expression = ts_node_child_res node 0
  and as_what = ts_node_child_res node 2
  and print_as state node =
    let name = get_name node in
    match name with
    | "const" -> make_node state node
    | _ -> match_rest state node print_type
  in
  let children =
    [ mk_child_res print_expression expression; mk_child_res print_as as_what ]
  in
  make_tree state node children

(* Statisfies-expression *)

and print_satisfies_expression state node =
  let expression = ts_node_named_child_res node 0
  and type_child = ts_node_named_child_res node 1 in
  let children =
    [ mk_child_res print_expression expression; mk_child_res print_type type_child ]
  in
  make_tree state node children

(* Instantiation expression *)

and print_instantiation_expression state node =
  let expression = ts_node_named_child_res node 0
  and type_arguments_field = ts_node_child_by_field_name_res node "type_arguments" in
  let children =
    [ mk_child_res print_expression expression
    ; mk_child_res print_type_arguments type_arguments_field
    ]
  in
  make_tree state node children

(* Type assertion *)

and print_type_assertion state node =
  let type_arguments = ts_node_named_child_res node 0
  and expression = ts_node_named_child_res node 1 in
  let children =
    [ mk_child_res print_type_arguments type_arguments
    ; mk_child_res print_expression expression
    ]
  in
  make_tree state node children

(* Subscript expression (see [print_member_expression]) *)

and print_subscript_expression state node =
  let object_field = ts_node_child_by_field_name_res node "object"
  and optional_chain_field = ts_node_child_by_field_name_opt node "optional_chain"
  and index_field = ts_node_child_by_field_name_res node "index"
  and print_chain state node =
    let name = get_name node in
    match name with
    | "optional_chain" -> make_node state node
    | _ -> match_rest state node print_unexpected_node
  and print_index state node =
    let name = get_name node in
    match name with
    | "sequence_expression" -> print_sequence_expression state node
    | _ -> print_expression state node
  in
  let children =
    [ mk_child_res print_expression object_field
    ; mk_child_opt print_chain optional_chain_field
    ; mk_child_res print_index index_field
    ]
  in
  make_tree state node children

(* Member expression *)

and print_member_expression state node =
  let object_field = ts_node_child_by_field_name_res node "object"
  and optional_chain_field = ts_node_child_by_field_name_opt node "optional_chain"
  and property_field = ts_node_child_by_field_name_res node "property"
  and print_object state node =
    let name = get_name node in
    match name with
    | "import" -> print_import state node
    | _ -> match_rest state node print_expression
  and print_selector state = function
    | None -> () (* "." *)
    | Some node ->
      (* "?." *)
      make_node state node
  in
  let children =
    [ mk_child_res print_object object_field
    ; mk_child print_selector optional_chain_field
    ; mk_child_res print_property_field property_field
    ]
  in
  make_tree state node children

(* Parenthesised expression *)

and print_parenthesized_expression state node =
  let child = ts_node_named_child_res node 0
  and print state node =
    let name = get_name node in
    match name with
    | "sequence_expression" -> print_sequence_expression state node
    | _ -> print_expression state node
  in
  make_unary_res state node print child

and print_import state node = make_node state node
and print_identifier state node = make_node state node
and print_undefined state node = make_node state node
and print_this state node = make_node state node
and print_super state node = make_node state node
and print_number state node = make_node state node
and print_string state node = make_node state node
and print_regex state node = make_node state node
and print_true state node = make_node state node
and print_false state node = make_node state node
and print_null state node = make_node state node

(* Template strings *)

and print_template_string state node =
  let children = collect_named_children node
  and print state node =
    let name = get_name node in
    match name with
    | "string_fragment" -> make_node state node
    | "escape_sequence" -> make_node state node
    | "template_substitution" -> make_node state node
    | _ -> match_rest state node print_unexpected_node
  in
  tree_of_list state node print children

(* Object *)

and print_object state node =
  let children = collect_named_children node
  and print state node =
    let name = get_name node in
    match name with
    | "pair" -> print_pair state node
    | "spread_element" -> print_spread_element state node
    | "method_definition" -> print_method_definition state node
    | "shorthand_property_identifier" ->
      print_shorthand_property_identifier_pattern state node
    | _ -> match_rest state node print_unexpected_node
  in
  tree_of_list state node print children

(* Pairs *)

and print_pair state node =
  let key_field = ts_node_child_by_field_name_res node "key"
  and value_field = ts_node_child_by_field_name_res node "value" in
  let children =
    [ mk_child_res print_property_name key_field
    ; mk_child_res print_expression value_field
    ]
  in
  make_tree state node children

(* Array (expression) *)

and print_array state node =
  let children = collect_named_children node in
  tree_of_list state node print_array_cell children

and print_array_cell state node =
  let name = get_name node in
  match name with
  | "spread_element" -> print_spread_element state node
  | _ -> match_rest state node print_expression

and print_spread_element state node =
  let expression = ts_node_named_child_res node 0 in
  make_unary_res state node print_expression expression

(* Function (expression) *)

and print_function_expression state node =
  let children = collect_children node in
  let async = has_node_named_opt "async" children
  and name_field = ts_node_child_by_field_name_opt node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_opt make_node async
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
  let async = ts_node_child_opt node 0
  and parameter_field = ts_node_child_by_field_name_opt node "parameter"
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    match parameter_field with
    | Some parameter_field ->
      [ mk_child_opt make_node async
      ; mk_child print_identifier parameter_field
      ; mk_child_res print_arrow_function_body body_field
      ]
    | None ->
      (* "_call_signature" inlined: *)
      let type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
      and parameters_field = ts_node_child_by_field_name_res node "parameters"
      and return_type_field = ts_node_child_by_field_name_opt node "return_type" in
      [ mk_child_opt make_node async
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ; mk_child_res print_arrow_function_body body_field
      ]
  in
  make_tree state node children

and print_arrow_function_body state node =
  let name = get_name node in
  match name with
  | "statement_block" -> print_statement_block state node
  | _ -> match_rest state node print_expression

(* Generator function *)

and print_generator_function state node =
  let children = collect_children node in
  let async = has_node_named_opt "async" children
  and name_field = ts_node_child_by_field_name_opt node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_opt make_node async
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
  let children = collect_children node in
  let decorators = filter_by_name "decorator" children
  and name_field = ts_node_child_by_field_name_opt node "name"
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and heritage_child = filter_first_by_name_opt "class_heritage" children
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    Tree.mk_children_list print_decorator decorators
    @ [ mk_child_opt print_type_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_opt print_class_heritage heritage_child
      ; mk_child_res print_class_body body_field
      ]
  in
  make_tree state node children

and print_class_heritage state node =
  let children =
    match first_child_named "extends_clause" node with
    | Some extends_clause ->
      let implements_clause = first_child_named "implements_clause" node in
      [ mk_child print_extends_clause extends_clause
      ; mk_child_opt print_implements_clause implements_clause
      ]
    | None ->
      (* [implements_clause] is never [None]. *)
      let implements_clause = first_child_named "implements_clause" node in
      [ mk_child_opt print_implements_clause implements_clause ]
  in
  make_tree state node children

and print_implements_clause state node =
  let children = collect_named_children node in
  tree_of_list state node print_type children

and print_extends_clause state node =
  let children =
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
  let children = List.fold_right ~f:mk_children pairs ~init:[] in
  make_tree state node children

and print_class_body state node =
  let children = collect_named_children node in
  let decorators = filter_by_name "decorator" children in
  let print state node =
    let name = get_name node in
    match name with
    | "decorator" -> ()
    | "method_definition" ->
      List.iter ~f:(print_decorator state) decorators;
      print_method_definition state node
    | "method_signature" -> print_method_signature state node
    | "class_static_block" -> print_class_static_block state node
    | "abstract_method_signature" -> print_abstract_method_signature state node
    | "index_signature" -> print_index_signature state node
    | "public_field_definition" -> print_public_field_definition state node
    | _ -> match_rest state node print_unexpected_node
  in
  tree_of_list state node print children

and print_method_definition state node =
  let children = collect_children node in
  let accessibility_modifier = filter_first_by_name_opt "accessibility_modifier" children
  and static = has_node_named_opt "static" children
  and override_modifier = filter_first_by_name_opt "override_modifier" children
  and readonly = has_node_named_opt "readonly" children
  and async = has_node_named_opt "async" children
  and set = has_node_named_opt "set" children
  and get = has_node_named_opt "get" children
  and star = has_node_named_opt "*" children
  and name_field = ts_node_child_by_field_name_res node "name"
  and qmark = has_node_named_opt "?" children
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_res node "body" in
  let children =
    [ mk_child_opt print_accessibility_modifier accessibility_modifier
    ; mk_child_opt make_node static
    ; mk_child_opt print_override_modifier override_modifier
    ; mk_child_opt make_node readonly
    ; mk_child_opt make_node async
    ; mk_child_opt make_node set
    ; mk_child_opt make_node get
    ; mk_child_opt make_node star
    ; mk_child_res print_property_name name_field
    ; mk_child_opt make_node qmark
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ; mk_child_res print_statement_block body_field
    ]
  in
  make_tree state node children

and print_return_type state node =
  let name = get_name node in
  match name with
  | "type_annotation" -> print_type_annotation state node
  | "asserts_annotation" -> print_asserts_annotation state node
  | _ -> match_rest state node print_type_predicate_annotation

and print_class_static_block state node =
  let body_field = ts_node_child_by_field_name_res node "body" in
  make_unary_res state node print_statement_block body_field

and print_abstract_method_signature state node =
  let children = collect_children node in
  let accessibility_modifier = filter_first_by_name_opt "accessibility_modifier" children
  and abstract = has_node_named_opt "abstract" children
  and override_modifier = filter_first_by_name_opt "override_modifier" children
  and set = has_node_named_opt "set" children
  and get = has_node_named_opt "get" children
  and star = has_node_named_opt "*" children
  and name_field = ts_node_child_by_field_name_res node "name"
  and qmark = has_node_named_opt "?" children
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type" in
  let children =
    [ mk_child_opt print_accessibility_modifier accessibility_modifier
    ; mk_child_opt make_node abstract
    ; mk_child_opt print_override_modifier override_modifier
    ; mk_child_opt make_node set
    ; mk_child_opt make_node get
    ; mk_child_opt make_node star
    ; mk_child_res print_property_name name_field
    ; mk_child_opt make_node qmark
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ]
  in
  make_tree state node children

and print_public_field_definition state node =
  let children = collect_children node in
  let decorators = filter_by_name "decorator" children
  and accessibility_modifier = filter_first_by_name_opt "accessibility_modifier" children
  and override_modifier = filter_first_by_name_opt "override_modifier" children
  and declare = has_node_named_opt "declare" children
  and static = has_node_named_opt "static" children
  and readonly = has_node_named_opt "readonly" children
  and accessor = has_node_named_opt "accessor" children
  and abstract = has_node_named_opt "abstract" children
  and name_field = ts_node_child_by_field_name_res node "name"
  and type_field = ts_node_child_by_field_name_opt node "type"
  (* "_initializer" inlined: *)
  and value_field = ts_node_child_by_field_name_opt node "value"
  and qmark = has_node_named_opt "?" children
  and emark = has_node_named_opt "!" children in
  let children =
    Tree.mk_children_list print_decorator decorators
    @ [ mk_child_opt print_accessibility_modifier accessibility_modifier
      ; mk_child_opt make_node declare
      ; mk_child_opt print_override_modifier override_modifier
      ; mk_child_opt make_node static
      ; mk_child_opt make_node readonly
      ; mk_child_opt make_node accessor
      ; mk_child_opt make_node abstract
      ; mk_child_res print_property_name name_field
      ; mk_child_opt make_node qmark
      ; mk_child_opt make_node emark
      ; mk_child_opt print_type_annotation type_field
      ; mk_child_opt print_expression value_field
      ]
  in
  make_tree state node children

(* Meta-property *)

and print_meta_property state node =
  let meta_child = ts_node_child_res node 0
  and print state node =
    let name = get_name node in
    match name with
    | "new" -> make_node state node (* "new.target"? *)
    | "import" -> make_node state node (* "import.meta"? *)
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print meta_child

(* Call expression *)

and print_call_expression state node =
  let function_field = ts_node_child_by_field_name_res node "function"
  and type_arguments_field = ts_node_child_by_field_name_opt node "type_arguments"
  and arguments_field = ts_node_child_by_field_name_res node "arguments"
  and print_function state node =
    let name = get_name node in
    match name with
    | "import" -> print_import state node
    | _ -> match_rest state node print_expression
  and print_arguments state node =
    let name = get_name node in
    match name with
    | "template_string" -> print_template_string state node
    | _ -> match_rest state node print_arguments
  in
  let children =
    [ mk_child_res print_function function_field
    ; mk_child_opt print_type_arguments type_arguments_field
    ; mk_child_res print_arguments arguments_field
    ]
  in
  make_tree state node children

and print_type_arguments state node =
  let children = collect_named_children node in
  tree_of_list state node print_type children

and print_arguments state node =
  let children = collect_named_children node in
  tree_of_list state node print_argument children

and print_argument state node =
  let name = get_name node in
  match name with
  | "spread_element" -> print_spread_element state node
  | _ -> match_rest state node print_expression

(* Non-null expression *)

and print_non_null_expression state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_expression child

(* Sequence expression *)

and print_sequence_expression state node =
  let children = collect_named_children node in
  tree_of_list state node print_expression children

(* TYPE

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and print_type state node =
  let name = get_name node in
  match name with
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
  let name = get_name node in
  match name with
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
  | "this_type" -> print_this state node
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
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_primary_type child

(* Type identifier *)

and print_type_identifier state node = print_identifier state node

(* Parenthesized type *)

and print_parenthesized_type state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

(* Predefined type *)

and print_predefined_type state node =
  match collect_children node with
  | [] -> ()
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
      let name = get_name node in
      match name with
      | "any" -> make_node state node
      | "number" -> make_node state node
      | "boolean" -> make_node state node
      | "string" -> make_node state node
      | "symbol" -> make_node state node
      | "unique symbol" -> make_node state node
      | "void" -> make_node state node
      | "unknown" -> make_node state node
      | "never" -> make_node state node
      | "object" -> make_node state node
      | _ -> match_rest state node print_unexpected_node
    in
    make_unary state node print child

(* Nested type identifier *)

and print_nested_type_identifier state node =
  let module_field = ts_node_child_by_field_name_res node "module"
  and name_field = ts_node_child_by_field_name_res node "name"
  and print_module_field state node =
    let name = get_name node in
    match name with
    | "identifier" -> print_identifier state node
    | "nested_identifier" -> print_nested_identifier state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_module_field module_field
    ; mk_child_res print_type_identifier name_field
    ]
  in
  make_tree state node children

(* Nested identifier *)

and print_nested_identifier state node =
  let object_field = ts_node_child_by_field_name_res node "object"
  and property_field = ts_node_child_by_field_name_res node "property"
  and print_object_field state node =
    let name = get_name node in
    match name with
    | "identifier" -> print_identifier state node
    | "member_expression" -> print_nested_identifier state node
    | _ -> match_rest state node print_unexpected_node
  and print_property_field state node =
    let name = get_name node in
    match name with
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

and print_generic_type state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and type_arguments_field = ts_node_child_by_field_name_res node "type_arguments"
  and print_name_field state node =
    let name = get_name node in
    match name with
    | "type_identifier" -> print_type_identifier state node
    | "nested_type_identifier" -> print_nested_type_identifier state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_name_field name_field
    ; mk_child_res print_type_arguments type_arguments_field
    ]
  in
  make_tree state node children

(* Object type *)

and print_object_type state node =
  let children = collect_named_children node in
  tree_of_list state node print_object_type_field children

and print_object_type_field state node =
  let name = get_name node in
  match name with
  | "export_statement" -> print_export_statement state node
  | "property_signature" -> print_property_signature state node
  | "call_signature" -> print_call_signature state node
  | "construct_signature" -> print_construct_signature state node
  | "index_signature" -> print_index_signature state node
  | "method_signature" -> print_method_signature state node
  | _ -> match_rest state node print_unexpected_node

and print_property_signature state node =
  let children = collect_children node in
  let accessibility_modifier = filter_first_by_name_opt "accessibility_modifier" children
  and static = has_node_named_opt "static" children
  and override_modifier = filter_first_by_name_opt "override_modifier" children
  and readonly = has_node_named_opt "readonly" children
  and name_field = ts_node_child_by_field_name_res node "name"
  and qmark = has_node_named_opt "?" children
  and type_field = ts_node_child_by_field_name_opt node "type" in
  let children =
    [ mk_child_opt print_accessibility_modifier accessibility_modifier
    ; mk_child_opt make_node static
    ; mk_child_opt print_override_modifier override_modifier
    ; mk_child_opt make_node readonly
    ; mk_child_res print_identifier name_field
    ; mk_child_opt make_node qmark
    ; mk_child_opt print_type_annotation type_field
    ]
  in
  make_tree state node children

(* Call signature *)

and print_call_signature state node =
  let type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type" in
  let children =
    [ mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ]
  in
  make_tree state node children

(* Asserts annotation *)

and print_asserts_annotation state node =
  let asserts = ts_node_child_res node 1 in
  make_unary_res state node print_asserts asserts

(* Type predicate annotation *)

and print_type_predicate_annotation state node =
  let asserts = ts_node_child_res node 1 in
  make_unary_res state node print_type_predicate asserts

(* Construct signature *)

and print_construct_signature state node =
  let abstract = has_child_named "abstract" node
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and type_field = ts_node_child_by_field_name_opt node "type" in
  let children =
    [ mk_child_opt make_node abstract
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_type_annotation type_field
    ]
  in
  make_tree state node children

(* Index signature *)

and print_index_signature state node =
  let sign_field = ts_node_child_by_field_name_opt node "sign"
  and name_field = ts_node_child_by_field_name_opt node "name"
  and type_field = ts_node_child_by_field_name_res node "type"
  and print_sign_field state node =
    match ts_node_child_opt node 0 with
    | None -> Tree.make_node state "readonly"
    | Some sign -> Tree.make_unary state "readonly" print_plus_minus sign
  and print_type_field state node =
    let name = get_name node in
    match name with
    | "type_annotation" -> print_type_annotation state node
    | "omitting_type_annotation" -> print_omitting_type_annotation state node
    | "adding_type_annotation" -> print_adding_type_annotation state node
    | "opting_type_annotation" -> print_opting_type_annotation state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    match name_field with
    | Some name_field ->
      let index_type_field = ts_node_child_by_field_name_res node "index_type" in
      [ mk_child_opt print_sign_field sign_field
      ; mk_child print_identifier name_field
      ; mk_child_res print_type index_type_field
      ; mk_child_res print_type_field type_field
      ]
    | None ->
      let mapped_type_clause = ts_node_named_child_res node 0 in
      [ mk_child_opt print_sign_field sign_field
      ; mk_child_res print_mapped_type_clause mapped_type_clause
      ; mk_child_res print_type_field type_field
      ]
  in
  make_tree state node children

and print_plus_minus state node =
  let name = get_name node in
  match name with
  | "+" -> make_node state node
  | "-" -> make_node state node
  | _ -> match_rest state node print_unexpected_node

and print_mapped_type_clause state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and type_field = ts_node_child_by_field_name_res node "type"
  and alias_field = ts_node_child_by_field_name_opt node "alias" in
  let children =
    [ mk_child_res print_type_identifier name_field
    ; mk_child_res print_type type_field
    ; mk_child_opt print_type alias_field
    ]
  in
  make_tree state node children

and print_omitting_type_annotation state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

and print_adding_type_annotation state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

and print_opting_type_annotation state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

(* Method signature *)

and print_method_signature state node =
  let children = collect_children node in
  let accessibility_modifier = filter_first_by_name_opt "accessibility_modifier" children
  and static = has_node_named_opt "static" children
  and override_modifier = filter_first_by_name_opt "override_modifier" children
  and readonly = has_node_named_opt "readonly" children
  and async = has_node_named_opt "async" children
  and set = has_node_named_opt "set" children
  and get = has_node_named_opt "get" children
  and star = has_node_named_opt "*" children
  and name_field = ts_node_child_by_field_name_res node "name"
  and qmark = has_node_named_opt "?" children
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_opt node "return_type" in
  let children =
    [ mk_child_opt print_accessibility_modifier accessibility_modifier
    ; mk_child_opt make_node static
    ; mk_child_opt print_override_modifier override_modifier
    ; mk_child_opt make_node readonly
    ; mk_child_opt make_node async
    ; mk_child_opt make_node set
    ; mk_child_opt make_node get
    ; mk_child_opt make_node star
    ; mk_child_res print_property_name name_field
    ; mk_child_opt make_node qmark
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_opt print_return_type return_type_field
    ]
  in
  make_tree state node children

(* Array type *)

and print_array_type state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

(* Tuple type *)

and print_tuple_type state node =
  let children = collect_named_children node in
  tree_of_list state node print_tuple_type_member children

and print_tuple_type_member state node =
  let name = get_name node in
  match name with
  | "required_parameter" -> print_tuple_parameter state node (* Alias *)
  | "optional_parameter" -> print_optional_tuple_parameter state node (* Alias *)
  | "optional_type" -> print_optional_type state node
  | "rest_type" -> print_rest_type state node
  | _ -> match_rest state node print_type (* "type" is a hidden rule *)

and print_tuple_parameter state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and type_field = ts_node_child_by_field_name_res node "type"
  and print_name_field state node =
    let name = get_name node in
    match name with
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
  let name_field = ts_node_child_by_field_name_res node "name"
  and type_field = ts_node_child_by_field_name_res node "type" in
  let children =
    [ mk_child_res print_identifier name_field
    ; mk_child_res print_type_annotation type_field
    ]
  in
  make_tree state node children

(* Type annotation *)

and print_type_annotation state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

(* Rest pattern *)

and print_rest_pattern state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_lhs_expression child

(* LHS expression *)

and print_lhs_expression state node =
  let name = get_name node in
  match name with
  | "member_expression" -> print_member_expression state node
  | "subscript_expression" -> print_subscript_expression state node
  | "identifier" -> print_identifier state node
  | "undefined" -> print_undefined state node
  | "object_pattern" -> print_object_pattern state node
  | "array_pattern" -> print_array_pattern state node
  | "non_null_expression" -> print_non_null_expression state node
  | _ -> match_rest state node print_unexpected_node

and print_optional_type state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

and print_rest_type state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

(* Type query *)

and print_type_query state node =
  let child = ts_node_named_child_res node 0
  and print state node =
    let name = get_name node in
    match name with
    | "subscript_expression" -> print_type_query_subscript_expression state node
    | "member_expression" -> print_type_query_member_expression state node
    | "call_expression" -> print_type_query_call_expression state node
    | "instantiation_expression" -> print_type_query_instantiation_expression state node
    | "identifier" -> print_identifier state node
    | "this" -> print_this state node
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print child

and print_type_query_subscript_expression state node =
  let object_field = ts_node_child_by_field_name_res node "object"
  and index_field = ts_node_child_by_field_name_res node "index"
  and print_index_field state node =
    let name = get_name node in
    match name with
    | "predefined_type" -> print_predefined_type state node
    | "string" -> make_node state node
    | "number" -> print_number state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_object_field object_field
    ; mk_child_res print_index_field index_field
    ]
  in
  make_tree state node children

and print_type_query_member_expression state node =
  let object_field = ts_node_child_by_field_name_res node "object"
  and property_field = ts_node_child_by_field_name_res node "property" in
  let children =
    [ mk_child_res print_object_field object_field
    ; mk_child_res print_property_field property_field
    ]
  in
  make_tree state node children

and print_object_field state node =
  let name = get_name node in
  match name with
  | "identifier" -> print_identifier state node
  | "this" -> print_this state node
  | "member_expression" -> print_type_query_member_expression state node
  | "subscript_expression" -> print_type_query_subscript_expression state node
  | "call_expression" -> print_type_query_call_expression state node
  | _ -> match_rest state node print_unexpected_node

and print_property_field state node =
  let name = get_name node in
  match name with
  | "private_property_identifier" -> print_identifier state node
  | "property_identifier" -> print_identifier state node
  | _ -> match_rest state node print_unexpected_node

and print_type_query_instantiation_expression state node =
  let function_field = ts_node_child_by_field_name_res node "function"
  and type_arguments_field = ts_node_child_by_field_name_res node "type_arguments" in
  let children =
    [ mk_child_res print_function_field function_field
    ; mk_child_res print_type_arguments type_arguments_field
    ]
  in
  make_tree state node children

and print_function_field state node =
  let name = get_name node in
  match name with
  | "import" -> print_import state node
  | "identifier" -> print_identifier state node
  | "member_expression" -> print_type_query_member_expression state node
  | "subscript_expression" -> print_type_query_subscript_expression state node
  | _ -> match_rest state node print_unexpected_node

and print_type_query_call_expression state node =
  let function_field = ts_node_child_by_field_name_res node "function"
  and arguments_field = ts_node_child_by_field_name_res node "arguments" in
  let children =
    [ mk_child_res print_function_field function_field
    ; mk_child_res print_arguments arguments_field
    ]
  in
  make_tree state node children

and print_index_type_query state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_primary_type child

(* Existential type *)

and print_existential_type state node = make_node state node

(* Literal type *)

and print_literal_type state node =
  let child = ts_node_named_child_res node 0 in
  let print state node =
    let name = get_name node in
    match name with
    | "unary_expression" -> print_unary_expression state node
    | "number" -> print_number state node
    | "string" -> print_string state node
    | "true" -> print_true state node
    | "false" -> print_false state node
    | "null" -> print_null state node
    | "undefined" -> print_undefined state node
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print child

(* Unary expression *)

and print_unary_expression state node =
  let operator_field = ts_node_child_by_field_name_res node "operator"
  and argument_field = ts_node_child_by_field_name_res node "argument"
  and print_unary_operator state node =
    let name = get_name node in
    match name with
    | "!" -> make_node state node
    | "~" -> make_node state node
    | "-" -> make_node state node
    | "+" -> make_node state node
    | "typeof" -> make_node state node
    | "void" -> make_node state node
    | "delete" -> make_node state node
    | _ -> match_rest state node print_unexpected_node
  in
  let children =
    [ mk_child_res print_unary_operator operator_field
    ; mk_child_res print_number argument_field
    ]
  in
  make_tree state node children

(* Look up type

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and print_lookup_type state node =
  let primary_type_child = ts_node_named_child_res node 0
  and type_child = ts_node_named_child_res node 1 in
  let children =
    [ mk_child_res print_primary_type primary_type_child
    ; mk_child_res print_type type_child
    ]
  in
  make_tree state node children

(* Conditional type *)

and print_conditional_type state node =
  let left_field = ts_node_child_by_field_name_res node "left"
  and right_field = ts_node_child_by_field_name_res node "right"
  and consequence_field = ts_node_child_by_field_name_res node "consequence"
  and alternative_field = ts_node_child_by_field_name_res node "alternative" in
  let children =
    [ mk_child_res print_type left_field
    ; mk_child_res print_type right_field
    ; mk_child_res print_type consequence_field
    ; mk_child_res print_type alternative_field
    ]
  in
  make_tree state node children

(* Template literal type *)

and print_template_literal_type state node = make_node state node

(* Intersection type *)

and print_intersection_type state node =
  let children = collect_named_children node in
  tree_of_list state node print_type children

(* Union type *)

and print_union_type state node =
  let children = collect_named_children node in
  tree_of_list state node print_type children

(* Function type *)

and print_function_type state node =
  let type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and return_type_field = ts_node_child_by_field_name_res node "return_type"
  and print_return_type state node =
    let name = get_name node in
    match name with
    | "asserts" -> print_asserts state node
    | "type_predicate" -> print_type_predicate state node
    | _ -> match_rest state node print_type
  in
  let children =
    [ mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_res print_return_type return_type_field
    ]
  in
  make_tree state node children

and print_asserts state node =
  let child = ts_node_child_res node 1
  and print state node =
    let name = get_name node in
    match name with
    | "type_predicate" -> print_type_predicate state node
    | "identifier" -> print_identifier state node
    | "this" -> print_this state node
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print child

and print_type_predicate state node =
  let name_field = ts_node_child_by_field_name_res node "name"
  and type_field = ts_node_child_by_field_name_res node "type" in
  let print_name_field state node =
    let name = get_name node in
    match name with
    | "identifier" -> print_identifier state node
    | "this" -> print_this state node
    | _ -> match_rest state node print_predefined_type
  in
  let children =
    [ mk_child_res print_name_field name_field; mk_child_res print_type type_field ]
  in
  make_tree state node children

(* Readonly type *)

and print_readonly_type state node =
  let child = ts_node_named_child_res node 0 in
  make_unary_res state node print_type child

(* Constructor type *)

and print_constructor_type state node =
  let abstract = has_child_named "abstract" node
  and type_parameters_field = ts_node_child_by_field_name_opt node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_res node "parameters"
  and type_field = ts_node_child_by_field_name_res node "type" in
  let children =
    [ mk_child_opt make_node abstract
    ; mk_child_opt print_type_parameters type_parameters_field
    ; mk_child_res print_formal_parameters parameters_field
    ; mk_child_res print_type type_field
    ]
  in
  make_tree state node children

and print_formal_parameters state node =
  let children = collect_named_children node in
  tree_of_list state node print_formal_parameter children

and print_formal_parameter state node =
  let name = get_name node in
  match name with
  | "required_parameter" -> print_required_parameter state node
  | "optional_parameter" -> print_optional_parameter state node
  | _ -> match_rest state node print_unexpected_node

and print_optional_parameter state node = print_required_parameter state node

and print_required_parameter state node =
  let children = collect_children node in
  (* "_parameter_name" inlined: *)
  let decorators = filter_by_name "decorator" children
  and accessibility_modifier = filter_first_by_name_opt "accessibility_modifier" children
  and override_modifier = filter_first_by_name_opt "override_modifier" children
  and readonly = has_node_named_opt "readonly" children
  and pattern_field = ts_node_child_by_field_name_res node "pattern"
  (* *)
  and type_field = ts_node_child_by_field_name_opt node "type"
  (* "_initializer" inlined: *)
  and value_field = ts_node_child_by_field_name_opt node "value"
  and print_pattern_field state node =
    let name = get_name node in
    match name with
    | "this" -> print_this state node
    | _ -> print_pattern state node
  in
  let children =
    Tree.mk_children_list print_decorator decorators
    @ [ mk_child_opt print_accessibility_modifier accessibility_modifier
      ; mk_child_opt print_override_modifier override_modifier
      ; mk_child_opt make_node readonly
      ; mk_child_res print_pattern_field pattern_field
      ; mk_child_opt print_type_annotation type_field
      ; mk_child_opt print_expression value_field
      ]
  in
  make_tree state node children

(* Decorator *)

and print_decorator state node =
  let child = ts_node_named_child_res node 0
  and print state node =
    let name = get_name node in
    match name with
    | "identifier" -> print_identifier state node
    | "member_expression" -> print_member_expression state node
    | "call_expression" -> print_call_expression state node
    | "parenthesized_expression" -> print_parenthesized_expression state node
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print child

(* Accessibility modifier *)

and print_accessibility_modifier state node =
  let child = ts_node_child_res node 0
  and print state node =
    let name = get_name node in
    match name with
    | "public" -> print_public state node
    | "private" -> print_private state node
    | "protected" -> print_protected state node
    | _ -> match_rest state node print_unexpected_node
  in
  make_unary_res state node print child

and print_public state node = make_node state node
and print_private state node = make_node state node
and print_protected state node = make_node state node

(* Override modifier *)

and print_override_modifier state node =
  let child = ts_node_child_res node 0 in
  make_unary_res state node make_node child

(* Infer type *)

and print_infer_type state node =
  let type_identifier_child = ts_node_named_child_res node 0
  and type_child = ts_node_named_child_opt node 1 in
  let children =
    [ mk_child_res print_identifier type_identifier_child
    ; mk_child_opt print_type type_child
    ]
  in
  make_tree state node children

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

(* Object pattern *)

and print_object_pattern state node =
  let children = collect_named_children node in
  tree_of_list state node print_object_pattern_field children

and print_object_pattern_field state node =
  let name = get_name node in
  match name with
  | "pair_pattern" -> print_pair_pattern state node
  | "rest_pattern" -> print_rest_pattern state node
  | "object_assignment_pattern" -> print_object_assignment_pattern state node
  | "shorthand_property_identifier_pattern" ->
    print_shorthand_property_identifier_pattern state node
  | _ -> match_rest state node print_unexpected_node

(* Pair pattern *)

and print_pair_pattern state node =
  let key_field = ts_node_child_by_field_name_res node "key"
  and value_field = ts_node_child_by_field_name_res node "value"
  and print_pair_value_field state node =
    let name = get_name node in
    match name with
    | "assignment_pattern" -> print_assignment_pattern state node
    | _ -> match_rest state node print_pattern
  in
  let children =
    [ mk_child_res print_property_name key_field
    ; mk_child_res print_pair_value_field value_field
    ]
  in
  make_tree state node children

(* Assignment pattern *)

and print_assignment_pattern state node =
  let left_field = ts_node_child_by_field_name_res node "left"
  and right_field = ts_node_child_by_field_name_res node "right" in
  let children =
    [ mk_child_res print_pattern left_field; mk_child_res print_expression right_field ]
  in
  make_tree state node children

(* Property names *)

and print_property_name state node =
  let name = get_name node in
  match name with
  | "property_identifier" -> print_identifier state node
  | "private_property_identifier" -> print_identifier state node
  | "string" -> print_string state node
  | "number" -> print_number state node
  | "computed_property_name" -> print_computed_property_name state node
  | _ -> match_rest state node print_unexpected_node

and print_computed_property_name state node =
  let expression = ts_node_child_res node 1 in
  make_unary_res state node print_expression expression

and print_shorthand_property_identifier_pattern state node = make_node state node

(* Object assignment pattern *)

and print_object_assignment_pattern state node =
  let left_field = ts_node_child_by_field_name_res node "left"
  and right_field = ts_node_child_by_field_name_res node "right"
  and print_left state node =
    let name = get_name node in
    match name with
    | "shorthand_property_identifier_pattern" ->
      print_shorthand_property_identifier_pattern state node
    | _ -> match_rest state node print_destructuring_pattern
  in
  let children =
    [ mk_child_res print_left left_field; mk_child_res print_expression right_field ]
  in
  make_tree state node children

(* Rule "_destructuring_pattern" is inlined. *)

and print_destructuring_pattern state node =
  let name = get_name node in
  match name with
  | "object_pattern" -> print_object_pattern state node
  | "array_pattern" -> print_array_pattern state node
  | _ -> match_rest state node print_unexpected_node

(* Array pattern *)

and print_array_pattern state node =
  let children = collect_named_children node in
  tree_of_list state node print_array_pattern_cell children

and print_array_pattern_cell state node =
  let name = get_name node in
  match name with
  | "assignment_pattern" -> print_assignment_pattern state node
  | _ -> match_rest state node print_pattern (* hidden rule *)

(* General patterns (hidden rule) *)

and print_pattern state node =
  let name = get_name node in
  match name with
  | "rest_pattern" -> print_rest_pattern state node
  | _ -> print_lhs_expression state node
