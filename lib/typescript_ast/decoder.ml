(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun   = Tree_sitter.Api.Functions

(* Integers needed by the tree-sitter APIs above *)

module UInt32 = Unsigned.UInt32

(* Tree-sitter API for TypeScript *)

let tree_sitter_typescript =
  Tree_sitter_typescript.Api.Functions.tree_sitter_typescript

(* ocaml-ctypes types and bindings (only global module opening) *)

open Ctypes

(* Type aliases for trees *)

type ts_tree     = TS_types.ts_tree structure
type ts_tree_ptr = TS_types.ts_tree structure Ctypes_static.ptr

(* Converting C strings of type 'char*' to OCaml strings of type
   [string]. *)

let string_of_char_ptr (ptr: char ptr) : string =
  let rec get_length (p: char ptr) : int =
    if !@p = '\000' then 0 else 1 + get_length (p +@ 1) in
  let length = get_length ptr in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    Bytes.set buffer i !@(ptr +@ i)
  done;
  Bytes.to_string buffer

(* Length of string as an unsigned integer *)

let uint32_len string = UInt32.of_int (String.length string)

(* Node to optional node *)

let node_to_opt node =
  if TS_fun.ts_node_is_null node then None else Some node

(* Wrapper for filtering fields *)

let ts_node_child_by_field_name node field =
  TS_fun.ts_node_child_by_field_name node field (uint32_len field)

(* Printing the tree *)
(*
let print_node (node: ts_tree) : unit =
  let ptr_char = TS_fun.ts_node_string node in
  Printf.printf "%s\n%!" @@ string_of_char_ptr ptr_char
*)

(* Converting a node to an OCaml string *)

let string_of_ts_node_type (node: ts_tree) : string =
  string_of_char_ptr @@ TS_fun.ts_node_type node

(* Parsing a string expected to contain a valid TypeScript program *)

let parse_typescript_string (source_code: string) : ts_tree_ptr =
  let parser     = TS_fun.ts_parser_new ()
  and language   = tree_sitter_typescript () in
  let _ : bool   = TS_fun.ts_parser_set_language parser language in (*[true]*)
  let null_tree  = from_voidp TS_types.ts_tree null in
  let parse_tree = TS_fun.ts_parser_parse_string
                     parser
                     null_tree
                     source_code
                     (UInt32.of_int @@ String.length source_code)
  in TS_fun.ts_parser_delete parser; parse_tree

(* Collating named children of a given node (we discard comment nodes) *)

let collect_named_children (node: ts_tree) : ts_tree list =
  let rec collect acc n =
    if UInt32.(equal zero n) then acc
    else let index = UInt32.pred n in
         let child = TS_fun.ts_node_named_child node index in
         match string_of_ts_node_type child with
         | "comment" -> collect acc index
         | _ -> collect (child :: acc) index
  in collect [] (TS_fun.ts_node_named_child_count node)

(* Collating all  children of a given node (we discard comment nodes) *)

let collect_children (node: ts_tree) : ts_tree list =
  let rec collect acc n =
    if UInt32.(equal zero n) then acc
    else let index = UInt32.pred n in
         let child = TS_fun.ts_node_child node index in
         match string_of_ts_node_type child with
         | "comment" -> collect acc index
         | _ -> collect (child :: acc) index
  in collect [] (TS_fun.ts_node_child_count node)

(* Extracting the name of a node *)

let get_name ?name node =
  match name with
    None -> string_of_ts_node_type node
  | Some name -> name

(* Anonymising a printer *)

let anon printer state node = printer state ?name:None node

(* Making a tree node *)

let make_node state ?name node =
  let name = get_name ?name node
  in Tree.make_node state name

(* Unexpected and TODO nodes *)

let print_unexpected_node state ?name node =
  let name = get_name ?name node in
  Tree.make_node state (name ^ "?")

let print_todo_node state ?name node =
  let name = get_name ?name node in
  Tree.make_node state (name ^ "!")

(* Printing the AST *)

let rec print_program state node =
  let name = string_of_ts_node_type node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_statement) children

(* Statements

   The JavasScript tree-sitter grammar have the non-terminals
   "statement" and "declaration" be supertypes, that is, hidden
   rules. *)

and print_statement state ?name node =
  let name = get_name ?name node in
  match name with
  | "export_statement" -> print_export_statement state ~name node
  | "import_statement" -> print_import_statement state ~name node
  | "debugger_statement" -> print_debugger_statement state ~name node
  | "expression_statement" -> print_expression_statement state ~name node
  | "statement_block" -> print_statement_block state ~name node
  | "if_statement" -> print_if_statement state ~name node
  | "switch_statement" -> print_switch_statement state ~name node
  | "for_statement" -> print_for_statement state ~name node
  | "while_statement" -> print_while_statement state ~name node
  | "do_statement" -> print_do_statement state ~name node
  | "try_statement" -> print_try_statement state ~name node
  | "with_statement" -> print_with_statement state ~name node
  | "break_statement" -> print_break_statement state ~name node
  | "continue_statement" -> print_continue_statement state ~name node
  | "return_statement" -> print_return_statement state ~name node
  | "throw_statement" -> print_throw_statement state ~name node
  | "empty_statement" -> print_empty_statement state ~name node
  | "labeled_statement" -> print_labeled_statement state ~name node
  (* Inlining declarations cases (hidden rule) *)
  | "function_declaration" -> print_function_declaration state ~name node
  | "generator_function_declaration" -> print_generator_function_declaration state ~name node
  | "class_declaration" -> print_class_declaration state ~name node
  | "lexical_declaration" -> print_lexical_declaration state ~name node
  | "variable_declaration" -> print_variable_declaration state ~name node
  | "function_signature" -> print_function_signature state ~name node
  | "abstract_class_declaration" -> print_abstract_class_declaration state ~name node
  | "module" -> print_module state ~name node
  | "internal_module" -> print_internal_module state ~name node
  | "type_alias_declaration" -> print_type_alias_declaration state ~name node
  | "enum_declaration" -> print_enum_declaration state ~name node
  | "interface_declaration" -> print_interface_declaration state ~name node
  | "import_alias" -> print_import_alias state ~name node
  | "ambient_declaration" -> print_ambient_declaration state ~name node
  (* Comments are ignored *)
  | "comment" -> ()
  | _ -> print_unexpected_node state ~name node

and print_export_statement state ?name node =
  Tree.make_node state (get_name ?name node)

and print_import_statement state ?name node =
  Tree.make_node state (get_name ?name node)

and print_debugger_statement state ?name node =
  Tree.make_node state (get_name ?name node)

(* Expression statements

   {@js[
    expression_statement: $ => seq($._expressions, $._semicolon),
    _expressions: $ => choice($.expression, $.sequence_expression),
    sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [print_expression]. *)

and print_expression_statement state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_expression) children

and print_statement_block state ?name node =
  print_todo_node state ?name node

and print_if_statement state ?name node =
  print_todo_node state ?name node

and print_switch_statement state ?name node =
  print_todo_node state ?name node

and print_for_statement state ?name node =
  print_todo_node state ?name node

and print_while_statement state ?name node =
  print_todo_node state ?name node

and print_do_statement state ?name node =
  print_todo_node state ?name node

and print_try_statement state ?name node =
  print_todo_node state ?name node

and print_with_statement state ?name node =
  print_todo_node state ?name node

and print_break_statement state ?name node =
  print_todo_node state ?name node

and print_continue_statement state ?name node =
  print_todo_node state ?name node

and print_return_statement state ?name node =
  print_todo_node state ?name node

and print_throw_statement state ?name node =
  print_todo_node state ?name node

and print_empty_statement state ?name node =
  print_todo_node state ?name node

and print_labeled_statement state ?name node =
  print_todo_node state ?name node

(* Declarations *)

and print_declaration state ?name node =
  let name = get_name ?name node in
  match name with
  | "function_declaration" -> print_function_declaration state ~name node
  | "generator_function_declaration" -> print_generator_function_declaration state ~name node
  | "class_declaration" -> print_class_declaration state ~name node
  | "lexical_declaration" -> print_lexical_declaration state ~name node
  | "variable_declaration" -> print_variable_declaration state ~name node
  | "function_signature" -> print_function_signature state ~name node
  | "abstract_class_declaration" -> print_abstract_class_declaration state ~name node
  | "module" -> print_module state ~name node
  | "internal_module" -> print_internal_module state ~name node
  | "type_alias_declaration" -> print_type_alias_declaration state ~name node
  | "enum_declaration" -> print_enum_declaration state ~name node
  | "interface_declaration" -> print_interface_declaration state ~name node
  | "import_alias" -> print_import_alias state ~name node
  | "ambient_declaration" -> print_ambient_declaration state ~name node
  (* Comments are discarded *)
  | "comment" -> ()
  | _ -> print_unexpected_node state ~name node

and print_function_declaration state ?name node =
  print_todo_node state ?name node

and print_generator_function_declaration state ?name node =
  print_todo_node state ?name node

and print_class_declaration state ?name node =
  print_todo_node state ?name node

and print_lexical_declaration state ?name node =
  print_todo_node state ?name node

and print_variable_declaration state ?name node =
  print_todo_node state ?name node

and print_function_signature state ?name node =
  print_todo_node state ?name node

and print_abstract_class_declaration state ?name node =
  print_todo_node state ?name node

and print_module state ?name node =
  print_todo_node state ?name node

and print_internal_module state ?name node =
  print_todo_node state ?name node

and print_type_alias_declaration state ?name node =
  let name = get_name ?name node in
  let type_name = ts_node_child_by_field_name node "name" in
  let type_parameters =
    node_to_opt @@ ts_node_child_by_field_name node "type_parameters" in
  let value = ts_node_child_by_field_name node "value" in
  let print_type_identifier = anon print_identifier in
  let print_type_parameters = anon print_type_parameters in
  let print_type = anon print_type in
  let children = Tree.[
    mk_child     print_type_identifier type_name;
    mk_child_opt print_type_parameters type_parameters;
    mk_child     print_type            value
  ]
  in Tree.make state name children

and print_type_parameters state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type_parameter) children

and print_type_parameter state ?name node =
  let name = get_name ?name node in
  let type_name = ts_node_child_by_field_name node "name" in
  let constraint_field =
    node_to_opt @@ ts_node_child_by_field_name node "constraint" in
  let value =
    node_to_opt @@ ts_node_child_by_field_name node "value" in
  let print_type_identifier = anon print_identifier in
  let print_constraint = anon print_constraint in
  let print_default_type = anon print_default_type in
  let children = Tree.[
    mk_child     print_type_identifier type_name;
    mk_child_opt print_constraint      constraint_field;
    mk_child_opt print_default_type    value
  ]
  in Tree.make state name children

and print_constraint state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_default_type state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_enum_declaration state ?name node =
  print_todo_node state ?name node

and print_interface_declaration state ?name node =
  print_todo_node state ?name node

and print_import_alias state ?name node =
  print_todo_node state ?name node

and print_ambient_declaration state ?name node =
  print_todo_node state ?name node

(* Expressions

   The JavasScript tree-sitter grammar have the non-terminals
   "expression" and "primary_expression" be supertypes, that is,
   hidden rules. Therefore we have to match all the RHS of those
   non-terminals in [print_expression], but also "sequence_expression"
   from the RHS of the hidden rule "_expressions" (see
   [print_expression_statement]). *)

and print_expression state ?name node =
  let name = get_name ?name node in
  match name with
  | "subscript_expression" -> print_subscript_expression state ~name node
  | "member_expression" -> print_member_expression state ~name node
  | "parenthesized_expression" -> print_parenthesized_expression state ~name node
  | "identifier" -> print_identifier state ~name node
  | "undefined" -> print_undefined state ~name node
  | "this" -> print_this state ~name node
  | "super" -> print_super state ~name node
  | "number" -> print_number state ~name node
  | "string" -> print_string state ~name node
  | "template_string" -> print_template_string state ~name node
  | "regex" -> print_regex state ~name node
  | "true" -> print_true state ~name node
  | "false" -> print_false state ~name node
  | "null" -> print_null state ~name node
  | "object" -> print_object state ~name node
  | "array" -> print_array state ~name node
  | "function_expression" -> function_expression state ~name node
  | "arrow_function" -> print_arrow_function state ~name node
  | "generator_function" -> print_generator_function state ~name node
  | "class" -> print_class state ~name node
  | "meta_property" -> print_meta_property state ~name node
  | "call_expression" -> print_call_expression state ~name node
  | "non_null_expression" -> print_non_null_expression state ~name node
  | "sequence_expression" -> print_sequence_expression state ~name node
  (* Comments are ignored *)
  | "comment" -> ()
  | _ -> print_unexpected_node state ~name node

and print_subscript_expression state ?name node =
  print_todo_node state ?name node

and print_member_expression state ?name node =
  print_todo_node state ?name node

and print_parenthesized_expression state ?name node =
  print_todo_node state ?name node

and print_identifier state ?name node =
  make_node state ?name node

and print_undefined state ?name node =
  make_node state ?name node

and print_this state ?name node =
  make_node state ?name node

and print_super state ?name node =
  make_node state ?name node

and print_number state ?name node =
  make_node state ?name node

and print_string state ?name node =
  make_node state ?name node

and print_template_string state ?name node =
  make_node state ?name node

and print_regex state ?name node =
  make_node state ?name node

and print_true state ?name node =
  make_node state ?name node

and print_false state ?name node =
  make_node state ?name node

and print_null state ?name node =
  make_node state ?name node

and print_object state ?name node =
  print_todo_node state ?name node

(* Arrays *)

and print_array state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_array_cell) children

and print_array_cell state ?name node =
  let name = get_name ?name node in
  match name with
  | "spread_element" -> print_spread_element state ~name node
  | "comment" -> () (* Comments are ignored *)
  | _ -> print_expression state ~name node

and print_spread_element state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_expression) children

(* *)

and function_expression state ?name node =
  print_todo_node state ?name node

and print_arrow_function state ?name node =
  print_todo_node state ?name node

and print_generator_function state ?name node =
  print_todo_node state ?name node

and print_class state ?name node =
  print_todo_node state ?name node

and print_meta_property state ?name node =
  print_todo_node state ?name node

(* Call expression *)

and print_call_expression state ?name node =
  let name = get_name ?name node in
  let function_ =
    ts_node_child_by_field_name node "function" in
  let type_arguments =
    node_to_opt @@ ts_node_child_by_field_name node "type_arguments" in
  let arguments =
    ts_node_child_by_field_name node "arguments" in
  let print_function state node =
    let name = string_of_ts_node_type node in
    match name with
      "import" -> make_node state ~name node
    | "comment" -> () (* Comments are ignored *)
    | _ -> print_expression state ~name node in
  let print_type_arguments = anon print_type_arguments in
  let children = Tree.[
    mk_child     print_function       function_;
    mk_child_opt print_type_arguments type_arguments;
    mk_child     print_arguments      arguments
  ]
  in Tree.make state name children

and print_type_arguments state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_arguments state node =
  let name = string_of_ts_node_type node in
  let children = collect_named_children node in
  Tree.of_list state name print_argument children

and print_argument state node = print_array_cell state node

(* *)

and print_non_null_expression state ?name node =
  print_todo_node state ?name node

and print_sequence_expression state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_expression) children

(* TYPES

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and print_type state ?name node =
  let name = get_name ?name node in
  match name with
  (* The (inlined) "primary_type" cases first *)
  | "parenthesized_type" -> print_parenthesized_type state ~name node
  | "predefined_type" -> print_predefined_type state ~name node
  | "type_identifier" -> print_identifier state ~name node (* Alias *)
  | "nested_type_identifier" -> print_nested_type_identifier state ~name node
  | "generic_type" -> print_generic_type state ~name node
  | "object_type" -> print_object_type state ~name node
  | "array_type" -> print_array_type state ~name node
  | "tuple_type" -> print_tuple_type state ~name node
  (*  | "flow_maybe_type" -> *)
  | "type_query" -> print_type_query state ~name node
  | "index_type_query" -> print_index_type_query state ~name node
  | "this" -> print_this state ~name node
  | "existential_type" -> print_existential_type state ~name node
  | "literal_type" -> print_literal_type state ~name node
  | "lookup_type" -> print_lookup_type state ~name node
  | "conditional_type" -> print_conditional_type state ~name node
  | "template_literal_type" -> print_template_literal_type state ~name node
  | "intersection_type" -> print_intersection_type state ~name node
  | "union_type" -> print_union_type state ~name node
  (* Rest of the types *)
  | "function_type" -> print_function_type state ~name node
  | "readonly_type" -> print_readonly_type state ~name node
  | "constructor_type" -> print_constructor_type state ~name node
  | "infer_type" -> print_infer_type state ~name node
  (* A couple of aliases *)
  | "member_expression" -> print_member_expression state ~name node
  | "call_expression" -> print_call_expression state ~name node
  (* Comments are ignored *)
  | "comment" -> ()
  | _ -> print_unexpected_node state ~name node

and print_parenthesized_type state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_predefined_type state ?name node =
  let name = get_name ?name node in
  match collect_children node with
    [] -> ()
  | child :: _ ->
    (* The tree-sitter parser for TypeScript has a bug: a child node
       "unique symbol" occurs repeated, for some mysterious
       reason. Here is the

       predefined_type: _ => choice(
         ...
         alias(seq('unique', 'symbol'), 'unique symbol')
         ...)
    *)
    let print state node =
      let name = string_of_ts_node_type node in
      match name with
      | "any" -> make_node state ~name node
      | "number" -> make_node state ~name node
      | "boolean" -> make_node state ~name node
      | "string" -> make_node state ~name node
      | "symbol" -> make_node state ~name node
      | "unique symbol" -> make_node state ~name node
      | "void" -> make_node state ~name node
      | "unknown" -> make_node state ~name node
      | "never" -> make_node state ~name node
      | "object" -> make_node state ~name node
      | "comment" -> () (* Comments are ignored *)
      | _ -> print_unexpected_node state ~name node
    in Tree.make_unary state name print child

and print_nested_type_identifier state ?name node =
  print_todo_node state ?name node

and print_generic_type state ?name node =
  print_todo_node state ?name node

and print_object_type state ?name node =
  print_todo_node state ?name node

and print_array_type state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_tuple_type state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  let print_tuple_type_component = anon print_tuple_type_component
  in Tree.of_list state name print_tuple_type_component children

and print_tuple_type_component state ?name node =
  let name = get_name ?name node in
  match name with
  | "required_parameter" -> print_tuple_parameter state ~name node (* Alias *)
  | "optional_parameter" -> print_optional_tuple_parameter state ~name node (* Alias *)
  | "optional_type" -> print_optional_type state ~name node
  | "rest_type" -> print_rest_type state ~name node
  | "comment" -> () (* Comments are discarded *)
  | _ -> print_type state node (* "type" is a hidden rule *)

and print_tuple_parameter state ?name node =
  let name = get_name ?name node in
  let name_field = ts_node_child_by_field_name node "name"
  and type_field = ts_node_child_by_field_name node "type" in
  let print_name_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "identifier" -> print_identifier state ~name node
    | "rest_pattern" -> print_rest_pattern state ~name node
    | "comment" -> () (* Comments discarded *)
    | _ -> print_unexpected_node state ~name node in
  let print_type_annotation = anon print_type_annotation in
  let children = Tree.[
    mk_child print_name_field      name_field;
    mk_child print_type_annotation type_field
  ]
  in Tree.make state name children

and print_type_annotation state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_rest_pattern state ?name node =
  print_todo_node state ?name node

and print_optional_tuple_parameter state ?name node =
  let name = get_name ?name node in
  let name_field = ts_node_child_by_field_name node "name"
  and type_field = ts_node_child_by_field_name node "type" in
  let children = Tree.[
    mk_child (anon print_identifier)      name_field;
    mk_child (anon print_type_annotation) type_field
  ]
  in Tree.make state name children

and print_optional_type state ?name node =
  print_todo_node state ?name node

and print_rest_type state ?name node =
  print_todo_node state ?name node

and print_type_query state ?name node =
  print_todo_node state ?name node

and print_index_type_query state ?name node =
  print_todo_node state ?name node

and print_existential_type state ?name node =
  print_todo_node state ?name node

and print_literal_type state ?name node =
  print_todo_node state ?name node

and print_lookup_type state ?name node =
  print_todo_node state ?name node

and print_conditional_type state ?name node =
  print_todo_node state ?name node

and print_template_literal_type state ?name node =
  print_todo_node state ?name node

and print_intersection_type state ?name node =
  print_todo_node state ?name node

and print_union_type state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_function_type state ?name node =
  print_todo_node state ?name node

and print_readonly_type state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_constructor_type state ?name node =
  print_todo_node state ?name node

and print_infer_type state ?name node =
  print_todo_node state ?name node
