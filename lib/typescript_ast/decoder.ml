(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun = Tree_sitter.Api.Functions

(* Integers needed by the tree-sitter APIs above *)

module UInt32 = Unsigned.UInt32

(* Tree-sitter API for TypeScript *)

let tree_sitter_typescript = Tree_sitter_typescript.Api.Functions.tree_sitter_typescript

(* ocaml-ctypes types and bindings (only global module opening) *)

open Ctypes

(* Type aliases for trees *)

type ts_tree = TS_types.ts_tree structure
type ts_tree_ptr = TS_types.ts_tree structure Ctypes_static.ptr

(* Converting C strings of type 'char*' to OCaml strings of type
   [string]. *)

let string_of_char_ptr (ptr : char ptr) : string =
  let rec get_length (p : char ptr) : int =
    if !@p = '\000' then 0 else 1 + get_length (p +@ 1)
  in
  let length = get_length ptr in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    Bytes.set buffer i !@(ptr +@ i)
  done;
  Bytes.to_string buffer

(* Length of string as an unsigned integer *)

let uint32_len string = UInt32.of_int (String.length string)

(* Wrappers for filtering fields (failure on null node or optional value) *)

let ts_node_child_by_field_name_exn node field =
  let child = TS_fun.ts_node_child_by_field_name node field (uint32_len field) in
  if TS_fun.ts_node_is_null child
  then
    failwith
      ("Decoder.ts_node_child_by_field_name_exn: Field \"" ^ field ^ "\" missing.\n")
  else child

let node_to_opt node = if TS_fun.ts_node_is_null node then None else Some node

let ts_node_child_by_field_name node field =
  node_to_opt @@ TS_fun.ts_node_child_by_field_name node field (uint32_len field)

(* Printing the tree *)
(*
let print_node (node: ts_tree) : unit =
  let ptr_char = TS_fun.ts_node_string node in
  Printf.printf "%s\n%!" @@ string_of_char_ptr ptr_char
*)

(* Converting a node to an OCaml string *)

let string_of_ts_node_type (node : ts_tree) : string =
  if TS_fun.ts_node_is_null node
  then failwith "Decoder.string_of_ts_node_type: Null node.";
  string_of_char_ptr @@ TS_fun.ts_node_type node

(* Parsing a string expected to contain a valid TypeScript program *)

let parse_typescript_string (source_code : string) : ts_tree_ptr =
  let parser = TS_fun.ts_parser_new ()
  and language = tree_sitter_typescript () in
  let (_ : bool) = TS_fun.ts_parser_set_language parser language in
  (*[true]*)
  let null_tree = from_voidp TS_types.ts_tree null in
  let parse_tree =
    TS_fun.ts_parser_parse_string
      parser
      null_tree
      source_code
      (UInt32.of_int @@ String.length source_code)
  in
  TS_fun.ts_parser_delete parser;
  parse_tree

(* Collating named children of a given node (we discard comment nodes) *)

let collect_named_children (node : ts_tree) : ts_tree list =
  if TS_fun.ts_node_is_null node
  then failwith "Decoder.collect_named_children: Null node.";
  let rec collect acc n =
    if UInt32.(equal zero n)
    then acc
    else (
      let index = UInt32.pred n in
      let child = TS_fun.ts_node_named_child node index in
      match string_of_ts_node_type child with
      | "comment" -> collect acc index
      | _ -> collect (child :: acc) index)
  in
  collect [] (TS_fun.ts_node_named_child_count node)

(* Collating all children of a given node (we discard comment nodes) *)

let collect_children (node : ts_tree) : ts_tree list =
  if TS_fun.ts_node_is_null node then failwith "Decoder.collect_children: Null node.";
  let rec collect acc n =
    if UInt32.(equal zero n)
    then acc
    else (
      let index = UInt32.pred n in
      let child = TS_fun.ts_node_child node index in
      match string_of_ts_node_type child with
      | "comment" -> collect acc index
      | _ -> collect (child :: acc) index)
  in
  collect [] (TS_fun.ts_node_child_count node)

(* Extracting a named child by its index *)

let ts_node_named_child_exn node index =
  let index = UInt32.of_int index
  and arity = TS_fun.ts_node_named_child_count node in
  match UInt32.compare index arity with
  | -1 -> TS_fun.ts_node_named_child node index
  | _ -> failwith "Decoder.ts_node_named_child_exn: Index out-of-bound."

let ts_node_named_child node index =
  let index = UInt32.of_int index
  and arity = TS_fun.ts_node_named_child_count node in
  match UInt32.compare index arity with
  | -1 -> Some (TS_fun.ts_node_named_child node index)
  | _ -> None

(* Extracting a child by its index *)

let ts_node_child_exn node index =
  let index = UInt32.of_int index
  and arity = TS_fun.ts_node_child_count node in
  match UInt32.compare index arity with
  | -1 -> TS_fun.ts_node_child node index
  | _ -> failwith "Decoder.ts_node_child_exn: Index out-of-bound."

let ts_node_child node index =
  let index = UInt32.of_int index
  and arity = TS_fun.ts_node_child_count node in
  match UInt32.compare index arity with
  | -1 -> Some (TS_fun.ts_node_child node index)
  | _ -> None

(* Extracting the name of a node *)

let get_name ?name node =
  match name with
  | None -> string_of_ts_node_type node
  | Some name -> name

(* Printing the ERROR and MISSING nodes *)

let print_error_node state ?name node = Tree.make_node state (get_name ?name node)
let print_missing_node state ?name node = Tree.make_node state (get_name ?name node)

(* Concluding a pattern matching with the remaining cases *)

let match_rest state ?name node print_default =
  let name = get_name ?name node in
  match name with
  (* Comments are ignored *)
  | "comment" -> ()
  (* Errors *)
  | "ERROR" -> print_error_node state ~name node
  | "MISSING" -> print_missing_node state ~name node
  (* Default case *)
  | _ -> print_default state ?name:(Some name) node

(* Anonymising a printer *)

let anon printer state node = printer state ?name:None node

(* Making a tree node *)

let make_node state ?name node =
  let name = get_name ?name node in
  Tree.make_node state name

(* Unexpected and TODO nodes *)

let print_unexpected_node state ?name node =
  let name = get_name ?name node in
  Tree.make_node state (name ^ "???")

let print_todo_node state ?name node =
  let name = get_name ?name node in
  Tree.make_node state (name ^ "!!!")

(* Filtering by name a list of nodes *)

let filter_by_name name nodes =
  let f node = String.equal name @@ string_of_ts_node_type node in
  Core.List.filter nodes ~f

let filter_first_by_name_exn name nodes =
  match filter_by_name name nodes with
  | node :: _ -> node
  | [] -> failwith ("Decoder.filter_one_by_name_exn: Missing node " ^ name ^ ".\n")

let filter_first_by_name name nodes =
  match filter_by_name name nodes with
  | node :: _ -> Some node
  | [] -> None

let has_node_named name nodes =
  match filter_by_name name nodes with
  | [] -> None
  | _ -> Some name

let has_child_named name node = has_node_named name @@ collect_named_children node

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
  | "generator_function_declaration" ->
    print_generator_function_declaration state ~name node
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
  | _ -> match_rest state ~name node print_unexpected_node

and print_export_statement state ?name node = Tree.make_node state (get_name ?name node)
and print_import_statement state ?name node = Tree.make_node state (get_name ?name node)
and print_debugger_statement state ?name node = Tree.make_node state (get_name ?name node)

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

(* Statement blocks *)

and print_statement_block state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_statement) children

and print_if_statement state ?name node = print_todo_node state ?name node
and print_switch_statement state ?name node = print_todo_node state ?name node
and print_for_statement state ?name node = print_todo_node state ?name node
and print_while_statement state ?name node = print_todo_node state ?name node
and print_do_statement state ?name node = print_todo_node state ?name node
and print_try_statement state ?name node = print_todo_node state ?name node
and print_with_statement state ?name node = print_todo_node state ?name node
and print_break_statement state ?name node = print_todo_node state ?name node
and print_continue_statement state ?name node = print_todo_node state ?name node
and print_return_statement state ?name node = print_todo_node state ?name node
and print_throw_statement state ?name node = print_todo_node state ?name node
and print_empty_statement state ?name node = print_todo_node state ?name node
and print_labeled_statement state ?name node = print_todo_node state ?name node

(* Declarations *)

and print_declaration state ?name node =
  let name = get_name ?name node in
  match name with
  | "function_declaration" -> print_function_declaration state ~name node
  | "generator_function_declaration" ->
    print_generator_function_declaration state ~name node
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
  | _ -> match_rest state ~name node print_unexpected_node

and print_function_declaration state ?name node = print_todo_node state ?name node

and print_generator_function_declaration state ?name node =
  print_todo_node state ?name node

and print_class_declaration state ?name node = print_todo_node state ?name node
and print_lexical_declaration state ?name node = print_todo_node state ?name node
and print_variable_declaration state ?name node = print_todo_node state ?name node
and print_function_signature state ?name node = print_todo_node state ?name node
and print_abstract_class_declaration state ?name node = print_todo_node state ?name node
and print_module state ?name node = print_todo_node state ?name node
and print_internal_module state ?name node = print_todo_node state ?name node

and print_type_alias_declaration state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and value_field = ts_node_child_by_field_name_exn node "value" in
  let children =
    Tree.
      [ mk_child (anon print_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_type) value_field
      ]
  in
  Tree.make state name children

and print_type_parameters state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_type_parameter) children

and print_type_parameter state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and constraint_field = ts_node_child_by_field_name node "constraint"
  and value_field = ts_node_child_by_field_name node "value" in
  let children =
    Tree.
      [ mk_child (anon print_identifier) name_field
      ; mk_child_opt (anon print_constraint) constraint_field
      ; mk_child_opt (anon print_default_type) value_field
      ]
  in
  Tree.make state name children

and print_constraint state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_default_type state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_enum_declaration state ?name node = print_todo_node state ?name node
and print_interface_declaration state ?name node = print_todo_node state ?name node
and print_import_alias state ?name node = print_todo_node state ?name node
and print_ambient_declaration state ?name node = print_todo_node state ?name node

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
  | _ -> match_rest state ~name node print_unexpected_node

and print_subscript_expression state ?name node = print_todo_node state ?name node
and print_member_expression state ?name node = print_todo_node state ?name node
and print_parenthesized_expression state ?name node = print_todo_node state ?name node
and print_import state ?name node = make_node state ?name node
and print_identifier state ?name node = make_node state ?name node
and print_undefined state ?name node = make_node state ?name node
and print_this state ?name node = make_node state ?name node
and print_super state ?name node = make_node state ?name node
and print_number state ?name node = make_node state ?name node
and print_string state ?name node = make_node state ?name node
and print_template_string state ?name node = make_node state ?name node
and print_regex state ?name node = make_node state ?name node
and print_true state ?name node = make_node state ?name node
and print_false state ?name node = make_node state ?name node
and print_null state ?name node = make_node state ?name node
and print_object state ?name node = print_todo_node state ?name node

(* Arrays *)

and print_array state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_array_cell) children

and print_array_cell state ?name node =
  let name = get_name ?name node in
  match name with
  | "spread_element" -> print_spread_element state ~name node
  | _ -> match_rest state ~name node print_expression

and print_spread_element state ?name node =
  let name = get_name ?name node
  and expression = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_expression) expression

(* *)

and function_expression state ?name node = print_todo_node state ?name node
and print_arrow_function state ?name node = print_todo_node state ?name node
and print_generator_function state ?name node = print_todo_node state ?name node
and print_class state ?name node = print_todo_node state ?name node
and print_meta_property state ?name node = print_todo_node state ?name node

(* Call expression *)

and print_call_expression state ?name node =
  let name = get_name ?name node
  and function_field = ts_node_child_by_field_name_exn node "function"
  and type_arguments_field = ts_node_child_by_field_name node "type_arguments"
  and arguments_field = ts_node_child_by_field_name_exn node "arguments"
  and print_function state node =
    let name = string_of_ts_node_type node in
    match name with
    | "import" -> print_import state ~name node
    | _ -> match_rest state ~name node print_expression
  in
  let children =
    Tree.
      [ mk_child print_function function_field
      ; mk_child_opt (anon print_type_arguments) type_arguments_field
      ; mk_child print_arguments arguments_field
      ]
  in
  Tree.make state name children

and print_type_arguments state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_arguments state node =
  let name = string_of_ts_node_type node in
  let children = collect_named_children node in
  Tree.of_list state name print_argument children

and print_argument state node =
  let name = get_name node in
  match name with
  | "spread_element" -> print_spread_element state ~name node
  | _ -> match_rest state ~name node print_expression

(* *)

and print_non_null_expression state ?name node = print_todo_node state ?name node

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
  | "function_type" -> print_function_type state ~name node
  | "readonly_type" -> print_readonly_type state ~name node
  | "constructor_type" -> print_constructor_type state ~name node
  | "infer_type" -> print_infer_type state ~name node
  (* A couple of aliases *)
  | "member_expression" -> print_member_expression state ~name node
  | "call_expression" -> print_call_expression state ~name node
  (* "primary_type" is hidden *)
  | _ -> match_rest state ~name node print_primary_type

and print_primary_type state ?name node =
  let name = get_name ?name node in
  match name with
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
  | _ -> match_rest state ~name node print_unexpected_node

and print_parenthesized_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_predefined_type state ?name node =
  let name = get_name ?name node in
  match collect_children node with
  | [] -> ()
  | child :: _ ->
    (* The tree-sitter parser for TypeScript has a bug: a child node
       "unique symbol" occurs repeated, for some mysterious
       reason. This a hack. Here is the production:

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
      | _ -> match_rest state ~name node print_unexpected_node
    in
    Tree.make_unary state name print child

and print_nested_type_identifier state ?name node = print_todo_node state ?name node
and print_generic_type state ?name node = print_todo_node state ?name node
and print_object_type state ?name node = print_todo_node state ?name node

and print_array_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_tuple_type state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_tuple_type_member) children

and print_tuple_type_member state ?name node =
  let name = get_name ?name node in
  match name with
  | "required_parameter" -> print_tuple_parameter state ~name node (* Alias *)
  | "optional_parameter" -> print_optional_tuple_parameter state ~name node (* Alias *)
  | "optional_type" -> print_optional_type state ~name node
  | "rest_type" -> print_rest_type state ~name node
  | _ -> match_rest state ~name node print_type (* "type" is a hidden rule *)

and print_tuple_parameter state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_field = ts_node_child_by_field_name_exn node "type"
  and print_name_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "identifier" -> print_identifier state ~name node
    | "rest_pattern" -> print_rest_pattern state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_name_field name_field
      ; mk_child (anon print_type_annotation) type_field
      ]
  in
  Tree.make state name children

and print_type_annotation state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_rest_pattern state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_lhs_expression) child

and print_lhs_expression state ?name node =
  let name = get_name ?name node in
  match name with
  | "member_expression" -> print_member_expression state ~name node
  | "subscript_expression" -> print_subscript_expression state ~name node
  | "identifier" -> print_identifier state ~name node
  | "undefined" -> print_undefined state ~name node
  | "object_pattern" -> print_object_pattern state ~name node
  | "array_pattern" -> print_array_pattern state ~name node
  | "non_null_expression" -> print_non_null_expression state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_optional_tuple_parameter state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_field = ts_node_child_by_field_name_exn node "type" in
  let children =
    Tree.
      [ mk_child (anon print_identifier) name_field
      ; mk_child (anon print_type_annotation) type_field
      ]
  in
  Tree.make state name children

and print_optional_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_rest_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_type_query state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "subscript_expression" -> print_type_query_subscript_expression state ~name node
    | "member_expression" -> print_type_query_member_expression state ~name node
    | "call_expression" -> print_type_query_call_expression state ~name node
    | "instantiation_expression" -> print_type_query_instantiation_expression state ~name node
    | "identifier" -> print_identifier state ~name node
    | "this" -> print_this state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in Tree.make_unary state name print child

and print_type_query_subscript_expression state ?name node =
  let name = get_name ?name node
  and object_field = ts_node_child_by_field_name_exn node "object"
  and index_field = ts_node_child_by_field_name_exn node "index" in
  let children =
    Tree.[ mk_child print_object_field object_field
         ; mk_child print_index_field index_field
         ]
  in Tree.make state name children

and print_index_field state node =
  let name = string_of_ts_node_type node in
  match name with
  | "predefined_type" -> print_predefined_type state ~name node
  | "string" -> make_node state ~name node
  | "number" -> print_number state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_type_query_member_expression state ?name node =
  let name = get_name ?name node
  and object_field = ts_node_child_by_field_name_exn node "object"
  and property_field = ts_node_child_by_field_name_exn node "property" in
  let children =
    Tree.[ mk_child print_object_field object_field
         ; mk_child print_property_field property_field
         ]
  in Tree.make state name children

and print_object_field state node =
  let name = string_of_ts_node_type node in
  match name with
  | "identifier" -> print_identifier state ~name node
  | "this" -> print_this state ~name node
  | "member_expression" -> print_type_query_member_expression state ~name node
  | "subscript_expression" -> print_type_query_subscript_expression state ~name node
  | "call_expression" -> print_type_query_call_expression state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_property_field state node =
  let name = string_of_ts_node_type node in
  match name with
  | "private_property_identifier" -> print_identifier state ~name node
  | "property_identifier" -> print_identifier state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_type_query_instantiation_expression state ?name node =
  let name = get_name ?name node
  and function_field = ts_node_child_by_field_name_exn node "function"
  and type_arguments_field = ts_node_child_by_field_name_exn node "type_arguments" in
  let children =
    Tree.[ mk_child print_function_field function_field
         ; mk_child (anon print_type_arguments) type_arguments_field
         ]
  in Tree.make state name children

and print_function_field state node =
  let name = string_of_ts_node_type node in
  match name with
  | "import" -> print_import state ~name node
  | "identifier" -> print_identifier state ~name node
  | "member_expression" -> print_type_query_member_expression state ~name node
  | "subscript_expression" -> print_type_query_subscript_expression state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_type_query_call_expression state ?name node =
  let name = get_name ?name node
  and function_field = ts_node_child_by_field_name_exn node "function"
  and arguments_field = ts_node_child_by_field_name_exn node "arguments" in
  let children =
    Tree.[ mk_child print_function_field function_field
         ; mk_child print_arguments arguments_field
         ]
  in Tree.make state name children

and print_index_type_query state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_primary_type) child

and print_existential_type state ?name node =
  make_node state ?name node

and print_literal_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  let print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "unary_expression" -> print_unary_expression state ~name node
    | "number" -> print_number state ~name node
    | "string" -> print_string state ~name node
    | "true" -> print_true state ~name node
    | "false" -> print_false state ~name node
    | "null" -> print_null state ~name node
    | "undefined" -> print_undefined state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.make_unary state name print child

and print_unary_expression state ?name node =
  let name = get_name ?name node
  and operator_field = ts_node_child_by_field_name_exn node "operator"
  and argument_field = ts_node_child_by_field_name_exn node "argument"
  and print_operator state node =
    let name = string_of_ts_node_type node in
    match name with
    | "+" -> make_node state ~name node
    | "-" -> make_node state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_operator operator_field
      ; mk_child (anon print_number) argument_field
      ]
  in
  Tree.make state name children

(* The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and print_lookup_type state ?name node =
  let name = get_name ?name node
  and primary_type_child = ts_node_named_child_exn node 0
  and type_child = ts_node_named_child_exn node 1 in
  let children =
    Tree.
      [ mk_child (anon print_primary_type) primary_type_child
      ; mk_child (anon print_type) type_child
      ]
  in
  Tree.make state name children

and print_conditional_type state ?name node =
  let name = get_name ?name node
  and left_field = ts_node_child_by_field_name_exn node "left"
  and right_field = ts_node_child_by_field_name_exn node "right"
  and consequence_field = ts_node_child_by_field_name_exn node "consequence"
  and alternative_field = ts_node_child_by_field_name_exn node "alternative" in
  let children =
    Tree.
      [ mk_child (anon print_type) left_field
      ; mk_child (anon print_type) right_field
      ; mk_child (anon print_type) consequence_field
      ; mk_child (anon print_type) alternative_field
      ]
  in
  Tree.make state name children

and print_template_literal_type state ?name node = make_node state ?name node

and print_intersection_type state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_union_type state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_function_type state ?name node =
  let name = get_name ?name node
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name_exn node "return_type"
  and print_return_type state node =
    let name = string_of_ts_node_type node in
    match name with
    | "asserts" -> print_asserts state ~name node
    | "type_predicate" -> print_type_predicate state ~name node
    | _ -> match_rest state ~name node print_type
  in
  let children =
    Tree.
      [ mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child print_return_type return_type_field
      ]
  in
  Tree.make state name children

and print_asserts state ?name node =
  let name = get_name ?name node in
  match name with
  | "type_predicate" -> print_type_predicate state ~name node
  | "identifier" -> print_identifier state ~name node
  | "this" -> print_this state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_type_predicate state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_field = ts_node_child_by_field_name_exn node "type" in
  let print_name_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "identifier" -> print_identifier state ~name node
    | "this" -> print_this state ~name node
    | _ -> match_rest state ~name node print_predefined_type
  in
  let children =
    Tree.[ mk_child print_name_field name_field; mk_child (anon print_type) type_field ]
  in
  Tree.make state name children

and print_readonly_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_constructor_type state ?name node =
  let name = get_name ?name node
  and abstract = has_child_named "abstract" node
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and type_field = ts_node_child_by_field_name_exn node "type" in
  let children =
    Tree.
      [ mk_child_opt make_node abstract
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child (anon print_type) type_field
      ]
  in
  Tree.make state name children

and print_formal_parameters state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_formal_parameter) children

and print_formal_parameter state ?name node =
  let name = get_name ?name node in
  match name with
  | "required_parameter" -> print_required_parameter state ~name node
  | "optional_parameter" -> print_optional_parameter state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_required_parameter state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  (* "_parameter_name" inlined: *)
  let decorators = filter_by_name "decorator" children
  and accessibility_modifier = filter_first_by_name "accessibility_modifier" children
  and override_modifier = filter_first_by_name "override_modifier" children
  and readonly = has_node_named "readonly" children
  and pattern_field = ts_node_child_by_field_name_exn node "pattern"
  (* *)
  and type_field = ts_node_child_by_field_name node "type"
  (* "_initializer" inlined: *)
  and value_field = ts_node_child_by_field_name node "value"
  and print_pattern_field state node =
    let name = get_name node in
    match name with
    | "this" -> print_this state ~name node
    | _ -> print_pattern state ~name node
  in
  let open Tree in
  let children =
    mk_children_list (anon print_decorator) decorators
    @ [ mk_child_opt (anon print_accessibility_modifier) accessibility_modifier
      ; mk_child_opt (anon print_override_modifier) override_modifier
      ; mk_child_opt make_node readonly
      ; mk_child print_pattern_field pattern_field
      ; mk_child_opt (anon print_type_annotation) type_field
      ; mk_child_opt (anon print_expression) value_field
      ]
  in
  make state name children

and print_decorator state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "identifier" -> print_identifier state ~name node
    | "member_expression" -> print_member_expression state ~name node
    | "call_expression" -> print_call_expression state ~name node
    | "parenthesized_expression" -> print_parenthesized_expression state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.make_unary state name print child

and print_accessibility_modifier state ?name node =
  let name = get_name ?name node
  and child = ts_node_child_exn node 0 in
  Tree.make_unary state name (anon make_node) child

and print_public state ?name node =
  let name = get_name ?name node in
  make_node state ~name node

and print_private state ?name node =
  let name = get_name ?name node in
  make_node state ~name node

and print_protected state ?name node =
  let name = get_name ?name node in
  make_node state ~name node

and print_override_modifier state ?name node =
  let name = get_name ?name node
  and child = ts_node_child_exn node 0 in
  Tree.make_unary state name (anon make_node) child

and print_optional_parameter state ?name node = print_required_parameter state ?name node

and print_infer_type state ?name node =
  let name = get_name ?name node
  and type_identifier_child = ts_node_named_child_exn node 0
  and type_child = ts_node_named_child node 1 in
  let children =
    Tree.
      [ mk_child (anon print_identifier) type_identifier_child
      ; mk_child_opt (anon print_type) type_child
      ]
  in
  Tree.make state name children

(* Patterns

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

and print_object_pattern state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_object_pattern_field) children

and print_object_pattern_field state ?name node =
  let name = get_name ?name node in
  match name with
  | "pair_pattern" -> print_pair_pattern state ~name node
  | "rest_pattern" -> print_rest_pattern state ~name node
  | "object_assignment_pattern" -> print_object_assignment_pattern state ~name node
  | "shorthand_property_identifier_pattern" ->
    print_shorthand_property_identifier_pattern state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_pair_pattern state ?name node =
  let name = get_name ?name node
  and key_field = ts_node_child_by_field_name_exn node "key"
  and value_field = ts_node_child_by_field_name_exn node "value"
  and print_pair_value_field state node =
    let name = get_name node in
    match name with
    | "assignment_pattern" -> print_assignment_pattern state ~name node
    | _ -> match_rest state ~name node print_pattern
  in
  let children =
    Tree.
      [ mk_child (anon print_property_name) key_field
      ; mk_child print_pair_value_field value_field
      ]
  in
  Tree.make state name children

and print_assignment_pattern state ?name node =
  let name = get_name ?name node
  and left_field = ts_node_child_by_field_name_exn node "left"
  and right_field = ts_node_child_by_field_name_exn node "right" in
  let children =
    Tree.
      [ mk_child (anon print_pattern) left_field
      ; mk_child (anon print_expression) right_field
      ]
  in
  Tree.make state name children

and print_property_name state ?name node =
  let name = get_name ?name node in
  match name with
  | "property_identifier" -> print_identifier state ~name node
  | "private_property_identifier" -> print_identifier state ~name node
  | "string" -> print_string state ~name node
  | "number" -> print_number state ~name node
  | "computed_property_name" -> print_computed_property_name state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_computed_property_name state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_expression) children

and print_object_assignment_pattern state ?name node =
  let name = get_name ?name node
  and left_field = ts_node_child_by_field_name_exn node "left"
  and right_field = ts_node_child_by_field_name_exn node "right"
  and print_left state node =
    let name = get_name node in
    match name with
    | "shorthand_property_identifier_pattern" ->
      print_shorthand_property_identifier_pattern state ~name node
    (* Rule "_destructuring_pattern" inlined: *)
    | "object_pattern" -> print_object_pattern state ~name node
    | "array_pattern" -> print_array_pattern state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.[ mk_child print_left left_field; mk_child (anon print_expression) right_field ]
  in
  Tree.make state name children

and print_shorthand_property_identifier_pattern state ?name node =
  make_node state ?name node

and print_array_pattern state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_array_pattern_cell) children

and print_array_pattern_cell state ?name node =
  let name = get_name ?name node in
  match name with
  | "assignment_pattern" -> print_assignment_pattern state ~name node
  | _ -> match_rest state ~name node print_pattern (* hidden rule *)

and print_pattern state ?name node =
  let name = get_name ?name node in
  match name with
  | "rest_pattern" -> print_rest_pattern state ~name node
  | _ -> print_lhs_expression state ~name node
