(* Misc *)

let ( <@ ) = Simple_utils.Ligo_fun.( <@ )

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

(* Jane Street's Core *)

open Core

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
  Tree.make_node state ("UNKNOWN: " ^ name)

let print_todo_node state ?name node =
  let name = get_name ?name node in
  Tree.make_node state ("TODO: " ^ name)

(* Filtering by name a list of nodes *)

let filter_by_name name nodes =
  let f = String.equal name <@ string_of_ts_node_type in
  List.filter nodes ~f

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
let first_child_named name node = filter_first_by_name name @@ collect_named_children node

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

and print_expression_statement state ?name node = print_expressions state ?name node

and print_expressions state ?name node =
  let name = get_name ?name node
  and child = ts_node_child_exn node 0
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "sequence_expression" -> print_sequence_expression state ~name node
    | _ -> print_expression state ~name node
  in
  Tree.make_unary state name print child

(* Statement blocks *)

and print_statement_block state ?name node =
  let name = get_name ?name node in
  let children = collect_named_children node in
  Tree.of_list state name (anon print_statement) children

(* If statement *)

and print_if_statement state ?name node =
  let name = get_name ?name node
  and condition_field = ts_node_child_by_field_name_exn node "condition"
  and consequence_field = ts_node_child_by_field_name_exn node "consequence"
  and alternative_field = ts_node_child_by_field_name node "alternative" in
  let children =
    Tree.
      [ mk_child (anon print_parenthesized_expression) condition_field
      ; mk_child (anon print_statement) consequence_field
      ; mk_child_opt (anon print_else_clause) alternative_field
      ]
  in
  Tree.make state name children

and print_else_clause state ?name node =
  let name = get_name ?name node
  and child = ts_node_child_exn node 1 in
  Tree.make_unary state name (anon print_statement) child

(* Switch statement *)

and print_switch_statement state ?name node =
  let name = get_name ?name node
  and value_field = ts_node_child_by_field_name_exn node "value"
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child (anon print_parenthesized_expression) value_field
      ; mk_child (anon print_switch_body) body_field
      ]
  in
  Tree.make state name children

and print_switch_body state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "switch_case" -> print_switch_case state ~name node
    | _ -> match_rest state ~name node print_switch_default
  in
  Tree.of_list state name print children

and print_switch_case state ?name node =
  let name = get_name ?name node
  and value_field = ts_node_child_by_field_name_exn node "value"
  and body_field = ts_node_child_by_field_name_exn node "body"
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "sequence_expression" -> print_sequence_expression state ~name node
    | _ -> print_expression state ~name node
  in
  let children =
    Tree.[ mk_child print value_field; mk_child (anon print_switch_body) body_field ]
  in
  Tree.make state name children

and print_switch_default state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_statement) children

(* For statement *)

and print_for_statement state ?name node =
  let name = get_name ?name node
  and initializer_field = ts_node_child_by_field_name_exn node "initializer"
  and condition_field = ts_node_child_by_field_name_exn node "condition"
  and increment_field = ts_node_child_by_field_name node "increment"
  and body_field = ts_node_child_by_field_name_exn node "body"
  and print_initializer state node =
    let name = string_of_ts_node_type node in
    match name with
    | "lexical_declaration" -> print_lexical_declaration state ~name node
    | "variable_declaration" -> print_variable_declaration state ~name node
    | "expression_statement" -> print_expression_statement state ~name node
    | "empty_statement" -> print_empty_statement state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  and print_condition state node =
    let name = string_of_ts_node_type node in
    match name with
    | "expression_statement" -> print_expression_statement state ~name node
    | "empty_statement" -> print_empty_statement state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  and print_increment state node =
    let name = string_of_ts_node_type node in
    match name with
    | "sequence_expression" -> print_sequence_expression state ~name node
    | _ -> print_expression state ~name node
  in
  let children =
    Tree.
      [ mk_child print_initializer initializer_field
      ; mk_child print_condition condition_field
      ; mk_child_opt print_increment increment_field
      ; mk_child (anon print_statement) body_field
      ]
  in
  Tree.make state name children

(* While statement *)

and print_while_statement state ?name node =
  let name = get_name ?name node
  and condition_field = ts_node_child_by_field_name_exn node "condition"
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child (anon print_parenthesized_expression) condition_field
      ; mk_child (anon print_statement) body_field
      ]
  in
  Tree.make state name children

(* Do statement *)

and print_do_statement state ?name node =
  let name = get_name ?name node
  and body_field = ts_node_child_by_field_name_exn node "body"
  and condition_field = ts_node_child_by_field_name_exn node "condition" in
  let children =
    Tree.
      [ mk_child (anon print_statement) body_field
      ; mk_child (anon print_parenthesized_expression) condition_field
      ]
  in
  Tree.make state name children

(* Try statement *)

and print_try_statement state ?name node =
  let name = get_name ?name node
  and body_field = ts_node_child_by_field_name_exn node "body"
  and handler_field = ts_node_child_by_field_name node "handler"
  and finalizer_field = ts_node_child_by_field_name node "finalizer" in
  let children =
    Tree.
      [ mk_child (anon print_statement_block) body_field
      ; mk_child_opt (anon print_catch_clause) handler_field
      ; mk_child_opt (anon print_finally_clause) finalizer_field
      ]
  in
  Tree.make state name children

and print_catch_clause state ?name node =
  let name = get_name ?name node
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    match ts_node_child_by_field_name node "parameter" with
    | Some parameter_field ->
      let print_parameter state node =
        let name = string_of_ts_node_type node in
        match name with
        | "identifier" -> print_identifier state ~name node
        | _ -> match_rest state ~name node print_destructuring_pattern
      in
      let type_field = ts_node_child_by_field_name node "type" in
      Tree.
        [ mk_child print_parameter parameter_field
        ; mk_child_opt (anon print_type_annotation) type_field
        ; mk_child (anon print_statement_block) body_field
        ]
    | None -> Tree.[ mk_child (anon print_statement_block) body_field ]
  in
  Tree.make state name children

and print_finally_clause state ?name node =
  let name = get_name ?name node
  and body_field = ts_node_child_by_field_name_exn node "body" in
  Tree.make_unary state name (anon print_statement_block) body_field

(* With statement *)

and print_with_statement state ?name node =
  let name = get_name ?name node
  and object_field = ts_node_child_by_field_name_exn node "object"
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child (anon print_parenthesized_expression) object_field
      ; mk_child (anon print_statement) body_field
      ]
  in
  Tree.make state name children

(* Break statement *)

and print_break_statement state ?name node =
  let name = get_name ?name node
  and label_field = ts_node_child_by_field_name node "label" in
  let children = Tree.[ mk_child_opt (anon print_identifier) label_field ] in
  Tree.make state name children

(* Continue statement *)

and print_continue_statement state ?name node =
  let name = get_name ?name node
  and label_field = ts_node_child_by_field_name node "label" in
  let children = Tree.[ mk_child_opt (anon print_identifier) label_field ] in
  Tree.make state name children

(* Return statement *)

and print_return_statement state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child node 0
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "sequence_expression" -> print_sequence_expression state ~name node
    | _ -> print_expression state ~name node
  in
  let children = Tree.[ mk_child_opt print child ] in
  Tree.make state name children

(* Throw statement *)

and print_throw_statement state ?name node = print_expressions state ?name node

(* Empty statement *)

and print_empty_statement state ?name node = make_node state ?name node

(* Labeled statement *)

and print_labeled_statement state ?name node =
  let name = get_name ?name node
  and label_field = ts_node_child_by_field_name_exn node "label"
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child (anon print_identifier) label_field
      ; mk_child (anon print_statement) body_field
      ]
  in
  Tree.make state name children

(* DECLARATION *)

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

(* Function declaration (see [print_function_signature]) *)

and print_function_declaration state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let async = has_node_named "async" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child_opt make_node async
      ; mk_child (anon print_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ; mk_child (anon print_statement_block) body_field
      ]
  in
  Tree.make state name children

(* Generator function declaration (see function declaration) *)

and print_generator_function_declaration state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let async = has_node_named "async" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child_opt make_node async
      ; mk_child (anon print_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ; mk_child (anon print_statement_block) body_field
      ]
  in
  Tree.make state name children

(* Class declaration (see [print_class] *)

and print_class_declaration state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let decorators = filter_by_name "decorator" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and heritage_child = filter_first_by_name "class_heritage" children
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let open Tree in
  let children =
    mk_children_list (anon print_decorator) decorators
    @ [ mk_child (anon print_type_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child_opt (anon print_class_heritage) heritage_child
      ; mk_child (anon print_class_body) body_field
      ]
  in
  Tree.make state name children

(* Lexical declaration (see [print_variable_declaration]) *)

and print_lexical_declaration state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node
  and kind_field = ts_node_child_by_field_name_exn node "kind" in
  let var_decls = filter_by_name "variable_declarator" children
  and print_set_or_const state node =
    let name = string_of_ts_node_type node in
    match name with
    | "let" -> make_node state ~name node
    | "const" -> make_node state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.mk_child print_set_or_const kind_field
    :: Tree.mk_children_list (anon print_variable_declarator) var_decls
  in
  Tree.make state name children

and print_variable_declarator state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and value_field = ts_node_child_by_field_name node "value"
  and print_name_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "identifier" -> print_identifier state ~name node
    | _ -> match_rest state ~name node print_destructuring_pattern
  in
  let children =
    Tree.
      [ mk_child print_name_field name_field
      ; mk_child_opt (anon print_expression) value_field
      ]
  in
  Tree.make state name children

(* Variable declaration (see [print_lexical_declaration]) *)

and print_variable_declaration state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  let var_decls = filter_by_name "variable_declarator" children in
  let children = Tree.mk_children_list (anon print_variable_declarator) var_decls in
  Tree.make state name children

(* Function signature (See [print_function_declaration]) *)

and print_function_signature state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let async = has_node_named "async" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type" in
  (* "statement_block" *)
  let children =
    Tree.
      [ mk_child_opt make_node async
      ; mk_child (anon print_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ]
  in
  Tree.make state name children

(* Abstract class declaration ( see [print_class_declaration]) *)

and print_abstract_class_declaration state ?name node =
  print_class_declaration state ?name node

(* Module *)

and print_module state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and body_field = ts_node_child_by_field_name node "body"
  and print_name state node =
    let name = string_of_ts_node_type node in
    match name with
    | "string" -> print_string state ~name node
    | "identifier" -> print_identifier state ~name node
    | "nested_identifier" -> print_nested_identifier state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_name name_field
      ; mk_child_opt (anon print_statement_block) body_field
      ]
  in
  Tree.make state name children

(* Internal module (a.k.a. namespaces) *)

and print_internal_module state ?name node = print_module state ?name node

(* Type alias declaration *)

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

(* Type parameters *)

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

(* Enum declaration *)

and print_enum_declaration state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child (anon print_identifier) name_field
      ; mk_child (anon print_enum_body) body_field
      ]
  in
  Tree.make state name children

and print_enum_body state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "enum_assignment" -> print_enum_assignment state ~name node
    | _ -> match_rest state ~name node print_property_name
  in
  Tree.of_list state name print children

and print_enum_assignment state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and value_field = ts_node_child_by_field_name node "value" in
  let children =
    Tree.
      [ mk_child (anon print_property_name) name_field
      ; mk_child_opt (anon print_expression) value_field
      ]
  in
  Tree.make state name children

(* Interface declaration *)

and print_interface_declaration state ?name node =
  let name = get_name ?name node
  and children = collect_children node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let extends_type_clause = filter_first_by_name "extends_type_clause" children in
  let children =
    Tree.
      [ mk_child (anon print_type_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child_opt (anon print_extends_type_clause) extends_type_clause
      ; mk_child (anon print_interface_body) body_field
      ]
  in
  Tree.make state name children

and print_interface_body state ?name node = print_object_type state ?name node

and print_extends_type_clause state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "type_identifier" -> print_type_identifier state ~name node
    | "nested_type_identifier" -> print_nested_type_identifier state ~name node
    | "generic_type" -> print_generic_type state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.of_list state name print children

(* Import alias *)

and print_import_alias state ?name node =
  let name = get_name ?name node
  and lhs = ts_node_child_exn node 1
  and rhs = ts_node_child_exn node 3
  and print_rhs state node =
    let name = string_of_ts_node_type node in
    match name with
    | "identifier" -> print_identifier state ~name node
    | "nested_identifier" -> print_nested_identifier state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children = Tree.[ mk_child (anon print_identifier) lhs; mk_child print_rhs rhs ] in
  Tree.make state name children

(* Ambient declaration *)

and print_ambient_declaration state ?name node =
  let name = get_name ?name node
  and fst_child = ts_node_named_child_exn node 0 in
  let child_name = string_of_ts_node_type fst_child in
  let children =
    match child_name with
    | "statement_block" -> Tree.[ mk_child (anon print_statement_block) fst_child ]
    | "property_identifier" ->
      let type_child = ts_node_child_exn node 5 in
      Tree.
        [ mk_child (anon print_identifier) fst_child
        ; mk_child (anon print_type) type_child
        ]
    | _ -> Tree.[ mk_child (anon print_declaration) fst_child ]
  in
  Tree.make state name children

(* EXPRESSION

   The JavasScript tree-sitter grammar have the non-terminals
   "expression" and "primary_expression" be supertypes, that is,
   hidden rules. Therefore we have to match all the RHS of those
   non-terminals in [print_expression]. *)

and print_expression state ?name node =
  let name = get_name ?name node in
  match name with
  (* "primary_expression" inlined: *)
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
  | "function_expression" -> print_function_expression state ~name node
  | "arrow_function" -> print_arrow_function state ~name node
  | "generator_function" -> print_generator_function state ~name node
  | "class" -> print_class state ~name node
  | "meta_property" -> print_meta_property state ~name node
  | "call_expression" -> print_call_expression state ~name node
  | "non_null_expression" -> print_non_null_expression state ~name node
  (* Rest of "expression": *)
  | "glimmer_template" -> print_glimmer_template state ~name node
  | "assignment_expression" -> print_assignment_expression state ~name node
  | "augmented_assignment_expression" ->
    print_augmented_assignment_expression state ~name node
  | "await_expression" -> print_await_expression state ~name node
  | "unary_expression" -> print_unary_expression state ~name node
  | "binary_expression" -> print_binary_expression state ~name node
  | "ternary_expression" -> print_ternary_expression state ~name node
  | "update_expression" -> print_update_expression state ~name node
  | "new_expression" -> print_new_expression state ~name node
  | "yield_expression" -> print_yield_expression state ~name node
  | "as_expression" -> print_as_expression state ~name node
  | "satisfies_expression" -> print_satisfies_expression state ~name node
  | "instantiation_expression" -> print_instantiation_expression state ~name node
  | "internal_module" -> print_internal_module state ~name node
  | "type_assertion" -> print_type_assertion state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

(* Glimmer template (not supported) *)

and print_glimmer_template state ?name node = make_node state ?name node

(* Assignment expression *)

and print_assignment_expression state ?name node =
  let name = get_name ?name node
  and using =
    let first_child = ts_node_child_exn node 0 in
    match string_of_ts_node_type first_child with
    | "using" -> Some "using"
    | _ -> None
  and left_field = ts_node_child_by_field_name_exn node "left"
  and right_field = ts_node_child_by_field_name_exn node "right"
  and print_left state node =
    let name = string_of_ts_node_type node in
    match name with
    | "parenthesized_expression" -> print_parenthesized_expression state ~name node
    | _ -> match_rest state ~name node print_lhs_expression
  in
  let children =
    Tree.
      [ mk_child_opt make_node using
      ; mk_child print_left left_field
      ; mk_child (anon print_expression) right_field
      ]
  in
  Tree.make state name children

(* Augmented assignment expression *)

and print_augmented_assignment_expression state ?name node =
  let name = get_name ?name node
  and left_field = ts_node_child_by_field_name_exn node "left"
  and right_field = ts_node_child_by_field_name_exn node "right"
  and operator = ts_node_child_by_field_name_exn node "operator"
  and print_left state node =
    let name = string_of_ts_node_type node in
    (* "_augmented_assignment_lhs" is inlined here (hidden rule): *)
    match name with
    | "member_expression" -> print_member_expression state ~name node
    | "subscript_expression" -> print_subscript_expression state ~name node
    | "identifier" -> print_identifier state ~name node
    | "parenthesized_expression" -> print_parenthesized_expression state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  and print_operator state node =
    let name = string_of_ts_node_type node in
    match name with
    | "+=" -> make_node state ~name node
    | "-=" -> make_node state ~name node
    | "*=" -> make_node state ~name node
    | "/=" -> make_node state ~name node
    | "%=" -> make_node state ~name node
    | "^=" -> make_node state ~name node
    | "&=" -> make_node state ~name node
    | "|=" -> make_node state ~name node
    | ">>=" -> make_node state ~name node
    | ">>>=" -> make_node state ~name node
    | "<<=" -> make_node state ~name node
    | "**=" -> make_node state ~name node
    | "&&=" -> make_node state ~name node
    | "||=" -> make_node state ~name node
    | "??=" -> make_node state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_operator operator
      ; mk_child print_left left_field
      ; mk_child (anon print_expression) right_field
      ]
  in
  Tree.make state name children

(* Await expression *)

and print_await_expression state ?name node =
  let name = get_name ?name node
  and expression = ts_node_child_exn node 1 in
  Tree.make_unary state name (anon print_expression) expression

(* Binary expression *)

and print_binary_expression state ?name node =
  let name = get_name ?name node
  and left_field = ts_node_child_by_field_name_exn node "left"
  and right_field = ts_node_child_by_field_name_exn node "right"
  and operator = ts_node_child_by_field_name_exn node "operator"
  and print_left state node =
    let name = string_of_ts_node_type node in
    match name with
    | "private_property_identifier" -> print_identifier state ~name node
    | _ -> match_rest state ~name node print_expression
  and print_operator state node =
    let name = string_of_ts_node_type node in
    match name with
    | "&&" -> make_node state ~name node
    | "||" -> make_node state ~name node
    | ">>" -> make_node state ~name node
    | ">>>" -> make_node state ~name node
    | "<<" -> make_node state ~name node
    | "&" -> make_node state ~name node
    | "^" -> make_node state ~name node
    | "|" -> make_node state ~name node
    | "+" -> make_node state ~name node
    | "-" -> make_node state ~name node
    | "*" -> make_node state ~name node
    | "/" -> make_node state ~name node
    | "%" -> make_node state ~name node
    | "**" -> make_node state ~name node
    | "<" -> make_node state ~name node
    | "<=" -> make_node state ~name node
    | "==" -> make_node state ~name node
    | "===" -> make_node state ~name node
    | "!=" -> make_node state ~name node
    | "!==" -> make_node state ~name node
    | ">=" -> make_node state ~name node
    | ">" -> make_node state ~name node
    | "??" -> make_node state ~name node
    | "instanceof" -> make_node state ~name node
    | "in" -> make_node state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_operator operator
      ; mk_child print_left left_field
      ; mk_child (anon print_expression) right_field
      ]
  in
  Tree.make state name children

(* Ternary expression *)

and print_ternary_expression state ?name node =
  let name = get_name ?name node
  and condition_field = ts_node_child_by_field_name_exn node "condition"
  and consequence_field = ts_node_child_by_field_name_exn node "consequence"
  and alternative_field = ts_node_child_by_field_name_exn node "alternative" in
  let children =
    Tree.
      [ mk_child (anon print_expression) condition_field
      ; mk_child (anon print_expression) consequence_field
      ; mk_child (anon print_expression) alternative_field
      ]
  in
  Tree.make state name children

(* Update expression *)

and print_update_expression state ?name node =
  let name = get_name ?name node
  and argument_field = ts_node_child_by_field_name_exn node "argument"
  and first_child = ts_node_child_exn node 0 in
  let children =
    match string_of_ts_node_type first_child with
    | "++" ->
      Tree.[ mk_child make_node "++"; mk_child (anon print_expression) argument_field ]
    | "--" ->
      Tree.[ mk_child make_node "--"; mk_child (anon print_expression) argument_field ]
    | _ ->
      let snd_child = ts_node_child_exn node 1 in
      (match string_of_ts_node_type snd_child with
      | "++" ->
        Tree.[ mk_child (anon print_expression) argument_field; mk_child make_node "++" ]
      | "--" ->
        Tree.[ mk_child (anon print_expression) argument_field; mk_child make_node "--" ]
      | _ -> [] (* Should not happen. *))
  in
  Tree.make state name children

(* New expression

   Note that the constructor field is a primary expression, but
   "primary_expression" is a supertype, that is, a hidden rule. We
   assume it is a "expression", since primary expressions are a subset
   of them. *)

and print_new_expression state ?name node =
  let name = get_name ?name node
  and constructor_field = ts_node_child_by_field_name_exn node "constructor"
  and type_arguments_field = ts_node_child_by_field_name node "type_arguments"
  and arguments_field = ts_node_child_by_field_name node "arguments" in
  let children =
    Tree.
      [ mk_child (anon print_expression) constructor_field
      ; mk_child_opt (anon print_type_arguments) type_arguments_field
      ; mk_child_opt (anon print_arguments) arguments_field
      ]
  in
  Tree.make state name children

(* Yield expression *)

and print_yield_expression state ?name node =
  let name = get_name ?name node in
  match ts_node_child node 1 with
  | None -> make_node state ~name node
  | Some child ->
    let child =
      match string_of_ts_node_type child with
      | "*" -> ts_node_child_exn node 2
      | _ -> child
    in
    Tree.make_unary state name (anon print_expression) child

(* As-expression *)

and print_as_expression state ?name node =
  let name = get_name ?name node
  and expression = ts_node_child_exn node 0
  and as_what = ts_node_child_exn node 2
  and print_as state node =
    let name = string_of_ts_node_type node in
    match name with
    | "const" -> make_node state ~name node
    | _ -> match_rest state ~name node print_type
  in
  let children =
    Tree.[ mk_child (anon print_expression) expression; mk_child print_as as_what ]
  in
  Tree.make state name children

(* Statisfies-expression *)

and print_satisfies_expression state ?name node =
  let name = get_name ?name node
  and expression = ts_node_named_child_exn node 0
  and type_child = ts_node_named_child_exn node 1 in
  let children =
    Tree.
      [ mk_child (anon print_expression) expression
      ; mk_child (anon print_type) type_child
      ]
  in
  Tree.make state name children

(* Instantiation expression *)

and print_instantiation_expression state ?name node =
  let name = get_name ?name node
  and expression = ts_node_named_child_exn node 0
  and type_arguments_field = ts_node_child_by_field_name_exn node "type_arguments" in
  let children =
    Tree.
      [ mk_child (anon print_expression) expression
      ; mk_child (anon print_type_arguments) type_arguments_field
      ]
  in
  Tree.make state name children

(* Type assertion *)

and print_type_assertion state ?name node =
  let name = get_name ?name node
  and type_arguments = ts_node_named_child_exn node 0
  and expression = ts_node_named_child_exn node 1 in
  let children =
    Tree.
      [ mk_child (anon print_type_arguments) type_arguments
      ; mk_child (anon print_expression) expression
      ]
  in
  Tree.make state name children

(* Subscript expression (see [print_member_expression]) *)

and print_subscript_expression state ?name node =
  let name = get_name ?name node
  and object_field = ts_node_child_by_field_name_exn node "object"
  and optional_chain_field = ts_node_child_by_field_name node "optional_chain"
  and index_field = ts_node_child_by_field_name_exn node "index"
  and print_chain state node =
    let name = string_of_ts_node_type node in
    match name with
    | "optional_chain" -> make_node state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  and print_index state node =
    let name = string_of_ts_node_type node in
    match name with
    | "sequence_expression" -> print_sequence_expression state ~name node
    | _ -> print_expression state ~name node
  in
  let children =
    Tree.
      [ mk_child (anon print_expression) object_field
      ; mk_child_opt print_chain optional_chain_field
      ; mk_child print_index index_field
      ]
  in
  Tree.make state name children

(* Member expression *)

and print_member_expression state ?name node =
  let name = get_name ?name node
  and object_field = ts_node_child_by_field_name_exn node "object"
  and optional_chain_field = ts_node_child_by_field_name node "optional_chain"
  and property_field = ts_node_child_by_field_name_exn node "property"
  and print_object state node =
    let name = string_of_ts_node_type node in
    match name with
    | "import" -> print_import state ~name node
    | _ -> match_rest state ~name node print_expression
  and print_selector state = function
    | None -> () (* "." *)
    | Some node ->
      (* "?." *)
      Tree.make_node state @@ string_of_ts_node_type node
  in
  let children =
    Tree.
      [ mk_child print_object object_field
      ; mk_child print_selector optional_chain_field
      ; mk_child print_property_field property_field
      ]
  in
  Tree.make state name children

(* Parenthesised expression *)

and print_parenthesized_expression state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "sequence_expression" -> print_sequence_expression state ~name node
    | _ -> print_expression state ~name node
  in
  Tree.make_unary state name print child

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

(* Object *)

and print_object state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  let print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "pair" -> print_pair state ~name node
    | "spread_element" -> print_spread_element state ~name node
    | "method_definition" -> print_method_definition state ~name node
    | "shorthand_property_identifier" ->
      print_shorthand_property_identifier_pattern state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.of_list state name print children

and print_pair state ?name node =
  let name = get_name ?name node
  and key_field = ts_node_child_by_field_name_exn node "key"
  and value_field = ts_node_child_by_field_name_exn node "value" in
  let children =
    Tree.
      [ mk_child (anon print_property_name) key_field
      ; mk_child (anon print_expression) value_field
      ]
  in
  Tree.make state name children

(* Array (expression) *)

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

(* Function (expression) *)

and print_function_expression state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let async = has_node_named "async" children
  and name_field = ts_node_child_by_field_name node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child_opt make_node async
      ; mk_child_opt (anon print_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ; mk_child (anon print_statement_block) body_field
      ]
  in
  Tree.make state name children

(* Arrow function *)

and print_arrow_function state ?name node =
  let name = get_name ?name node
  and async =
    let first_child = ts_node_child_exn node 0 in
    match string_of_ts_node_type first_child with
    | "async" -> Some "async"
    | _ -> None
  and parameter_field = ts_node_child_by_field_name node "parameter"
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    match parameter_field with
    | Some parameter_field ->
      Tree.
        [ mk_child_opt make_node async
        ; mk_child (anon print_identifier) parameter_field
        ; mk_child (anon print_arrow_function_body) body_field
        ]
    | None ->
      (* "_call_signature" inlined: *)
      let type_parameters_field = ts_node_child_by_field_name node "type_parameters"
      and parameters_field = ts_node_child_by_field_name_exn node "parameters"
      and return_type_field = ts_node_child_by_field_name node "return_type" in
      Tree.
        [ mk_child_opt make_node async
        ; mk_child_opt (anon print_type_parameters) type_parameters_field
        ; mk_child (anon print_formal_parameters) parameters_field
        ; mk_child_opt (anon print_return_type) return_type_field
        ; mk_child (anon print_arrow_function_body) body_field
        ]
  in
  Tree.make state name children

and print_arrow_function_body state ?name node =
  let name = get_name ?name node in
  match name with
  | "statement_block" -> print_statement_block state ~name node
  | _ -> match_rest state ~name node print_expression

(* Generator function *)

and print_generator_function state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let async = has_node_named "async" children
  and name_field = ts_node_child_by_field_name node "name"
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child_opt make_node async
      ; mk_child_opt (anon print_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ; mk_child (anon print_statement_block) body_field
      ]
  in
  Tree.make state name children

(* Class *)

and print_class state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let decorators = filter_by_name "decorator" children
  and name_field = ts_node_child_by_field_name node "name"
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and heritage_child = filter_first_by_name "class_heritage" children
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let open Tree in
  let children =
    mk_children_list (anon print_decorator) decorators
    @ [ mk_child_opt (anon print_type_identifier) name_field
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child_opt (anon print_class_heritage) heritage_child
      ; mk_child (anon print_class_body) body_field
      ]
  in
  Tree.make state name children

and print_class_heritage state ?name node =
  let name = get_name ?name node in
  let children =
    match first_child_named "extends_clause" node with
    | Some extends_clause ->
      let implements_clause = first_child_named "implements_clause" node in
      Tree.
        [ mk_child (anon print_extends_clause) extends_clause
        ; mk_child_opt (anon print_implements_clause) implements_clause
        ]
    | None ->
      (* [implements_clause] is never [None]. *)
      let implements_clause = first_child_named "implements_clause" node in
      Tree.[ mk_child_opt (anon print_implements_clause) implements_clause ]
  in
  Tree.make state name children

and print_implements_clause state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_extends_clause state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  let rec pair_up acc = function
    | value :: snd :: nodes ->
      if String.equal (string_of_ts_node_type snd) "type_arguments"
      then pair_up ((value, Some snd) :: acc) nodes
      else pair_up ((value, None) :: acc) (snd :: nodes)
    | [ value ] -> List.rev ((value, None) :: acc)
    | [] -> List.rev acc
  in
  let pairs = pair_up [] children in
  let mk_children (value, type_arguments_opt) acc =
    let value_child = Tree.mk_child (anon print_expression) value in
    match type_arguments_opt with
    | None -> value_child :: acc
    | Some type_arguments ->
      value_child :: Tree.mk_child (anon print_type_arguments) type_arguments :: acc
  in
  let children = List.fold_right ~f:mk_children pairs ~init:[] in
  Tree.make state name children

and print_class_body state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  let decorators = filter_by_name "decorator" children in
  let print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "decorator" -> ()
    | "method_definition" ->
      List.iter ~f:(print_decorator state) decorators;
      print_method_definition state ~name node
    | "method_signature" -> print_method_signature state ~name node
    | "class_static_block" -> print_class_static_block state ~name node
    | "abstract_method_signature" -> print_abstract_method_signature state ~name node
    | "index_signature" -> print_index_signature state ~name node
    | "public_field_definition" -> print_public_field_definition state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.of_list state name print children

and print_method_definition state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let accessibility_modifier = filter_first_by_name "accessibility_modifier" children
  and static = has_node_named "static" children
  and override_modifier = filter_first_by_name "override_modifier" children
  and readonly = has_node_named "readonly" children
  and async = has_node_named "async" children
  and set = has_node_named "set" children
  and get = has_node_named "get" children
  and star = has_node_named "*" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  and qmark = has_node_named "?" children
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type"
  (* "statement_block" *)
  and body_field = ts_node_child_by_field_name_exn node "body" in
  let children =
    Tree.
      [ mk_child_opt (anon print_accessibility_modifier) accessibility_modifier
      ; mk_child_opt make_node static
      ; mk_child_opt (anon print_override_modifier) override_modifier
      ; mk_child_opt make_node readonly
      ; mk_child_opt make_node async
      ; mk_child_opt make_node set
      ; mk_child_opt make_node get
      ; mk_child_opt make_node star
      ; mk_child (anon print_property_name) name_field
      ; mk_child_opt make_node qmark
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ; mk_child (anon print_statement_block) body_field
      ]
  in
  Tree.make state name children

and print_return_type state ?name node =
  let name = get_name ?name node in
  match name with
  | "type_annotation" -> print_type_annotation state ~name node
  | "asserts_annotation" -> print_asserts_annotation state ~name node
  | _ -> match_rest state ~name node print_type_predicate_annotation

and print_class_static_block state ?name node =
  let name = get_name ?name node
  and body_field = ts_node_child_by_field_name_exn node "body" in
  Tree.make_unary state name (anon print_statement_block) body_field

and print_abstract_method_signature state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let accessibility_modifier = filter_first_by_name "accessibility_modifier" children
  and abstract = has_node_named "abstract" children
  and override_modifier = filter_first_by_name "override_modifier" children
  and set = has_node_named "set" children
  and get = has_node_named "get" children
  and star = has_node_named "*" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  and qmark = has_node_named "?" children
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type" in
  let children =
    Tree.
      [ mk_child_opt (anon print_accessibility_modifier) accessibility_modifier
      ; mk_child_opt make_node abstract
      ; mk_child_opt (anon print_override_modifier) override_modifier
      ; mk_child_opt make_node set
      ; mk_child_opt make_node get
      ; mk_child_opt make_node star
      ; mk_child (anon print_property_name) name_field
      ; mk_child_opt make_node qmark
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ]
  in
  Tree.make state name children

and print_public_field_definition state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let decorators = filter_by_name "decorator" children
  and accessibility_modifier = filter_first_by_name "accessibility_modifier" children
  and override_modifier = filter_first_by_name "override_modifier" children
  and declare = has_node_named "declare" children
  and static = has_node_named "static" children
  and readonly = has_node_named "readonly" children
  and accessor = has_node_named "accessor" children
  and abstract = has_node_named "abstract" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_field = ts_node_child_by_field_name node "type"
  (* "_initializer" inlined: *)
  and value_field = ts_node_child_by_field_name node "value"
  and qmark = has_node_named "?" children
  and emark = has_node_named "!" children in
  let open Tree in
  let children =
    mk_children_list (anon print_decorator) decorators
    @ [ mk_child_opt (anon print_accessibility_modifier) accessibility_modifier
      ; mk_child_opt make_node declare
      ; mk_child_opt (anon print_override_modifier) override_modifier
      ; mk_child_opt make_node static
      ; mk_child_opt make_node readonly
      ; mk_child_opt make_node accessor
      ; mk_child_opt make_node abstract
      ; mk_child (anon print_property_name) name_field
      ; mk_child_opt make_node qmark
      ; mk_child_opt make_node emark
      ; mk_child_opt (anon print_type_annotation) type_field
      ; mk_child_opt (anon print_expression) value_field
      ]
  in
  Tree.make state name children

(* Meta-property *)

and print_meta_property state ?name node =
  let name = get_name ?name node
  and meta_child = ts_node_child_exn node 0
  and print state node =
    let name = get_name node in
    match name with
    | "new" -> Tree.make_node state "new.target"
    | "import" -> Tree.make_node state "import.meta"
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.make_unary state name print meta_child

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
      ; mk_child (anon print_arguments) arguments_field
      ]
  in
  Tree.make state name children

and print_type_arguments state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

and print_arguments state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_argument) children

and print_argument state ?name node =
  let name = get_name ?name node in
  match name with
  | "spread_element" -> print_spread_element state ~name node
  | _ -> match_rest state ~name node print_expression

(* Non-null expression *)

and print_non_null_expression state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_expression) child

(* Sequence expression *)

and print_sequence_expression state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_expression) children

(* TYPE

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
  | "type_identifier" -> print_type_identifier state ~name node
  | "nested_type_identifier" -> print_nested_type_identifier state ~name node
  | "generic_type" -> print_generic_type state ~name node
  | "object_type" -> print_object_type state ~name node
  | "array_type" -> print_array_type state ~name node
  | "tuple_type" -> print_tuple_type state ~name node
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

(* Type identifier *)

and print_type_identifier state ?name node = print_identifier state ?name node

(* Parenthesized type *)

and print_parenthesized_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

(* Predefined type *)

and print_predefined_type state ?name node =
  let name = get_name ?name node in
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

(* Nested type identifier *)

and print_nested_type_identifier state ?name node =
  let name = get_name ?name node
  and module_field = ts_node_child_by_field_name_exn node "module"
  and name_field = ts_node_child_by_field_name_exn node "name"
  and print_module_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "identifier" -> print_identifier state ~name node
    | "nested_identifier" -> print_nested_identifier state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_module_field module_field
      ; mk_child (anon print_type_identifier) name_field
      ]
  in
  Tree.make state name children

(* Nested identifier *)

and print_nested_identifier state ?name node =
  let name = get_name ?name node
  and object_field = ts_node_child_by_field_name_exn node "object"
  and property_field = ts_node_child_by_field_name_exn node "property"
  and print_object_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "identifier" -> print_identifier state ~name node
    | "member_expression" -> print_nested_identifier state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  and print_property_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "property_identifier" -> print_identifier state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_object_field object_field
      ; mk_child print_property_field property_field
      ]
  in
  Tree.make state name children

(* Generic type *)

and print_generic_type state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_arguments_field = ts_node_child_by_field_name_exn node "type_arguments"
  and print_name_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "type_identifier" -> print_type_identifier state ~name node
    | "nested_type_identifier" -> print_nested_type_identifier state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_name_field name_field
      ; mk_child (anon print_type_arguments) type_arguments_field
      ]
  in
  Tree.make state name children

(* Object type *)

and print_object_type state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_object_type_field) children

and print_object_type_field state ?name node =
  let name = get_name ?name node in
  match name with
  | "export_statement" -> print_export_statement state ~name node
  | "property_signature" -> print_property_signature state ~name node
  | "call_signature" -> print_call_signature state ~name node
  | "construct_signature" -> print_construct_signature state ~name node
  | "index_signature" -> print_index_signature state ~name node
  | "method_signature" -> print_method_signature state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

and print_property_signature state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let accessibility_modifier = filter_first_by_name "accessibility_modifier" children
  and static = has_node_named "static" children
  and override_modifier = filter_first_by_name "override_modifier" children
  and readonly = has_node_named "readonly" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  and qmark = has_node_named "?" children
  and type_field = ts_node_child_by_field_name node "type" in
  let children =
    Tree.
      [ mk_child_opt (anon print_accessibility_modifier) accessibility_modifier
      ; mk_child_opt make_node static
      ; mk_child_opt (anon print_override_modifier) override_modifier
      ; mk_child_opt make_node readonly
      ; mk_child (anon print_identifier) name_field
      ; mk_child_opt make_node qmark
      ; mk_child_opt (anon print_type_annotation) type_field
      ]
  in
  Tree.make state name children

(* Call signature *)

and print_call_signature state ?name node =
  let name = get_name ?name node
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type" in
  let children =
    Tree.
      [ mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ]
  in
  Tree.make state name children

(* Asserts annotation *)

and print_asserts_annotation state ?name node =
  let name = get_name ?name node
  and asserts = ts_node_child_exn node 1 in
  Tree.make_unary state name (anon print_asserts) asserts

(* Type predicate annotation *)

and print_type_predicate_annotation state ?name node =
  let name = get_name ?name node
  and asserts = ts_node_child_exn node 1 in
  Tree.make_unary state name (anon print_type_predicate) asserts

(* Construct signature *)

and print_construct_signature state ?name node =
  let name = get_name ?name node
  and abstract = has_child_named "abstract" node
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and type_field = ts_node_child_by_field_name node "type" in
  let children =
    Tree.
      [ mk_child_opt make_node abstract
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_type_annotation) type_field
      ]
  in
  Tree.make state name children

(* Index signature *)

and print_index_signature state ?name node =
  let name = get_name ?name node
  and sign_field = ts_node_child_by_field_name node "sign"
  and name_field = ts_node_child_by_field_name node "name"
  and type_field = ts_node_child_by_field_name_exn node "type"
  and print_sign_field state node =
    match ts_node_child node 0 with
    | None -> Tree.make_node state "readonly"
    | Some sign -> Tree.make_unary state "readonly" print_plus_minus sign
  and print_type_field state node =
    let name = get_name node in
    match name with
    | "type_annotation" -> print_type_annotation state ~name node
    | "omitting_type_annotation" -> print_omitting_type_annotation state ~name node
    | "adding_type_annotation" -> print_adding_type_annotation state ~name node
    | "opting_type_annotation" -> print_opting_type_annotation state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    match name_field with
    | Some name_field ->
      let index_type_field = ts_node_child_by_field_name_exn node "index_type" in
      Tree.
        [ mk_child_opt print_sign_field sign_field
        ; mk_child (anon print_identifier) name_field
        ; mk_child (anon print_type) index_type_field
        ; mk_child print_type_field type_field
        ]
    | None ->
      let mapped_type_clause = ts_node_named_child_exn node 0 in
      Tree.
        [ mk_child_opt print_sign_field sign_field
        ; mk_child (anon print_mapped_type_clause) mapped_type_clause
        ; mk_child print_type_field type_field
        ]
  in
  Tree.make state name children

and print_mapped_type_clause state ?name node =
  let name = get_name ?name node
  and name_field = ts_node_child_by_field_name_exn node "name"
  and type_field = ts_node_child_by_field_name_exn node "type"
  and alias_field = ts_node_child_by_field_name node "alias" in
  let children =
    Tree.
      [ mk_child (anon print_type_identifier) name_field
      ; mk_child (anon print_type) type_field
      ; mk_child_opt (anon print_type) alias_field
      ]
  in
  Tree.make state name children

and print_omitting_type_annotation state ~name node =
  let child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_adding_type_annotation state ~name node =
  let child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_opting_type_annotation state ~name node =
  let child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

(* Method signature *)

and print_method_signature state ?name node =
  let name = get_name ?name node
  and children = collect_children node in
  let accessibility_modifier = filter_first_by_name "accessibility_modifier" children
  and static = has_node_named "static" children
  and override_modifier = filter_first_by_name "override_modifier" children
  and readonly = has_node_named "readonly" children
  and async = has_node_named "async" children
  and set = has_node_named "set" children
  and get = has_node_named "get" children
  and star = has_node_named "*" children
  and name_field = ts_node_child_by_field_name_exn node "name"
  and qmark = has_node_named "?" children
  (* "_call_signature" inlined: *)
  and type_parameters_field = ts_node_child_by_field_name node "type_parameters"
  and parameters_field = ts_node_child_by_field_name_exn node "parameters"
  and return_type_field = ts_node_child_by_field_name node "return_type" in
  let children =
    Tree.
      [ mk_child_opt (anon print_accessibility_modifier) accessibility_modifier
      ; mk_child_opt make_node static
      ; mk_child_opt (anon print_override_modifier) override_modifier
      ; mk_child_opt make_node readonly
      ; mk_child_opt make_node async
      ; mk_child_opt make_node set
      ; mk_child_opt make_node get
      ; mk_child_opt make_node star
      ; mk_child (anon print_property_name) name_field
      ; mk_child_opt make_node qmark
      ; mk_child_opt (anon print_type_parameters) type_parameters_field
      ; mk_child (anon print_formal_parameters) parameters_field
      ; mk_child_opt (anon print_return_type) return_type_field
      ]
  in
  Tree.make state name children

(* Array type *)

and print_array_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

(* Tuple type *)

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

(* Type annotation *)

and print_type_annotation state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

(* Rest pattern *)

and print_rest_pattern state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_lhs_expression) child

(* LHS expression *)

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

and print_optional_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

and print_rest_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

(* Type query *)

and print_type_query state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "subscript_expression" -> print_type_query_subscript_expression state ~name node
    | "member_expression" -> print_type_query_member_expression state ~name node
    | "call_expression" -> print_type_query_call_expression state ~name node
    | "instantiation_expression" ->
      print_type_query_instantiation_expression state ~name node
    | "identifier" -> print_identifier state ~name node
    | "this" -> print_this state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.make_unary state name print child

and print_type_query_subscript_expression state ?name node =
  let name = get_name ?name node
  and object_field = ts_node_child_by_field_name_exn node "object"
  and index_field = ts_node_child_by_field_name_exn node "index"
  and print_index_field state node =
    let name = string_of_ts_node_type node in
    match name with
    | "predefined_type" -> print_predefined_type state ~name node
    | "string" -> make_node state ~name node
    | "number" -> print_number state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  let children =
    Tree.
      [ mk_child print_object_field object_field; mk_child print_index_field index_field ]
  in
  Tree.make state name children

and print_type_query_member_expression state ?name node =
  let name = get_name ?name node
  and object_field = ts_node_child_by_field_name_exn node "object"
  and property_field = ts_node_child_by_field_name_exn node "property" in
  let children =
    Tree.
      [ mk_child print_object_field object_field
      ; mk_child print_property_field property_field
      ]
  in
  Tree.make state name children

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
    Tree.
      [ mk_child print_function_field function_field
      ; mk_child (anon print_type_arguments) type_arguments_field
      ]
  in
  Tree.make state name children

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
    Tree.
      [ mk_child print_function_field function_field
      ; mk_child (anon print_arguments) arguments_field
      ]
  in
  Tree.make state name children

and print_index_type_query state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_primary_type) child

(* Existential type *)

and print_existential_type state ?name node = make_node state ?name node

(* Literal type *)

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

(* Unary expression *)

and print_unary_expression state ?name node =
  let name = get_name ?name node
  and operator_field = ts_node_child_by_field_name_exn node "operator"
  and argument_field = ts_node_child_by_field_name_exn node "argument" in
  let children =
    Tree.
      [ mk_child print_plus_minus operator_field
      ; mk_child (anon print_number) argument_field
      ]
  in
  Tree.make state name children

and print_plus_minus state node =
  let name = string_of_ts_node_type node in
  match name with
  | "+" -> make_node state ~name node
  | "-" -> make_node state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

(* Look up type

   The non-terminals "type" and "primary_type" are supertypes in the
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

(* Conditional type *)

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

(* Template literal type *)

and print_template_literal_type state ?name node = make_node state ?name node

(* Intersection type *)

and print_intersection_type state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

(* Union type *)

and print_union_type state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_type) children

(* Function type *)

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
  let name = get_name ?name node
  and child = ts_node_child_exn node 1
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "type_predicate" -> print_type_predicate state ~name node
    | "identifier" -> print_identifier state ~name node
    | "this" -> print_this state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.make_unary state name print child

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

(* Readonly type *)

and print_readonly_type state ?name node =
  let name = get_name ?name node
  and child = ts_node_named_child_exn node 0 in
  Tree.make_unary state name (anon print_type) child

(* Constructor type *)

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

and print_optional_parameter state ?name node = print_required_parameter state ?name node

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

(* Decorator *)

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

(* Accessibility modifier *)

and print_accessibility_modifier state ?name node =
  let name = get_name ?name node
  and child = ts_node_child_exn node 0
  and print state node =
    let name = string_of_ts_node_type node in
    match name with
    | "public" -> print_public state ~name node
    | "private" -> print_private state ~name node
    | "protected" -> print_protected state ~name node
    | _ -> match_rest state ~name node print_unexpected_node
  in
  Tree.make_unary state name print child

and print_public state ?name node =
  let name = get_name ?name node in
  make_node state ~name node

and print_private state ?name node =
  let name = get_name ?name node in
  make_node state ~name node

and print_protected state ?name node =
  let name = get_name ?name node in
  make_node state ~name node

(* Override modifier *)

and print_override_modifier state ?name node =
  let name = get_name ?name node
  and child = ts_node_child_exn node 0 in
  Tree.make_unary state name (anon make_node) child

(* Infer type *)

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

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

(* Object pattern *)

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

(* Pair pattern *)

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

(* Assignment pattern *)

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

(* Property names *)

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
  let name = get_name ?name node
  and expression = ts_node_child_exn node 1 in
  Tree.make_unary state name (anon print_expression) expression

and print_shorthand_property_identifier_pattern state ?name node =
  make_node state ?name node

(* Object assignment pattern *)

and print_object_assignment_pattern state ?name node =
  let name = get_name ?name node
  and left_field = ts_node_child_by_field_name_exn node "left"
  and right_field = ts_node_child_by_field_name_exn node "right"
  and print_left state node =
    let name = get_name node in
    match name with
    | "shorthand_property_identifier_pattern" ->
      print_shorthand_property_identifier_pattern state ~name node
    | _ -> match_rest state ~name node print_destructuring_pattern
  in
  let children =
    Tree.[ mk_child print_left left_field; mk_child (anon print_expression) right_field ]
  in
  Tree.make state name children

(* Rule "_destructuring_pattern" is inlined. *)

and print_destructuring_pattern state ?name node =
  let name = get_name ?name node in
  match name with
  | "object_pattern" -> print_object_pattern state ~name node
  | "array_pattern" -> print_array_pattern state ~name node
  | _ -> match_rest state ~name node print_unexpected_node

(* Array pattern *)

and print_array_pattern state ?name node =
  let name = get_name ?name node
  and children = collect_named_children node in
  Tree.of_list state name (anon print_array_pattern_cell) children

and print_array_pattern_cell state ?name node =
  let name = get_name ?name node in
  match name with
  | "assignment_pattern" -> print_assignment_pattern state ~name node
  | _ -> match_rest state ~name node print_pattern (* hidden rule *)

(* General patterns (hidden rule) *)

and print_pattern state ?name node =
  let name = get_name ?name node in
  match name with
  | "rest_pattern" -> print_rest_pattern state ~name node
  | _ -> print_lhs_expression state ~name node
