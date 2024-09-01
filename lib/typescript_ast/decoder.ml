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

(* Collating named children of a given node *)

let collect_named_children (node: ts_tree) =
  let rec collect acc n =
    if UInt32.(equal zero n) then acc
    else let index = UInt32.pred n in
         let child = TS_fun.ts_node_named_child node index
         in collect (child :: acc) index
  in collect [] (TS_fun.ts_node_named_child_count node)

let print_unexpected_node state name _node =
  Tree.make_node state (name ^ "?")

(* Printing the AST *)

let rec print_program state program_node =
  let node_name = string_of_ts_node_type program_node in
  let children = collect_named_children program_node in
  Tree.of_list state node_name print_statement children

(* Statements *)

and print_statement state node =
  let name = string_of_ts_node_type node in
  match name with
  | "export_statement" -> print_export_statement state name node
  | "import_statement" -> print_import_statement state name node
  | "debugger_statement" -> print_debugger_statement state name node
  | "expression_statement" -> print_expression_statement state name node
  | "declaration" -> print_declaration state name node
  | "statement_block" -> print_statement_block state name node
  | "if_statement" -> print_if_statement state name node
  | "switch_statement" -> print_switch_statement state name node
  | "for_statement" -> print_for_statement state name node
  | "while_statement" -> print_while_statement state name node
  | "do_statement" -> print_do_statement state name node
  | "try_statement" -> print_try_statement state name node
  | "with_statement" -> print_with_statement state name node
  | "break_statement" -> print_break_statement state name node
  | "continue_statement" -> print_continue_statement state name node
  | "return_statement" -> print_return_statement state name node
  | "throw_statement" -> print_throw_statement state name node
  | "empty_statement" -> print_empty_statement state name node
  | "labeled_statement" -> print_labeled_statement state name node
  | _ -> print_unexpected_node state name node

and print_export_statement state name _node =
  Tree.make_node state name

and print_import_statement state name _node =
  Tree.make_node state name

and print_debugger_statement state name _node =
  Tree.make_node state name

and print_expression_statement state name node =
  let children = collect_named_children node in
  Tree.of_list state name print_expression children

and print_declaration state name _node =
  Tree.make_node state name

and print_statement_block state name _node =
  Tree.make_node state name

and print_if_statement state name _node =
  Tree.make_node state name

and print_switch_statement state name _node =
  Tree.make_node state name

and print_for_statement state name _node =
  Tree.make_node state name

and print_while_statement state name _node =
  Tree.make_node state name

and print_do_statement state name _node =
  Tree.make_node state name

and print_try_statement state name _node =
  Tree.make_node state name

and print_with_statement state name _node =
  Tree.make_node state name

and print_break_statement state name _node =
  Tree.make_node state name

and print_continue_statement state name _node =
  Tree.make_node state name

and print_return_statement state name _node =
  Tree.make_node state name

and print_throw_statement state name _node =
  Tree.make_node state name

and print_empty_statement state name _node =
  Tree.make_node state name

and print_labeled_statement state name _node =
  Tree.make_node state name

(* Expressions *)

and print_expression state node =
  let name = string_of_ts_node_type node in
  match name with
    "array" -> print_array state name node
  | _ -> print_unexpected_node state name node (* TODO *)

and print_array state name _node =
  Tree.make_node state name

(* Example *)

let () =
  let code = "[1, null]; f(2)" in
  (* Parsing the code *)
  let tree : ts_tree_ptr =
    parse_typescript_string code in
  (* Empty state for building the AST *)
  let buffer = Buffer.create 1023 in
  let state =
    Tree.mk_state ~buffer ~regions:false ~layout:true ~offsets:true `Byte in
  (* Extracting all the nodes *)
  let program_node : ts_tree =
    TS_fun.ts_tree_root_node tree in
  let () = print_program state program_node in
  let ast_buffer = Tree.to_buffer state
  in Printf.printf "%s\n%!" (Buffer.contents ast_buffer)
