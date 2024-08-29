(* This module is a minimal example of parsing some TypeScript program *)

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

let print_node (node: ts_tree) : unit =
  let ptr_char = TS_fun.ts_node_string node in
  Printf.printf "%s\n%!" @@ string_of_char_ptr ptr_char

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

(* The example per se *)

let () =
  let code = "[1, null]" in
  (* Parsing the code *)
  let tree : ts_tree_ptr =
    parse_typescript_string code in
  (* Extracting all the nodes *)
  let program_node : ts_tree =
    TS_fun.ts_tree_root_node tree in
  let expr_stmt_node : ts_tree =
    TS_fun.ts_node_named_child program_node (UInt32.of_int 0) in
  let array_node : ts_tree =
    TS_fun.ts_node_named_child expr_stmt_node (UInt32.of_int 0) in
  let number_node : ts_tree =
    TS_fun.ts_node_named_child array_node (UInt32.of_int 0) in
  let null_node : ts_tree =
    TS_fun.ts_node_named_child array_node (UInt32.of_int 1) in
  (* Checking the node types *)
  assert (string_of_ts_node_type program_node = "program");
  assert (string_of_ts_node_type expr_stmt_node = "expression_statement");
  assert (string_of_ts_node_type array_node = "array");
  assert (string_of_ts_node_type number_node = "number");
  assert (string_of_ts_node_type null_node = "null");
  (* Checking the child counts *)
  assert (TS_fun.ts_node_child_count program_node = UInt32.of_int 1);
  assert (TS_fun.ts_node_child_count expr_stmt_node = UInt32.of_int 1);
  assert (TS_fun.ts_node_child_count array_node = UInt32.of_int 5);
  assert (TS_fun.ts_node_child_count number_node = UInt32.of_int 0);
  (* Printing the tree and freeing the memory *)
  print_node program_node;
  TS_fun.ts_tree_delete tree
