open Tree_sitter.Api
open Types
open Functions
open Tree_sitter_typescript.Api
open Functions
open Ctypes
open Unsigned

(* converts from c string of type `char *` to ocaml string of type `string`  *)
let string_of_ptr_char ptr =
  let rec get_length p = if !@p = '\000' then 0 else 1 + get_length (p +@ 1) in
  let length = get_length ptr in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    Bytes.set buffer i !@(ptr +@ i)
  done;
  Bytes.to_string buffer

let print_node node =
  let ptr_char = ts_node_string node in
  Printf.printf "\n\n TypeScript CST : %s\n\n" @@ string_of_ptr_char ptr_char

let ts_node_type_string node = string_of_ptr_char @@ ts_node_type node

let parse_typescript_string source_code =
  let parser = ts_parser_new () in
  let language = tree_sitter_typescript () in
  let _ = ts_parser_set_language parser language in
  let null_tree = from_voidp ts_tree null in
  let tree =
    ts_parser_parse_string
      parser
      null_tree
      source_code
      (UInt32.of_int @@ String.length source_code)
  in
  ts_parser_delete parser;
  tree

let () =
  let code = "[1, null]" in
  let tree = parse_typescript_string code in
  (* extract the node *)
  let root_node = ts_tree_root_node tree in
  let expr_stmt_node = ts_node_named_child root_node (UInt32.of_int 0) in
  let array_node = ts_node_named_child expr_stmt_node (UInt32.of_int 0) in
  let number_node = ts_node_named_child array_node (UInt32.of_int 0) in
  let null_node = ts_node_named_child array_node (UInt32.of_int 1) in
  (* get the node type *)
  assert (ts_node_type_string root_node = "program");
  assert (ts_node_type_string expr_stmt_node = "expression_statement");
  assert (ts_node_type_string array_node = "array");
  assert (ts_node_type_string number_node = "number");
  assert (ts_node_type_string null_node = "null");
  (* get the child count *)
  assert (ts_node_child_count root_node = UInt32.of_int 1);
  assert (ts_node_child_count array_node = UInt32.of_int 5);
  assert (ts_node_child_count number_node = UInt32.of_int 0);
  print_node root_node;
  ts_tree_delete tree
