(* This module defines with Ctypes some types from the tree-sitter API
   https://github.com/tree-sitter/tree-sitter/blob/master/lib/include/tree_sitter/api.h
*)

open Ctypes

module Types (S : Ctypes.TYPE) = struct
  (*open S*)

  (* Some basic types *)

  let ts_symbol : Unsigned.uint16 typ = uint16_t
  let ts_state_id : Unsigned.uint16 typ = uint16_t
  let ts_field_id : Unsigned.uint16 typ = uint16_t

  (* Single source location:

     typedef struct TSPoint {
       uint32_t row;
       uint32_t column;
     } TSPoint;
  *)

  type ts_point

  let ts_point : ts_point structure typ = structure "TSPoint"
  let row : _ field = field ts_point "row" uint
  let column : _ field = field ts_point "column" uint
  let () = seal ts_point

  (* Range in the source

     typedef struct TSRange {
       TSPoint start_point;
       TSPoint end_point;
       uint32_t start_byte;
       uint32_t end_byte;
     } TSRange;
  *)

  type ts_range

  let ts_range : ts_range structure typ = structure "TSRange"
  let start_point : _ field = field ts_range "start_point" ts_point
  let end_point : _ field = field ts_range "end_point" ts_point
  let start_byte : _ field = field ts_range "start_byte" uint32_t
  let end_byte : _ field = field ts_range "end_byte" uint32_t
  let () = seal ts_range

  (* The source language (TSLanguage) *)

  type ts_language

  let ts_language : ts_language structure typ = structure "TSLanguage"

  (* The syntax tree (TSTree) *)

  type ts_tree

  let ts_tree : ts_tree structure typ = structure "TSTree"

  (* The parser (TSParser) *)

  type ts_parser

  let ts_parser : ts_parser structure typ = structure "TSParser"

  (* Node in the syntax tree

     typedef struct TSNode {
       uint32_t context[4];
       const void *id;
       const TSTree *tree;
     } TSNode;
  *)

  type ts_node

  let ts_node : ts_tree structure typ = structure "TSNode"
  let context : _ field = field ts_node "context" (array 4 uint32_t)
  let id : _ field = field ts_node "id" (ptr void)
  let tree : _ field = field ts_node "tree" (ptr ts_tree)
  let () = seal ts_node
end
