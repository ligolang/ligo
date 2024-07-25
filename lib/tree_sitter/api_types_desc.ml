open Ctypes

(* structs defined in tree-sitter *)
(* check: https://github.com/tree-sitter/tree-sitter/blob/master/lib/include/tree_sitter/api.h *)
module Types (S : Ctypes.TYPE) = struct
  open S

  (* ////////// Types ////////// *)

  let ts_symbol = uint16_t
  let ts_state_id = uint16_t
  let ts_field_id = uint16_t

  (* ////////// struct TSPoint ////////// *)

  type ts_point

  let ts_point : ts_point structure typ = structure "TSPoint"
  let row = field ts_point "row" uint
  let column = field ts_point "column" uint
  let () = seal ts_point

  (* ////////// struct TSRange ////////// *)

  type ts_range

  let ts_range : ts_range structure typ = structure "TSRange"
  let start_point = field ts_range "start_point" ts_point
  let end_point = field ts_range "end_point" ts_point
  let start_byte = field ts_range "start_byte" uint32_t
  let end_byte = field ts_range "end_byte" uint32_t
  let () = seal ts_range

  (* ////////// struct TSLanguage ////////// *)

  type ts_language

  let ts_language : ts_language structure typ = structure "TSLanguage"

  (* ////////// struct TSTree ////////// *)

  type ts_tree

  let ts_tree : ts_tree structure typ = structure "TSTree"

  (* ////////// struct TSParser ////////// *)

  type ts_parser

  let ts_parser : ts_parser structure typ = structure "TSParser"

  (* ////////// struct TSNode ////////// *)

  type ts_node

  let ts_node : ts_tree structure typ = structure "TSNode"
  let context = field ts_node "context" (array 4 uint32_t)
  let id = field ts_node "id" (ptr void)
  let tree = field ts_node "tree" (ptr ts_tree)
  let () = seal ts_node
end
