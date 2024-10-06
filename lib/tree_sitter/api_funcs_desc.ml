(* This module defines with Ctypes some foreign function bindings from
   the tree-sitter API
 https://github.com/tree-sitter/tree-sitter/blob/master/lib/include/tree_sitter/api.h
*)

open Ctypes
open Api_types (* See [Api_types_desc.ml] *)

module Functions (S : FOREIGN) = struct
  open S

  (* PARSER *)

  (* Create a new parser.

     TSParser *ts_parser_new(void);
  *)
  let ts_parser_new = foreign "ts_parser_new" (void @-> returning @@ ptr ts_parser)

  (* Delete the parser, freeing all of the memory that it used.

     void ts_parser_delete(TSParser *self);
  *)
  let ts_parser_delete = foreign "ts_parser_delete" (ptr ts_parser @-> returning void)

  (* Return a boolean indicating whether or not the language was
     successfully assigned.

     bool ts_parser_set_language(TSParser *self, const TSLanguage *language);
  *)
  let ts_parser_set_language =
    foreign "ts_parser_set_language" (ptr ts_parser @-> ptr ts_language @-> returning bool)

  (* Use the parser to parse some source code stored in one contiguous
     buffer. The first two parameters are the same as in the
     [ts_parser_parse] function above. The second two parameters
     indicate the location of the buffer and its length in bytes.

     TSTree *ts_parser_parse_string(
       TSParser *self,
       const TSTree *old_tree,
       const char *string,
       uint32_t length
     );
  *)
  let ts_parser_parse_string =
    foreign
      "ts_parser_parse_string"
      (ptr ts_parser @-> ptr ts_tree @-> string @-> uint32_t @-> returning @@ ptr ts_tree)

  (* TREE *)

  (* Create a shallow copy of the syntax tree

     TSTree *ts_tree_copy(const TSTree *self);
  *)
  let ts_tree_copy = foreign "ts_tree_copy" (ptr ts_tree @-> returning @@ ptr ts_tree)

  (* Delete the syntax tree, freeing all of the memory that it used

     void ts_tree_delete(TSTree *self);
  *)
  let ts_tree_delete = foreign "ts_tree_delete" (ptr ts_tree @-> returning void)

  (* Get the root node of the syntax tree

     TSNode ts_tree_root_node(const TSTree *self);
  *)
  let ts_tree_root_node = foreign "ts_tree_root_node" (ptr ts_tree @-> returning ts_node)

  (* NODE *)

  (* Get the node's type as a null-terminated string

     const char *ts_node_type(TSNode self);
  *)
  let ts_node_type = foreign "ts_node_type" (ts_node @-> returning @@ ptr char)

  (* Get an S-expression representing the node as a string

     char *ts_node_string(TSNode self);
  *)
  let ts_node_string = foreign "ts_node_string" (ts_node @-> returning @@ ptr char)

  (* Get the node's number of children

     uint32_t ts_node_child_count(TSNode self);
  *)
  let ts_node_child_count = foreign "ts_node_child_count" (ts_node @-> returning uint32_t)

  (* Get the node's child at the given index (0 is the first)

     TSNode ts_node_child(TSNode self, uint32_t child_index);
  *)
  let ts_node_child = foreign "ts_node_child" (ts_node @-> uint32_t @-> returning ts_node)

  (* Get the node's *named* child at the given index (0 is the first)

     TSNode ts_node_named_child(TSNode self, uint32_t child_index);
  *)
  let ts_node_named_child =
    foreign "ts_node_named_child" (ts_node @-> uint32_t @-> returning ts_node)

  (* Get the node's number of *named* children

     uint32_t ts_node_named_child_count(TSNode self);
  *)
  let ts_node_named_child_count =
    foreign "ts_node_named_child_count" (ts_node @-> returning uint32_t)

  (* Get the node's next sibling

     TSNode ts_node_next_sibling(TSNode self);
  *)
  let ts_node_next_sibling = foreign "ts_node_next_sibling" (ts_node @-> returning ts_node)

  (* Get the node's previous sibling

     TSNode ts_node_prev_sibling(TSNode self);
  *)
  let ts_node_prev_sibling = foreign "ts_node_prev_sibling" (ts_node @-> returning ts_node)

  (* Get the node's next *named* sibling.

     TSNode ts_node_next_named_sibling(TSNode self);
  *)
  let ts_node_next_named_sibling =
    foreign "ts_node_next_named_sibling" (ts_node @-> returning ts_node)

  (* Get the node's previous *named* sibling

     TSNode ts_node_prev_named_sibling(TSNode self);
  *)
  let ts_node_prev_named_sibling =
    foreign "ts_node_prev_named_sibling" (ts_node @-> returning ts_node)

  (* Get the node's child by *field*

     TSNode ts_node_child_by_field_name(
       TSNode self,
       const char *field_name,
       uint32_t field_name_length);
  *)
  let ts_node_child_by_field_name =
    foreign
      "ts_node_child_by_field_name"
      (ts_node @-> string @-> uint32_t @-> returning ts_node)

  (* Checking is a node is null

     bool ts_node_is_null(TSNode);
  *)

  let ts_node_is_null = foreign "ts_node_is_null" (ts_node @-> returning bool)

  (* Syntax nodes store their position in the source code both in terms
   of raw bytes and row/column coordinates:

   uint32_t ts_node_start_byte(TSNode);
   uint32_t ts_node_end_byte(TSNode);

   typedef struct {
     uint32_t row;
     uint32_t column;
   } TSPoint;

   TSPoint ts_node_start_point(TSNode);
   TSPoint ts_node_end_point(TSNode);
 *)

  let ts_node_start_byte = foreign "ts_node_start_byte" (ts_node @-> returning uint32_t)
  let ts_node_end_byte = foreign "ts_node_end_byte" (ts_node @-> returning uint32_t)
  let ts_node_start_point = foreign "ts_node_start_point" (ts_node @-> returning ts_point)
  let ts_node_end_point = foreign "ts_node_end_point" (ts_node @-> returning ts_point)
end
