open Ctypes
open Api_types

(* functions api defined in tree-sitter *)
(* check: https://github.com/tree-sitter/tree-sitter/blob/master/lib/include/tree_sitter/api.h *)
module Functions (S : FOREIGN) = struct
  open S

  (************************)
  (*** Section - Parser ***)
  (************************)

  (* Create a new parser. *)
  let ts_parser_new = foreign "ts_parser_new" (void @-> returning @@ ptr ts_parser)

  (* Delete the parser, freeing all of the memory that it used. *)
  let ts_parser_delete = foreign "ts_parser_delete" (ptr ts_parser @-> returning void)

  (*
   * Returns a boolean indicating whether or not the language was successfully
   * assigned. True means assignment succeeded. False means there was a version
   * mismatch: the language was generated with an incompatible version of the
   * Tree-sitter CLI. Check the language's version using [`ts_language_version`]
   * and compare it to this library's [`TREE_SITTER_LANGUAGE_VERSION`] and
   * [`TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION`] constants.
   *)
  let ts_parser_set_language =
    foreign "ts_parser_set_language" (ptr ts_parser @-> ptr ts_language @-> returning bool)

  (*
   * Use the parser to parse some source code stored in one contiguous buffer.
   * The first two parameters are the same as in the [`ts_parser_parse`] function
   * above. The second two parameters indicate the location of the buffer and its
   * length in bytes.
   *)
  let ts_parser_parse_string =
    foreign
      "ts_parser_parse_string"
      (ptr ts_parser @-> ptr ts_tree @-> string @-> uint32_t @-> returning @@ ptr ts_tree)

  (************************)
  (**** Section - Tree ****)
  (************************)

  (*
   * Create a shallow copy of the syntax tree. This is very fast.
   *
   * You need to copy a syntax tree in order to use it on more than one thread at
   * a time, as syntax trees are not thread safe.
   *)
  let ts_tree_copy = foreign "ts_tree_copy" (ptr ts_tree @-> returning @@ ptr ts_tree)

  (* Delete the syntax tree, freeing all of the memory that it used. *)
  let ts_tree_delete = foreign "ts_tree_delete" (ptr ts_tree @-> returning void)

  (*  Get the root node of the syntax tree. *)
  let ts_tree_root_node = foreign "ts_tree_root_node" (ptr ts_tree @-> returning ts_node)

  (************************)
  (**** Section - Node ****)
  (************************)

  (* Get the node's type as a null-terminated string. *)
  let ts_node_type = foreign "ts_node_type" (ts_node @-> returning @@ ptr char)

  (*
   * Get an S-expression representing the node as a string.

   * This string is allocated with `malloc` and the caller is responsible for
   * freeing it using `free`.   
   *)
  let ts_node_string = foreign "ts_node_string" (ts_node @-> returning @@ ptr char)

  (* Get the node's number of children. *)
  let ts_node_child_count = foreign "ts_node_child_count" (ts_node @-> returning uint32_t)

  (* Get the node's *named* child at the given index. *)
  let ts_node_named_child =
    foreign "ts_node_named_child" (ts_node @-> uint32_t @-> returning ts_node)

  (* Get the node's number of *named* children. *)
  let ts_node_named_child_count =
    foreign "ts_node_named_child_count" (ts_node @-> returning uint32_t)

  (* Get the node's next / previous sibling. *)
  let ts_node_next_sibling = foreign "ts_node_next_sibling" (ts_node @-> returning ts_node)
  let ts_node_prev_sibling = foreign "ts_node_prev_sibling" (ts_node @-> returning ts_node)

  (* Get the node's next / previous *named* sibling. *)
  let ts_node_next_named_sibling =
    foreign "ts_node_next_named_sibling" (ts_node @-> returning ts_node)

  let ts_node_prev_named_sibling =
    foreign "ts_node_prev_named_sibling" (ts_node @-> returning ts_node)
end
