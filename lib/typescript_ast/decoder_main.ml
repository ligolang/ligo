(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun   = Tree_sitter.Api.Functions

(* Example *)

let code = "[1, null]; f(2)"

(* Parsing the code *)

let tree : Decoder.ts_tree_ptr =
  Decoder.parse_typescript_string code

(* Getting ahold of the root *)

let program_node : Decoder.ts_tree = TS_fun.ts_tree_root_node tree

(* Empty state for building the AST *)

let buffer = Buffer.create 1023

let state =
  Tree.mk_state ~buffer ~regions:false ~layout:true ~offsets:true `Byte

(* Decoding the CST into an AST in [state] *)

let () = Decoder.print_program state program_node

(* Printing the AST in [state] *)

let () = Printf.printf "%s%!" (Buffer.contents @@ Tree.to_buffer state)
