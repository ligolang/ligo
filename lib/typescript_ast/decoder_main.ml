(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun = Tree_sitter.Api.Functions

(* Parsing *)

let parse input =
  (* Parsing the code *)
  let tree : Decoder.ts_tree_ptr = Decoder.parse_typescript_string input in
  (* Getting ahold of the root *)
  let program_node : Decoder.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Empty state for building the AST *)
  let buffer = Buffer.create 1023 in
  let state = Tree.mk_state ~buffer ~regions:false ~layout:true ~offsets:true `Byte in
  (* Decoding the CST into an AST in [state] *)
  let () = Decoder.print_program state program_node in
  (* Printing the AST in [state] *)
  Printf.printf "%s%!" (Buffer.contents @@ Tree.to_buffer state)

(* Reading the input TypeScript, parsing and printing the AST *)

open Core

let cli_args : string array = Sys.get_argv ()

let () =
  match Array.length cli_args with
  | 2 -> parse @@ In_channel.read_all cli_args.(1)
  | _ -> prerr_endline ("Usage: " ^ cli_args.(0) ^ " [file]")
