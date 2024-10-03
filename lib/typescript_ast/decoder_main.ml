(* Printing the tree-sitter CST for TypeScript *)

module Ts_wrap = Typescript_ast.Ts_wrap
module Print_cst = Typescript_ast.Print_cst

(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun = Tree_sitter.Api.Functions

(* Parsing *)

let parse input =
  (* Parsing the code *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string input in
  (* Getting ahold of the root *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  let cst : string = Print_cst.print_program program_node in
  Printf.printf "%s%!" cst

(* Reading the input TypeScript, parsing and printing the AST *)

open Core

let cli_args : string array = Sys.get_argv ()

let () =
  match Array.length cli_args with
  | 2 -> parse @@ In_channel.read_all cli_args.(1)
  | _ -> prerr_endline ("Usage: " ^ cli_args.(0) ^ " [file]")
