(* Printing the tree-sitter CST for TypeScript *)

module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Region = Simple_utils.Region

(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun = Tree_sitter.Api.Functions

(* Parsing *)

let parse file line_map =
  (* Loading the code as text *)
  let input : string = Core.In_channel.read_all file in
  (* Parsing the code *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string input in
  (* Getting ahold of the root *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Printing the tree from the root *)
  let cst : string = Print_cst.print_program file line_map program_node in
  Printf.printf "%s%!" cst;
  (* Releasing the memory allocated to the tree *)
  TS_fun.ts_tree_delete tree

(* Reading the input TypeScript, parsing and printing the AST *)

open Core

let cli_args : string array = Sys.get_argv ()

let () =
  match Array.length cli_args with
  | 2 ->
    let file = cli_args.(1) in
    (match Loc_map.scan file with
    | Ok line_map -> parse file line_map
    | Error { region = _; value } -> Printf.eprintf "Error: %s\n%!" value)
  | _ -> prerr_endline ("Usage: " ^ cli_args.(0) ^ " [file]")
