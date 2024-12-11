(* Decoding the tree-sitter CST for TypeScript *)

(* Vendored *)

module Region = Simple_utils.Region

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun = Tree_sitter.Api.Functions

(* Local *)

module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Ast = Typescript_ast.Ast
module Ast_to_cst = Typescript_ast.Ast_to_cst

(* Parsing *)

let parse file line_map : (Ast.t, string) result =
  (* Loading the code as a string *)
  let input : string = Core.In_channel.read_all file in
  (* Parsing the code into a tree *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string input in
  (* Getting ahold of the root of the tree *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Decoding the tree *)
  let ast = Decode.dec_program file line_map program_node in
  (* Releasing the memory allocated to the tree *)
  let () = TS_fun.ts_tree_delete tree in
  ast

(* Reading the input TypeScript, parsing and printing the AST *)

open Core

let cli_args : string array = Sys.get_argv ()

let () =
  match Array.length cli_args with
  | 2 ->
    let file = cli_args.(1) in
    (match Loc_map.scan file with
    | Ok line_map ->
      (match parse file line_map with
      | Ok _ast -> () (* TODO: Print *)
      | Error msg -> Printf.eprintf "Error: %s\n%!" msg)
    | Error { region; value = _ } -> Printf.eprintf "Error: %s\n%!" (region#compact `Byte))
  | _ -> prerr_endline ("Usage: " ^ cli_args.(0) ^ " [file]")
