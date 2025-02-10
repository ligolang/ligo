(* Decoding the tree-sitter CST for TypeScript and stripping it *)

open Core

(* Vendored *)

module Region = Simple_utils.Region

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun = Tree_sitter.Api.Functions

(* Local *)

module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Ast = Typescript_ast.Ast
module Decode = Typescript_decoder.Decode
module Strip = Typescript_stripper.Strip

(* Parsing *)

(* TODO: The return type for Ok should be [Ast_stripped.t]. That means
   that [Decode.dec_program] should have type [(_, string Region.reg)
   result] *)

let parse filename : (Ast.t, string) Result.t =
  (* Loading the code as text *)
  let file : string = In_channel.read_all filename in
  (* Building the map from line+columns to positions *)
  let line_map = Loc_map.scan_string file in
  (* Parsing the code into a tree *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string file in
  (* Getting ahold of the root of the tree *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Decoding the CST *)
  let ast = Decode.dec_program ~filename ~file line_map program_node in
  (* Releasing the memory allocated to the CST *)
  let () = TS_fun.ts_tree_delete tree in
  ast

(* Reading the input TypeScript, parsing and printing the AST *)

open Core

let cli_args : string array = Sys.get_argv ()

let () =
  match Array.length cli_args with
  | 2 ->
    let filename = cli_args.(1) in
    (match parse filename with
    | Error msg -> Printf.eprintf "Error: %s\n%!" msg
    | Ok ast ->
      (match Strip.statements ast with
      | Ok _ -> Printf.printf "Stripped.\n%!"
      | Error { region; value } ->
        Printf.eprintf "Error: %s\n%s\n%!" value (region#compact `Byte)))
  | _ -> prerr_endline ("Usage: " ^ cli_args.(0) ^ " [file]")
