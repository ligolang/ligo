(* Decoding the tree-sitter CST for TypeScript and stripping it *)

open Core

(* Vendored *)

module Region = Simple_utils.Region
module Snippet = Simple_utils.Snippet

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

let parse debug_arg filename : (Ast.t, string Region.reg) Result.t =
  (* Loading the code as text *)
  let file : string = In_channel.read_all filename in
  (* Building the map from line+columns to positions *)
  let line_map = Loc_map.scan_string file in
  (* Parsing the code into a tree *)
  let tree : Ts_wrap.ts_tree_ptr = Ts_wrap.parse_typescript_string file in
  (* Getting ahold of the root of the tree *)
  let program_node : Ts_wrap.ts_tree = TS_fun.ts_tree_root_node tree in
  (* Decoding the CST *)
  let ast = Decode.dec_program ~debug_arg ~filename ~file line_map program_node in
  (* Releasing the memory allocated to the CST *)
  let () = TS_fun.ts_tree_delete tree in
  ast

(* Reading the input TypeScript, parsing and printing the AST *)

let usage_msg = "Usage: strip_main [-no-colour] <filename>.ts"
let no_colour = ref false
let debug = ref false
let input_file = ref ""
let anon_fun filename = input_file := filename

let speclist =
  [ "-no-colour", Arg.Set no_colour, "Colourless code snippets in errors."
  ; "-debug", Arg.Set debug, "A missing field yields internal information."
  ]

(* Formatting error messages (snippets) *)

let format_msg Region.{ value; region } =
  sprintf
    "%sError: %s"
    (Format.asprintf "%a" (Snippet.pp_lift ~no_colour:!no_colour) region)
    value

(* Main *)

let () =
  Arg.parse speclist anon_fun usage_msg;
  match parse !debug !input_file with
  | Error msg -> Printf.eprintf "%s\n%!" (format_msg msg)
  | Ok ast ->
    (match Strip.statements ast with
    | Ok _ -> Printf.printf "Done.\n%!"
    | Error msg -> Printf.printf "%s\n%!" (format_msg msg))
