(* Checking and collecting all error nodes in the C CST
   (tree-sitter-generated) *)

open Core
open Typescript_ast.Ts_wrap
module Region = Simple_utils.Region
module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Syntax_err = Typescript_ast.Syntax_err
module Wrap = Lexing_shared.Wrap

(* Monadic let-binder for result values *)

let ( let* ) v f = Core.Result.bind v ~f

(* Source map for converting vertical and horizontal offset ranges
   into regions *)

let get_region : (ts_tree -> Region.t) ref =
  ref (fun _ -> failwith "Internal error: Check.get_region")

(* The input source (default: a hundred lines) *)

let input : Buffer.t ref = ref (Buffer.create (80 * 100))

(* Traversing the CST *)

let rec check_program ~filename ~file (map : Loc_map.t) node =
  (* Setting up the extraction of source regions *)
  let () = get_region := Ts_wrap.get_region filename map in
  (* Setting the input as a top-level string buffer *)
  let () = Buffer.add_string !input file in
  (* Collating errors from the stripped AST *)
  check_statements [] node

(* STATEMENTS

   The JavaScript tree-sitter grammar has the non-terminals
   "statement" be a supertype, that is, a hidden rule. *)

and check_statements errors node =
  ignore node; errors
