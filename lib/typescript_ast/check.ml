(* Checking and collecting all error nodes in the C CST
   (tree-sitter-generated) *)

module Region = Simple_utils.Region
module Wrap = Lexing_shared.Wrap
open Ts_wrap

(* Monadic let-binder for result values *)

let ( let* ) v f = Core.Result.bind v ~f

(* Source map for converting vertical and horizontal offset ranges
   into regions *)

let get_region : (ts_tree -> Region.t) ref =
  ref (fun _ -> failwith "Internal error: Check.get_region")

(* Traversing the CST *)

let rec check_program file (map : Loc_map.t) node =
  (* Opening a read channel for lexemes *)
  let () = Lexeme.open_input ~file in
  (* Setting up the extracting of source regions *)
  let () = get_region := Ts_wrap.get_region file map in
  (* Collecting all ERROR and MISSING nodes *)
  let* () = check_tree [] node in
  (* Closing the input channel for reading lexemes *)
  let () = Lexeme.close_input () in
  Ok ()

(* STATEMENTS

   The JavaScript tree-sitter grammar has the non-terminals
   "statement" be a supertype, that is, a hidden rule. *)

and check_tree errors node =
  ignore (errors, node);
  Error "TODO: check_tree"
(*  let children = Ts_wrap.collect_all_children node in
  let f node acc =


  Core.List.fold_right ~f ~init:[] children


(*        match string_of_ts_node_type child with
        | "ERROR" | "MISSING" -> fold (child :: acc) index
        | _ -> fold errors (child :: normal) index)
 *)
 *)
