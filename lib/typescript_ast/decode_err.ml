(* Errors for decoding standalone expressions and type expressions

   NOTE: This is the consequence of the lack of multiple entrypoints
   in Tree-sitter grammars.
 *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap

(* Errors *)

type t =
  | No_single_expression (* When parsing an expression *)
  | No_single_type_expr (* When parsing a type expression *)

type error = t

let to_string = function
  | No_single_expression -> "No single expression found."
  | No_single_type_expr -> "No single type expression found."

(* Creating errors *)

let make (region : Region.t) (error : t) =
  let value = Printf.sprintf "%s:\n%s" (region#to_string `Byte) (to_string error) in
  Error Region.{value; region}
