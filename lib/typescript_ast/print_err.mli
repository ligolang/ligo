(* This module defines the syntax errors when printing the CST *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap

(* Errors *)

type t =
  | Export_keyword
  | Export_clause_or_star

type error = t

val to_string : error -> string
