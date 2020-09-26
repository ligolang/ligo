(* Module for lexing boolean expressions of conditional directives *)

(* Regions *)

module Region = Simple_utils.Region

val string_of_token : E_Parser.token -> string

(* Errors *)

type error = Invalid_character of char

val error_to_string : error -> string

val format : error Region.reg -> string Region.reg

(* Lexing boolean expressions (may raise [Error]) *)

exception Error of error Region.reg

val scan : Lexing.lexbuf -> E_Parser.token
