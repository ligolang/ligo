(* The main module of the preprocessor (see [lex]) *)

(* Regions *)

module Region = Simple_utils.Region

val mk_reg : Lexing.lexbuf -> Region.t

(* Errors *)

type error =
  Directive_inside_line
| Missing_endif
| Invalid_line_indicator of string
| No_line_indicator
| End_line_indicator
| Newline_in_string    (* For #include argument only *)
| Unterminated_string  (* For #include argument only *)
| Dangling_endif
| Open_region_in_conditional
| Dangling_endregion
| Conditional_in_region
| If_follows_elif
| Else_follows_else
| Dangling_else
| Elif_follows_else
| Dangling_elif
| Reserved_symbol of string
| Multiply_defined_symbol of string
| Error_directive of string
| Parse_error
| Invalid_symbol
| File_not_found of string
| Invalid_character of char
| Unterminated_comment of string
| Unterminated_inclusion

val format : error Region.reg -> string Region.reg

(* Preprocessing a lexing buffer *)

val lex :
  EvalOpt.options ->
  Lexing.lexbuf ->
  (Buffer.t, Buffer.t * error Region.reg) Stdlib.result

(* Evaluation of boolean expressions *)

module Env : Set.S with type elt = string

val eval : Env.t -> E_AST.t -> bool
