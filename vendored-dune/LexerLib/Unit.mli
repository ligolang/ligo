(* Lexical units *)

(* Vendor dependencies *)

module Directive = Preprocessor.Directive

(* Note how lexical units ['token lex_unit] are parameterised by the
   type ['token] of the tokens, because the lexer library must not
   make any assumption about the tokens. *)

type 'token lex_unit = [
  `Token     of 'token
| `Markup    of Markup.t
| `Directive of Directive.t
]

type 'token t = 'token lex_unit

(* Printing *)

type 'a formatter =
  offsets:bool -> [`Byte | `Point] -> 'a -> string

val to_string :
  token_to_string:('token formatter) -> 'token t formatter

val to_lexeme :
  token_to_lexeme:('token -> string) -> 'token t -> string
