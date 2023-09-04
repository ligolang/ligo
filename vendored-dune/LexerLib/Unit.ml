(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Directive = Preprocessor.Directive

(* Lexical units *)

type 'token lex_unit = [
  `Token     of 'token
| `Markup    of Markup.t
| `Directive of Directive.t
]

type 'token t = 'token lex_unit

(* Printing *)

type 'a formatter =
  offsets:bool -> [`Byte | `Point] -> 'a -> string

let to_string ~token_to_string ~offsets mode = function
  `Token token ->
     token_to_string ~offsets mode token
| `Markup markup ->
     Markup.to_string ~offsets mode markup
| `Directive directive ->
     Directive.to_string ~offsets mode directive

let to_lexeme ~token_to_lexeme = function
  `Token token -> token_to_lexeme token
| `Markup markup -> [Markup.to_lexeme markup]
| `Directive directive -> [(Directive.to_lexeme directive).Region.value]
