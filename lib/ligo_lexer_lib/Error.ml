(* Lexing errors *)

type error =
  Invalid_utf8_sequence
| Unterminated_comment
| Unterminated_string
| Newline_in_string
| Invalid_character_in_string of char
| Undefined_escape_sequence
| Invalid_directive of Preprocessor.Error.t

type t = error

let sprintf = Printf.sprintf

let to_string = function
  Invalid_utf8_sequence ->
    "Invalid UTF-8 sequence."
| Undefined_escape_sequence ->
    "Undefined escape sequence.\n\
     Hint: Remove or replace the sequence."
| Unterminated_string ->
    "Unterminated string.\n\
     Hint: Close with double quotes."
| Unterminated_comment ->
    sprintf "Unterminated comment."
| Newline_in_string ->
    "The string starting here is interrupted by a line break.\n\
     Hint: Remove the break, close the string before or insert a \
     backslash."
| Invalid_character_in_string c ->
    sprintf "Invalid character %S in string.\n\
             Hint: Remove or replace the character." (Char.escaped c)
| Invalid_directive err ->
    Preprocessor.Error.to_string err
