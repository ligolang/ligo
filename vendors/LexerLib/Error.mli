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

val to_string : t -> string
