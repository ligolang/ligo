(* Extracting a decorator from a line comment *)

type name = string
type argument = string

val scan : Lexing.lexbuf -> (name * argument option) option
