(* Attributes *)

type key = string

type value =
  String of string
| Ident  of string

type attribute = key * value option

type t = attribute

val to_lexeme : attribute -> string
val to_string : attribute -> string
