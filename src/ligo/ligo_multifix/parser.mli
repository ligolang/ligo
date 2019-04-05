
(* The type of tokens. *)

type token = Lex.Token.token

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val entry_point: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.entry_point Location.wrap)
