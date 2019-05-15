(* Abstract lexical tokens for Mini-ML *)

type t =
  (* Symbols *)

  ARROW
| CONS
| CAT
  (*| APPEND*)
| MINUS
| PLUS
| SLASH
| TIMES
| LPAR
| RPAR
| LBRACKET
| RBRACKET
| LBRACE
| RBRACE
| COMMA
| SEMI
| VBAR
| COLON
| DOT
| WILD
| EQ
| NE
| LT
| GT
| LE
| GE
| BOOL_OR
| BOOL_AND

(* Identifiers, numbers and strings *)

| Ident  of string
| Constr of string
| Int    of (string * Z.t)
| Nat    of (string * Z.t)
| Mtz    of (string * Z.t)
| Str    of string
| Bytes  of (string * Hex.t)

(* Keywords *)

(*| And*)
| Begin
| Else
| End
| False
| Fun
| If
| In
| Let
| List
| Map
| Match
| Mod
| Not
| Of
| Or
| Set
| Then
| True
| Type
| With

| LetEntry
| MatchNat

(*
| Contract
| Sig
| Struct
*)

(* Virtual tokens *)

| EOF (* End of file *)

type token = t

let sprintf = Printf.sprintf

let to_string = function
  ARROW    -> "->"
| CONS     -> "::"
| CAT      -> "^"
(*| APPEND   -> "@"*)
| MINUS    -> "-"
| PLUS     -> "+"
| SLASH    -> "/"
| TIMES    -> "*"
| LPAR     -> "("
| RPAR     -> ")"
| LBRACKET -> "["
| RBRACKET -> "]"
| LBRACE   -> "{"
| RBRACE   -> "}"
| COMMA    -> ","
| SEMI     -> ";"
| VBAR     -> "|"
| COLON    -> ":"
| DOT      -> "."
| WILD     -> "_"
| EQ       -> "="
| NE       -> "<>"
| LT       -> "<"
| GT       -> ">"
| LE       -> "<="
| GE       -> ">="
| BOOL_OR  -> "||"
| BOOL_AND -> "&&"
| Ident id -> sprintf "Ident %s"   id
| Constr id -> sprintf "Constr %s" id
| Int (lex,z) -> sprintf "Int %s (%s)" lex (Z.to_string z)
| Nat (lex,z) -> sprintf "Nat %s (%s)" lex (Z.to_string z)
| Mtz (lex,z) -> sprintf "Mtz %s (%s)" lex (Z.to_string z)
| Str n    -> sprintf "Str \"%s\"" n
| Bytes (lex,h) -> sprintf "Bytes %s (0x%s)" lex (Hex.to_string h)
(*| And      -> "and"*)
| Begin    -> "begin"
| Else     -> "else"
| End      -> "end"
| False    -> "false"
| Fun      -> "fun"
| If       -> "if"
| In       -> "in"
| Let      -> "let"
| List     -> "list"
| Map      -> "map"
| Match    -> "match"
| Mod      -> "mod"
| Not      -> "not"
| Of       -> "of"
| Or       -> "or"
| Set      -> "set"
| Then     -> "then"
| True     -> "true"
| Type     -> "type"
| With     -> "with"
| LetEntry -> "let%entry"
| MatchNat -> "match%nat"
| EOF      -> "EOF"
