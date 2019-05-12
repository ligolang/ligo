(* Lexical tokens for Mini-ML *)

type t =
  (* Symbols *)

  ARROW    (* "->" *)
| CONS     (* "::" *)
| CAT      (* "^"  *)
| APPEND   (* "@"  *)

  (* Arithmetics *)

| MINUS    (* "-" *)
| PLUS     (* "+" *)
| SLASH    (* "/" *)
| TIMES    (* "*" *)

  (* Compounds *)

| LPAR     (* "(" *)
| RPAR     (* ")" *)
| LBRACK   (* "[" *)
| RBRACK   (* "]" *)
| LBRACE   (* "{" *)
| RBRACE   (* "}" *)

  (* Separators *)

| COMMA    (* "," *)
| SEMI     (* ";" *)
| VBAR     (* "|" *)
| COLON    (* ":" *)
| DOT      (* "." *)

  (* Wildcard *)

| WILD     (* "_" *)

  (* Comparisons *)

| EQ       (* "="  *)
| NE       (* "<>" *)
| LT       (* "<"  *)
| GT       (* ">"  *)
| LE       (* "=<" *)
| GE       (* ">=" *)

| BOOL_OR  (* "||" *)
| BOOL_AND (* "&&" *)

  (* Identifiers, labels, numbers and strings *)

| Ident  of string
| Constr of string
| Int    of (string * Z.t)
| Nat    of (string * Z.t)
| Mtz    of (string * Z.t)
| Str    of string

  (* Keywords *)

| And
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

  (* Liquidity specific *)

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

val to_string: t -> string
