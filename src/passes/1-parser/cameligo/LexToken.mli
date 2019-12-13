(* This signature defines the lexical tokens for LIGO

   _Tokens_ are the abstract units which are used by the parser to
   build the abstract syntax tree (AST), in other words, the stream of
   tokens is the minimal model of the input program, carrying
   implicitly all its structure in a linear encoding, and nothing
   else, in particular, comments and whitespace are absent.

     A _lexeme_ is a specific character string (concrete
   representation) denoting a token (abstract representation). Tokens
   can be thought of as sets, and lexemes as elements of those sets --
   there is often an infinite number of lexemes, but a small number of
   tokens. (Think of identifiers as lexemes and one token.)

     The tokens are qualified here as being "lexical" because the
   parser generator Menhir expects to define them, in which context
   they are called "parsing tokens", and they are made to match each
   other. (This is an idiosyncratic terminology.)

     The type of the lexical tokens is the variant [t], also
   aliased to [token].
*)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

type lexeme = string

(* TOKENS *)

type t =
  (* Symbols *)

  ARROW of Region.t  (* "->" *)
| CONS  of Region.t  (* "::" *)
| CAT   of Region.t  (* "^"  *)
(*| APPEND   (* "@"  *)*)

  (* Arithmetics *)

| MINUS of Region.t    (* "-" *)
| PLUS  of Region.t    (* "+" *)
| SLASH of Region.t    (* "/" *)
| TIMES of Region.t    (* "*" *)

  (* Compounds *)

| LPAR     of Region.t  (* "(" *)
| RPAR     of Region.t  (* ")" *)
| LBRACKET of Region.t  (* "[" *)
| RBRACKET of Region.t  (* "]" *)
| LBRACE   of Region.t  (* "{" *)
| RBRACE   of Region.t  (* "}" *)

  (* Separators *)

| COMMA of Region.t  (* "," *)
| SEMI  of Region.t  (* ";" *)
| VBAR  of Region.t  (* "|" *)
| COLON of Region.t  (* ":" *)
| DOT   of Region.t  (* "." *)

  (* Wildcard *)

| WILD of Region.t  (* "_" *)

  (* Comparisons *)

| EQ of Region.t      (* "="  *)
| NE of Region.t      (* "<>" *)
| LT of Region.t      (* "<"  *)
| GT of Region.t      (* ">"  *)
| LE of Region.t      (* "=<" *)
| GE of Region.t      (* ">=" *)

| BOOL_OR  of Region.t (* "||" *)
| BOOL_AND of Region.t (* "&&" *)

  (* Identifiers, labels, numbers and strings *)

| Ident  of string Region.reg
| Constr of string Region.reg
| Int    of (string * Z.t) Region.reg
| Nat    of (string * Z.t) Region.reg
| Mutez  of (string * Z.t) Region.reg
| String of string Region.reg
| Bytes  of (string * Hex.t) Region.reg

  (* Keywords *)

(*| And*)
| Begin of Region.t
| Else  of Region.t
| End   of Region.t
| False of Region.t
| Fun   of Region.t
| If    of Region.t
| In    of Region.t
| Let   of Region.t
| Match of Region.t
| Mod   of Region.t
| Not   of Region.t
| Of    of Region.t
| Or    of Region.t
| Then  of Region.t
| True  of Region.t
| Type  of Region.t
| With  of Region.t

(* Data constructors *)

| C_None  of Region.t  (* "None"  *)
| C_Some  of Region.t  (* "Some"  *)

(* Virtual tokens *)

| EOF of Region.t (* End of file *)

type token = t

(* Projections

   The difference between extracting the lexeme and a string from a
   token is that the latter is the textual representation of the OCaml
   value denoting the token (its abstract syntax), rather than its
   lexeme (concrete syntax).
*)

val to_lexeme : token -> lexeme
val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string
val to_region : token -> Region.t

(* Injections *)

type   int_err = Non_canonical_zero
type ident_err = Reserved_name
type   nat_err = Invalid_natural
               | Non_canonical_zero_nat
type   sym_err = Invalid_symbol

val mk_int    : lexeme -> Region.t -> (token,   int_err) result
val mk_nat    : lexeme -> Region.t -> (token,   nat_err) result
val mk_mutez  : lexeme -> Region.t -> (token,   int_err) result
val mk_ident  : lexeme -> Region.t -> (token, ident_err) result
val mk_sym    : lexeme -> Region.t -> (token,   sym_err) result
val mk_string : lexeme -> Region.t -> token
val mk_bytes  : lexeme -> Region.t -> token
val mk_constr : lexeme -> Region.t -> token
val eof       : Region.t -> token

(* Predicates *)

val is_string : token -> bool
val is_bytes  : token -> bool
val is_int    : token -> bool
val is_ident  : token -> bool
val is_kwd    : token -> bool
val is_constr : token -> bool
val is_sym    : token -> bool
val is_eof    : token -> bool
