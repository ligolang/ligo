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

(* Dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module Markup = Lexer_shared.Markup

(* TOKENS *)

type lexeme = string

type t =
  (* Identifiers, labels, numbers and strings *)

| Ident    of string Region.reg
| Constr   of string Region.reg
| Int      of (string * Z.t) Region.reg
| Nat      of (string * Z.t) Region.reg
| Mutez    of (string * Z.t) Region.reg
| String   of string Region.reg
| Verbatim of string Region.reg
| Bytes    of (string * Hex.t) Region.reg
| Attr     of string Region.reg
| Lang     of lexeme Region.reg Region.reg

  (* Symbols *)

| CAT     of Region.t (* "++" *)

  (* Arithmetics *)

| MINUS   of Region.t (* "-" *)
| PLUS    of Region.t (* "+" *)
| SLASH   of Region.t (* "/" *)
| TIMES   of Region.t (* "*" *)

  (* Compounds *)

| LPAR     of Region.t (* "(" *)
| RPAR     of Region.t (* ")" *)
| LBRACKET of Region.t (* "[" *)
| RBRACKET of Region.t (* "]" *)
| LBRACE   of Region.t (* "{" *)
| RBRACE   of Region.t (* "}" *)

  (* Separators *)

| COMMA    of Region.t (* ","   *)
| SEMI     of Region.t (* ";"   *)
| VBAR     of Region.t (* "|"   *)
| COLON    of Region.t (* ":"   *)
| DOT      of Region.t (* "."   *)
| ELLIPSIS of Region.t (* "..." *)
| ARROW    of Region.t (* "=>"  *)

  (* Wildcard *)

| WILD of Region.t     (* "_" *)

  (* Comparisons *)

| EQ   of Region.t (* "="  *)
| EQEQ of Region.t (* "==" *)
| NE   of Region.t (* "!=" *)
| LT   of Region.t (* "<"  *)
| GT   of Region.t (* ">"  *)
| LE   of Region.t (* "<=" *)
| GE   of Region.t (* ">=" *)

  (* Logic *)

| BOOL_OR  of Region.t (* "||" *)
| BOOL_AND of Region.t (* "&&" *)
| NOT      of Region.t (* ! *)

  (* Keywords *)

| Else   of Region.t
| False  of Region.t
| If     of Region.t
| Let    of Region.t
| Mod    of Region.t
| Or     of Region.t
| Rec    of Region.t
| Switch of Region.t
| True   of Region.t
| Type   of Region.t

  (* Data constructors *)

| C_None  of Region.t  (* "None"  *)
| C_Some  of Region.t  (* "Some"  *)

(* Virtual tokens *)

| EOF of Region.t (* End of file *)

(* Projections

   The difference between extracting the lexeme and a string from a
   token is that the latter is the textual representation of the OCaml
   value denoting the token (its abstract syntax), rather than its
   lexeme (concrete syntax).
*)

type token = t

val to_lexeme : token -> lexeme
val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string
val to_region : token -> Region.t

(* comments *)
val block_comment_start : lexeme -> bool
val block_comment_end   : lexeme -> bool
val line_comment_start  : lexeme -> bool

(* Injections *)

type   int_err = Non_canonical_zero
type ident_err = Reserved_name
type   nat_err = Invalid_natural
               | Non_canonical_zero_nat
type   sym_err = Invalid_symbol
type   kwd_err = Invalid_keyword

val mk_int      : lexeme -> Region.t -> (token,   int_err) result
val mk_nat      : lexeme -> Region.t -> (token,   nat_err) result
val mk_mutez    : lexeme -> Region.t -> (token,   int_err) result
val mk_ident    : lexeme -> Region.t -> (token, ident_err) result
val mk_sym      : lexeme -> Region.t -> (token,   sym_err) result
val mk_kwd      : lexeme -> Region.t -> (token,   kwd_err) result
val mk_string   : lexeme -> Region.t -> token
val mk_verbatim : lexeme -> Region.t -> token
val mk_bytes    : lexeme -> Region.t -> token
val mk_constr   : lexeme -> Region.t -> token
val mk_attr     : lexeme -> Region.t -> token
val mk_lang     : lexeme Region.reg -> Region.t -> token
val eof         : Region.t -> token

(* Predicates *)

val is_eof    : token -> bool

(* Style *)

type error

val error_to_string : error -> string

exception Error of error Region.reg

val format_error :
  ?offsets:bool -> [`Byte | `Point] ->
  error Region.reg -> file:bool -> string Region.reg

val check_right_context :
  token ->
  (Lexing.lexbuf -> (Markup.t list * token) option) ->
  Lexing.lexbuf ->
  unit

(* Unlexing the tokens: from strings containing the textual
   representation of tokens to lexemes. For example, [concrete "SEMI"
   = ";"]. *)

val concrete : string -> string
