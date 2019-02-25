(* This signature defines the lexical tokens for Ligo

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

type lexeme = string

(* TOKENS *)

type t =
  (* Literals *)

  String of lexeme Region.reg
| Bytes  of (lexeme * MBytes.t) Region.reg
| Int    of (lexeme * Z.t) Region.reg
| Ident  of lexeme Region.reg
| Constr of lexeme Region.reg

  (* Symbols *)

| SEMI     of Region.t  (* ";"   *)
| COMMA    of Region.t  (* ","   *)
| LPAR     of Region.t  (* "("   *)
| RPAR     of Region.t  (* ")"   *)
| LBRACE   of Region.t  (* "{"   *)
| RBRACE   of Region.t  (* "}"   *)
| LBRACKET of Region.t  (* "["   *)
| RBRACKET of Region.t  (* "]"   *)
| CONS     of Region.t  (* "<:"  *)
| VBAR     of Region.t  (* "|"   *)
| ARROW    of Region.t  (* "->"  *)
| ASGNMNT  of Region.t  (* ":="  *)
| EQUAL    of Region.t  (* "="   *)
| COLON    of Region.t  (* ":"   *)
| OR       of Region.t  (* "||"  *)
| AND      of Region.t  (* "&&"  *)
| LT       of Region.t  (* "<"   *)
| LEQ      of Region.t  (* "<="  *)
| GT       of Region.t  (* ">"   *)
| GEQ      of Region.t  (* ">="  *)
| NEQ      of Region.t  (* "=/=" *)
| PLUS     of Region.t  (* "+"   *)
| MINUS    of Region.t  (* "-"   *)
| SLASH    of Region.t  (* "/"   *)
| TIMES    of Region.t  (* "*"   *)
| DOT      of Region.t  (* "."   *)
| WILD     of Region.t  (* "_"   *)
| CAT      of Region.t  (* "^"   *)

  (* Keywords *)

| Begin      of Region.t
| Const      of Region.t
| Down       of Region.t
| If         of Region.t
| In         of Region.t
| Is         of Region.t
| For        of Region.t
| Function   of Region.t
| Parameter  of Region.t
| Storage    of Region.t
| Type       of Region.t
| Of         of Region.t
| Operations of Region.t
| Var        of Region.t
| End        of Region.t
| Then       of Region.t
| Else       of Region.t
| Match      of Region.t
| Null       of Region.t
| Procedure  of Region.t
| Record     of Region.t
| Step       of Region.t
| To         of Region.t
| Mod        of Region.t
| Not        of Region.t
| While      of Region.t
| With       of Region.t

  (* Data constructors *)

| C_False of Region.t  (* "False" *)
| C_None  of Region.t  (* "None"  *)
| C_Some  of Region.t  (* "Some"  *)
| C_True  of Region.t  (* "True"  *)
| C_Unit  of Region.t  (* "Unit"  *)

  (* Virtual tokens *)

| EOF of Region.t


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

type int_err =
  Non_canonical_zero

type ident_err = Reserved_name

val mk_string : lexeme -> Region.t -> token
val mk_bytes  : lexeme -> Region.t -> token
val mk_int    : lexeme -> Region.t -> (token,   int_err) result
val mk_ident  : lexeme -> Region.t -> (token, ident_err) result
val mk_constr : lexeme -> Region.t -> token
val mk_sym    : lexeme -> Region.t -> token
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
