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

     The signature [S] exports an abstract type [token], so a lexer
   can be a functor over tokens. This enables to externalise
   version-dependent constraints in any module whose signature matches
   [S]. Generic functions to construct tokens are required.

     Note the predicate [is_eof], which caracterises the virtual token
   for end-of-file, because it requires special handling. Some of
   those functions may yield errors, which are defined as values of
   the type [int_err] etc. These errors can be better understood by
   reading the ocamllex specification for the lexer ([Lexer.mll]). *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Directive = Preprocessor.Directive

(* TOKENS *)

module type S =
  sig
    type lexeme = string

    type token
    type t = token

    (* Projections

       The difference between extracting the lexeme and a string from
       a token is that the latter is the textual representation of the
       OCaml value denoting the token (its abstract syntax), rather
       than its lexeme (concrete syntax). Note that [concrete] is used
       by the module [UnlexerGen] to transform the textual
       representation of a token (not a lexeme) into a lexeme. *)

    val to_lexeme : token -> lexeme list
    val to_string : offsets:bool -> [`Byte | `Point] -> token -> string
    val to_region : token -> Region.t
    val concrete  : string -> lexeme

    (* TRANSFORMATIONS *)

    val add_directive : Directive.t -> token -> token

    (* INJECTIONS *)

    (* Preprocessing directives *)

    val mk_directive : Directive.t -> token

    (* Integers *)

    val mk_int : lexeme -> Z.t -> Region.t -> token

    (* Natural numbers *)

    type nat_err = Wrong_nat_syntax of string (* Hint *)

    val mk_nat :
      lexeme -> Z.t -> Region.t -> (token, nat_err) result

    (* Mutez *)

    type mutez_err = Wrong_mutez_syntax of string (* Hint *)

    val mk_mutez :
      lexeme -> suffix:string -> Int64.t ->
      Region.t -> (token, mutez_err) result

    (* Symbols *)

    type sym_err = Invalid_symbol of string

    val mk_sym : lexeme -> Region.t -> (token, sym_err) result

    (* Keywords *)

    type kwd_err = Invalid_keyword

    val mk_kwd : lexeme -> Region.t -> (token, kwd_err) result

    (* Code injection *)

    type lang_err = Wrong_lang_syntax of string (* Hint *)

    val mk_lang :
      lexeme Region.reg -> Region.t -> (token, lang_err) result

    (* Bytes *)

    val mk_bytes : lexeme -> string -> Region.t -> token

    (* Attributes *)

    val mk_attr :
      key:Attr.key -> ?value:Attr.value -> Region.t -> token

    (* Others *)

    val mk_ident    : lexeme -> Region.t -> token
    val mk_string   : lexeme -> Region.t -> token
    val mk_verbatim : lexeme -> Region.t -> token
    val mk_uident   : lexeme -> Region.t -> token
    val mk_eof      : Region.t -> token

    (* Predicates *)

    val is_int    : token -> bool
    val is_string : token -> bool
    val is_bytes  : token -> bool
    val is_hex    : token -> bool
    val is_sym    : token -> bool
    val is_eof    : token -> bool

    (* Verbatim strings *)

    val verbatim_delimiters : string * string
  end
