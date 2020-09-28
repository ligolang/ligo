(* Lexer specification for LIGO, to be processed by [ocamllex].

   The underlying design principles are:

     (1) enforce stylistic constraints at a lexical level, in order to
         early reject potentially misleading or poorly written
         LIGO contracts;

     (2) provide precise error messages with hints as how to fix the
         issue, which is achieved by consulting the lexical
         right-context of lexemes;

     (3) be as independent as possible from the LIGO version, so
         upgrades have as little impact as possible on this
         specification: this is achieved by using the most general
         regular expressions to match the lexing buffer and broadly
         distinguish the syntactic categories, and then delegating a
         finer, second analysis to an external module making the
         tokens (hence a functor below);

     (4) support unit testing (lexing of the whole input with debug
         traces).

     A limitation to the independence with respect to the LIGO version
   lies in the errors that the external module building the tokens
   (which may be version-dependent) may have to report. Indeed these
   errors have to be contextualised by the lexer in terms of input
   source regions, so useful error messages can be printed, therefore
   they are part of the signature [TOKEN] that parameterises the
   functor generated here. For instance, if, in a future release of
   LIGO, new tokens are added, and the recognition of their lexemes
   entails new errors, the signature [TOKEN] will have to be augmented
   and this lexer specification changed. However, in practice, it is
   more likely that instructions or types will be added, instead of
   new kinds of tokens.
*)

module Region = Simple_utils.Region
module Pos = Simple_utils.Pos

(* TOKENS *)

(* The signature [TOKEN] exports an abstract type [token], so a lexer
   can be a functor over tokens. This enables to externalise
   version-dependent constraints in any module whose signature matches
   [TOKEN]. Generic functions to construct tokens are required.

   Note the predicate [is_eof], which caracterises the virtual token
   for end-of-file, because it requires special handling. Some of
   those functions may yield errors, which are defined as values of
   the type [int_err] etc. These errors can be better understood by
   reading the ocamllex specification for the lexer ([Lexer.mll]).
*)

type lexeme = string

module type TOKEN =
  sig
    type token

    (* Errors *)

    type   int_err = Non_canonical_zero
    type ident_err = Reserved_name
    type   nat_err = Invalid_natural
                   | Non_canonical_zero_nat
    type   sym_err = Invalid_symbol

    (* Injections *)

    val mk_int      : lexeme -> Region.t -> (token,   int_err) result
    val mk_nat      : lexeme -> Region.t -> (token,   nat_err) result
    val mk_mutez    : lexeme -> Region.t -> (token,   int_err) result
    val mk_ident    : lexeme -> Region.t -> (token, ident_err) result
    val mk_sym      : lexeme -> Region.t -> (token,   sym_err) result
    val mk_string   : lexeme -> Region.t -> token
    val mk_verbatim : lexeme -> Region.t -> token
    val mk_bytes    : lexeme -> Region.t -> token
    val mk_constr   : lexeme -> Region.t -> token
    val mk_attr     : lexeme -> Region.t -> token
    val mk_lang     : lexeme Region.reg -> Region.t -> token
    val eof         : Region.t -> token

    (* Predicates *)

    val is_eof    : token -> bool

    (* Projections *)

    val to_lexeme : token -> lexeme
    val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string
    val to_region : token -> Region.t

    (* Style *)

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool ->
      [`Byte | `Point] ->
      error Region.reg ->
      file:bool ->
      string Region.reg

    val check_right_context :
      token ->
      (Lexing.lexbuf -> (Markup.t list * token) option) ->
      Lexing.lexbuf ->
      unit
  end

(* The signature of the lexer *)

module type S =
  sig
    module Token : TOKEN
    type token = Token.token

    (* The scanner *)

    val scan : token LexerLib.state -> Lexing.lexbuf -> token LexerLib.state

    (* Errors (specific to the generic lexer, not to the tokens) *)

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool -> [`Byte | `Point] ->
      error Region.reg -> file:bool -> string Region.reg
  end

(* The functorised interface

   Note that the module parameter [Token] is re-exported as a
   submodule in [S].
*)

module Make (Token : TOKEN) : S with module Token = Token
