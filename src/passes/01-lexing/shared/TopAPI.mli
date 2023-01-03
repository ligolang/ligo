(* This module is a wrapper for running the LIGO lexers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Lexbuf = Simple_utils.Lexbuf
module Unit   = LexerLib.Unit

module type PARAMETERS = LexerLib.CLI.PARAMETERS

(* Registering warnings *)

module type WARNING =
  sig
    val add : Main_warnings.all -> unit
  end

(* This module factors the common actions expected from LexerMain in
   all LIGO syntaxes, like reading and checking the command-line,
   building the preprocessor, the lexer, composing and calling them,
   applying self-passes on the lexical units, filtering the tokens,
   and finally applying self-passes on them. *)

module type S =
  sig
    module Token : Token.S

    (* Checking the CLI

       Note that the function [check_cli] is pure and therefore leaves
       to its callers to perform the side-effect of printing any error
       and information, nor not. *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    val check_cli : unit -> cli_status

    (* Errors *)

    (* The type ['item error] denotes erroneous values. The field
       [preprocessed] holds the result of preprocessing, if any (it
       could have failed). The field [use_items] contains the list of
       items, which can either be lexical units or tokens, up to the
       error. The field [message] is the error message. *)

    type message = string Region.reg

    type 'item error = {
      preprocessed : Buffer.t option;
      used_items   : 'item list;
      message      : message
    }

    (* Lexer for tokens with Menhir in mind. *)

    (* Menhir requests tokens on-demand, one by one, and expects the
       lexer to have type [Lexing.lexbuf -> token]. The function
       [scan_token] internally scans all tokens first (see
       [scan_all_tokens] below) before feeding them to the parser. The
       return type is of type [(token, token error) result], which
       means that the caller of [scan_token] has to project the result
       and use an exception in case of error. *)

    type token = Token.t

    val scan_token : no_colour:bool -> Lexing.lexbuf -> (token, token error) result

    val used_tokens : unit -> token list

    val clear : unit -> unit

    (* Scanning all lexical units *)

    (* The first component returned by a lexer of type ['a lexer] is a
       value of type [Std.t], which is a record containing two string
       fields: one meant to be sent to [stdout] and the other to
       [stderr]. It is up to the client of this module to perform the
       side-effect of printing them or not. *)

    type 'a lexer = Lexbuf.input -> Std.t * ('a list, 'a error) result

    (* Scanning all lexical units. If specified by [Options], the
       preprocessor may be run before, and/or the pipeline of
       self-passes [UnitPasses] after. *)

    val scan_all_units : no_colour:bool -> token Unit.t lexer

    (* Scanning all tokens. *)

    (* Calling [scan_all_tokens] is equivalent to calling first
       [scan_all_units], then the self-passes on the lexical units,
       followed by filtering the tokens (see [filter_tokens] below),
       and then applying the self-passes on the tokens. The value of
       type [Std.t] produced by [scan_all_units] and [scan_all_tokens]
       has been used by the self-passes to accumulate their I/O. *)

    val scan_all_tokens : no_colour:bool -> token lexer
  end

module Make
         (Preprocessor : Preprocessor.TopAPI.S)
         (Parameters   : PARAMETERS)
         (Token        : Token.S)
         (UnitPasses   : Pipeline.PASSES with type item = Token.t Unit.t)
         (TokenPasses  : Pipeline.PASSES with type item = Token.t)
         (Warning      : WARNING)
       : S with module Token = Token
