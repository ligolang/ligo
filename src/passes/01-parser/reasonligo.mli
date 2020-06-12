(** This file provides an interface to the ReasonLIGO parser. *)

open Trace
module AST = Parser_cameligo.AST

(** Open a ReasonLIGO filename given by string and convert into an
    abstract syntax tree. *)
val parse_file : string -> (AST.t , Errors.parser_error) result

(** Convert a given string into a ReasonLIGO abstract syntax tree *)
val parse_string : string -> (AST.t , Errors.parser_error) result

(** Parse a given string as a ReasonLIGO expression and return an
    expression AST.

    This is intended to be used for interactive interpreters, or other
    scenarios where you would want to parse a ReasonLIGO expression
    outside of a contract. *)
val parse_expression : string -> (AST.expr , Errors.parser_error) result

(** Preprocess a given ReasonLIGO file and preprocess it. *)
val preprocess : string -> (Buffer.t , Errors.parser_error) result

(** Pretty-print a given CameLIGO file (after parsing it). *)
val pretty_print : string -> (Buffer.t , Errors.parser_error) result
