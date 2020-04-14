(** This file provides an interface to the ReasonLIGO parser. *)

module AST = Parser_cameligo.AST

(** Open a ReasonLIGO filename given by string and convert into an
    abstract syntax tree. *)
val parse_file : string -> AST.t Trace.result

(** Convert a given string into a ReasonLIGO abstract syntax tree *)
val parse_string : string -> AST.t Trace.result

(** Parse a given string as a ReasonLIGO expression and return an
    expression AST.

    This is intended to be used for interactive interpreters, or other
    scenarios where you would want to parse a ReasonLIGO expression
    outside of a contract. *)
val parse_expression : string -> AST.expr Trace.result

(** Preprocess a given ReasonLIGO file and preprocess it. *)
val preprocess : string -> Buffer.t Trace.result
