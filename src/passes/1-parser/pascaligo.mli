(** This file provides an interface to the PascaLIGO parser. *)

module AST = Parser_pascaligo.AST

(** Open a PascaLIGO filename given by string and convert into an
    abstract syntax tree. *)
val parse_file : string -> AST.t Trace.result

(** Convert a given string into a PascaLIGO abstract syntax tree *)
val parse_string : string -> AST.t Trace.result

(** Parse a given string as a PascaLIGO expression and return an
    expression AST.

    This is intended to be used for interactive interpreters, or other
    scenarios where you would want to parse a PascaLIGO expression
    outside of a contract. *)
val parse_expression : string -> AST.expr Trace.result
