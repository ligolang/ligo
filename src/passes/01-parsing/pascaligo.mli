(** This file provides an interface to the PascaLIGO parser. *)

open Errors
open Trace
module CST = Cst.Pascaligo

(** Open a PascaLIGO filename given by string and convert into an
    abstract syntax tree. *)
val parse_file : string list -> string -> (CST.t, parser_error) result

(** Convert a given string into a PascaLIGO abstract syntax tree *)
val parse_program_string : string list -> string -> (CST.t , Errors.parser_error) result
val parse_program_stdin : string list -> unit -> (CST.t , Errors.parser_error) result

(** Parse a given string as a PascaLIGO expression and return an
    expression CST.

    This is intended to be used for interactive interpreters, or other
    scenarios where you would want to parse a PascaLIGO expression
    outside of a contract. *)
val parse_expression : string list -> string -> (CST.expr, parser_error) result

(** Preprocess a given PascaLIGO file and preprocess it. *)
val preprocess : string list -> string -> (Buffer.t, parser_error) result

(** Take a PascaLIGO cst and pretty_print it *)
val pretty_print : CST.t -> (Buffer.t, _) result

val pretty_print_expression : CST.expr -> (Buffer.t, _) result
