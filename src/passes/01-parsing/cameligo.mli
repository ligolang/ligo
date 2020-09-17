(** This file provides an interface to the CameLIGO parser. *)

open Trace
module CST = Cst.Cameligo

(** Open a CameLIGO filename given by string and convert into an
    abstract syntax tree. *)
val parse_file : string -> (CST.t , Errors.parser_error) result

(** Convert a given string into a CameLIGO abstract syntax tree *)
val parse_string : string -> (CST.t , Errors.parser_error) result

(** Parse a given string as a CameLIGO expression and return an
    expression CST.

    This is intended to be used for interactive interpreters, or other
    scenarios where you would want to parse a CameLIGO expression
    outside of a contract. *)
val parse_expression : string -> (CST.expr , Errors.parser_error) result

(** Preprocess a given CameLIGO file and preprocess it. *)
val preprocess : string -> (Buffer.t , Errors.parser_error) result

(** Pretty-print a given CameLIGO file (after parsing it). *)
val pretty_print_from_source : string -> (Buffer.t, Errors.parser_error) result

(** Take a CameLIGO cst and pretty_print it *)
val pretty_print : CST.t -> (Buffer.t, _) result

val pretty_print_expression : CST.expr -> (Buffer.t, _) result

val pretty_print_pattern: CST.pattern -> (Buffer.t, _) result

val pretty_print_type_expr: CST.type_expr -> (Buffer.t, _) result