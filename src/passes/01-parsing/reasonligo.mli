(** This file provides an interface to the ReasonLIGO parser. *)

open Trace
module CST = Cst.Reasonligo

(** Open a ReasonLIGO filename given by string and convert into an
    abstract syntax tree. *)
val parse_file : string list -> string -> (CST.t , Errors.parser_error) result

(** Convert a given string into a ReasonLIGO abstract syntax tree *)
val parse_program_string : string list -> string -> (CST.t , Errors.parser_error) result
val parse_program_stdin : string list -> unit -> (CST.t , Errors.parser_error) result

(** Parse a given string as a ReasonLIGO expression and return an
    expression CST.

    This is intended to be used for interactive interpreters, or other
    scenarios where you would want to parse a ReasonLIGO expression
    outside of a contract. *)
val parse_expression : string list -> string -> (CST.expr , Errors.parser_error) result

(** Preprocess a given ReasonLIGO file and preprocess it. *)
val preprocess : string list -> string -> (Buffer.t , Errors.parser_error) result

(** Pretty-print a given ReasonLIGO file (after parsing it). *)
val pretty_print_from_source : string list -> string -> (Buffer.t , Errors.parser_error) result

(** Take a ReasonLIGO cst and pretty_print it *)
val pretty_print : CST.t -> (Buffer.t, _) result

val pretty_print_expression : CST.expr -> (Buffer.t, _) result

val pretty_print_pattern: CST.pattern -> (Buffer.t, _) result

val pretty_print_type_expr: CST.type_expr -> (Buffer.t, _) result