(* This file provides an interface to the ReasonLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module CST    = Cst.Reasonligo
module Errors = Parsing_shared.Errors

(* Results and errors *)

type cst    = (CST.t,    Errors.t) Trace.result
type expr   = (CST.expr, Errors.t) Trace.result
type buffer = (Buffer.t, Errors.t) Trace.result

(* Parsing *)

type file_path = string

(* All function read a string buffer but they differ in the way they
   interpret it: [from_file] assumes that its contents comes
   originally from a file, [from_string] assumes that its contents
   comes originally from a string, and [expression] assumes that is
   contents is an expression and comes from a string. *)

val from_file   : Buffer.t -> file_path -> cst
val from_string : Buffer.t -> cst
val expression  : Buffer.t -> expr

(* Aliases *)

val parse_file       : Buffer.t -> file_path -> cst
val parse_string     : Buffer.t -> cst
val parse_expression : Buffer.t -> expr

(* Pretty-printing *)

(* The function [pretty_print_file] reads a string buffer and assumes
   that its contents originally comes from a file. *)

val pretty_print            : CST.t -> Buffer.t
val pretty_print_expression : CST.expr -> Buffer.t
val pretty_print_pattern    : CST.pattern -> Buffer.t
val pretty_print_type_expr  : CST.type_expr -> Buffer.t
val pretty_print_file       : Buffer.t -> file_path -> buffer
val pretty_print_cst        : Buffer.t -> file_path -> buffer
