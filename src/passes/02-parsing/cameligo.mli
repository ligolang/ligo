(* This file provides an interface to the CameLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module CST    = Cst_cameligo.CST
module Errors = Parsing_shared.Errors

(* Parsing *)

type file_path = string

type raise = (Errors.t, Main_warnings.all) Trace.raise

type 'a parser = raise:raise -> Buffer.t -> 'a

(* All functions read a string buffer but they differ in the way they
   interpret it: [from_file] assumes that its contents comes
   originally from a file, [from_string] assumes that its contents
   comes originally from a string, and [expression] assumes that is
   contents is an expression and comes from a string. *)

val from_file   : (file_path -> CST.t) parser
val from_string : CST.t parser
val expression  : CST.expr parser

(* Aliases *)

val parse_file       : (file_path -> CST.t) parser
val parse_string     : CST.t parser
val parse_expression : CST.expr parser

(* Pretty-printing *)

val pretty_print            : CST.t -> Buffer.t
val pretty_print_expression : CST.expr -> Buffer.t
val pretty_print_pattern    : CST.pattern -> Buffer.t
val pretty_print_type_expr  : CST.type_expr -> Buffer.t

(* The function [pretty_print_file] reads a string buffer and assumes
   that its contents originally comes from a file. *)

val pretty_print_file : (file_path -> Buffer.t) parser
val pretty_print_cst  : (file_path -> Buffer.t) parser
