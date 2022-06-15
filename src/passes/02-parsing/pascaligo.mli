(* Interfacing the PascaLIGO parser with the compiler *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module CST    = Cst_pascaligo.CST
module Errors = Parsing_shared.Errors

(* Parsing *)

type file_path = string

(* All function read a string buffer but they differ in the way they
   interpret it: [from_file] assumes that its contents comes
   originally from a file, [from_string] assumes that its contents
   comes originally from a string, and [expression] assumes that is
   contents is an expression and comes from a string. *)

type 'a parser = raise:Errors.t Trace.raise -> Buffer.t -> 'a

val from_file   : add_warning:(Main_warnings.all -> unit) -> (file_path -> CST.t) parser
val from_string : add_warning:(Main_warnings.all -> unit) -> CST.t parser
val expression  : add_warning:(Main_warnings.all -> unit) -> CST.expr parser

(* Aliases *)

val parse_file       : add_warning:(Main_warnings.all -> unit) -> (file_path -> CST.t) parser
val parse_string     : add_warning:(Main_warnings.all -> unit) -> CST.t parser
val parse_expression : add_warning:(Main_warnings.all -> unit) -> CST.expr parser

(* Pretty-printing *)

(* The function [pretty_print_file] reads a string buffer and assumes
   that its contents originally comes from a file. *)

val pretty_print            : CST.t -> Buffer.t
val pretty_print_expression : CST.expr -> Buffer.t
val pretty_print_pattern    : CST.pattern -> Buffer.t
val pretty_print_type_expr  : CST.type_expr -> Buffer.t

val pretty_print_file :
  add_warning:(Main_warnings.all -> unit) -> raise:Errors.t Trace.raise -> Buffer.t -> file_path -> Buffer.t

val pretty_print_cst :
  add_warning:(Main_warnings.all -> unit) -> raise:Errors.t Trace.raise -> Buffer.t -> file_path -> Buffer.t
