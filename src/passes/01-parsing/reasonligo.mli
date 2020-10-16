(* This file provides an interface to the ReasonLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module CST = Cst.Reasonligo

(* Results and errors *)

type error  = Errors.parse_error
type cst    = (CST.t, error) Trace.result
type expr   = (CST.expr, error) Trace.result
type buffer = (Buffer.t , error) Trace.result

(* Some parameters' types *)

type file_path = string
type dirs      = file_path list (* For #include directives *)

(* Parsing *)

val parse_file           : dirs -> file_path -> cst (* contract in a file   *)
val parse_program_string : dirs -> string -> cst    (* contract in a string *)
val parse_program_stdin  : dirs -> unit -> cst      (* contract in stdin    *)
val parse_expression     : dirs -> string -> expr   (* expr in a string     *)

(* Preprocessing *)

val preprocess : dirs -> file_path -> buffer (* from a file *)

(* Pretty-printing *)

val pretty_print             : CST.t -> Buffer.t
val pretty_print_expression  : CST.expr -> Buffer.t
val pretty_print_pattern     : CST.pattern -> Buffer.t
val pretty_print_type_expr   : CST.type_expr -> Buffer.t
val pretty_print_from_source : dirs -> file_path -> buffer (* from a file *)
