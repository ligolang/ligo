(* This file provides an interface to the CameLIGO parser. *)

(* Vendor dependencies *)

module Trace   = Simple_utils.Trace
module Options = LexerLib.Options

(* Internal dependencies *)

module CST    = Cst_cameligo.CST
module Errors = Parsing_shared.Errors
module Pretty = Parsing_cameligo.Pretty

(* The functor *)

module Make (Options: Options.S) :
  sig
    (* Parsing *)

    type file_path = string

    type raise = (Errors.t, Main_warnings.all) Trace.raise

    type 'a parser =
      ?jsligo:string option option ->
      ?preprocess:bool ->
      ?project_root:file_path ->
      raise:raise ->
      Buffer.t ->
      'a

    (* All functions read a string buffer but they differ in the way
       they interpret it: [from_file] assumes that its contents comes
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

    (* Pretty-printing from source *)

    (* The function [pretty_print_file] reads a string buffer and
       assumes that its contents originally comes from a file. *)

    val pretty_print_file : Pretty.state -> (file_path -> Buffer.t) parser
    val pretty_print_cst  : (file_path -> Buffer.t) parser
  end

(* Pretty-printing from the CST *)

val pretty_print            : Pretty.state -> CST.t -> Buffer.t
val pretty_print_expression : Pretty.state -> CST.expr -> Buffer.t
val pretty_print_pattern    : ?cols:int -> Pretty.state -> CST.pattern -> Buffer.t
val pretty_print_type_expr  : Pretty.state -> CST.type_expr -> Buffer.t
val pretty_print_signature_expr  : Pretty.state -> CST.signature_expr -> Buffer.t
