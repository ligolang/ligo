(* This file provides an interface to the ReasonLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies on ReasonLIGO *)

module File        = Lexer_reasonligo.File
module Comments    = Lexer_reasonligo.Comments
module Token       = Lexer_reasonligo.Token
module Self_lexing   = Lexer_reasonligo.Self_lexing
module ParErr      = Parser_reasonligo.ParErr

(*module Printer   = Cst_reasonligo.Printer*)

module MakeParser = Shared_parser.Common.MakeParser
module MkParser   = MakeParser (File) (Comments) (Token) (ParErr)

module CST = Cst.Reasonligo

(* Parser for contracts *)

module ContractParser_Menhir =
  struct
    include Parser_reasonligo.Parser
    type tree = CST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end
  end

module ContractParser = MkParser (ContractParser_Menhir) (Self_lexing)

(* Parser for expressions *)

module ExprParser_Menhir =
  struct
    include Parser_reasonligo.Parser
    type tree = CST.expr

    let main = interactive_expr

    module Incremental =
      struct
        let main = Incremental.interactive_expr
      end
  end

module ExprParser = MkParser (ExprParser_Menhir) (Self_lexing)

(* Results and errors *)

type error = Errors.parse_error

type cst    = (CST.t ,   error) Trace.result
type expr   = (CST.expr, error) Trace.result
type buffer = (Buffer.t, error) Trace.result

let fail msg = Trace.fail @@ Errors.generic msg
type file_path = string
type dirs      = file_path list (* For #include directives *)

(* Calling the parsers *)

let apply thunk =
  match thunk () with
    Stdlib.Ok tree -> Trace.ok tree
  | Stdlib.Error msg -> fail msg
(* Parsing contracts *)

let parse_file dirs buffer file_path =
  apply (fun () -> ContractParser.parse_file dirs buffer file_path)

let parse_module_string dirs buffer =
  apply (fun () -> ContractParser.parse_string dirs buffer)

(* Parsing expressions *)

let parse_expression dirs buffer =
  apply (fun () -> ExprParser.parse_string dirs buffer)

(* Calling the pretty-printers *)

module Pretty    = Parser_reasonligo.Pretty
module MkPretty  = Shared_parser.Common.MakePretty
module AllPretty = MkPretty (CST) (Pretty)

let pretty_print            = AllPretty.print_cst
let pretty_print_expression = AllPretty.print_expr
let pretty_print_type_expr  = AllPretty.print_type_expr
let pretty_print_pattern    = AllPretty.print_pattern

let pretty_print_from_source dirs buffer file_path =
  Trace.map pretty_print
  @@ apply (fun () -> ContractParser.parse_file dirs buffer file_path)
