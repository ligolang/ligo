(* This file provides an interface to the PascaLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies on CameLIGO *)

module File        = Lexer_pascaligo.File
module Comments    = Lexer_pascaligo.Comments
module Token       = Lexer_pascaligo.Token
module Self_lexing = Lexer_pascaligo.Self_lexing
module ParErr      = Parser_pascaligo.ParErr

(*module Printer   = Cst_pascaligo.Printer*)

module MakeParser = Shared_parser.Common.MakeParser
module MkParser   = MakeParser (File) (Comments) (Token) (ParErr)

module CST = Cst.Pascaligo

(* Parser for contracts *)

module ContractParser_Menhir =
  struct
    include Parser_pascaligo.Parser
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
    include Parser_pascaligo.Parser
    type tree = CST.expr

    let main = interactive_expr

    module Incremental =
      struct
        let main = Incremental.interactive_expr
      end
  end

module ExprParser = MkParser (ExprParser_Menhir) (Self_lexing)

(* Results and errors *)

type error  = Errors.parse_error
type cst    = (CST.t ,   error) Trace.result
type expr   = (CST.expr, error) Trace.result
type buffer = (Buffer.t, error) Trace.result

let fail msg = Trace.fail @@ Errors.generic msg
type file_path = string
type dirs      = file_path list (* For #include directives *)

(* Calling the parsers *)

let apply = function
  Stdlib.Ok tree   -> Trace.ok tree
| Stdlib.Error msg -> fail msg

(* Parsing contracts *)

let parse_file dirs buffer file_path =
  ContractParser.parse_file dirs buffer file_path |> apply

let parse_module_string dirs string =
  ContractParser.parse_string dirs string |> apply

(* Parsing expressions *)

let parse_expression dirs string =
  ExprParser.parse_string dirs string |> apply

(* Calling the pretty-printers *)

module Pretty    = Parser_pascaligo.Pretty
module MkPretty  = Shared_parser.Common.MakePretty
module AllPretty = MkPretty (CST) (Pretty)

let pretty_print            = AllPretty.print_cst
let pretty_print_expression = AllPretty.print_expr
let pretty_print_type_expr  = AllPretty.print_type_expr
let pretty_print_pattern    = AllPretty.print_pattern

let pretty_print_from_source dirs buffer file_path =
  match ContractParser.parse_file dirs buffer file_path with
    Stdlib.Ok tree -> Trace.ok @@ pretty_print @@ tree
  | Stdlib.Error msg -> fail msg
