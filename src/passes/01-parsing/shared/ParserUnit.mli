(* Functor to build a standalone LIGO parser *)

(* Dependencies *)

module Region  = Simple_utils.Region
module EvalOpt = Lexer_shared.EvalOpt
module Lexer   = Lexer_shared.Lexer
module SSet    : Set.S with type elt = string
                        and type t = Set.Make(String).t

(* A subtype of [EvalOpt.options] *)

module type SubIO =
  sig
    type options = <
      libs    : string list;
      verbose : SSet.t;
      offsets : bool;
      block   : EvalOpt.block_comment option;
      line    : EvalOpt.line_comment option;
      ext     : string;
      mode    : [`Byte | `Point];
      cmd     : EvalOpt.command;
      mono    : bool;
      pretty  : bool
    >

    val options : options
    val make : input:string option -> expr:bool -> EvalOpt.options
  end

(* Signature for the printers *)

module type Printer =
  sig
    type state
    type ast
    type expr

    val mk_state :
      offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

    val pp_cst       : state -> ast -> unit
    val pp_expr      : state -> expr -> unit
    val print_tokens : state -> ast -> unit
    val print_expr   : state -> expr -> unit
  end

(* Main functor to make the parser *)

module Make (Lexer : Lexer.S)
            (AST : sig type t type expr end)
            (Parser : ParserAPI.PARSER
                      with type ast   = AST.t
                       and type expr  = AST.expr
                       and type token = Lexer.token)
            (ParErr : sig val message : int -> string end)
            (ParserLog : Printer with type ast = AST.t
                                 and type expr = AST.expr)
            (SubIO: SubIO) :
  sig
    (* Error handling reexported from [ParserAPI] without the
       exception [Point] *)

    type message = string
    type valid   = Parser.token
    type invalid = Parser.token
    type error   = message * valid option * invalid

    val format_error :
      ?offsets:bool -> [`Byte | `Point] -> error -> string Region.reg

    val short_error :
      ?offsets:bool -> [`Point | `Byte] -> message -> Region.t -> string

    (* Parsers *)

    val contract_in_file :
      string -> (AST.t, message Region.reg) Stdlib.result

    val contract_in_string :
      string -> (AST.t, message Region.reg) Stdlib.result

    val contract_in_stdin :
      unit -> (AST.t, message Region.reg) Stdlib.result

    val expr_in_string :
      string -> (AST.expr, message Region.reg) Stdlib.result

    val expr_in_stdin :
      unit -> (AST.expr, message Region.reg) Stdlib.result

    val preprocess :
      string -> (Buffer.t, message Region.reg) Stdlib.result
  end
