(* Functor to build a standalone LIGO parser *)

module Region = Simple_utils.Region

module type IO =
  sig
    val ext : string              (* LIGO file extension *)
    val options : EvalOpt.options (* CLI options *)
  end

module type Pretty =
  sig
    type state
    type ast
    type expr

    val mk_state :
      offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

    val pp_ast       : state -> ast -> unit
    val pp_expr      : state -> expr -> unit
    val print_tokens : state -> ast -> unit
    val print_expr   : state -> expr -> unit
  end

module Make (Lexer: Lexer.S)
            (AST: sig type t type expr end)
            (Parser: ParserAPI.PARSER
                     with type ast   = AST.t
                      and type expr  = AST.expr
                      and type token = Lexer.token)
            (ParErr: sig val message : int -> string end)
            (ParserLog: Pretty with type ast  = AST.t
                                and type expr = AST.expr)
            (IO: IO) :
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

    type 'a parser = Lexer.instance -> ('a, message Region.reg) result

    val apply : Lexer.instance -> 'a parser -> ('a, message Region.reg) result

    val parse_contract : AST.t parser
    val parse_expr     : AST.expr parser
  end
