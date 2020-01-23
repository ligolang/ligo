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
    (* Error handling (reexported from [ParserAPI]) *)

    type message = string
    type valid   = Parser.token
    type invalid = Parser.token
    type error   = message * valid option * invalid

    exception Point of error

    val format_error :
      ?offsets:bool -> [`Byte | `Point] -> error -> string

    val short_error :
      ?offsets:bool -> [`Point | `Byte] -> string -> Region.t -> string

    (* Parsers *)

    val parse :
      (Lexer.instance ->
       (Lexing.lexbuf -> Lexer.token) ->
       Buffer.t -> ParserLog.state -> ('a, string) result) ->
      ('a, string) result

    val parse_contract :
      Lexer.instance ->
      (Lexing.lexbuf -> Lexer.token) ->
      Buffer.t -> ParserLog.state ->
      (AST.t, string) Stdlib.result

    val parse_expr :
      Lexer.instance ->
      (Lexing.lexbuf -> Lexer.token) ->
      Buffer.t -> ParserLog.state -> (AST.expr, string) Stdlib.result

  end
