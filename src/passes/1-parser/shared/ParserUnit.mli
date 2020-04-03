(* Functor to build a standalone LIGO parser *)

module Region = Simple_utils.Region

type language = [`PascaLIGO | `CameLIGO | `ReasonLIGO]

module SSet : Set.S with type elt = string and type t = Set.Make(String).t

module type SubIO =
  sig
    type options = <
      libs    : string list;
      verbose : SSet.t;
      offsets : bool;
      lang    : language;
      ext     : string;   (* ".ligo", ".mligo", ".religo" *)
      mode    : [`Byte | `Point];
      cmd     : EvalOpt.command;
      mono    : bool
    >

    val options : options
    val make : input:string option -> expr:bool -> EvalOpt.options
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

module Make (Lexer : Lexer.S)
            (AST : sig type t type expr end)
            (Parser : ParserAPI.PARSER
                      with type ast   = AST.t
                       and type expr  = AST.expr
                       and type token = Lexer.token)
            (ParErr : sig val message : int -> string end)
            (ParserLog : Pretty with type ast  = AST.t
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

    val parse_file :
      string -> (AST.t, message Region.reg) Stdlib.result

    val parse_string :
      string -> (AST.t, message Region.reg) Stdlib.result

    val parse_expression :
      string -> (AST.expr, message Region.reg) Stdlib.result
  end
