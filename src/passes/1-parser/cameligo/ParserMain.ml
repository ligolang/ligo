(** Driver for the CameLIGO parser *)

module IO =
  struct
    let ext = ".mligo"
    let options = EvalOpt.read "CameLIGO" ext
  end

module ExtParser =
  struct
    type ast = AST.t
    type expr = AST.expr
    include Parser
  end

module ExtParserLog =
  struct
    type ast = AST.t
    include ParserLog
  end

module MyLexer = Lexer.Make (LexToken)

module Unit =
  ParserUnit.Make (IO)(MyLexer)(AST)(ExtParser)(ParErr)(ExtParserLog)

let () = Unit.run ()
