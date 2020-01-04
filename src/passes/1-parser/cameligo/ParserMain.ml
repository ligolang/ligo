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

module M = ParserUnit.Make (IO)
                           (Lexer.Make (LexToken))
                           (AST)
                           (ExtParser)
                           (ParErr)
                           (ExtParserLog)
