(** Driver for the ReasonLIGO parser *)

module IO =
  struct
    let ext = ".religo"
    let options = EvalOpt.read "ReasonLIGO" ext
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

(* Main *)

let () =
  try Unit.run () with
    (* Ad hoc errors from the parsers *)

    SyntaxError.Error (SyntaxError.WrongFunctionArguments expr) ->
      let () = Unit.close_all () in
      let msg = "It looks like you are defining a function, \
                 however we do not\n\
                 understand the parameters declaration.\n\
                 Examples of valid functions:\n\
                 let x = (a: string, b: int) : int => 3;\n\
                 let x = (a: string) : string => \"Hello, \" ++ a;\n"
      and reg = AST.expr_to_region expr in
      let error = Unit.short_error ~offsets:IO.options#offsets
                                   IO.options#mode msg reg
      in Printf.eprintf "\027[31m%s\027[0m%!" error
