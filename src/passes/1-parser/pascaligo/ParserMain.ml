(** Driver for the PascaLIGO parser *)

module IO =
  struct
    let ext = ".ligo"
    let options = EvalOpt.read "PascaLIGO" ext
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

open! SyntaxError

let () =
  try Unit.run () with
    (* Ad hoc errors from the parser *)

    Error (Reserved_name name) ->
      let () = Unit.close_all () in
      let token =
        MyLexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         Stdlib.Error _ ->
           assert false (* Should not fail if [name] is valid. *)
       | Ok invalid ->
           let point = "Reserved name.\nHint: Change the name.\n",
                       None, invalid in
           let error =
             Unit.format_error ~offsets:IO.options#offsets
                               IO.options#mode point
           in Printf.eprintf "\027[31m%s\027[0m%!" error)

  | Error (Duplicate_parameter name) ->
      let () = Unit.close_all () in
      let token =
        MyLexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         Stdlib.Error _ ->
           assert false (* Should not fail if [name] is valid. *)
       | Ok invalid ->
           let point = "Duplicate parameter.\nHint: Change the name.\n",
                       None, invalid in
           let error =
             Unit.format_error ~offsets:IO.options#offsets
                               IO.options#mode point
           in Printf.eprintf "\027[31m%s\027[0m%!" error)

  | Error (Duplicate_variant name) ->
      let () = Unit.close_all () in
      let token =
        MyLexer.Token.mk_constr name.Region.value name.Region.region in
      let point = "Duplicate variant in this type declaration.\n\
                   Hint: Change the name.\n",
                  None, token in
      let error =
        Unit.format_error ~offsets:IO.options#offsets
                          IO.options#mode point
      in Printf.eprintf "\027[31m%s\027[0m%!" error
