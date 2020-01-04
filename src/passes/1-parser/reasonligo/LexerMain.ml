(** Driver for the ReasonLIGO lexer *)

module IO =
  struct
    let ext = ".religo"
    let options = EvalOpt.read "ReasonLIGO" ext
  end

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))
