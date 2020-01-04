(** Driver for the CameLIGO lexer *)

module IO =
  struct
    let ext = ".mligo"
    let options = EvalOpt.read "CameLIGO" ext
  end

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))
