(* Driver for the ReasonLIGO lexer *)

module Region = Simple_utils.Region

module IO =
  struct
    let options = EvalOpt.(read ~lang:ReasonLIGO ~ext:".religo")
  end

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))

let () =
  match M.trace () with
    Stdlib.Ok () -> ()
  | Error Region.{value; _} -> Utils.highlight value
