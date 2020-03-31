(* Driver for the CameLIGO lexer *)

module Region = Simple_utils.Region

module IO =
  struct
    let options = EvalOpt.(read ~lang:CameLIGO ~ext:".mligo")
  end

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))

let () =
  match M.trace () with
    Stdlib.Ok () -> ()
  | Error Region.{value; _} -> Utils.highlight value
