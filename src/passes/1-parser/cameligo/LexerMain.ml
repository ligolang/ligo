(* Driver for the CameLIGO lexer *)

module Region = Simple_utils.Region

module IO =
  struct
    let ext = ".mligo"
    let options = EvalOpt.read "CameLIGO" ext
  end

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))

let () =
  match M.trace () with
    Stdlib.Ok () -> ()
  | Error Region.{value; _} -> Utils.highlight value
