(* Driver for the PascaLIGO lexer *)

module Region = Simple_utils.Region

module IO =
  struct
    let ext = ".ligo"
    let options = EvalOpt.read "PascaLIGO" ext
  end

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))

let () =
  match M.trace () with
    Stdlib.Ok _ -> ()
  | Error Region.{value; _} ->
     Printf.eprintf "\027[31m%s\027[0m%!" value
