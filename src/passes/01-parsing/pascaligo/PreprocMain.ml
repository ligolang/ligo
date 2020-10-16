(* Driver for the preprocessor for PascaLIGO *)

module Comments    = Lexer_pascaligo.Comments
module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module MainGen     = Preprocessor.PreprocMainGen
module Preproc     = MainGen.Make (Preproc_CLI)

let () =
  match Preproc.preprocess () with
    Stdlib.Ok buffer ->
      Printf.printf "%s%!" (Buffer.contents buffer)
  | _ -> ()
