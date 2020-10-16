(* Driver for the preprocessor for ReasonLIGO *)

module Comments    = Lexer_reasonligo.Comments
module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module MainGen     = Preprocessor.PreprocMainGen
module Preproc     = MainGen.Make (Preproc_CLI)

let () =
  match Preproc.preprocess () with
    Stdlib.Ok buffer ->
      Printf.printf "%s%!" (Buffer.contents buffer)
  | _ -> ()
