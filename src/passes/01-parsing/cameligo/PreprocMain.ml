(* Driver for the preprocessor for CameLIGO *)

module Comments    = Lexer_cameligo.Comments
module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module MainGen     = Preprocessor.PreprocMainGen
module Preproc     = MainGen.Make (Preproc_CLI)

let () =
  match Preproc.preprocess () with
    Stdlib.Ok (buffer,_) ->
      Printf.printf "%s%!" (Buffer.contents buffer)
  | _ -> ()
