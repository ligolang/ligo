(* Standalone preprocessor with default settings *)

module Comments =
  struct
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    let block = None
    let line  = None
  end

module CLI     = Preprocessor.CLI.Make (Comments)
module MainGen = Preprocessor.PreprocMainGen
module Preproc = MainGen.Make (CLI)

let () = Preproc.check_cli ()

let () =
  match Preproc.preprocess () with
    Stdlib.Ok buffer ->
      Printf.printf "%s%!" (Buffer.contents buffer)
  | _ -> ()
