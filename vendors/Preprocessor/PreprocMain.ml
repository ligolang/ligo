(* Standalone preprocessor with default settings for PascaLIGO *)

(* Vendors dependencies *)

module Std    = Simple_utils.Std
module Lexbuf = Simple_utils.Lexbuf
module CLI    = Preprocessor.CLI
module TopAPI = Preprocessor.TopAPI

module Config =
  struct
    type block_comment_delimiters = <opening : string; closing : string>
    type line_comment_delimiter   = string (*Opening of a line comment*)
    type string_delimiter         = string
    type verbatim_delimiters      = <opening : string; closing : string>

    let block =
      object
        method opening = "(*"
        method closing = "*)"
      end

    let block    = Some block
    let line     = Some "//"
    let string   = Some "\""
    let verbatim = None
    let file_ext = None
  end

module Parameters = CLI.Make (Config)
module Main = TopAPI.Make (Parameters)

let () =
  let open Main in
  match check_cli () with
    Main.Ok ->
      let file   = Option.value Parameters.Options.input ~default:"" in
      let std, _ = preprocess (Lexbuf.File file)
      in Printf.printf  "%s%!" (Std.string_of std.out);
         Printf.eprintf "%s%!" (Std.string_of std.err)
  | Info  msg -> Printf.printf "%s%!" msg
  | Error msg -> Printf.eprintf "%s\n%!" msg
