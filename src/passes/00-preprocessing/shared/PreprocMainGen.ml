module Std    = Simple_utils.Std
module Lexbuf = Simple_utils.Lexbuf

module type CONFIG = Preprocessor.Config.S

module Make (Config : CONFIG) =
  struct
    module Parameters = Preprocessor.CLI.Make (Config)
    module Main       = Preprocessor.TopAPI.Make (Parameters)

    open Main

    let run () =
      match check_cli () with
        Main.Ok ->
          let file   = Option.value Parameters.Options.input ~default:"" in
          let std, _ = preprocess (Lexbuf.File file)
          in begin
               Printf.printf  "%s%!" (Std.string_of std.out);
               Printf.eprintf "%s%!" (Std.string_of std.err)
             end
      | Info  msg -> Printf.printf "%s%!" msg (* Note the absence of "\n" *)
      | Error msg -> Printf.eprintf "%s\n%!" msg
  end
