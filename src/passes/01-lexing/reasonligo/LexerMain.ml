(* Driver for the ReasonLIGO lexer *)

module Comments         = Preprocessing_reasonligo.Comments
module Modules          = Preprocessing_reasonligo.Modules
module File             = Preprocessing_reasonligo.File
module Token            = Lexing_reasonligo.Token
module Preprocessor_CLI = Preprocessor.CLI.Make (Comments) (Modules)
module Lexer_CLI        = LexerLib.CLI.Make (Preprocessor_CLI)
module Self_tokens      = Lexing_reasonligo.Self_tokens
module MainGen          = Lexing_shared.LexerMainGen

(* TODO: this run in dune build, make it run with dune runtest instead *)
module Main = MainGen.Make (File) (Token) (Lexer_CLI) (Self_tokens)
module None_warning = struct
   let add_warning = fun _ -> ()
end

module Main_test = Main(None_warning)
let () = Main_test.check_cli ()
let () = Main_test.scan_all ()
