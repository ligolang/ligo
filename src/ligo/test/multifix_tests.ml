open Trace
open Test_helpers
open Ligo.Parser_multifix

let basic () : unit result =
  let%bind _ = User.parse_file "./contracts/new-syntax.mligo" in
  ok ()

let simplify () : unit result =
  let%bind raw = User.parse_file "./contracts/new-syntax.mligo" in
  let%bind _simpl = Ligo.Simplify_multifix.main raw in
  ok ()

let main = "Parser Multifix", [
    test "basic" basic ;
    test "simplify" simplify ;
]
