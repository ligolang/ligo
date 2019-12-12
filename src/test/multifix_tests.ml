open Trace
open Test_helpers
open Parser.Cameligo

let basic () : unit result =
  let%bind _ = User.parse_file "./contracts/new-syntax.mligo" in
  ok ()

let simplify () : unit result =
  let%bind raw = User.parse_file "./contracts/new-syntax.mligo" in
  let%bind _simpl = Simplify.Cameligo.main raw in
  ok ()

let main = "Multifix", [
    test "basic" basic ;
    test "simplify" simplify ;
]
