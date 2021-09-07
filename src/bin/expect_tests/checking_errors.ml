open Cli_expect

(* Record *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "expression" ; "cameligo" ; "type foo = { foo : int } in let a : foo = {foo = 1n} in a" ] ;
  [%expect {|
             Invalid type(s).
             Expected: "record[foo -> int]", but got: "record[foo -> nat]".
             |}]

(* Record_update *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "expression" ; "cameligo" ; "let a = {foo = 1} in { a with foo = 1n}" ; "--infer" ] ;
  [%expect {|
             Invalid type(s).
             Expected: "int", but got: "nat".
             |}]

(* Constructor *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "expression" ; "cameligo" ; "type foo = Foo of int in let a = Foo (5n) in a" ] ;
  [%expect {|
             Invalid type(s).
             Expected: "int", but got: "nat".
             |}]
