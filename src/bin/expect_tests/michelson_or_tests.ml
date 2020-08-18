open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "double_michelson_or.mligo" ; "main" ; "unit" ; "(M_left (1) : storage)" ] ;
  [%expect {| ( LIST_EMPTY() , M_right("one") ) |}];

  run_ligo_good [ "dry-run" ; contract "double_michelson_or.ligo" ; "main" ; "unit" ; "(M_left (1) : storage)" ] ;
  [%expect {| ( LIST_EMPTY() , M_right("one") ) |}]


let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_or_tree.mligo" ; "main" ] ;
  [%expect {|
    { parameter unit ;
      storage (or (int %three) (or %four (int %one) (nat %two))) ;
      code { DROP ; PUSH int 1 ; LEFT nat ; RIGHT int ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_michelson_or.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "bad_michelson_or.mligo", line 6, characters 12-27
          Incorrect usage of type "michelson_or".
          The contructor "M_right" must be annotated with a variant type.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_or_tree_intermediary.ligo" ; "main" ] ;
  [%expect {|
    { parameter unit ;
      storage (or (int %three) (or (int %one) (nat %two))) ;
      code { DROP ; PUSH int 1 ; LEFT nat ; RIGHT int ; NIL operation ; PAIR } } |}]

