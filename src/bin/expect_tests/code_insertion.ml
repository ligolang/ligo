open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_michelson_insertion_1.ligo" ; "main" ] ;
  [%expect{|
    ligo: error
          Error(s) occurred while type checking the contract:
          Ill typed contract:
            1: { parameter nat ;
            2:   storage nat ;
            3:   code { LAMBDA (pair nat nat) nat ADD ; SWAP ; EXEC ; NIL operation ; PAIR } }
          At line 3 characters 35 to 38, unexpected primitive, only a sequence
          can be used here.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_michelson_insertion_2.ligo" ; "main" ] ;
  [%expect{|
    ligo: error
          in file "bad_michelson_insertion_2.ligo", line 5, characters 32-40
          Invalid type(s).
          Expected: "nat", but got: "( nat * nat )".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; bad_contract "bad_michelson_insertion_3.ligo" ; "main" ] ;
  [%expect{|
    { parameter nat ;
      storage nat ;
      code { DUP ; CDR ; SWAP ; CAR ; ADD ; NIL operation ; PAIR } } |}]
