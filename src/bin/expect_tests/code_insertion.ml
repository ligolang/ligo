open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_michelson_insertion_1.ligo" ; "main" ] ;
  [%expect{|
    ligo: error
          Compiler bug
          Ill typed contract:
            01: { parameter nat ;
            02:   storage nat ;
            03:   code { DUP
            04:          /* [ pair (nat @parameter) (nat @storage)
            05:             : pair (nat @parameter) (nat @storage) ] */ ;
            06:          LAMBDA (pair nat nat) nat ADD ;
            07:          SWAP ;
            08:          EXEC ;
            09:          NIL operation ;
            10:          PAIR ;
            11:          DIP { DROP } } }
          At line 6 characters 35 to 38, unexpected primitive, only a sequence
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
          in file "bad_michelson_insertion_2.ligo", line 3, characters 9-13
          Constant declaration 'main'
          Bad types: expected nat got ( nat * nat )


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
      code { DUP ;
             DUP ;
             CDR ;
             SWAP ;
             CAR ;
             ADD ;
             NIL operation ;
             PAIR ;
             DIP { DROP } } } |}]
