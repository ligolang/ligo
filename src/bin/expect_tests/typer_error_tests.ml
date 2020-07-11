open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_1.mligo"; "main"];
  [%expect {|
    ligo: error
          in file "error_function_annotation_1.mligo", line 1, characters 0-3
          Constant declaration 'main'
          Bad types: expected int -> unit got int -> int


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_2.mligo"; "f"];
  [%expect {|
    ligo: error
          in file "error_function_annotation_2.mligo", line 1, characters 14-43
          Constant declaration 'f'
          Bad types: expected int got ( int * int ) -> int


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_3.mligo"; "f"];
  [%expect {|
    ligo: error
          in file "error_function_annotation_3.mligo", line 6, characters 0-3
          Constant declaration 'main'
          Bad types:
          expected ( int * sum[Add -> int , Sub -> int] ) -> ( (type_operator: list(operation)) * sum[Add -> int , Sub -> int] )
          got ( int * sum[Add -> int , Sub -> int] ) -> sum[Add -> int , Sub -> int]


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_no_tail_recursive_function.mligo"; "f"];
  [%expect {|
    ligo: error
          in file "error_no_tail_recursive_function.mligo", line 2, characters 14-21
          Recursion must be achieved through tail-calls only


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_type.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_type.ligo", line 3, characters 18-28
          Constant declaration 'foo'
          Expected arguments with one of the following combinations of type:
          (nat , nat) or (int , int) or (mutez , mutez) or (nat , int) or (int , nat) or (timestamp , int) or (int , timestamp)
          but got int , string


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_1.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_1.mligo", line 3, characters 19-27
          Constant declaration 'foo'
          Bad types: expected string got int


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_2.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_2.mligo", line 3, characters 24-39
          Constant declaration 'foo'
          Bad types:
          expected (type_operator: list(string))
          got (type_operator: option(int))


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_3.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_3.mligo", line 3, characters 34-53
          Constant declaration 'foo'
          Bad types:
          expected ( int * string * sum[false -> unit , true -> unit] )
          got ( int * string )


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_4.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_4.mligo", line 4, characters 17-56
          Constant declaration 'foo'
          Bad types:
          expected record[a -> int , c -> sum[false -> unit , true -> unit] , d -> string]
          got record[a -> int , b -> string , c -> sum[false -> unit , true -> unit]]


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_5.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_5.mligo", line 1, characters 10-17
          Unbound type variable 'boolean'


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_6.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_6.mligo", line 1, characters 30-64
          Constant declaration 'foo'
          Bad types:
          expected (type_operator: Map (int,string))
          got (type_operator: Map (int,sum[false -> unit , true -> unit]))


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_7.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_7.mligo", line 4, characters 17-56
          Constant declaration 'foo'
          Bad types:
          expected record[a -> int , b -> string]
          got record[a -> int , b -> string , c -> sum[false -> unit , true -> unit]]


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/id.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "id.mligo", line 28, characters 0-3
          Constant declaration 'buy'
          in file "id.mligo", line 3, character 18 to line 7, character 1
          Expected an option but got record[controller -> address , owner -> address , profile -> bytes]


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

(* 
  This test is here to ensure compatibility with comparable pairs introduced in carthage
  note that only "comb pairs" are allowed to be compared (would be beter if any pair would be comparable ?)
*)
let%expect_test _ =
  run_ligo_good [ "interpret" ; "Set.literal [ (1,(2,3)) ; (2,(3,4)) ]" ; "--syntax=cameligo" ] ;
  [%expect {|
    SET_ADD(( 2 , ( 3 , 4 ) ) , SET_ADD(( 1 , ( 2 , 3 ) ) , SET_EMPTY())) |}];

  run_ligo_bad [ "interpret" ; "Set.literal [ (1,2,3) ; (2,3,4) ]" ; "--syntax=cameligo" ] ;
  [%expect {|
    ligo: error
          pair does not have a comparable structure. (hint: use (a,(b,c)) instead of (a,b,c))


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/failwith_wrong_type.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "failwith_wrong_type.ligo", line 2, characters 19-46
          Constant declaration 'bad'
          Expected arguments with one of the following combinations of type:
          (string) or (nat) or (int)
          but got (type_operator: list(int))


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/compare_sum_types.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "compare_sum_types.ligo", line 3, characters 9-13
          Constant declaration 'main'
          Those two types are not comparable:
          - sum[Bar -> unit , Foo -> unit]
          - sum[Bar -> unit , Foo -> unit]


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]