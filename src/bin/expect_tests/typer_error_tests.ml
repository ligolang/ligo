open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_1.mligo"; "main"];
  [%expect {|
    ligo: error
          in file "error_function_annotation_1.mligo", line 1, characters 26-27
          Invalid type(s).
          Expected: "unit", but got: "int".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_2.mligo"; "f"];
  [%expect {|
    ligo: error
          in file "error_function_annotation_2.mligo", line 1, characters 14-43
          Invalid type(s).
          Expected: "int", but got: "( int * int ) -> int".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_3.mligo"; "f"];
  [%expect {|
    ligo: error

          Invalid type(s).
          Expected: "( list (operation) * sum[Add -> int , Sub -> int] )", but got: "
          sum[Add -> int , Sub -> int]".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_no_tail_recursive_function.mligo"; "f"];
  [%expect {|
    ligo: error
          in file "error_no_tail_recursive_function.mligo", line 2, characters 14-21
          Recursive call not in tail position.
          The value of a recursive call must be immediately returned by the defined function.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_type.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_type.ligo", line 3, characters 18-28
          Invalid arguments.
          Expected an argument of type (nat, nat) or (int, int) or (mutez, mutez) or (nat, int) or (int, nat) or (timestamp, int) or (int, timestamp), but got an argument of type int, string.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_1.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_1.mligo", line 3, characters 19-27
          Invalid type(s).
          Expected: "string", but got: "int".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_2.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_2.mligo", line 3, characters 24-39
          Invalid type(s).
          Expected: "list (string)", but got: "option (int)".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_3.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_3.mligo", line 3, characters 34-53
          Invalid type(s).
          Expected: "( int * string * sum[false -> unit , true -> unit] )", but got: "
          ( int * string )".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_4.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_4.mligo", line 4, characters 17-56
          Invalid type(s).
          Expected: "record[a -> int , c -> sum[false -> unit , true -> unit] , d -> string]", but got: "
          record[a -> int , b -> string , c -> sum[false -> unit , true -> unit]]".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_5.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_5.mligo", line 1, characters 10-17
          Type "boolean" not found.


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_6.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_6.mligo", line 1, characters 30-64
          Invalid type(s).
          Expected: "Map (int , string)", but got: "Map (int ,
          sum[false -> unit , true -> unit])".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_7.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "error_typer_7.mligo", line 4, characters 18-48
          Invalid type(s).
          Expected: "record[a -> int , b -> string]", but got: "record[a -> int , b -> string , c -> sum[false -> unit , true -> unit]]".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/id.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "id.mligo", line 45, characters 4-51
          Incorrect argument.
          Expected an option, but got an argument of type "record[controller -> address , owner -> address , profile -> bytes]".


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
          Invalid comparable value. When using a tuple of with more than 2 components, structure the tuple like this: "(a, (b, c))".


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
          Invalid arguments.
          Expected an argument of type (string) or (nat) or (int), but got an argument of type list (int).


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/compare_sum_types.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "compare_sum_types.ligo", line 4, characters 29-36
          Invalid arguments.
          These types cannot be compared: "sum[Bar -> unit , Foo -> unit]" and "
          sum[Bar -> unit , Foo -> unit]".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/invalid_field_record_update.mligo" ; "main" ] ;
  [%expect {|
    ligo: error
          in file "invalid_field_record_update.mligo", line 4, characters 50-54
          Invalid record field "nofield" in record "{ storage with { nofield = 2048 } }".


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |}]