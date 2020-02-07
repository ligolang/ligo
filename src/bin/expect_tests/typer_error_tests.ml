open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_type.ligo" ; "main" ] ;
  [%expect {|
    ligo: in file "error_type.ligo", line 3, characters 18-28. Adding modulo with wrong types: Expected arguments with one of the following combinations of types: add(nat , nat) or add(int , int) or add(mutez , mutez) or add(nat , int) or add(int , nat) or add(timestamp , int) or add(int , timestamp) but got this combination instead: add(int , string)

     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_1.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "error_typer_1.mligo", line 3, characters 19-27. different type constructors: Expected these two constant type constructors to be the same, but they're different {"a":"string","b":"int"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_2.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "error_typer_2.mligo", line 3, characters 24-39. different type constructors: Expected these two n-ary type constructors to be the same, but they're different {"a":"(TO_list(string))","b":"(TO_option(int))"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_3.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "error_typer_3.mligo", line 3, characters 34-53. tuples have different sizes: Expected these two types to be the same, but they're different (both are tuples, but with a different number of arguments) {"a":"( int * string * bool )","b":"( int * string )"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_4.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "error_typer_4.mligo", line 4, characters 17-56. different keys in records:  {"key_a":"c","key_b":"b","a":"record[a -> int , c -> bool , d -> string]","b":"record[a -> int , b -> string , c -> bool]"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_5.mligo" ; "main" ] ;
  [%expect {|
    ligo: unbound type variable:  {"variable":"boolean","in":"- E[]\tT[] ]","did_you_mean":"bool"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_6.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "error_typer_6.mligo", line 1, characters 30-64. different type constructors: Expected these two constant type constructors to be the same, but they're different {"a":"string","b":"bool"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_7.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "error_typer_7.mligo", line 4, characters 18-48. records have different sizes: Expected these two types to be the same, but they're different (both are records, but with a different number of arguments) {"a":"record[a -> int , b -> string]","b":"record[a -> int , b -> string , c -> bool]"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/id.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "id.mligo", line 45, characters 4-51. Expected a different type: Expected the type option but got the type record[controller -> address , owner -> address , profile -> bytes]

     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}] ;
