open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_1.mligo" ; "main" ] ;
  [%expect {| ligo: in file "error_typer_1.mligo", line 3, characters 19-27. different type constructors: Expected these two constant type constructors to be the same, but they're different {"a":"string","b":"int"} |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_2.mligo" ; "main" ] ;
  [%expect {| ligo: in file "error_typer_2.mligo", line 3, characters 24-39. different type constructors: Expected these two n-ary type constructors to be the same, but they're different {"a":"(TO_list(string))","b":"(TO_option(int))"} |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_3.mligo" ; "main" ] ;
  [%expect {| ligo: in file "error_typer_3.mligo", line 3, characters 34-53. tuples have different sizes: Expected these two types to be the same, but they're different (both are tuples, but with a different number of arguments) {"a":"tuple[int , string , bool]","b":"tuple[int , string]"} |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_4.mligo" ; "main" ] ;
  [%expect {| ligo: in file "error_typer_4.mligo", line 4, characters 17-56. different keys in record:  {"key_a":"d","key_b":"c"} |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_5.mligo" ; "main" ] ;
  [%expect {| ligo: unbound type variable:  {"variable":"boolean","in":"- E[]\tT[] ]","did_you_mean":"bool"} |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_6.mligo" ; "main" ] ;
  [%expect {| ligo: in file "error_typer_6.mligo", line 1, characters 30-64. different type constructors: Expected these two constant type constructors to be the same, but they're different {"a":"string","b":"bool"} |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_7.mligo" ; "main" ] ;
  [%expect {| ligo: in file "error_typer_7.mligo", line 4, characters 17-56. records have different sizes: Expected these two types to be the same, but they're different (both are records, but with a different number of arguments) {"a":"record[b -> string , a -> int]","b":"record[c -> bool , b -> string , a -> int]"} |} ] ;


