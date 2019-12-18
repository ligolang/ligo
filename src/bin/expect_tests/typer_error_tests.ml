open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/error_typer_1.mligo" ; "foo" ] ;
  [%expect {| ligo: in file "error_typer_1.mligo", line 3, characters 19-27. different type constructors: Expected these two constant type constructors to be the same, but they're different {"a":"string","b":"int"} |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/error_typer_2.mligo" ; "foo" ] ;
  [%expect {| ligo: in file "error_typer_2.mligo", line 3, characters 24-39. different type constructors: Expected these two n-ary type constructors to be the same, but they're different {"a":"(TO_list(string))","b":"(TO_option(int))"} |} ] ;

  (* run_ligo_bad [ "compile-contract" ; "../../test/contracts/error_typer_3.mligo" ; "foo" ] ;
   * [%expect …some type error… ] ; *)
