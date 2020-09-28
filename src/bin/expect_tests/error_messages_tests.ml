open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/gitlab_111.religo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/gitlab_111.religo", line 2, characters 0-3:[0m
      1 | let a =
      2 | [1m[31mlet[0m b = 2;

    [1m[31mError[0m: This is an incorrect let binding.
    Examples of correct let bindings:
    let a: int = 4;
    let (a: int, b: int) = (1, 2);
    let func = (a: int, b: int) => a + b; |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/missing_rpar.religo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/missing_rpar.religo", line 5, characters 0-3:[0m
      4 |
      5 | [1m[31mlet[0m z = 4;
    [1m[31mError[0m: 273: Syntax error. |} ] ;

