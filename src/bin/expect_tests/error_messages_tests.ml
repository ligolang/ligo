open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/gitlab_111.religo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/gitlab_111.religo", line 2, characters 0-3
      1 | let a =
      2 | let b = 2;
    This is an incorrect let binding.
    Examples of correct let bindings:
    let a: int = 4;
    let (a: int, b: int) = (1, 2);
    let func = (a: int, b: int) => a + b; |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/missing_rpar.religo" ; "main" ] ;
  [%expect {|
    in file "../../test/contracts/negative/missing_rpar.religo", line 5, characters 0-3
      4 |
      5 | let z = 4;
    Syntax error #273. |} ] ;

