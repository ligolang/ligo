open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_syntax.ligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_syntax.ligo", line 1, characters 16-17:[0m
      1 | type foo is bar [1m[31m-[0m 42

    [1m[31mError[0m: Ill-formed declaration.
    At this point, if the declaration is complete, another is possible. |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_arguments.religo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_function_arguments.religo", line 1, characters 14-27:[0m
      1 | let div = (a, [1m[31mb : nat * nat[0m) : option (nat) =>
      2 |   if (b == 0n) { None; } else { Some (a/b); }

    [1m[31mError[0m: It looks like you are defining a function, however we do not
    understand the parameters declaration.
    Examples of valid functions:
    let x = (a: string, b: int) : int => 3;
    let tuple = ((a, b): (int, int)) => a + b;
    let x = (a: string) : string => "Hello, " ++ a; |} ] ;
