open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_syntax.ligo" ; "main" ] ;
  [%expect {|
    ligo: error
    Parse error in file "error_syntax.ligo", line 1, characters 16-17 at "-", after "bar":
    16: <syntax error>


    If you're not sure how to fix this error, you can do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_arguments.religo" ; "main" ] ;
  [%expect {|
    ligo: error
    in file "error_function_arguments.religo", line 1, characters 14-27
    It looks like you are defining a function, however we do not
    understand the parameters declaration.
    Examples of valid functions:
    let x = (a: string, b: int) : int => 3;
    let tuple = ((a, b): (int, int)) => a + b;
    let x = (a: string) : string => "Hello, " ++ a;



    If you're not sure how to fix this error, you can do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

