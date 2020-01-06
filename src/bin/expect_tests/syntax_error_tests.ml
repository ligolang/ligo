open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_syntax.ligo" ; "main" ] ;
  [%expect {|
    ligo: parser error: Parse error at "-" from (1, 16) to (1, 17). In file "|../../test/contracts/negative/error_syntax.ligo"
           {"parser_loc":"in file \"\", line 1, characters 16-17"} |} ] ;
