open Cli_expect

let%expect_test _ =
  run_ligo_bad ["compile" ; "contract"; "../../test/preprocessor/directive_inside_line.ligo"];
  [%expect {|
../../test/preprocessor/directive_inside_line.ligo: No such file or directory
 |}];
