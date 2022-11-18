open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "asi_line_comment.jsligo" ];
  [%expect {|
    Everything at the top-level was executed.
    - test1 exited with value 1.
    - test2 exited with value 2. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "asi_block_comment.jsligo" ];
  [%expect {|
    Everything at the top-level was executed.
    - test1 exited with value 1.
    - test2 exited with value 2. |}]
