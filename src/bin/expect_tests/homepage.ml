open Cli_expect

let base path = "../../../gitlab-pages/website/src/components/HomepageCodeExamples/" ^ path

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; base "cameligo.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_initial_storage exited with value ().
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; base "jsligo.jsligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_initial_storage exited with value ().
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; base "pascaligo.ligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_initial_storage exited with value ().
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; base "reasonligo.religo" ] ;
  [%expect{|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Everything at the top-level was executed.
    - test_initial_storage exited with value ().
    - test_increment exited with value (). |}]