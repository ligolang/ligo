open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "cycle_A.mligo" ; "main" ] ;
  [%expect {|
    Dependency cycle detected :
     `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "D.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/D.mligo
        |-- ../../test/contracts/build/C.mligo
        |   |-- ../../test/contracts/build/A.mligo
        |   `-- ../../test/contracts/build/B.mligo
        |       `-- ../../test/contracts/build/A.mligo
        `-- ../../test/contracts/build/E.mligo
            |-- ../../test/contracts/build/F.mligo
            `-- ../../test/contracts/build/G.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "cycle_A.mligo"; "--display-format=json" ] ;
  [%expect {|
    {"root":"../../test/contracts/build/cycle_A.mligo","child":{"file":"../../test/contracts/build/cycle_B.mligo","child":{"file":"../../test/contracts/build/cycle_C.mligo","child":{"file":"../../test/contracts/build/cycle_A.mligo","child":{"file":"../../test/contracts/build/cycle_B.mligo"}}}}} |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "D.mligo"; "--display-format=json" ] ;
  [%expect {|
    {"root":"../../test/contracts/build/D.mligo","child":{"file":"../../test/contracts/build/C.mligo","child":{"file":"../../test/contracts/build/A.mligo"},"child":{"file":"../../test/contracts/build/B.mligo","child":{"file":"../../test/contracts/build/A.mligo"}}},"child":{"file":"../../test/contracts/build/E.mligo","child":{"file":"../../test/contracts/build/F.mligo"},"child":{"file":"../../test/contracts/build/G.mligo"}}} |}]
