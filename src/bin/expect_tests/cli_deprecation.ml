open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; test "fake.mligo"; "-v"; "v" ];
  [%expect {|
    View declaration through CLI is deprecated.
    Please use annotations in the source:

    [@view]
    let v = ... |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; test "fake.jsligo"; "-v"; "v" ];
  [%expect {|
    View declaration through CLI is deprecated.
    Please use annotations in the source:

    @view
    const v = ... |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; test "fake.mligo"; "-e"; "e" ];
  [%expect {|
    Entry-point declaration through CLI is deprecated.
    Please use annotations in the source:

    [@entry]
    let e = ... |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; test "fake.jsligo"; "-e"; "e" ];
  [%expect {|
    Entry-point declaration through CLI is deprecated.
    Please use annotations in the source:

    @entry
    const e = ... |}]
