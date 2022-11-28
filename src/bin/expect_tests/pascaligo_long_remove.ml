open Cli_expect

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "r1"; "--init-file"; test "long_remove.ligo" ];
  [%expect {|
    { "b" } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "r2"; "--init-file"; test "long_remove.ligo" ];
  [%expect {|
    { Elt "bar" { "b" } ; Elt "foo" { "a" } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "r3"; "--init-file"; test "long_remove.ligo" ];
  [%expect {|
    { Elt "x" { Elt "bar" { "b" } ; Elt "foo" { "a" } } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "r4"; "--init-file"; test "long_remove.ligo" ];
  [%expect {|
    { Elt "foo" (Pair { Elt "bar" { "a" } } 1) } |}]
