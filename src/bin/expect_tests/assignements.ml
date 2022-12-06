open Cli_expect

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "a1"; "--init-file"; test "long_assign.ligo" ];
  [%expect {|
    { Elt "foo" (Pair (Pair 1 2) "y") } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "a2"; "--init-file"; test "long_assign.ligo" ];
  [%expect {|
    { Elt "foo" { Elt "bar" (Pair (Pair "a" "b") 2) } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "a3"; "--init-file"; test "long_assign.ligo" ];
  [%expect {|
    (Pair (Pair { Elt "foo" 1 } 2) 3) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "pascaligo"; "a4"; "--init-file"; test "long_assign.ligo" ];
  [%expect {|
    { Elt "foo" (Pair { Elt "bar" 2 ; Elt "foo" 1 } 3) } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "af1"
    ; "--init-file"
    ; test "long_assign.ligo"
    ];
  [%expect {|
    { Elt "foo" (Pair (Pair -1 -2) "y") } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "af2"
    ; "--init-file"
    ; test "long_assign.ligo"
    ];
  [%expect {|
    { Elt "foo" { Elt "bar" (Pair (Pair "" "") 2) } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "af3"
    ; "--init-file"
    ; test "long_assign.ligo"
    ];
  [%expect {|
    (Pair (Pair { Elt "fooz" 1 } 2) 3) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "af4"
    ; "--init-file"
    ; test "long_assign.ligo"
    ];
  [%expect {|
    { Elt "fooz" (Pair { Elt "barz" 2 ; Elt "foo" 1 } 3) } |}]
