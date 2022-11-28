open Cli_expect

(* patches *)

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "w1"
    ; "--init-file"
    ; test "patch_long_path.ligo"
    ];
  [%expect {|
    (Pair (Pair 1 2) "y") |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "w2"
    ; "--init-file"
    ; test "patch_long_path.ligo"
    ];
  [%expect {|
    { Elt "bar" (Pair 1 "one") ; Elt "foo" (Pair 12 "douze") } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "w3"
    ; "--init-file"
    ; test "patch_long_path.ligo"
    ];
  [%expect {|
    { Elt "one" 1 ; Elt "two" 2 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "w4"
    ; "--init-file"
    ; test "patch_long_path.ligo"
    ];
  [%expect
    {|
    { Elt "x" { Elt "one" 1 ; Elt "three" 3 ; Elt "two" 2 } ;
      Elt "y" { Elt "five" 5 ; Elt "four" 4 } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "w5"
    ; "--init-file"
    ; test "patch_long_path.ligo"
    ];
  [%expect {|
    { Elt "foo" { Elt "bar" (Pair "a" "b") } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "w6"
    ; "--init-file"
    ; test "patch_long_path.ligo"
    ];
  [%expect {|
    (Pair 1 "b") |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "w7"
    ; "--init-file"
    ; test "patch_long_path.ligo"
    ];
  [%expect {|
    { Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "w8"
    ; "--init-file"
    ; test "patch_long_path.ligo"
    ];
  [%expect {|
    { Elt "bar" { "c" ; "d" } ; Elt "foo" { "a" ; "b" } } |}]
