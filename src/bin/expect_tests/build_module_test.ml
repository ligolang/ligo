open Cli_expect

let contract basename = "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print"; "dependency-graph"; contract "cycle_A.mligo" ];
  [%expect
    {|
    `-- 3 -- ../../test/contracts/build/cycle_A.mligo
        `-- 2 -- ../../test/contracts/build/cycle_B.mligo
            `-- 1 -- ../../test/contracts/build/cycle_C.mligo
                `-- 3 -- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good
    [ "print"; "dependency-graph"; contract "cycle_A.mligo"; "--format"; "json" ];
  [%expect
    {|
    {
      "status": "error",
      "stage": "build system",
      "content": {
        "message": "`-- 3 -- ../../test/contracts/build/cycle_A.mligo\n    `-- 2 -- ../../test/contracts/build/cycle_B.mligo\n        `-- 1 -- ../../test/contracts/build/cycle_C.mligo\n            `-- 3 -- ../../test/contracts/build/cycle_A.mligo\n"
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "print"; "dependency-graph"; contract "D.mligo" ];
  [%expect
    {|
    `-- 7 -- ../../test/contracts/build/D.mligo
        |-- 5 -- ../../test/contracts/build/C.mligo
        |   |-- 1 -- ../../test/contracts/build/A.mligo
        |   `-- 2 -- ../../test/contracts/build/B.mligo
        |       `-- 1 -- ../../test/contracts/build/A.mligo
        `-- 6 -- ../../test/contracts/build/E.mligo
            |-- 3 -- ../../test/contracts/build/F.mligo
            `-- 4 -- ../../test/contracts/build/G.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print"; "dependency-graph"; contract "D.mligo"; "--format"; "json" ];
  [%expect
    {|
    {
      "status": "error",
      "stage": "build system",
      "content": {
        "message": "`-- 7 -- ../../test/contracts/build/D.mligo\n    |-- 5 -- ../../test/contracts/build/C.mligo\n    |   |-- 1 -- ../../test/contracts/build/A.mligo\n    |   `-- 2 -- ../../test/contracts/build/B.mligo\n    |       `-- 1 -- ../../test/contracts/build/A.mligo\n    `-- 6 -- ../../test/contracts/build/E.mligo\n        |-- 3 -- ../../test/contracts/build/F.mligo\n        `-- 4 -- ../../test/contracts/build/G.mligo\n"
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "B.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 42 ;
             DUP 2 ;
             ADD ;
             DIG 2 ;
             CDR ;
             SWAP ;
             DUG 2 ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "instance/main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "FA2_TOKEN_UNDEFINED" ;
             PUSH string "AAAA" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "D.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 10 ;
             ADD ;
             SWAP ;
             UNPAIR ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "instance/main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "FA2_TOKEN_UNDEFINED" ;
             PUSH string "AAAA" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "D.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 10 ;
             ADD ;
             SWAP ;
             UNPAIR ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; contract "cycle_A.mligo" ];
  [%expect
    {|
    Dependency cycle detected :
     `-- 3 -- ../../test/contracts/build/cycle_A.mligo
        `-- 2 -- ../../test/contracts/build/cycle_B.mligo
            `-- 1 -- ../../test/contracts/build/cycle_C.mligo
                `-- 3 -- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "type_B.mligo" ];
  [%expect
    {|
    File "../../test/contracts/build/type_B.mligo", line 5, characters 5-6:
      4 | \tlet s = s + 1 in
      5 | \tlet p = p ^ "titi" in
                ^
      6 | \t([] : operation list), s
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter string ;
      storage int ;
      code { CDR ; PUSH int 1 ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "tata"; "--init-file"; contract "C.mligo" ];
  [%expect {| 44 |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "C1.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "C_test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "x"; "--init-file"; contract "Xmain.mligo" ];
  [%expect {|
    { 1 ; 2 ; 3 } |}]

let%expect_test _ =
  run_ligo_good [ "print"; "dependency-graph"; contract "Xmain.mligo"; "--format"; "dev" ];
  [%expect
    {|
    `-- 4 -- ../../test/contracts/build/Xmain.mligo
        |-- 3 -- ../../test/contracts/build/Xfoo.mligo
        |   |-- 1 -- ../../test/contracts/build/Xlist.mligo
        |   `-- 2 -- ../../test/contracts/build/Xset.mligo
        `-- 1 -- ../../test/contracts/build/Xlist.mligo |}]

let%expect_test _ =
  run_ligo_bad
    [ "run"; "interpret"; "--init-file"; contract "module_scoping_bug.mligo"; "x" ];
  [%expect
    {|
    File "../../test/contracts/build/module_scoping_bug.mligo", line 24, characters 8-13:
     23 |
     24 | let x = B.A.a
                  ^^^^^

     Module "B.A" not found. |}]

let pwd = Caml.Sys.getcwd ()
let () = Caml.Sys.chdir "../../test/contracts/build/"

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "test_libraries/src/main.mligo"
    ; "--library"
    ; "test_libraries/lib/parameter,test_libraries/lib/storage"
    ];
  [%expect
    {|
    { parameter (or (or (nat %a) (int %b)) (or (string %c) (bool %d))) ;
      storage nat ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT { ADD } { DROP } }
               { IF_LEFT { DROP } { DROP } } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; "test_libraries/src/main.mligo"
    ; "Parameter.initial_parameter ()"
    ; "--library"
    ; "test_libraries/lib/parameter,test_libraries/lib/storage"
    ];
  [%expect {|
    (Right (Left "Hello")) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; "test_libraries/src/main.mligo"
    ; "Storage.initial_storage ()"
    ; "--library"
    ; "test_libraries/lib/parameter,test_libraries/lib/storage"
    ];
  [%expect {|
    42 |}]

let () = Caml.Sys.chdir pwd
