open Cli_expect

let () = Caml.Sys.chdir "../../test/projects/"
let pwd = Caml.Sys.getcwd ()

(* FIXME (@alistair.obrien): 
   This test is disabled until we patch the FA2 package with the correct exports / public imports 
   MR that introduced this: https://gitlab.com/ligolang/ligo/-/merge_requests/3112
   Related issue: https://gitlab.com/ligolang/ligo/-/issues/2160
*)
(* 
let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test"
    ; "originate_contract/test.mligo"
    ; "--project-root"
    ; "originate_contract"
    ; "--no-warn"
    ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 42, characters 25-47
  Called from Cli_expect_tests__Package_management.(fun) in file "src/bin/expect_tests/package_management.ml", line 7, characters 2-152
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "/Users/ajob410/tezos/ligo/_build/.sandbox/725ad249cb3ec43b31d1122fc75c6904/default/src/test/projects/originate_contract/.ligo/source/i/tezos_ligo_fa2__1.0.1__93f08e6c/test/fa2/single_asset.test.mligo", line 125, characters 77-113:
  124 |     Success _ -> failwith "This test should fail"
  125 |   | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval FA2_single_asset.Errors.not_operator))
                                                                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  126 |   | Fail _ -> failwith "invalid test failure"

   Module "FA2_single_asset.Errors" not found. |}] *)

let%expect_test _ =
  run_ligo_good [ "install"; "--project-root"; "complex_project_with_one_dependency" ];
  [%expect {| Project root: complex_project_with_one_dependency |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "measure-contract"
    ; "using_scope_pkg_project/src/a/b/c/contract.mligo"
    ; "--project-root"
    ; "using_scope_pkg_project"
    ];
  [%expect {| 95 bytes |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; "originate_contract/main.mligo"
    ; "--project-root"
    ; "originate_contract"
    ];
  [%expect
    {|
    File "originate_contract/main.mligo", line 1, characters 0-30:
      1 | #import "tezos-ligo-fa2" "FA2"
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      2 |
    File "tezos-ligo-fa2" not found. |}]

let () = Caml.Sys.chdir "using_scope_pkg_project"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "src/a/b/c/contract.test.mligo"; "--project-root"; "." ];
  [%expect
    {|
    File "src/a/b/c/contract.test.mligo", line 5, characters 13-27:
      4 |   let initial_storage = [1 ; 2 ; 3] in
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
                       ^^^^^^^^^^^^^^
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "src/a/b/c/contract.test.mligo", line 6, characters 10-27:
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      7 |   let storage = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "src/a/b/c/contract.test.mligo", line 7, characters 16-32:
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      7 |   let storage = Test.get_storage orig.addr in
                          ^^^^^^^^^^^^^^^^
      8 |   assert (storage = [3 ; 2 ; 1])
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "src/a/b/c/contract.test.mligo" ];
  [%expect
    {|
    File "src/a/b/c/contract.test.mligo", line 5, characters 13-27:
      4 |   let initial_storage = [1 ; 2 ; 3] in
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
                       ^^^^^^^^^^^^^^
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "src/a/b/c/contract.test.mligo", line 6, characters 10-27:
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      7 |   let storage = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "src/a/b/c/contract.test.mligo", line 7, characters 16-32:
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      7 |   let storage = Test.get_storage orig.addr in
                          ^^^^^^^^^^^^^^^^
      8 |   assert (storage = [3 ; 2 ; 1])
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "using_scope_pkg_project/src/a/b/c"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "contract.test.mligo" ];
  [%expect
    {|
    File "contract.test.mligo", line 5, characters 13-27:
      4 |   let initial_storage = [1 ; 2 ; 3] in
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
                       ^^^^^^^^^^^^^^
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "contract.test.mligo", line 6, characters 10-27:
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      7 |   let storage = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "contract.test.mligo", line 7, characters 16-32:
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      7 |   let storage = Test.get_storage orig.addr in
                          ^^^^^^^^^^^^^^^^
      8 |   assert (storage = [3 ; 2 ; 1])
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "using_scope_pkg_project/src/a/b"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "c/contract.test.mligo" ];
  [%expect
    {|
    File "c/contract.test.mligo", line 5, characters 13-27:
      4 |   let initial_storage = [1 ; 2 ; 3] in
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
                       ^^^^^^^^^^^^^^
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "c/contract.test.mligo", line 6, characters 10-27:
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      7 |   let storage = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "c/contract.test.mligo", line 7, characters 16-32:
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      7 |   let storage = Test.get_storage orig.addr in
                          ^^^^^^^^^^^^^^^^
      8 |   assert (storage = [3 ; 2 ; 1])
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "using_scope_pkg_project/src/a"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "b/c/contract.test.mligo" ];
  [%expect
    {|
    File "b/c/contract.test.mligo", line 5, characters 13-27:
      4 |   let initial_storage = [1 ; 2 ; 3] in
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
                       ^^^^^^^^^^^^^^
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "b/c/contract.test.mligo", line 6, characters 10-27:
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      7 |   let storage = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "b/c/contract.test.mligo", line 7, characters 16-32:
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      7 |   let storage = Test.get_storage orig.addr in
                          ^^^^^^^^^^^^^^^^
      8 |   assert (storage = [3 ; 2 ; 1])
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "using_scope_pkg_project/src"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "a/b/c/contract.test.mligo" ];
  [%expect
    {|
    File "a/b/c/contract.test.mligo", line 5, characters 13-27:
      4 |   let initial_storage = [1 ; 2 ; 3] in
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
                       ^^^^^^^^^^^^^^
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "a/b/c/contract.test.mligo", line 6, characters 10-27:
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      7 |   let storage = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "a/b/c/contract.test.mligo", line 7, characters 16-32:
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      7 |   let storage = Test.get_storage orig.addr in
                          ^^^^^^^^^^^^^^^^
      8 |   assert (storage = [3 ; 2 ; 1])
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test"
    ; "using_scope_pkg_project/src/a/b/c/contract.test.mligo"
    ; "--project-root"
    ; "using_scope_pkg_project"
    ];
  [%expect
    {|
    File "using_scope_pkg_project/src/a/b/c/contract.test.mligo", line 5, characters 13-27:
      4 |   let initial_storage = [1 ; 2 ; 3] in
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
                       ^^^^^^^^^^^^^^
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "using_scope_pkg_project/src/a/b/c/contract.test.mligo", line 6, characters 10-27:
      5 |   let orig = Test.originate (contract_of C) initial_storage 0tez in
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^
      7 |   let storage = Test.get_storage orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "using_scope_pkg_project/src/a/b/c/contract.test.mligo", line 7, characters 16-32:
      6 |   let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
      7 |   let storage = Test.get_storage orig.addr in
                          ^^^^^^^^^^^^^^^^
      8 |   assert (storage = [3 ; 2 ; 1])
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; "dao_path_bug/main.mligo"; "--project-root"; "dao_path_bug" ];
  [%expect
    {|
    { parameter unit ;
      storage (option nat) ;
      code { DROP ; SENDER ; UNIT ; VIEW "total_supply" nat ; NIL operation ; PAIR } } |}]

let () = Caml.Sys.chdir "dao_path_bug"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (option nat) ;
      code { DROP ; SENDER ; UNIT ; VIEW "total_supply" nat ; NIL operation ; PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "include_include/main.mligo"
    ; "--project-root"
    ; "include_include"
    ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "Hello" ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir "include_include"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "Hello" ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "include_import/main.mligo"
    ; "--project-root"
    ; "include_import"
    ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "World" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir "include_import"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "World" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "import_import/main.mligo"
    ; "--project-root"
    ; "import_import"
    ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "World" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir "import_import"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "Hello" ;
             PUSH string "World" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; "import_include/main.mligo"
    ; "--project-root"
    ; "import_include"
    ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "World" ;
             PUSH string " Work" ;
             PUSH string "Hello" ;
             CONCAT ;
             CONCAT ;
             PUSH string " Work" ;
             PUSH string "Hello" ;
             CONCAT ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir "import_include"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "main.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "World" ;
             PUSH string " Work" ;
             PUSH string "Hello" ;
             CONCAT ;
             CONCAT ;
             PUSH string " Work" ;
             PUSH string "Hello" ;
             CONCAT ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test"
    ; "using_ligo_breathalyser/test.mligo"
    ; "--project-root"
    ; "using_ligo_breathalyser"
    ; "--no-warn"
    ];
  [%expect
    {|
    (1 , 2 , 3)
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Caml.Sys.chdir "using_ligo_breathalyser"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; "test.mligo"; "--no-warn" ];
  [%expect
    {|
    (1 , 2 , 3)
    Everything at the top-level was executed.
    - test exited with value (). |}];
  run_ligo_good [ "run"; "test"; "test.mligo"; "--project-root"; "."; "--no-warn" ];
  [%expect
    {|
    (1 , 2 , 3)
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Caml.Sys.chdir pwd

let%expect_test _ =
  let test s =
    s
    |> String.split_lines
    |> List.length
    |> fun len -> if len > 0 then "Test passed" else "Test failed"
  in
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; "import_import/main.mligo"
    ; "--project-root"
    ; "import_import"
    ; "--format"
    ; "dev"
    ];
  print_endline @@ test [%expect.output];
  [%expect {|
    Test passed |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; "import_include/main.mligo"
    ; "--project-root"
    ; "import_include"
    ; "--format"
    ; "dev"
    ];
  print_endline @@ test [%expect.output];
  [%expect {|
    Test passed |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; "include_import/main.mligo"
    ; "--project-root"
    ; "include_import"
    ; "--format"
    ; "dev"
    ];
  print_endline @@ test [%expect.output];
  [%expect {|
    Test passed |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; "include_include/main.mligo"
    ; "--project-root"
    ; "include_include"
    ; "--format"
    ; "dev"
    ];
  print_endline @@ test [%expect.output];
  [%expect {|
    Test passed |}];
  run_ligo_good
    [ "info"
    ; "list-declarations"
    ; "include_include/main.mligo"
    ; "--project-root"
    ; "include_include"
    ];
  [%expect
    {|
    include_include/main.mligo declarations:
    $contract
    $views
    $main
    main
    hello |}]

(* main file resolution tests *)

let () = Caml.Sys.chdir "main_file_resolution/valid_main"

(* FIXME (@alistair.obrien): 
   This test is disabled until we patch the breathalyzer package with the correct exports / public imports 
   MR that introduced this: https://gitlab.com/ligolang/ligo/-/merge_requests/3112
   Related issue: https://gitlab.com/ligolang/ligo/-/issues/2161
*)
(* 
let%expect_test _ =
  run_ligo_good [ "run"; "test"; "main.mligo"; "--no-warn" ];
  [%expect.unreachable];
  Caml.Sys.chdir pwd;
  Caml.Sys.chdir "main_file_resolution/invalid_main";
  run_ligo_bad [ "run"; "test"; "main.mligo" ];
  [%expect.unreachable];
  Caml.Sys.chdir pwd;
  Caml.Sys.chdir "main_file_resolution/scoped_valid_main";
  run_ligo_good [ "run"; "test"; "main.mligo" ];
  [%expect.unreachable];
  Caml.Sys.chdir pwd;
  Caml.Sys.chdir "main_file_resolution/scoped_invalid_main";
  run_ligo_bad [ "run"; "test"; "main.mligo" ];
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 42, characters 25-47
  Called from Cli_expect_tests__Package_management.(fun) in file "src/bin/expect_tests/package_management.ml", line 624, characters 2-60
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "main.mligo", line 3, characters 11-28:
    2 |
    3 | let test = Breath.Logger.log Trace "Hello World"
                   ^^^^^^^^^^^^^^^^^

   Module "Breath.Logger" not found. |}] *)

let () = Caml.Sys.chdir pwd

(* ligo publish tests *)

let ligo_bin_path = "../../../../../install/default/bin/ligo"
let () = Caml.Sys.chdir "publish_invalid_main"

let%expect_test _ =
  run_ligo_bad [ "registry"; "publish"; "--dry-run" ];
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file...
    Error: main file does not exists.
    Please specify a valid LIGO file in ligo.json. |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_invalid_main2"

let%expect_test _ =
  run_ligo_bad [ "registry"; "publish"; "--dry-run" ];
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file...
    Error: Invalid LIGO file specifed in main field of ligo.json
    Valid extension for LIGO files are (.mligo, .jsligo) |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_invalid_storage"

let%expect_test _ =
  run_ligo_bad [ "registry"; "publish"; "--dry-run" ];
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file...
    Error: Check `storage_fn` & `storage_arg` in packge.json or check your LIGO storage expression |}]

let () = Caml.Sys.chdir pwd

let clean_size ~prefix line =
  if String.is_prefix ~prefix line
  then
    if String.is_suffix ~suffix:"kB" line
    then Format.asprintf "%s*** kB" prefix
    else if String.is_suffix ~suffix:"MB" line
    then Format.asprintf "%s*** MB" prefix
    else if String.is_suffix ~suffix:"GB" line
    then Format.asprintf "%s*** GB" prefix
    else if String.is_suffix ~suffix:"B" line
    then Format.asprintf "%s*** B" prefix
    else line
  else line


let remove_dynamic_info_from_log log =
  String.split_lines log
  |> List.filter ~f:(fun line ->
         not
           (String.is_prefix ~prefix:"    shasum:" line
           || String.is_prefix ~prefix:"    integrity:" line))
  |> (fun lines ->
       List.map lines ~f:(fun line ->
           if String.is_prefix ~prefix:"    package size:  " line
           then clean_size ~prefix:"    package size:  " line
           else if String.is_prefix ~prefix:"    unpacked size: " line
           then clean_size ~prefix:"    unpacked size: " line
           else line))
  |> String.concat ~sep:"\n"


let () = Caml.Sys.chdir "publish_lib_lt_1mb"

let%expect_test _ =
  run_ligo_good [ "registry"; "publish"; "--dry-run" ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: test_package_3@0.0.1
        === Tarball Details ===
        name:          test_package_3
        version:       0.0.1
        filename:      test_package_3-0.0.1.tgz
        package size:  *** kB
        unpacked size: *** kB
        total files:   3 |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_contract_lt_1mb"

let%expect_test _ =
  run_ligo_good [ "registry"; "publish"; "--dry-run" ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: test_package_4@0.0.1
        === Tarball Details ===
        name:          test_package_4
        version:       0.0.1
        filename:      test_package_4-0.0.1.tgz
        package size:  *** kB
        unpacked size: *** kB
        total files:   3 |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_contract_gt_1mb"

let%expect_test _ =
  run_ligo_good [ "registry"; "publish"; "--dry-run" ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: test_package_5@0.0.1
        === Tarball Details ===
        name:          test_package_5
        version:       0.0.1
        filename:      test_package_5-0.0.1.tgz
        package size:  *** kB
        unpacked size: *** MB
        total files:   3 |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "publish_contract_slash_in_pkg_name"

let%expect_test _ =
  run_ligo_good [ "registry"; "publish"; "--dry-run" ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: @ligo/slash@0.0.1
        === Tarball Details ===
        name:          @ligo/slash
        version:       0.0.1
        filename:      @ligo/slash-0.0.1.tgz
        package size:  *** B
        unpacked size: *** B
        total files:   3 |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "test_ligoignore"

let%expect_test _ =
  run_ligo_good [ "registry"; "publish"; "--dry-run" ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: testing_.ligoignore@0.0.1
        === Tarball Details ===
        name:          testing_.ligoignore
        version:       0.0.1
        filename:      testing_.ligoignore-0.0.1.tgz
        package size:  *** B
        unpacked size: *** B
        total files:   1 |}]

let () = Caml.Sys.chdir pwd
let () = Caml.Sys.chdir "test_ligoignore_with_empty_lines"

let%expect_test _ =
  run_ligo_good [ "registry"; "publish"; "--dry-run" ];
  let dry_run_log = remove_dynamic_info_from_log [%expect.output] in
  print_endline dry_run_log;
  [%expect
    {|
    ==> Reading manifest... Done
    ==> Validating manifest file... Done
    ==> Finding project root... Done
    ==> Packing tarball... Done
        publishing: testing_.ligoignore2@0.0.1
        === Tarball Details ===
        name:          testing_.ligoignore2
        version:       0.0.1
        filename:      testing_.ligoignore2-0.0.1.tgz
        package size:  *** B
        unpacked size: *** B
        total files:   2 |}]

let spawn_unpublish_mock_server () =
  let port = 8000 in
  Lwt.async @@ fun () -> Unpublish_mock_server.server_lwt port


let () = Caml.Sys.chdir pwd
(* Spawns a mock server with a single package called @foo/bar-pkg with two versions
   1.0.4
   1.0.5
 *)

let () = spawn_unpublish_mock_server ()

(* Tries to unpublish version 1.0.5 *)
let%expect_test _ =
  run_ligo_good
    [ "registry"
    ; "unpublish"
    ; "--package-name"
    ; "@foo/bar-pkg"
    ; "--package-version"
    ; "1.0.5"
    ; "--registry"
    ; "http://localhost:8000/-/api"
    ; "--ligorc-path"
    ; "./.ligorc"
    ];
  [%expect {|
    ==> Checking auth token... Done
    ==> Unpublishing package... Done |}]

(* Unpublishing the only version remaining, 1.0.4. Equivalent to unpublishing the entire package *)
let%expect_test _ =
  run_ligo_good
    [ "registry"
    ; "unpublish"
    ; "--package-name"
    ; "@foo/bar-pkg"
    ; "--package-version"
    ; "1.0.4"
    ; "--registry"
    ; "http://localhost:8000/-/api"
    ; "--ligorc-path"
    ; "./.ligorc"
    ];
  [%expect
    {|
    ==> Checking auth token... Done
    ==> Package @foo/bar-pkg has only one version 1.0.4. Unpublishing the entire package..... Done |}]

(* Try to unpublish again, this time, only failing *)
let%expect_test _ =
  run_ligo_bad
    [ "registry"
    ; "unpublish"
    ; "--package-name"
    ; "@foo/bar-pkg"
    ; "--registry"
    ; "http://localhost:8000/-/api"
    ; "--ligorc-path"
    ; "./.ligorc"
    ];
  [%expect
    {|
    ==> Checking auth token... Done
    ==> Deleting package completely...
    Package not found |}]

let () = Caml.Sys.chdir pwd
