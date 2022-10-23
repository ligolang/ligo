open Cli_expect

let () = Sys_unix.chdir "../../test/projects/"

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "originate_contract/test.mligo" ; "--project-root" ; "originate_contract" ; "--no-warn" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value KT1QVWJTnMi6XJFPpnASjbfi53qokforNwdP(None). |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract" ; "using_scope_pkg_project/src/a/b/c/contract.mligo" ; "--project-root" ; "using_scope_pkg_project" ] ;
  [%expect{|
    95 bytes |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract" ; "originate_contract/main.mligo" ; "--project-root" ; "originate_contract" ] ;
  [%expect{|
    File "originate_contract/main.mligo", line 1, characters 0-30:
      1 | #import "tezos-ligo-fa2" "FA2"
      2 |
    File "tezos-ligo-fa2" not found. |}]

let pwd = Sys_unix.getcwd ()
let () = Sys_unix.chdir "using_scope_pkg_project"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "src/a/b/c/contract.test.mligo" ; "--project-root" ; "." ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "src/a/b/c/contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys_unix.chdir pwd

let pwd = Sys_unix.getcwd ()
let () = Sys_unix.chdir "using_scope_pkg_project/src/a/b/c"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys_unix.chdir pwd

let pwd = Sys_unix.getcwd ()
let () = Sys_unix.chdir "using_scope_pkg_project/src/a/b"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "c/contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys_unix.chdir pwd

let pwd = Sys_unix.getcwd ()
let () = Sys_unix.chdir "using_scope_pkg_project/src/a"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "b/c/contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys_unix.chdir pwd

let pwd = Sys_unix.getcwd ()
let () = Sys_unix.chdir "using_scope_pkg_project/src"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "a/b/c/contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys_unix.chdir pwd

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "using_scope_pkg_project/src/a/b/c/contract.test.mligo" ; "--project-root" ; "using_scope_pkg_project" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract" ; "dao_path_bug/main.mligo" ; "--project-root" ; "dao_path_bug" ] ;
  [%expect{|
    { parameter unit ;
      storage (option nat) ;
      code { DROP ; SENDER ; UNIT ; VIEW "total_supply" nat ; NIL operation ; PAIR } } |}]

let pwd = Sys_unix.getcwd ()
let () = Sys_unix.chdir "dao_path_bug"
let%expect_test _ =
  run_ligo_good [ "compile"; "contract" ; "main.mligo" ] ;
  [%expect{|
    { parameter unit ;
      storage (option nat) ;
      code { DROP ; SENDER ; UNIT ; VIEW "total_supply" nat ; NIL operation ; PAIR } } |}]
let () = Sys_unix.chdir pwd