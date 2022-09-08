open Cli_expect

let () = Sys.chdir "../../test/projects/"

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "originate_contract/test.mligo" ; "--project-root" ; "originate_contract" ; "--no-warn" ] ;
  [%expect{|
    File "originate_contract/test.mligo", line 9, characters 77-99:
      8 |     let (addr, _, _) = Test.originate_from_file f "main" ([]: string list) v_mich 0tez in
      9 |     let taddr : (SingleAsset.parameter, SingleAsset.storage) typed_address = Test.cast_address addr in
     10 |     let contr = Test.to_contract taddr in
    :
    Everything at the top-level was executed.
    - test exited with value KT1EMmL4W1wQMgt36Vet6EmGBarD1czMwQMC(None).
    Run-time warning: cast changing the type of an address. |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract" ; "using_scope_pkg_project/src/a/b/c/contract.mligo" ; "--project-root" ; "using_scope_pkg_project" ] ;
  [%expect{|
    95 bytes |}]

let pwd = Sys.getcwd ()
let () = Sys.chdir "using_scope_pkg_project"
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
let () = Sys.chdir pwd

let pwd = Sys.getcwd ()
let () = Sys.chdir "using_scope_pkg_project/src/a/b/c"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys.chdir pwd

let pwd = Sys.getcwd ()
let () = Sys.chdir "using_scope_pkg_project/src/a/b"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "c/contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys.chdir pwd

let pwd = Sys.getcwd ()
let () = Sys.chdir "using_scope_pkg_project/src/a"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "b/c/contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys.chdir pwd

let pwd = Sys.getcwd ()
let () = Sys.chdir "using_scope_pkg_project/src"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "a/b/c/contract.test.mligo" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys.chdir pwd

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "using_scope_pkg_project/src/a/b/c/contract.test.mligo" ; "--project-root" ; "using_scope_pkg_project" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
