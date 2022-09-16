open Cli_expect

let () = Sys.chdir "../../test/projects/"

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "originate_contract/test.mligo" ; "--project-root" ; "originate_contract" ; "--no-warn" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value KT1KD4LM2SpR88dNjniDUb2EXKYDNj1phjkE(None). |}]

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
