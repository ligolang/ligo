open Cli_expect

let () = Sys.chdir "../../test/projects/"

let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "originate_contract/test.mligo" ; "--project-root" ; "originate_contract" ; "--no-warn" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value KT1Riu7zn7S1PCTu197y2i29TGheSLzfeaZ6(None). |}]

let%expect_test _ = 
  run_ligo_good [ "info"; "measure-contract" ; "using_scope_pkg_project/contract.mligo" ; "--project-root" ; "using_scope_pkg_project" ] ;
  [%expect{|
    95 bytes |}]

let pwd = Sys.getcwd ()
let () = Sys.chdir "using_scope_pkg_project"
let%expect_test _ =
  run_ligo_good [ "run"; "test" ; "contract.test.mligo" ; "--project-root" ; "." ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test_originate exited with value (). |}]
let () = Sys.chdir pwd