open Cli_expect

let pwd = Sys_unix.getcwd ()
let () = Sys_unix.chdir "../../test/contracts/include/test1"

let%expect_test _ =
  run_ligo_good [ "print"; "preprocessed"; "root.ligo"; "--lib"; "includes" ];
  [%expect
    {|
    # 1 "root.ligo"

    # 1 "includes/b1.ligo" 1

    # 1 "includes/b2/b2.ligo" 1

    # 1 "includes/b2/../b3.ligo" 1
    const b3 = 3

    # 2 "includes/b2/b2.ligo" 2

    const b2 = b3 * 3

    # 2 "includes/b1.ligo" 2

    const b1 = b2 * 2 + b3

    # 2 "root.ligo" 2 |}]

let () =
  Sys_unix.chdir pwd;
  Sys_unix.chdir "../../test/contracts/include/test2"


let%expect_test _ =
  run_ligo_good [ "print"; "preprocessed"; "Root.mligo"; "--lib"; "bug" ];
  [%expect
    {|
    # 1 "Root.mligo"

    # 1 "bug/A.mligo" 1

    # 1 "bug/dir/B.mligo" 1
    let x : int = 42
    # 2 "bug/A.mligo" 2

    # 2 "Root.mligo" 2 |}]

let () =
  Sys_unix.chdir pwd;
  Sys_unix.chdir "../../test/contracts/include/test3"


let%expect_test _ =
  run_ligo_good [ "print"; "preprocessed"; "B1.ligo"; "--lib"; "B2" ];
  [%expect
    {|
    # 1 "B1.ligo"

    # 1 "B2/B2.ligo" 1

    # 1 "B2/../B3.ligo" 1
    const b3 = 3
    # 2 "B2/B2.ligo" 2

    const b2 = b3 * 3
    # 2 "B1.ligo" 2

    const b1 = b2 * 2 + b3 |}]

let () =
  Sys_unix.chdir pwd;
  Sys_unix.chdir "../../test/contracts/include/test4/current"


let%expect_test _ =
  run_ligo_good [ "print"; "preprocessed"; "../Root.ligo"; "--lib"; "../bug" ];
  [%expect
    {|
    # 1 "../Root.ligo"

    # 1 "../bug/nested/A.ligo" 1

    # 1 "../bug/nested/../B.ligo" 1
    const x = 42
    # 2 "../bug/nested/A.ligo" 2

    # 2 "../Root.ligo" 2 |}]

let () =
  Sys_unix.chdir pwd;
  Sys_unix.chdir "../../test/contracts/include/missing_asi"


let%expect_test _ =
  run_ligo_good [ "print"; "preprocessed"; "b.jsligo" ];
  [%expect
    {|
    # 1 "b.jsligo"

    # 1 "a.jsligo" 1
    const x = 1

    const y = x + 1
    # 2 "b.jsligo" 2

    const z = y + x

    const test = assert(z == 3) |}];
  run_ligo_good [ "run"; "test"; "b.jsligo" ];
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () =
  Sys_unix.chdir pwd;
  Sys_unix.chdir "../../test/contracts/include"


let%expect_test _ =
  run_ligo_bad [ "print"; "preprocessed"; "include_cycle1/a.mligo" ];
  [%expect
    {|
    File "include_cycle1/a.mligo", line 1, characters 9-18:
      1 | #include "b.mligo"
    Error: Dependency cycle between:
    -> "include_cycle1/a.mligo"
    -> "include_cycle1/b.mligo"
    -> "include_cycle1/c.mligo" |}];
  run_ligo_bad [ "print"; "preprocessed"; "include_cycle2/a.mligo" ];
  [%expect
    {|
    File "include_cycle2/b.mligo", line 1, characters 9-18:
      1 | #include "c.mligo"
    Error: Dependency cycle between:
    -> "include_cycle2/b.mligo"
    -> "include_cycle2/c.mligo" |}];
  run_ligo_bad [ "print"; "preprocessed"; "include_cycle3/a.mligo" ];
  [%expect
    {|
    File "include_cycle3/c.mligo", line 1, characters 9-18:
      1 | #include "c.mligo"
    Error: Dependency cycle between:
    -> "include_cycle3/c.mligo" |}];
  run_ligo_bad [ "print"; "preprocessed"; "mutual_incl/foo.mligo" ];
  [%expect
    {|
    File "mutual_incl/foo.mligo", line 1, characters 9-20:
      1 | #include "bar.mligo"
      2 |
    Error: Dependency cycle between:
    -> "mutual_incl/foo.mligo"
    -> "mutual_incl/bar.mligo" |}]

let () = Sys_unix.chdir pwd
