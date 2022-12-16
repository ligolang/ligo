open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/bugs/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "missing_stdlib_and_let_mut_in.jsligo"; "--format"; "dev"; "--with-types" ];
  [%expect {|
    Scopes:
    [ _useless#2 s#0  ] File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 44-48
    [ do_nothing#1 s#0  ] File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 2-26
    [ iter_op#3  ] File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20

    Variable definitions:
    (_useless#2 -> _useless)
    Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 20-28
    Body Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 44-48
    Content: |core: int|
    references: []
    (do_nothing#1 -> do_nothing)
    Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 6-16
    Body Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 6-16
    Content: |resolved: int -> unit|
    references:
      File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 13-23
    (iter_op#3 -> iter_op)
    Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 6-13
    Body Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 6-13
    Content: |resolved: list (int) -> unit|
    references:
      File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20
    (s#0 -> s)
    Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 17-18
    Body Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 6-48
    Content: |core: list (int)|
    references:
      File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 25-26
    (test#4 -> test)
    Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 6-10
    Body Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20
    Content: |resolved: list (int) -> unit|
    references: []
    Type definitions:
    Module definitions: |}]
