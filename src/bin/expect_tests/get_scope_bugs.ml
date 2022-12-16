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

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "missing_stdlib.ligo"; "--format"; "dev"; "--with-types" ];
  [%expect {|
    Scopes:
    [ p#0  ] File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 2, characters 30-55
    [ c#1 p#0  ] File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 3, characters 7-23

    Variable definitions:
    (c#1 -> c)
    Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 2, characters 8-9
    Body Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 2, characters 30-56
    Content: |core: contract (unit)|
    references:
      File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 3, characters 22-23
    (main#2 -> main)
    Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 1, characters 9-13
    Body Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 1, character 0 to line 3, character 24
    Content: |core: key_hash -> address|
    references: []
    (p#0 -> p)
    Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 1, characters 21-22
    Body Range: File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 1, character 48 to line 3, character 24
    Content: |core: key_hash|
    references:
      File "../../test/contracts/get_scope_tests/bugs/missing_stdlib.ligo", line 2, characters 54-55
    Type definitions:
    Module definitions: |}]