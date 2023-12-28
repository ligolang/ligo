open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/regressions/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "missing_stdlib_and_let_mut_in.jsligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ s#1:17-18 _useless#2:20-28  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 44-48
    [ s#1:17-18 do_nothing#2:6-16  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 2-27
    [ iter_op#1:6-13  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20

    Variable definitions:
    (iter_op#1:6-13 -> iter_op)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 6-13
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, character 16 to line 4, character 1
    Content: |resolved: [s]list (int) -> unit|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20
    Mod Path =
    Def Type = Global
    (s#1:17-18 -> s)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 1, characters 17-18
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 6-48
    Content: |core: list (int)|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 25-26
    Mod Path =
    Def Type = Parameter
    (do_nothing#2:6-16 -> do_nothing)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 6-16
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 19-48
    Content: |resolved: [_useless]int -> unit|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 3, characters 13-23
    Mod Path =
    Def Type = Local
    (_useless#2:20-28 -> _useless)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 20-28
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 2, characters 44-48
    Content: |core: int|
    references: []
    Mod Path =
    Def Type = Parameter
    (test#6:6-10 -> test)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 6-10
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib_and_let_mut_in.jsligo", line 6, characters 13-20
    Content: |resolved: [s]list (int) -> unit|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "missing_stdlib.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [ p#1:11-12  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 10-14
    [ p#1:11-12  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 26-52
    [ p#1:11-12 c#2:6-7  ] File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 3, characters 2-17

    Variable definitions:
    (check#1:4-9 -> check)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 1, characters 4-9
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, character 2 to line 3, character 17
    Content: |core: key_hash -> address|
    references: []
    Mod Path =
    Def Type = Global
    (p#1:11-12 -> p)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 1, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, character 2 to line 3, character 17
    Content: |core: key_hash|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 50-51
    Mod Path =
    Def Type = Parameter
    (c#2:6-7 -> c)
    Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 2, characters 26-52
    Content: |core: contract (unit)|
    references:
      File "../../test/contracts/get_scope_tests/regressions/missing_stdlib.mligo", line 3, characters 16-17
    Mod Path =
    Def Type = Local
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "buggy_file_with_core_types.jsligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 1, character 12 to line 5, character 1
    [ user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, characters 14-18
    [ user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 8, characters 13-21
    [ user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 9, characters 13-17
    [ user#1:5-9  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 10, characters 13-20
    [ user#1:5-9 alice#7:6-11  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 18-22
    [ user#1:5-9 alice#7:6-11  ] File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-30

    Variable definitions:
    (alice#7:6-11 -> alice)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, characters 6-11
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, character 21 to line 11, character 1
    Content: |core: user|
    references:
      File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-30
    Mod Path =
    Def Type = Global
    (alice_admin#13:4-15 -> alice_admin)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 4-15
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-32
    Content: |core: bool|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    (user#1:5-9 -> user)
    Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 1, characters 5-9
    Body Range: File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 1, character 12 to line 5, character 1
    Content: : |record[id -> nat ,
                       is_admin -> bool ,
                       name -> string({ name: id }, { name: is_admin }, { name: name })]|
    references:
      File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 7, characters 14-18
    Module definitions:
    Errors:
    File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 25-32:
     12 |
     13 | let alice_admin : bool = alice.i
                                   ^^^^^^^

    Invalid record field "i" in record.
    Warnings:
    File "../../test/contracts/get_scope_tests/regressions/buggy_file_with_core_types.jsligo", line 13, characters 0-32:
     12 |
     13 | let alice_admin : bool = alice.i
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Toplevel let declaration is silently changed to const declaration. |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "wrong_reference1.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 8-10
    [ x#2:6-7  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 10-11
    [ x#1:4-5 f#2:4-5  ] File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9

    Variable definitions:
    (x#1:4-5 -> x)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 1, characters 8-10
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9
    Mod Path =
    Def Type = Global
    (f#2:4-5 -> f)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 10-11
    Content: |resolved: ∀ gen#3 : * . [x]gen#3 -> int|
    references: []
    Mod Path =
    Def Type = Global
    (x#2:6-7 -> x)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 6-7
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 2, characters 10-11
    Content: |resolved: gen#3|
    references: []
    Mod Path =
    Def Type = Parameter
    (g#3:4-5 -> g)
    Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/wrong_reference1.mligo", line 3, characters 8-9
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module_alias_def_reference.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 19-20
    [ A#2:7-8 B#3:11-12 toto#4:12-16 D#9:11-12  ] File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-21

    Variable definitions:
    Type definitions:
    Module definitions:
    (A#2:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 2, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 2, character 11 to line 6, character 3
    Content: Members: Variable definitions:
                      Type definitions:
                      Module definitions:
                      (B#3:11-12 -> B)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 3, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 3, character 15 to line 5, character 7
                      Content: Members: Variable definitions:
                                        (toto#4:12-16 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 12-16
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 4, characters 19-20
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 17-21
                                        Mod Path = "A""B"
                                        Def Type = Module_field
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 17-18




    references:
      File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 15-16



    (C#8:7-8 -> C)
    Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 8, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 8, character 11 to line 11, character 3
    Content: Members: Variable definitions:
                      (tata#10:8-12 -> tata)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 8-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-21
                      Content: |resolved: int|
                      references: []
                      Mod Path = "C"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:
                      (D#9:11-12 -> D)
                      Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 9, characters 15-18
                      Content: A#9:15-16.B#9:17-18 (-> A#2:7-8.B#3:11-12)
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/module_alias_def_reference.mligo", line 10, characters 15-16




    references: [] |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_module_alias_def_reference.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, characters 16-19
    [ titi#2:9-13  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 18-22
    [ titi#2:9-13  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 25-27
    [ A#1:7-8 titi#2:9-13 C#3:11-12 toto#4:12-16 D#8:7-8  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 11-17
    [ A#1:7-8 titi#2:9-13 C#3:11-12 toto#4:12-16 D#8:7-8 E#11:11-12  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 4-10

    Variable definitions:
    (toto#10:4-8 -> toto)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, character 4 to line 12, character 10
    Content: |core: D.titi|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 1, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 1, character 11 to line 6, character 3
    Content: Members: Variable definitions:
                      Type definitions:
                      (titi#2:9-13 -> titi)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, characters 9-13
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 2, characters 16-19
                      Content: : |int|
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 18-22 ,
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 13-17
                      Module definitions:
                      (C#3:11-12 -> C)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 3, characters 11-12
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 3, character 15 to line 5, character 7
                      Content: Members: Variable definitions:
                                        (toto#4:12-16 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 12-16
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 4, characters 25-27
                                        Content: |core: titi|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 6-10
                                        Mod Path = "A""C"
                                        Def Type = Module_field
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 17-18




    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 15-16



    (D#8:7-8 -> D)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 7-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 8, characters 11-12
    Content: A#8:11-12 (-> A#1:7-8)
    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 10, characters 11-12



    (E#11:11-12 -> E)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 11, characters 15-18
    Content: A#11:15-16.C#11:17-18 (-> A#1:7-8.C#3:11-12)
    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference.mligo", line 12, characters 4-5 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_module_alias_def_reference2.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 1, characters 11-14
    [  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 23-24
    [ A#2:11-12 C#3:15-16 toto#4:16-20 F#8:15-16  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-25
    [ A#2:11-12 C#3:15-16 toto#4:16-20 E#7:11-12 F#8:15-16 toto#9:12-16  ] File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 4-10

    Variable definitions:
    (toto#1:4-8 -> toto)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 1, characters 4-8
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 2, character 4 to line 11, character 10
    Content: |core: int|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    Module definitions:
    (A#2:11-12 -> A)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 2, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 2, character 15 to line 6, character 7
    Content: Members: Variable definitions:
                      Type definitions:
                      Module definitions:
                      (C#3:15-16 -> C)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 3, characters 15-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 3, character 19 to line 5, character 11
                      Content: Members: Variable definitions:
                                        (toto#4:16-20 -> toto)
                                        Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 16-20
                                        Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 4, characters 23-24
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 21-25
                                        Mod Path = "A""C"
                                        Def Type = Module_field
                                        Type definitions:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 21-22




    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 19-20



    (E#7:11-12 -> E)
    Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 7, characters 11-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 7, character 15 to line 10, character 7
    Content: Members: Variable definitions:
                      (toto#9:12-16 -> toto)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 12-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-25
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 6-10
                      Mod Path = "E"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:
                      (F#8:15-16 -> F)
                      Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 15-16
                      Body Range: File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 8, characters 19-22
                      Content: A#8:19-20.C#8:21-22 (-> A#2:11-12.C#3:15-16)
                      references:
                        File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 9, characters 19-20




    references:
      File "../../test/contracts/get_scope_tests/regressions/local_module_alias_def_reference2.mligo", line 11, characters 4-5 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "duplicate_unused_warnings.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 1, characters 20-21
    [ s_x#2:9-12  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 2, characters 16-17
    [  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 3, characters 12-13
    [ m#1:4-5  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 6, characters 21-22
    [ m#1:4-5 _#7:9-10  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 7, characters 14-15
    [ m#1:4-5  ] File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 8, characters 12-13

    Variable definitions:
    (m#1:4-5 -> m)
    Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 1, character 8 to line 3, character 13
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (s_x#2:9-12 -> s_x)
    Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 2, characters 9-12
    Body Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 2, characters 16-17
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Local
    (m2#6:4-6 -> m2)
    Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 6, characters 4-6
    Body Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 6, character 9 to line 8, character 13
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (_#7:9-10 -> _)
    Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 7, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/regressions/duplicate_unused_warnings.mligo", line 7, characters 14-15
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Local
    Type definitions:
    Module definitions: |}]
