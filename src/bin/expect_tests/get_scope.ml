open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "lambda_letin.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  print_endline @@ replace_extraneous_slashes @@ [%expect.output];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 11-28
    [ a#1:4-5 i#4:37-38 j#4:47-48  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-21
    [ a#1:4-5 i#4:37-38 j#4:47-48 g#5:8-9  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-25
    [ a#1:4-5 i#4:37-38 j#4:47-48 g#5:8-9 k#6:8-9  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-21
    [ a#1:4-5 f#4:6-7  ] File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-7

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, character 0 to line 9, character 7
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (f#4:6-7 -> f)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, character 2 to line 9, character 2
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3
    Mod Path =
    Def Type = Local
    (i#4:37-38 -> i)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-38
    Decl Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 37-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9
    Mod Path =
    Def Type = Parameter
    (j#4:47-48 -> j)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-48
    Decl Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 47-54
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5
    Mod Path =
    Def Type = Parameter
    (g#5:8-9 -> g)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, character 4 to line 6, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25 ,
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17
    Mod Path =
    Def Type = Local
    (k#6:8-9 -> k)
    Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, character 4 to line 7, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "letin.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  print_endline @@ replace_extraneous_slashes @@ [%expect.output];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ a#1:4-5 c#4:6-7  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ a#1:4-5 c#4:6-7 e#6:8-9  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ a#1:4-5 c#4:6-7 e#6:8-9 f#7:8-9  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ a#1:4-5 c#4:6-7 d#5:6-7  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 3, character 0 to line 10, character 11
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (c#4:6-7 -> c)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, character 2 to line 5, character 2
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    Mod Path =
    Def Type = Local
    (d#5:6-7 -> d)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 5, character 2 to line 10, character 2
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    Mod Path =
    Def Type = Local
    (e#6:8-9 -> e)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, character 4 to line 7, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    Mod Path =
    Def Type = Local
    (f#7:8-9 -> f)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, character 4 to line 8, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "lambda.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 11-27
    [ a#1:4-5 i#4:36-37 j#4:46-47  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-63
    [ a#1:4-5 f#4:6-7  ] File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-7

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 3, character 0 to line 5, character 7
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (f#4:6-7 -> f)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, character 2 to line 5, character 2
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3
    Mod Path =
    Def Type = Local
    (i#4:36-37 -> i)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-37
    Decl Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 36-43
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63
    Mod Path =
    Def Type = Parameter
    (j#4:46-47 -> j)
    Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-47
    Decl Range: File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 46-53
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59
    Mod Path =
    Def Type = Parameter
    Type definitions:
    Constructors and fields:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "match.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 14-40
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30  ] File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 8-9
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 c#6:13-14  ] File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 x#7:8-9  ] File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-18
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 y#8:8-9  ] File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-18
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 b#5:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 9-21
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 b#5:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 12-13
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 b#5:4-5 c#13:8-9  ] File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 b#5:4-5 hd#15:4-6 tl#15:8-10  ] File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 b#5:4-5 c#10:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 17-18
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 b#5:4-5 c#10:4-5 d#18:13-14  ] File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-31
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 b#5:4-5 c#10:4-5 s#19:10-11  ] File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-21
    [ mytype#1:5-11 Foo#1:14-17 Bar#1:27-30 a#3:4-5 b#5:4-5 c#10:4-5  ] File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13

    Variable definitions:
    (a#3:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13
    Mod Path =
    Def Type = Global
    (b#5:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 5, character 0 to line 8, character 18
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (c#6:13-14 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-22
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27
    Mod Path =
    Def Type = Local
    (x#7:8-9 -> x)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 4-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14
    Mod Path =
    Def Type = Local
    (y#8:8-9 -> y)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 4-9
    Content: |resolved: string|
    references: []
    Mod Path =
    Def Type = Local
    (c#10:4-5 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 10, character 0 to line 15, character 15
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (c#13:8-9 -> c)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 13, character 4 to line 14, character 4
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Local
    (hd#15:4-6 -> hd)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-10
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Local
    (tl#15:8-10 -> tl)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-10
    Content: |resolved: list (int)|
    references: []
    Mod Path =
    Def Type = Local
    (d#17:4-5 -> d)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 17, character 0 to line 20, character 13
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (d#18:13-14 -> d)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 9-22
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31
    Mod Path =
    Def Type = Local
    (s#19:10-11 -> s)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 10-11
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 4-12
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17
    Mod Path =
    Def Type = Local
    Type definitions:
    (mytype#1:5-11 -> mytype)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 5-11
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40
    Content: |sum[Bar -> string , Foo -> int({ name: Foo }, { name: Bar })]|
    references: []
    Constructors and fields:
    (Foo#1:14-17 -> Foo)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 14-17
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 14-24
    Content: int
    Constructor

    (Bar#1:27-30 -> Bar)
    Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 27-30
    Decl Range: File "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 27-40
    Content: string
    Constructor

    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "rec.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 8-9
    [ a#1:4-5 c#4:10-11 i#4:37-38 j#4:39-40  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-21
    [ a#1:4-5 c#4:10-11 i#4:37-38 j#4:39-40 k#5:8-9  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-11
    [ a#1:4-5 c#4:10-11 z#8:10-11 n#8:13-14 m#8:23-24  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 7-12
    [ a#1:4-5 c#4:10-11 z#8:10-11 n#8:13-14 m#8:23-24  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 18-19
    [ a#1:4-5 c#4:10-11 z#8:10-11 n#8:13-14 m#8:23-24  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 25-36
    [ a#1:4-5 c#4:10-11 z#8:10-11  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 10-18
    [ a#1:4-5 c#4:10-11 z#8:10-11 v#11:6-7  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 10-15
    [ a#1:4-5 c#4:10-11 z#8:10-11 v#11:6-7 b#12:6-7  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 2-10
    [ a#1:4-5 b#3:4-5 x#15:8-9 y#15:11-12  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 16, characters 5-10
    [ a#1:4-5 b#3:4-5 x#15:8-9 y#15:11-12  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 16, characters 16-18
    [ a#1:4-5 b#3:4-5 x#15:8-9 y#15:11-12  ] File "../../test/contracts/get_scope_tests/rec.mligo", line 17, characters 7-16
    [ a#1:4-5 b#3:4-5 c#4:10-11 z#8:10-11 n#8:13-14 m#8:23-24 x#15:8-9 y#15:11-12  ]

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 5-6
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 3, character 0 to line 13, character 10
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (c#4:10-11 -> c)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, character 2 to line 8, character 2
    Content: |core: [_]( int * int ) -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 2-3
    Mod Path =
    Def Type = Local
    (i#4:37-38 -> i)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, character 4
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13
    Mod Path =
    Def Type = Local
    (j#4:39-40 -> j)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, character 4
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17
    Mod Path =
    Def Type = Local
    (k#5:8-9 -> k)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 5, character 4 to line 6, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8
    Mod Path =
    Def Type = Local
    (z#8:10-11 -> z)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 10-11
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 11, character 2
    Content: |core: [n, m]int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 25-26 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 10-11
    Mod Path =
    Def Type = Local
    (n#8:13-14 -> n)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 13-14
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 13-20
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 7-8 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 28-29
    Mod Path =
    Def Type = Parameter
    (m#8:23-24 -> m)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 23-24
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 23-30
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 18-19 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 35-36
    Mod Path =
    Def Type = Parameter
    (v#11:6-7 -> v)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 11, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 11, character 2 to line 12, character 2
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 14-15
    Mod Path =
    Def Type = Local
    (b#12:6-7 -> b)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 12, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 12, character 2 to line 13, character 2
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 13, characters 8-9
    Mod Path =
    Def Type = Local
    (x#15:8-9 -> x)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 15, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 15, character 0 to line 17, character 16
    Content: |core: [y]int -> unit|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 17, characters 7-8
    Mod Path =
    Def Type = Global
    (y#15:11-12 -> y)
    Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 15, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/rec.mligo", line 15, characters 11-18
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/rec.mligo", line 16, characters 5-6 ,
      File "../../test/contracts/get_scope_tests/rec.mligo", line 17, characters 10-11
    Mod Path =
    Def Type = Parameter
    Type definitions:
    Constructors and fields:
    Module definitions:
 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "shadowing.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-15
    [ a#1:4-5 c#4:6-7  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-17
    [ a#1:4-5 c#4:6-7 e#6:8-9  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-21
    [ c#4:6-7 e#6:8-9 a#7:8-9  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-13
    [ a#1:4-5 c#4:6-7 d#5:6-7  ] File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-11

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, character 0 to line 10, character 11
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (c#4:6-7 -> c)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, character 2 to line 5, character 2
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7
    Mod Path =
    Def Type = Local
    (d#5:6-7 -> d)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, character 2 to line 10, character 2
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11
    Mod Path =
    Def Type = Local
    (e#6:8-9 -> e)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, character 4 to line 7, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13
    Mod Path =
    Def Type = Local
    (a#7:8-9 -> a)
    Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, character 4 to line 8, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "records.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 13-36
    [ myrec#1:5-10  ] File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 8-9
    [ myrec#1:5-10 a#3:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 22-23
    [ myrec#1:5-10 a#3:4-5 i#6:18-19  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 35-36
    [ myrec#1:5-10 a#3:4-5 i#6:18-19 j#6:31-32  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-45
    [ myrec#1:5-10 a#3:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 53-55
    [ myrec#1:5-10 a#3:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 2-44
    [ myrec#1:5-10 a#3:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4
    [ myrec#1:5-10 a#3:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24
    [ myrec#1:5-10 a#3:4-5 b#6:4-5 g#16:19-20  ] File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-41

    Variable definitions:
    (a#3:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 23-24 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41
    Mod Path =
    Def Type = Global
    (b#6:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 0-56
    Content: |resolved: myrec|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4 ,
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-33
    Mod Path =
    Def Type = Global
    (i#6:18-19 -> i)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-27
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43
    Mod Path =
    Def Type = Local
    (j#6:31-32 -> j)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-40
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45
    Mod Path =
    Def Type = Local
    (e#15:4-5 -> e)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 15, character 0 to line 16, character 44
    Content: |resolved: myrec|
    references: []
    Mod Path =
    Def Type = Global
    (g#16:19-20 -> g)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 15-28
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29
    Mod Path =
    Def Type = Local
    Type definitions:
    (myrec#1:5-10 -> myrec)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 5-10
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36
    Content: |record[bar -> int , foo -> int({ name: foo }, { name: bar })]|
    references: []
    Constructors and fields:
    (foo#1:14-17 -> foo)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 14-17
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 14-23
    Content: int
    Field

    (bar#1:26-29 -> bar)
    Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 26-29
    Decl Range: File "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 26-35
    Content: int
    Field

    Module definitions: |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "constant.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    [ a#1:4-5 c#5:10-11  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ a#1:4-5 c#5:10-11 d#5:26-27  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ a#1:4-5 e#6:9-10  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, character 0 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    Mod Path =
    Def Type = Global
    (c#5:10-11 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Decl Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-17
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    Mod Path =
    Def Type = Parameter
    (d#5:26-27 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Decl Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-35
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    Mod Path =
    Def Type = Local
    (e#6:9-10 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Decl Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-18
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "application.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 11-27
    [ i#2:36-37 j#2:46-47  ] File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-63
    [ f#2:6-7  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 2-37
    [ f#2:6-7 b#3:7-8  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-19
    [ f#2:6-7 c#3:26-27  ] File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/application.mligo", line 1, character 0 to line 3, character 37
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (f#2:6-7 -> f)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, character 2 to line 3, character 2
    Content: |core: int -> int -> int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17
    Mod Path =
    Def Type = Local
    (i#2:36-37 -> i)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-37
    Decl Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 36-43
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63
    Mod Path =
    Def Type = Parameter
    (j#2:46-47 -> j)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-47
    Decl Range: File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 46-53
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59
    Mod Path =
    Def Type = Parameter
    (b#3:7-8 -> b)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Local
    (c#3:26-27 -> c)
    Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27
    Decl Range: File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 22-35
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions:
 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "include.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  print_endline @@ replace_extraneous_slashes @@ [%expect.output];
  [%expect
    {|
    Scopes:
    [ a#1:4-5 b#3:4-5  ] File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 8-9
    [ x#3:4-5 a#1:4-5 b#3:4-5  ] File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-13
    [  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-15
    [ a#1:4-5 c#4:6-7  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-17
    [ a#1:4-5 c#4:6-7 e#6:8-9  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-21
    [ a#1:4-5 c#4:6-7 e#6:8-9 f#7:8-9  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-17
    [ a#1:4-5 c#4:6-7 d#5:6-7  ] File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-11

    Variable definitions:
    (x#3:4-5 -> x)
    Range: File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9
    Mod Path =
    Def Type = Global
    (y#5:4-5 -> y)
    Range: File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 0-13
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 3, character 0 to line 10, character 11
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (c#4:6-7 -> c)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 4, character 2 to line 5, character 2
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7
    Mod Path =
    Def Type = Local
    (d#5:6-7 -> d)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 5, character 2 to line 10, character 2
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11
    Mod Path =
    Def Type = Local
    (e#6:8-9 -> e)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 6, character 4 to line 7, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13
    Mod Path =
    Def Type = Local
    (f#7:8-9 -> f)
    Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/letin.mligo", line 7, character 4 to line 8, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "nominal_types.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 19-45
    [ foo_variant#1:5-16 Foo#1:19-22 Bar#1:32-35  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 18-58
    [ foo_variant#1:5-16 Foo#1:19-22 Bar#1:32-35 foo_record#2:5-15  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 12-13
    [ foo_variant#1:5-16 Foo#1:19-22 Bar#1:32-35 foo_record#2:5-15 a#4:4-5  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 12-14
    [ foo_variant#1:5-16 Foo#1:19-22 Bar#1:32-35 foo_record#2:5-15 a#4:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    [ foo_variant#1:5-16 Foo#1:19-22 Bar#1:32-35 foo_record#2:5-15 a#4:4-5 b#6:4-5  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    [ foo_variant#1:5-16 Foo#1:19-22 Bar#1:32-35 foo_record#2:5-15 a#4:4-5 b#6:4-5 c#8:4-5 p#13:11-12  ] File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 43-44

    Variable definitions:
    (a#4:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 4, characters 0-13
    Content: |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 9, characters 8-9
    Mod Path =
    Def Type = Global
    (b#6:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 6, characters 0-14
    Content: |resolved: foo_variant|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 10, characters 8-9
    Mod Path =
    Def Type = Global
    (c#8:4-5 -> c)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 8, character 0 to line 11, character 1
    Content: |resolved: foo_record|
    references: []
    Mod Path =
    Def Type = Global
    (check#13:4-9 -> check)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 4-9
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 0-48
    Content: |core: foo_record -> foo_variant|
    references: []
    Mod Path =
    Def Type = Global
    (p#13:11-12 -> p)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 11-25
    Content: |core: foo_record|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 43-44
    Mod Path =
    Def Type = Parameter
    Type definitions:
    (foo_variant#1:5-16 -> foo_variant)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 5-16
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 0-45
    Content: |sum[Bar -> string , Foo -> int({ name: Foo }, { name: Bar })]|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 26-37 ,
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 46-57 ,
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 29-40
    (foo_record#2:5-15 -> foo_record)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 5-15
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 0-58
    Content: |record[bar -> foo_variant ,
                     foo -> foo_variant({ name: foo }, { name: bar })]|
    references:
      File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 13, characters 15-25
    Constructors and fields:
    (Foo#1:19-22 -> Foo)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 19-22
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 19-29
    Content: int
    Constructor

    (Bar#1:32-35 -> Bar)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 32-35
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 1, characters 32-45
    Content: string
    Constructor

    (foo#2:20-23 -> foo)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 20-23
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 20-37
    Content: foo_variant
    Field

    (bar#2:40-43 -> bar)
    Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 40-43
    Decl Range: File "../../test/contracts/get_scope_tests/nominal_types.mligo", line 2, characters 40-57
    Content: foo_variant
    Field

    Module definitions: |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 15-16
    [ A#1:7-8  ] File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 8-14
    [ A#1:7-8 a#5:4-5  ] File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 11-12
    [ A#1:7-8 a#5:4-5 B#7:7-8  ] File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 8-14
    [ A#1:7-8 a#5:4-5 B#7:7-8 b#9:4-5  ] File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 16-17
    [ A#1:7-8 a#5:4-5 B#7:7-8 b#9:4-5 C#12:11-12  ] File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 15-16
    [ A#1:7-8 a#5:4-5 B#7:7-8 b#9:4-5 C#12:11-12 D#15:11-12  ] File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-7

    Variable definitions:
    (a#5:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 0-14
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (b#9:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 0-14
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (titi#11:4-8 -> titi)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 11, characters 4-8
    Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 11, character 0 to line 16, character 7
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    Constructors and fields:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 1, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 1, character 0 to line 3, character 3
    Content: Members: Variable definitions:
                      (toto#2:8-12 -> toto)
                      Range: File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 8-12
                      Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 2, characters 4-16
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 10-14 ,
                        File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 10-14
                      Mod Path = "A"
                      Def Type = Module_field
                      Type definitions:
                      Constructors and fields:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 5, characters 8-9 ,
      File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 11-12



    (B#7:7-8 -> B)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 7, characters 0-12
    Content: A#7:11-12 (-> A#1:7-8)
    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 9, characters 8-9



    (C#12:11-12 -> C)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 12, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 12, character 4 to line 15, character 4
    Content: Members: Variable definitions:
                      (a#13:12-13 -> a)
                      Range: File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 12-13
                      Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 13, characters 8-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 6-7
                      Mod Path = "C"
                      Def Type = Module_field
                      Type definitions:
                      Constructors and fields:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 15-16



    (D#15:11-12 -> D)
    Range: File "../../test/contracts/get_scope_tests/module.mligo", line 15, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/module.mligo", line 15, character 4 to line 16, character 4
    Content: C#15:15-16 (-> C#12:11-12)
    references:
      File "../../test/contracts/get_scope_tests/module.mligo", line 16, characters 4-5 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module2.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 12-13
    [ x#2:8-9  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 4, characters 16-17
    [ x#2:8-9 B#3:11-12  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 15-16
    [ A#1:7-8  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 11-14
    [ A#1:7-8 C#9:7-8  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 11-12
    [ A#1:7-8 C#9:7-8 D#11:7-8  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 11-14
    [ A#1:7-8 C#9:7-8 D#11:7-8 F#13:7-8  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 9-14
    [ A#1:7-8 C#9:7-8 D#11:7-8 F#13:7-8 a1#15:4-6  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 9-12
    [ A#1:7-8 C#9:7-8 D#11:7-8 F#13:7-8 a1#15:4-6 a2#16:4-6  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 9-12
    [ A#1:7-8 C#9:7-8 D#11:7-8 F#13:7-8 a1#15:4-6 a2#16:4-6 a3#17:4-6  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 9-12
    [ A#1:7-8 C#9:7-8 D#11:7-8 F#13:7-8 a1#15:4-6 a2#16:4-6 a3#17:4-6 a4#18:4-6  ] File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 9-12

    Variable definitions:
    (a1#15:4-6 -> a1)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 0-14
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (a2#16:4-6 -> a2)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 0-12
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (a3#17:4-6 -> a3)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 0-12
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (a4#18:4-6 -> a4)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 0-12
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (a5#19:4-6 -> a5)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 0-12
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    Constructors and fields:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 1, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 1, character 0 to line 7, character 3
    Content: Members: Variable definitions:
                      (x#2:8-9 -> x)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 8-9
                      Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 2, characters 4-13
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 11-12
                      Mod Path = "A"
                      Def Type = Module_field
                      Type definitions:
                      Constructors and fields:
                      Module definitions:
                      (B#3:11-12 -> B)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 3, characters 11-12
                      Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 3, character 4 to line 5, character 7
                      Content: Members: Variable definitions:
                                        (y#4:12-13 -> y)
                                        Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 4, characters 12-13
                                        Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 4, characters 8-17
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 11-12 ,
                                          File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 11-12
                                        Mod Path = "A""B"
                                        Def Type = Module_field
                                        Type definitions:
                                        Constructors and fields:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 13-14 ,
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 11-12



                      (E#6:11-12 -> E)
                      Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 11-12
                      Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 6, characters 4-16
                      Content: B#6:15-16 (-> B#3:11-12)
                      references:
                        File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 13-14




    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 15, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module2.mligo", line 16, characters 9-10



    (C#9:7-8 -> C)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 9, characters 0-14
    Content: A#9:11-12.B#9:13-14 (-> A#1:7-8.B#3:11-12)
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 17, characters 9-10



    (D#11:7-8 -> D)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 11, characters 0-12
    Content: A#11:11-12 (-> A#1:7-8)
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 18, characters 9-10



    (F#13:7-8 -> F)
    Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module2.mligo", line 13, characters 0-14
    Content: A#13:11-12.E#13:13-14 (A#1:7-8.E#6:11-12 -> B#3:11-12)
    references:
      File "../../test/contracts/get_scope_tests/module2.mligo", line 19, characters 9-10 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module3.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 12-13
    [ A#1:7-8  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 16-17
    [ A#1:7-8 y#7:12-13  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 16-19
    [ A#1:7-8 y#7:12-13 z#8:12-13  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 21-22
    [ A#1:7-8 y#7:12-13 z#8:12-13 a1#10:16-18  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 21-22
    [ A#1:7-8 y#7:12-13 z#8:12-13 a1#10:16-18 a2#11:16-18  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 12, characters 21-22
    [ A#1:7-8 B#6:11-12  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 16, characters 15-16
    [ A#1:7-8 B#6:11-12 D#16:11-12  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 15-18
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 13-16
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 13-16
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 13-19
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 13-19
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 13-19
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 13-16
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 13-16
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 13-19
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 13-19
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 13-19
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10 c5#29:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 13-17
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10 c5#29:8-10 e1#31:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 13-17
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10 c5#29:8-10 e1#31:8-10 e2#32:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 13-17
    [ A#1:7-8 B#6:11-12 D#16:11-12 E#17:11-12 b1#19:8-10 b2#20:8-10 b3#21:8-10 b4#22:8-10 b5#23:8-10 c1#25:8-10 c2#26:8-10 c3#27:8-10 c4#28:8-10 c5#29:8-10 e1#31:8-10 e2#32:8-10 e3#33:8-10  ] File "../../test/contracts/get_scope_tests/module3.mligo", line 35, character 4 to line 38, character 16

    Variable definitions:
    (x#5:4-5 -> x)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 5, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 5, character 0 to line 38, character 16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (b1#19:8-10 -> b1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 19, character 4 to line 20, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 4-6
    Mod Path =
    Def Type = Local
    (b2#20:8-10 -> b2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 20, character 4 to line 21, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 9-11
    Mod Path =
    Def Type = Local
    (b3#21:8-10 -> b3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 21, character 4 to line 22, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 14-16
    Mod Path =
    Def Type = Local
    (b4#22:8-10 -> b4)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 22, character 4 to line 23, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 19-21
    Mod Path =
    Def Type = Local
    (b5#23:8-10 -> b5)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 23, character 4 to line 25, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 36, characters 24-26
    Mod Path =
    Def Type = Local
    (c1#25:8-10 -> c1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 25, character 4 to line 26, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 4-6
    Mod Path =
    Def Type = Local
    (c2#26:8-10 -> c2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 26, character 4 to line 27, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 9-11
    Mod Path =
    Def Type = Local
    (c3#27:8-10 -> c3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 27, character 4 to line 28, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 14-16
    Mod Path =
    Def Type = Local
    (c4#28:8-10 -> c4)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 28, character 4 to line 29, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 19-21
    Mod Path =
    Def Type = Local
    (c5#29:8-10 -> c5)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 29, character 4 to line 31, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 37, characters 24-26
    Mod Path =
    Def Type = Local
    (e1#31:8-10 -> e1)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 31, character 4 to line 32, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 38, characters 4-6
    Mod Path =
    Def Type = Local
    (e2#32:8-10 -> e2)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 32, character 4 to line 33, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 38, characters 9-11
    Mod Path =
    Def Type = Local
    (e3#33:8-10 -> e3)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 8-10
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 33, character 4 to line 35, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 38, characters 14-16
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 1, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 1, character 0 to line 3, character 3
    Content: Members: Variable definitions:
                      (x#2:8-9 -> x)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 8-9
                      Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 2, characters 4-13
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 18-19
                      Mod Path = "A"
                      Def Type = Module_field
                      Type definitions:
                      Constructors and fields:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 16-17



    (B#6:11-12 -> B)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 6, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 6, character 4 to line 16, character 4
    Content: Members: Variable definitions:
                      (y#7:12-13 -> y)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 12-13
                      Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 7, characters 8-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 21-22 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 15-16
                      Mod Path = "B"
                      Def Type = Module_field
                      (z#8:12-13 -> z)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 12-13
                      Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 8, characters 8-19
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 21-22 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 15-16
                      Mod Path = "B"
                      Def Type = Module_field
                      Type definitions:
                      Constructors and fields:
                      Module definitions:
                      (C#9:15-16 -> C)
                      Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 9, characters 15-16
                      Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 9, character 8 to line 13, character 11
                      Content: Members: Variable definitions:
                                        (a1#10:16-18 -> a1)
                                        Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 16-18
                                        Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 10, characters 12-22
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 15-17
                                        Mod Path = "B""C"
                                        Def Type = Module_field
                                        (a2#11:16-18 -> a2)
                                        Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 16-18
                                        Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 11, characters 12-22
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 15-17
                                        Mod Path = "B""C"
                                        Def Type = Module_field
                                        (a3#12:16-18 -> a3)
                                        Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 12, characters 16-18
                                        Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 12, characters 12-22
                                        Content: |resolved: int|
                                        references:
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 17-19 ,
                                          File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 15-17
                                        Mod Path = "B""C"
                                        Def Type = Module_field
                                        Type definitions:
                                        Constructors and fields:
                                        Module definitions:

                      references:
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 17-18 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 15-16




    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 16, characters 15-16 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 15-16 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 19, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 20, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 21, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 22, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 23, characters 13-14



    (D#16:11-12 -> D)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 16, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 16, character 4 to line 17, character 4
    Content: B#16:15-16 (-> B#6:11-12)
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 25, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 26, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 27, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 28, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 29, characters 13-14



    (E#17:11-12 -> E)
    Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 17, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/module3.mligo", line 17, character 4 to line 19, character 4
    Content: B#17:15-16.C#17:17-18 (-> B#6:11-12.C#9:15-16)
    references:
      File "../../test/contracts/get_scope_tests/module3.mligo", line 31, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 32, characters 13-14 ,
      File "../../test/contracts/get_scope_tests/module3.mligo", line 33, characters 13-14 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module4.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 2, characters 12-13
    [ a#2:8-9  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 16-17
    [ a#2:8-9 B#3:11-12  ] File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 4-7

    Variable definitions:
    (x#1:4-5 -> x)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 1, character 0 to line 6, character 7
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (a#2:8-9 -> a)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 2, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 2, character 4 to line 3, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 16-17
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions:
    (B#3:11-12 -> B)
    Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 3, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 3, character 4 to line 6, character 4
    Content: Members: Variable definitions:
                      (y#4:12-13 -> y)
                      Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 12-13
                      Decl Range: File "../../test/contracts/get_scope_tests/module4.mligo", line 4, characters 8-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 6-7
                      Mod Path = "B"
                      Def Type = Module_field
                      Type definitions:
                      Constructors and fields:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/module4.mligo", line 6, characters 4-5 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module5.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 4, characters 20-21
    [ C#3:15-16  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 19-20
    [ B#2:11-12  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 15-16
    [ A#1:7-8  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 11-12
    [ A#1:7-8 D#11:7-8  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 11-16
    [ A#1:7-8 D#11:7-8 G#13:7-8  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 9-16
    [ A#1:7-8 D#11:7-8 G#13:7-8 x1#15:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 9-16
    [ A#1:7-8 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 9-16
    [ A#1:7-8 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 9-16
    [ A#1:7-8 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6 x4#18:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 9-16
    [ A#1:7-8 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6 x4#18:4-6 x5#19:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 9-16
    [ A#1:7-8 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6 x4#18:4-6 x5#19:4-6 x6#20:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 9-16
    [ A#1:7-8 D#11:7-8 G#13:7-8 x1#15:4-6 x2#16:4-6 x3#17:4-6 x4#18:4-6 x5#19:4-6 x6#20:4-6 x7#21:4-6  ] File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 9-16

    Variable definitions:
    (x1#15:4-6 -> x1)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 0-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (x2#16:4-6 -> x2)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 0-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (x3#17:4-6 -> x3)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 0-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (x4#18:4-6 -> x4)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 0-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (x5#19:4-6 -> x5)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 0-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (x6#20:4-6 -> x6)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 0-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (x7#21:4-6 -> x7)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 0-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (x8#22:4-6 -> x8)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 4-6
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 0-16
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    Constructors and fields:
    Module definitions:
    (A#1:7-8 -> A)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 1, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 1, character 0 to line 9, character 3
    Content: Members: Variable definitions:
                      Type definitions:
                      Constructors and fields:
                      Module definitions:
                      (B#2:11-12 -> B)
                      Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 2, characters 11-12
                      Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 2, character 4 to line 7, character 7
                      Content: Members: Variable definitions:
                                        Type definitions:
                                        Constructors and fields:
                                        Module definitions:
                                        (C#3:15-16 -> C)
                                        Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 3, characters 15-16
                                        Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 3, character 8 to line 5, character 11
                                        Content: Members: Variable definitions:
                                                          (x#4:16-17 -> x)
                                                          Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 4, characters 16-17
                                                          Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 4, characters 12-21
                                                          Content: |resolved: int|
                                                          references:
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 15-16 ,
                                                            File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 15-16
                                                          Mod Path = "A""B""C"
                                                          Def Type = Module_field
                                                          Type definitions:
                                                          Constructors and fields:
                                                          Module definitions:

                                        references:
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 19-20 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 13-14



                                        (F#6:15-16 -> F)
                                        Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 15-16
                                        Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 6, characters 8-20
                                        Content: C#6:19-20 (-> C#3:15-16)
                                        references:
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 15-16 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 13-14 ,
                                          File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 13-14




                      references:
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 15-16 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 11-12



                      (E#8:11-12 -> E)
                      Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 11-12
                      Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 8, characters 4-16
                      Content: B#8:15-16 (-> B#2:11-12)
                      references:
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 13-14 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 11-12 ,
                        File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 11-12




    references:
      File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 15, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 16, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 17, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 18, characters 9-10



    (D#11:7-8 -> D)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 11, characters 0-12
    Content: A#11:11-12 (-> A#1:7-8)
    references:
      File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 11-12 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 19, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 20, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 21, characters 9-10 ,
      File "../../test/contracts/get_scope_tests/module5.mligo", line 22, characters 9-10



    (G#13:7-8 -> G)
    Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/module5.mligo", line 13, characters 0-16
    Content: D#13:11-12.E#13:13-14.F#13:15-16 (D#11:7-8.E#8:11-12.F#6:15-16 -> C#3:15-16)
    references: [] |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "module_shadowing.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
      Scopes:
      [  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 2, characters 12-13
      [ A#1:7-8  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 11-12
      [ A#1:7-8 D#5:7-8  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, characters 12-13
      [ A#1:7-8 D#5:7-8 x#8:8-9  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 16-19
      [ D#5:7-8 x#8:8-9 A#9:11-12  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, characters 15-16
      [ D#5:7-8 x#8:8-9 A#9:11-12 C#12:11-12  ] File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 4-11

      Variable definitions:
      (y#7:4-5 -> y)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 7, characters 4-5
      Decl Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 7, character 0 to line 13, character 11
      Content: |resolved: int|
      references: []
      Mod Path =
      Def Type = Global
      (x#8:8-9 -> x)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, characters 8-9
      Decl Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 8, character 4 to line 9, character 4
      Content: |resolved: int|
      references:
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 4-5
      Mod Path =
      Def Type = Local
      Type definitions:
      Constructors and fields:
      Module definitions:
      (A#1:7-8 -> A)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 1, characters 7-8
      Decl Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 1, character 0 to line 3, character 3
      Content: Members: Variable definitions:
                        (x#2:8-9 -> x)
                        Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 2, characters 8-9
                        Decl Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 2, characters 4-13
                        Content: |resolved: int|
                        references:
                          File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 18-19
                        Mod Path = "A"
                        Def Type = Module_field
                        Type definitions:
                        Constructors and fields:
                        Module definitions:

      references:
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 11-12 ,
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 16-17



      (D#5:7-8 -> D)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 7-8
      Decl Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 5, characters 0-12
      Content: A#5:11-12 (-> A#1:7-8)
      references: []



      (A#9:11-12 -> A)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 9, characters 11-12
      Decl Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 9, character 4 to line 12, character 4
      Content: Members: Variable definitions:
                        (x#10:12-13 -> x)
                        Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 12-13
                        Decl Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 10, characters 8-19
                        Content: |resolved: int|
                        references:
                          File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 10-11
                        Mod Path = "A"
                        Def Type = Module_field
                        Type definitions:
                        Constructors and fields:
                        Module definitions:

      references:
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, characters 15-16 ,
        File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 13, characters 8-9



      (C#12:11-12 -> C)
      Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, characters 11-12
      Decl Range: File "../../test/contracts/get_scope_tests/module_shadowing.mligo", line 12, character 4 to line 13, character 4
      Content: A#12:15-16 (-> A#9:11-12)
      references: [] |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "types.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
      Scopes:
      [  ] File "../../test/contracts/get_scope_tests/types.mligo", line 2, characters 13-16
      [ t#2:9-10  ] File "../../test/contracts/get_scope_tests/types.mligo", line 3, characters 16-28
      [ t#2:9-10 x#3:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 12-13
      [ t#2:9-10 x#3:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 16-17
      [ t#2:9-10 x#3:12-13 a#5:8-9  ] File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 12-18
      [ t#2:9-10 x#3:12-13 a#5:8-9  ] File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 31-36
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19  ] File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 16-19
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19  ] File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 22-29
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19 g#12:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 16-22
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19 g#12:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 37-44
      [ t#2:9-10 x#3:12-13 a#5:8-9 b#6:8-9 foo#9:13-16 bar#10:16-19 g#12:12-13 h#14:12-13  ] File "../../test/contracts/get_scope_tests/types.mligo", line 15, characters 8-10
      [ A#1:7-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 18, characters 11-12
      [ A#1:7-8 B#18:7-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 8-11
      [ A#1:7-8 B#18:7-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 14-15
      [ A#1:7-8 B#18:7-8 c#21:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 11-14
      [ A#1:7-8 B#18:7-8 c#21:4-5 hmm#23:5-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 8-11
      [ A#1:7-8 B#18:7-8 c#21:4-5 hmm#23:5-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 26-28
      [ A#1:7-8 B#18:7-8 c#21:4-5 hmm#23:5-8 d#24:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 8-11
      [ A#1:7-8 B#18:7-8 c#21:4-5 hmm#23:5-8 d#24:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 14-15
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 d#24:4-5 c#27:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 11-14
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 d#24:4-5 c#27:4-5 idk#29:5-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 8-12
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 d#24:4-5 c#27:4-5 idk#29:5-8  ] File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 27-31
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 32, characters 9-12
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6  ] File "../../test/contracts/get_scope_tests/types.mligo", line 33, characters 12-21
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15  ] File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 8-9
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15  ] File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 12-14
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15 e#36:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 8-12
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15 e#36:4-5  ] File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 21-26
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15 e#36:4-5 f#38:4-5 qux#41:9-12  ] File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 12-15
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15 e#36:4-5 f#38:4-5 qux#41:9-12  ] File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 18-23
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15 e#36:4-5 f#38:4-5 qux#41:9-12 i#43:8-9 boo#44:12-15  ] File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 12-15
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15 e#36:4-5 f#38:4-5 qux#41:9-12 i#43:8-9 boo#44:12-15  ] File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 27-29
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15 e#36:4-5 f#38:4-5 qux#41:9-12 i#43:8-9 boo#44:12-15 j#46:8-9  ] File "../../test/contracts/get_scope_tests/types.mligo", line 47, characters 4-6
      [ A#1:7-8 B#18:7-8 hmm#23:5-8 c#27:4-5 idk#29:5-8 d#30:4-5 s#32:5-6 q#33:8-9 Baz#33:12-15 e#36:4-5 f#38:4-5 exp2#40:4-8 x#49:14-15  ] File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 30-35

      Variable definitions:
      (c#21:4-5 -> c)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 4-5
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 0-15
      Content: |core: B.t|
      references: []
      Mod Path =
      Def Type = Global
      (d#24:4-5 -> d)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 4-5
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 0-30
      Content: |core: hmm (nat)|
      references: []
      Mod Path =
      Def Type = Global
      (c#27:4-5 -> c)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 4-5
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 0-15
      Content: |core: A.t|
      references: []
      Mod Path =
      Def Type = Global
      (d#30:4-5 -> d)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 4-5
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 0-33
      Content: |core: idk (bool)|
      references: []
      Mod Path =
      Def Type = Global
      (e#36:4-5 -> e)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 4-5
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 0-14
      Content: |core: s|
      references: []
      Mod Path =
      Def Type = Global
      (f#38:4-5 -> f)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 4-5
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 0-26
      Content: |core: q (bool)|
      references: []
      Mod Path =
      Def Type = Global
      (exp2#40:4-8 -> exp2)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 40, characters 4-8
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 40, character 0 to line 47, character 6
      Content: |resolved: unit|
      references: []
      Mod Path =
      Def Type = Global
      (i#43:8-9 -> i)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 8-9
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 43, character 4 to line 44, character 4
      Content: |core: qux|
      references: []
      Mod Path =
      Def Type = Local
      (j#46:8-9 -> j)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 8-9
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 46, character 4 to line 47, character 4
      Content: |core: boo (nat)|
      references: []
      Mod Path =
      Def Type = Local
      (fn#49:4-6 -> fn)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 4-6
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 0-35
      Content: |resolved: [x]A.t -> s|
      references: []
      Mod Path =
      Def Type = Global
      (x#49:14-15 -> x)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 14-15
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 14-21
      Content: |core: A.t|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 34-35
      Mod Path =
      Def Type = Parameter
      Type definitions:
      (hmm#23:5-8 -> hmm)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 5-8
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 0-14
      Content: |B.x|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 24, characters 12-15
      (idk#29:5-8 -> idk)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 5-8
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 0-14
      Content: |A.x|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 30, characters 13-16
      (s#32:5-6 -> s)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 32, characters 5-6
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 32, characters 0-12
      Content: |nat|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 36, characters 8-9 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 25-26
      (q#33:8-9 -> q)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 33, characters 8-9
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 33, characters 0-21
      Content: |funtype 'a : * . sum[Baz -> 'a({ name: Baz })]|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 38, characters 13-14
      (qux#41:9-12 -> qux)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 41, characters 9-12
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 41, character 4 to line 43, character 4
      Content: |bool|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 43, characters 12-15
      (boo#44:12-15 -> boo)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 44, characters 12-15
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 44, character 4 to line 46, character 4
      Content: |funtype 'a : * . option ('a)|
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 46, characters 16-19
      Constructors and fields:
      (Baz#33:12-15 -> Baz)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 33, characters 12-15
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 33, characters 12-21
      Content: 'a
      Constructor

      Module definitions:
      (A#1:7-8 -> A)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 1, characters 7-8
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 1, character 0 to line 16, character 3
      Content: Members: Variable definitions:
                        (a#5:8-9 -> a)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 8-9
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 4-17
                        Content: |core: t|
                        references: []
                        Mod Path = "A"
                        Def Type = Module_field
                        (b#6:8-9 -> b)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 8-9
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 4-38
                        Content: |core: x (string)|
                        references: []
                        Mod Path = "A"
                        Def Type = Module_field
                        (exp1#8:8-12 -> exp1)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 8, characters 8-12
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 8, character 4 to line 15, character 10
                        Content: |resolved: unit|
                        references: []
                        Mod Path = "A"
                        Def Type = Module_field
                        (g#12:12-13 -> g)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 12-13
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 12, character 8 to line 14, character 8
                        Content: |core: foo|
                        references: []
                        Mod Path = "A"
                        Def Type = Local
                        (h#14:12-13 -> h)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 12-13
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 14, character 8 to line 15, character 8
                        Content: |core: bar (string)|
                        references: []
                        Mod Path = "A"
                        Def Type = Local
                        Type definitions:
                        (t#2:9-10 -> t)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 2, characters 9-10
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 2, characters 4-16
                        Content: |int|
                        references:
                          File "../../test/contracts/get_scope_tests/types.mligo", line 5, characters 12-13 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 10-11 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 10-11 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 20-21
                        (x#3:12-13 -> x)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 3, characters 12-13
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 3, characters 4-28
                        Content: |funtype 'a : * . record[foo -> 'a({ name: foo })]|
                        references:
                          File "../../test/contracts/get_scope_tests/types.mligo", line 6, characters 19-20 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 13-14 ,
                          File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 13-14
                        (foo#9:13-16 -> foo)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 9, characters 13-16
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 9, character 8 to line 10, character 8
                        Content: |string|
                        references:
                          File "../../test/contracts/get_scope_tests/types.mligo", line 12, characters 16-19
                        (bar#10:16-19 -> bar)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 10, characters 16-19
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 10, character 8 to line 12, character 8
                        Content: |funtype 'a : * . record[bar -> 'a({ name: bar })]|
                        references:
                          File "../../test/contracts/get_scope_tests/types.mligo", line 14, characters 23-26
                        Constructors and fields:
                        (foo#3:18-21 -> foo)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 3, characters 18-21
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 3, characters 18-26
                        Content: 'a
                        Field

                        (bar#10:24-27 -> bar)
                        Range: File "../../test/contracts/get_scope_tests/types.mligo", line 10, characters 24-27
                        Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 10, characters 24-32
                        Content: 'a
                        Field

                        Module definitions:

      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 18, characters 11-12 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 27, characters 8-9 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 29, characters 11-12 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 49, characters 18-19



      (B#18:7-8 -> B)
      Range: File "../../test/contracts/get_scope_tests/types.mligo", line 18, characters 7-8
      Decl Range: File "../../test/contracts/get_scope_tests/types.mligo", line 18, characters 0-12
      Content: A#18:11-12 (-> A#1:7-8)
      references:
        File "../../test/contracts/get_scope_tests/types.mligo", line 21, characters 8-9 ,
        File "../../test/contracts/get_scope_tests/types.mligo", line 23, characters 11-12 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "import_x.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/import_x.mligo", line 1, characters 11-94
    [ X#1:7-8  ] File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 8-13
    [  ] File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 9-33
    [ x#1:5-6  ] File "../../test/contracts/get_scope_tests/x.mligo", line 3, characters 8-9
    [ x#1:5-6 y#3:4-5  ] File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 14-15
    [ x#1:5-6 y#3:4-5  ] File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 22-24

    Variable definitions:
    (z#3:4-5 -> z)
    Range: File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 0-13
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    Constructors and fields:
    Module definitions:
    (X#1:7-8 -> X)
    Range: File "../../test/contracts/get_scope_tests/import_x.mligo", line 1, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/import_x.mligo", line 1, characters 0-94
    Content: Members: Variable definitions:
                      (y#3:4-5 -> y)
                      Range: File "../../test/contracts/get_scope_tests/x.mligo", line 3, characters 4-5
                      Decl Range: File "../../test/contracts/get_scope_tests/x.mligo", line 3, characters 0-9
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 14-15
                      Mod Path = "Mangled_module__p__p__s__p__p__s_test_s_contracts_s_get_u_scope_u_tests_s_x_p_mligo"
                      Def Type = Module_field
                      (x#4:4-5 -> x)
                      Range: File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 4-5
                      Decl Range: File "../../test/contracts/get_scope_tests/x.mligo", line 4, characters 0-25
                      Content: |resolved: X.x|
                      references:
                        File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 10-11
                      Mod Path = "Mangled_module__p__p__s__p__p__s_test_s_contracts_s_get_u_scope_u_tests_s_x_p_mligo"
                      Def Type = Module_field
                      Type definitions:
                      (x#1:5-6 -> x)
                      Range: File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 5-6
                      Decl Range: File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 0-33
                      Content: |record[a -> int ,
                                       b -> string({ name: a }, { name: b })]|
                      references: []
                      Constructors and fields:
                      (a#1:11-12 -> a)
                      Range: File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 11-12
                      Decl Range: File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 11-18
                      Content: int
                      Field

                      (b#1:21-22 -> b)
                      Range: File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 21-22
                      Decl Range: File "../../test/contracts/get_scope_tests/x.mligo", line 1, characters 21-31
                      Content: string
                      Field

                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/import_x.mligo", line 3, characters 8-9 |}];
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "local_module_using_local_binding.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 1, characters 8-9
    [ a#1:4-5  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 4, characters 12-13
    [ a#4:8-9  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 16-17
    [ a#4:8-9 A#5:11-12  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 8, characters 4-7
    [ a#1:4-5 b#3:4-5  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 10, characters 8-9
    [ a#1:4-5 b#3:4-5 c#10:4-5  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 13, characters 12-13
    [ a#1:4-5 b#3:4-5 c#13:8-9  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 15, characters 16-17
    [ a#1:4-5 b#3:4-5 c#15:12-13  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 16, characters 16-17
    [ a#1:4-5 b#3:4-5 c#13:8-9 C#14:11-12  ] File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 18, characters 4-7

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 1, characters 0-9
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 3, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 3, character 0 to line 8, character 7
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (a#4:8-9 -> a)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 4, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 4, character 4 to line 5, character 4
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 16-17
    Mod Path =
    Def Type = Local
    (c#10:4-5 -> c)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 10, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 10, characters 0-9
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (d#12:4-5 -> d)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 12, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 12, character 0 to line 18, character 7
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (c#13:8-9 -> c)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 13, characters 8-9
    Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 13, character 4 to line 14, character 4
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Local
    Type definitions:
    Constructors and fields:
    Module definitions:
    (A#5:11-12 -> A)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 5, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 5, character 4 to line 8, character 4
    Content: Members: Variable definitions:
                      (x#6:12-13 -> x)
                      Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 12-13
                      Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 6, characters 8-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 8, characters 6-7
                      Mod Path = "A"
                      Def Type = Module_field
                      Type definitions:
                      Constructors and fields:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 8, characters 4-5



    (C#14:11-12 -> C)
    Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 14, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 14, character 4 to line 18, character 4
    Content: Members: Variable definitions:
                      (c#15:12-13 -> c)
                      Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 15, characters 12-13
                      Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 15, characters 8-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 16, characters 16-17
                      Mod Path = "C"
                      Def Type = Module_field
                      (x#16:12-13 -> x)
                      Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 16, characters 12-13
                      Decl Range: File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 16, characters 8-17
                      Content: |resolved: int|
                      references:
                        File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 18, characters 6-7
                      Mod Path = "C"
                      Def Type = Module_field
                      Type definitions:
                      Constructors and fields:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/local_module_using_local_binding.mligo", line 18, characters 4-5 |}]

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "complex_patterns.mligo"
    ; "--format"
    ; "dev"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  [%expect
    {|
    Scopes:
    [  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 1, characters 11-12
    [  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 1, characters 14-21
    [ a#1:4-5 b#1:7-8  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 3, characters 9-32
    [ a#1:4-5 b#1:7-8 t#3:5-6  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 4, characters 17-18
    [ a#1:4-5 b#1:7-8 t#3:5-6  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 4, characters 20-21
    [ t#3:5-6 a#4:6-7 b#4:9-10  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 6, characters 9-19
    [ t#3:5-6 a#4:6-7 b#4:9-10 u#6:5-6 Foo#6:9-12  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 7, characters 18-20
    [ t#3:5-6 a#4:6-7 b#4:9-10 u#6:5-6 Foo#6:9-12 x#7:9-10  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 9, characters 9-28
    [ t#3:5-6 a#4:6-7 b#4:9-10 u#6:5-6 Foo#6:9-12 x#7:9-10 v#9:5-6 Bar#9:9-12  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 10, characters 24-25
    [ t#3:5-6 a#4:6-7 b#4:9-10 u#6:5-6 Foo#6:9-12 x#7:9-10 v#9:5-6 Bar#9:9-12  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 10, characters 27-28
    [ t#3:5-6 a#4:6-7 b#4:9-10 u#6:5-6 Foo#6:9-12 x#7:9-10 v#9:5-6 Bar#9:9-12 i#10:10-11 j#10:13-14  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 12, characters 9-17
    [ t#3:5-6 a#4:6-7 b#4:9-10 u#6:5-6 Foo#6:9-12 x#7:9-10 v#9:5-6 Bar#9:9-12 i#10:10-11 j#10:13-14 w#12:5-6 Qux#12:9-12  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 13, characters 24-25
    [ t#3:5-6 a#4:6-7 b#4:9-10 u#6:5-6 Foo#6:9-12 x#7:9-10 v#9:5-6 Bar#9:9-12 i#10:10-11 j#10:13-14 w#12:5-6 Qux#12:9-12  ] File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 13, characters 27-28

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 1, characters 4-5
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 1, characters 0-21
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 4, characters 17-18
    Mod Path =
    Def Type = Global
    (b#1:7-8 -> b)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 1, characters 7-8
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 1, characters 0-21
    Content: |resolved: string|
    references:
      File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 4, characters 20-21
    Mod Path =
    Def Type = Global
    (a#4:6-7 -> a)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 4, characters 6-7
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 4, characters 0-23
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 10, characters 24-25 ,
      File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 13, characters 24-25
    Mod Path =
    Def Type = Global
    (b#4:9-10 -> b)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 4, characters 9-10
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 4, characters 0-23
    Content: |resolved: string|
    references:
      File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 10, characters 27-28 ,
      File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 13, characters 27-28
    Mod Path =
    Def Type = Global
    (x#7:9-10 -> x)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 7, characters 9-10
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 7, characters 0-20
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (i#10:10-11 -> i)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 10, characters 10-11
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 10, characters 0-29
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (j#10:13-14 -> j)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 10, characters 13-14
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 10, characters 0-29
    Content: |resolved: string|
    references: []
    Mod Path =
    Def Type = Global
    (a#13:10-11 -> a)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 13, characters 10-11
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 13, characters 0-29
    Content: |resolved: int|
    references: []
    Mod Path =
    Def Type = Global
    (b#13:13-14 -> b)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 13, characters 13-14
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 13, characters 0-29
    Content: |resolved: string|
    references: []
    Mod Path =
    Def Type = Global
    Type definitions:
    (t#3:5-6 -> t)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 3, characters 5-6
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 3, characters 0-32
    Content: |record[a -> int , b -> string({ name: a }, { name: b })]|
    references:
      File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 12, characters 16-17
    (u#6:5-6 -> u)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 6, characters 5-6
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 6, characters 0-19
    Content: |sum[Foo -> int({ name: Foo })]|
    references: []
    (v#9:5-6 -> v)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 9, characters 5-6
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 9, characters 0-28
    Content: |sum[Bar -> ( int * string )({ name: Bar })]|
    references: []
    (w#12:5-6 -> w)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 12, characters 5-6
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 12, characters 0-17
    Content: |sum[Qux -> t({ name: Qux })]|
    references: []
    Constructors and fields:
    (a#3:11-12 -> a)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 3, characters 11-12
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 3, characters 11-18
    Content: int
    Field

    (b#3:20-21 -> b)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 3, characters 20-21
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 3, characters 20-30
    Content: string
    Field

    (Foo#6:9-12 -> Foo)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 6, characters 9-12
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 6, characters 9-19
    Content: int
    Constructor

    (Bar#9:9-12 -> Bar)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 9, characters 9-12
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 9, characters 9-28
    Content: ( int * string )
    Constructor

    (Qux#12:9-12 -> Qux)
    Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 12, characters 9-12
    Decl Range: File "../../test/contracts/get_scope_tests/complex_patterns.mligo", line 12, characters 9-17
    Content: t
    Constructor

    Module definitions:
 |}]

(* Unresponsive "go-to-definition" VSCode extension command
   cause by [Location.compare] behaviour w.r.t. LSet / LMap
   (https://gitlab.com/ligolang/ligo/-/merge_requests/2532)
*)
let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "get-scope"
    ; gs "min_repr_2532/main.mligo"
    ; "--format"
    ; "json"
    ; "--with-types"
    ; "--no-stdlib"
    ];
  let () =
    let open Yojson.Safe in
    from_string [%expect.output]
    |> Util.member "definitions"
    |> Util.member "types"
    |> Util.member "parameter#4:5-14"
    |> Util.member "references"
    |> pretty_to_string
    |> print_endline
  in
  [%expect
    {|
      [
        [
          "File",
          {
            "start": {
              "byte": {
                "pos_fname": "../../test/contracts/get_scope_tests/min_repr_2532/main.mligo",
                "pos_lnum": 3,
                "pos_bol": 0,
                "pos_cnum": 25
              },
              "point_num": 163,
              "point_bol": 138
            },
            "stop": {
              "byte": {
                "pos_fname": "../../test/contracts/get_scope_tests/min_repr_2532/main.mligo",
                "pos_lnum": 3,
                "pos_bol": 0,
                "pos_cnum": 34
              },
              "point_num": 172,
              "point_bol": 138
            }
          }
        ]
      ] |}]
