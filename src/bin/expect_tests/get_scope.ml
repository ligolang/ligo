open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/"^s

let%expect_test _ =
  run_ligo_good [ "get-scope" ; gs "lambda_letin.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f5 a0 ] in file "lambda_letin.mligo", line 9, characters 2-7
    [ k4 j2 i1 g3 a0 ] in file "lambda_letin.mligo", line 7, characters 4-21
    [ j2 i1 g3 a0 ] in file "lambda_letin.mligo", line 6, character 4 to line 7, character 21
    [ j2 i1 a0 ] in file "lambda_letin.mligo", line 5, character 4 to line 7, character 21
    [ ] in file "lambda_letin.mligo", line 1, characters 0-9

    Variable definitions:
    (k4 -> k) in file "lambda_letin.mligo", line 6, characters 12-25
    (j2 -> j) in file "lambda_letin.mligo", line 5, character 4 to line 7, character 21
    (i1 -> i) in file "lambda_letin.mligo", line 4, character 32 to line 7, character 21
    (g3 -> g) in file "lambda_letin.mligo", line 5, characters 12-21
    (f5 -> f) in file "lambda_letin.mligo", line 4, characters 8-9
    (b6 -> b) in file "lambda_letin.mligo", line 3, character 0 to line 9, character 7
    (a0 -> a) in file "lambda_letin.mligo", line 1, characters 0-9
    Type definitions: |} ];

  run_ligo_good [ "get-scope" ; gs "letin.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ d4 c1 a0 ] in file "letin.mligo", line 10, characters 2-11
    [ f3 e2 c1 a0 ] in file "letin.mligo", line 8, characters 4-17
    [ e2 c1 a0 ] in file "letin.mligo", line 7, character 4 to line 8, character 17
    [ c1 a0 ] in file "letin.mligo", line 6, character 4 to line 8, character 17
    [ a0 ] in file "letin.mligo", line 4, character 2 to line 10, character 11
    [ ] in file "letin.mligo", line 1, characters 0-9

    Variable definitions:
    (f3 -> f) in file "letin.mligo", line 7, characters 12-21
    (e2 -> e) in file "letin.mligo", line 6, characters 12-17
    (d4 -> d) in file "letin.mligo", line 6, character 4 to line 8, character 17
    (c1 -> c) in file "letin.mligo", line 4, characters 10-15
    (b5 -> b) in file "letin.mligo", line 3, character 0 to line 10, character 11
    (a0 -> a) in file "letin.mligo", line 1, characters 0-9
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "lambda.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f3 a0 ] in file "lambda.mligo", line 5, characters 2-7
    [ j2 i1 a0 ] in file "lambda.mligo", line 4, characters 58-63
    [ ] in file "lambda.mligo", line 1, characters 0-9

    Variable definitions:
    (j2 -> j) in file "lambda.mligo", line 4, characters 58-63
    (i1 -> i) in file "lambda.mligo", line 4, characters 31-63
    (f3 -> f) in file "lambda.mligo", line 4, characters 8-9
    (b4 -> b) in file "lambda.mligo", line 3, character 0 to line 5, character 7
    (a0 -> a) in file "lambda.mligo", line 1, characters 0-9
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "match.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ s11 mytype0 c9 b5 a1 ] in file "match.mligo", line 19, characters 16-21
    [ mytype0 c9 b5 a1 ] in file "match.mligo", line 20, characters 12-13
    [ mytype0 d10 c9 b5 a1 ] in file "match.mligo", line 18, characters 22-32
    [ mytype0 c9 b5 a1 ] in file "match.mligo", line 18, characters 9-32
    [ tl8 mytype0 hd7 b5 a1 ] in file "match.mligo", line 15, characters 14-15
    [ mytype0 c6 b5 a1 ] in file "match.mligo", line 14, characters 4-5
    [ mytype0 b5 a1 ] in file "match.mligo", line 13, character 4 to line 14, character 5
    [ mytype0 b5 a1 ] in file "match.mligo", line 11, characters 9-21
    [ y4 mytype0 a1 ] in file "match.mligo", line 8, characters 13-18
    [ x3 mytype0 a1 ] in file "match.mligo", line 7, characters 13-18
    [ mytype0 c2 a1 ] in file "match.mligo", line 6, characters 22-27
    [ mytype0 a1 ] in file "match.mligo", line 6, characters 9-27
    [ mytype0 ] in file "match.mligo", line 3, characters 0-9

    Variable definitions:
    (s11 -> s) in file "match.mligo", line 19, characters 16-21
    (d12 -> d) in file "match.mligo", line 17, character 0 to line 20, character 3
    (d10 -> d) in file "match.mligo", line 18, characters 17-18
    (c9 -> c) in file "match.mligo", line 10, character 0 to line 15, character 3
    (b5 -> b) in file "match.mligo", line 5, character 0 to line 8, character 3
    (a1 -> a) in file "match.mligo", line 3, characters 0-9
    Type definitions:
    (mytype0 -> mytype) in file "match.mligo", line 1, characters 0-40 |} ] ;

  run_ligo_good [ "get-scope" ; gs "rec.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ k5 j4 i3 generated2 c1 a0 ] in file "rec.mligo", line 6, characters 4-11
    [ j4 i3 generated2 c1 a0 ] in file "rec.mligo", line 5, character 4 to line 6, character 11
    [ i3 generated2 c1 a0 ] in file "rec.mligo", line 4, characters 36-49
    [ generated2 c1 a0 ] in file "rec.mligo", line 4, characters 36-49
    [ c1 a0 ]
    [ ] in file "rec.mligo", line 1, characters 0-9

    Variable definitions:
    (k5 -> k) in file "rec.mligo", line 5, characters 12-21
    (j4 -> j)
    (i3 -> i)
    (generated2 -> generated) in file "rec.mligo", line 4, characters 36-49
    (c1 -> c) in file "rec.mligo", line 4, character 2 to line 8, character 9
    (b6 -> b) in file "rec.mligo", line 3, character 0 to line 8, character 9
    (a0 -> a) in file "rec.mligo", line 1, characters 0-9
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "shadowing.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ d4 c1 a0 ] in file "shadowing.mligo", line 10, characters 2-11
    [ e2 c1 a3 ] in file "shadowing.mligo", line 8, characters 4-13
    [ e2 c1 a0 ] in file "shadowing.mligo", line 7, character 4 to line 8, character 13
    [ c1 a0 ] in file "shadowing.mligo", line 6, character 4 to line 8, character 13
    [ a0 ] in file "shadowing.mligo", line 4, character 2 to line 10, character 11
    [ ] in file "shadowing.mligo", line 1, characters 0-9

    Variable definitions:
    (e2 -> e) in file "shadowing.mligo", line 6, characters 12-17
    (d4 -> d) in file "shadowing.mligo", line 6, character 4 to line 8, character 13
    (c1 -> c) in file "shadowing.mligo", line 4, characters 10-15
    (b5 -> b) in file "shadowing.mligo", line 3, character 0 to line 10, character 11
    (a3 -> a) in file "shadowing.mligo", line 7, characters 12-21
    (a0 -> a) in file "shadowing.mligo", line 1, characters 0-9
    Type definitions: |} ] ;