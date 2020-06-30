open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/"^s

let%expect_test _ =
  run_ligo_good [ "get-scope" ; gs "lambda_letin.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f#5 a#0 ] in file "lambda_letin.mligo", line 9, characters 2-7
    [ k#4 j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 7, characters 4-21
    [ j#2 i#1 g#3 a#0 ] in file "lambda_letin.mligo", line 6, character 4 to line 7, character 21
    [ j#2 i#1 a#0 ] in file "lambda_letin.mligo", line 5, character 4 to line 7, character 21
    [ ] in file "lambda_letin.mligo", line 1, characters 0-9

    Variable definitions:
    (k#4 -> k) in file "lambda_letin.mligo", line 6, characters 12-25
    (j#2 -> j) in file "lambda_letin.mligo", line 5, character 4 to line 7, character 21
    (i#1 -> i) in file "lambda_letin.mligo", line 4, character 32 to line 7, character 21
    (g#3 -> g) in file "lambda_letin.mligo", line 5, characters 12-21
    (f#5 -> f) in file "lambda_letin.mligo", line 4, characters 8-9
    (b#6 -> b) in file "lambda_letin.mligo", line 3, character 0 to line 9, character 7
    (a#0 -> a) in file "lambda_letin.mligo", line 1, characters 0-9
    Type definitions: |} ];

  run_ligo_good [ "get-scope" ; gs "letin.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ d#4 c#1 a#0 ] in file "letin.mligo", line 10, characters 2-11
    [ f#3 e#2 c#1 a#0 ] in file "letin.mligo", line 8, characters 4-17
    [ e#2 c#1 a#0 ] in file "letin.mligo", line 7, character 4 to line 8, character 17
    [ c#1 a#0 ] in file "letin.mligo", line 6, character 4 to line 8, character 17
    [ a#0 ] in file "letin.mligo", line 4, character 2 to line 10, character 11
    [ ] in file "letin.mligo", line 1, characters 0-9

    Variable definitions:
    (f#3 -> f) in file "letin.mligo", line 7, characters 12-21
    (e#2 -> e) in file "letin.mligo", line 6, characters 12-17
    (d#4 -> d) in file "letin.mligo", line 6, character 4 to line 8, character 17
    (c#1 -> c) in file "letin.mligo", line 4, characters 10-15
    (b#5 -> b) in file "letin.mligo", line 3, character 0 to line 10, character 11
    (a#0 -> a) in file "letin.mligo", line 1, characters 0-9
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "lambda.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f#3 a#0 ] in file "lambda.mligo", line 5, characters 2-7
    [ j#2 i#1 a#0 ] in file "lambda.mligo", line 4, characters 58-63
    [ ] in file "lambda.mligo", line 1, characters 0-9

    Variable definitions:
    (j#2 -> j) in file "lambda.mligo", line 4, characters 58-63
    (i#1 -> i) in file "lambda.mligo", line 4, characters 31-63
    (f#3 -> f) in file "lambda.mligo", line 4, characters 8-9
    (b#4 -> b) in file "lambda.mligo", line 3, character 0 to line 5, character 7
    (a#0 -> a) in file "lambda.mligo", line 1, characters 0-9
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "match.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ s#11 mytype#0 c#9 b#5 a#1 ] in file "match.mligo", line 19, characters 16-21
    [ mytype#0 c#9 b#5 a#1 ] in file "match.mligo", line 20, characters 12-13
    [ mytype#0 d#10 c#9 b#5 a#1 ] in file "match.mligo", line 18, characters 22-32
    [ mytype#0 c#9 b#5 a#1 ] in file "match.mligo", line 18, characters 9-32
    [ tl#8 mytype#0 hd#7 b#5 a#1 ] in file "match.mligo", line 15, characters 14-15
    [ mytype#0 c#6 b#5 a#1 ] in file "match.mligo", line 14, characters 4-5
    [ mytype#0 b#5 a#1 ] in file "match.mligo", line 13, character 4 to line 14, character 5
    [ mytype#0 b#5 a#1 ] in file "match.mligo", line 11, characters 9-21
    [ y#4 mytype#0 a#1 ] in file "match.mligo", line 8, characters 13-18
    [ x#3 mytype#0 a#1 ] in file "match.mligo", line 7, characters 13-18
    [ mytype#0 c#2 a#1 ] in file "match.mligo", line 6, characters 22-27
    [ mytype#0 a#1 ] in file "match.mligo", line 6, characters 9-27
    [ mytype#0 ] in file "match.mligo", line 3, characters 0-9

    Variable definitions:
    (s#11 -> s) in file "match.mligo", line 19, characters 16-21
    (d#12 -> d) in file "match.mligo", line 17, character 0 to line 20, character 3
    (d#10 -> d) in file "match.mligo", line 18, characters 17-18
    (c#9 -> c) in file "match.mligo", line 10, character 0 to line 15, character 3
    (b#5 -> b) in file "match.mligo", line 5, character 0 to line 8, character 3
    (a#1 -> a) in file "match.mligo", line 3, characters 0-9
    Type definitions:
    (mytype#0 -> mytype) in file "match.mligo", line 1, characters 0-40 |} ] ;

  run_ligo_good [ "get-scope" ; gs "rec.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ c#5 b#6 a#0 ] in file "rec.mligo", line 9, characters 2-10
    [ c#5 a#0 ] in file "rec.mligo", line 8, character 2 to line 9, character 10
    [ k#4 j#3 i#2 c#1 a#0 ] in file "rec.mligo", line 6, characters 4-11
    [ j#3 i#2 c#1 a#0 ] in file "rec.mligo", line 5, character 4 to line 6, character 11
    [ i#2 c#1 a#0 ] in file "rec.mligo", line 4, characters 36-49
    [ c#1 a#0 ] in file "rec.mligo", line 4, characters 36-49
    [ c#1 a#0 ]
    [ ] in file "rec.mligo", line 1, characters 0-9

    Variable definitions:
    (k#4 -> k) in file "rec.mligo", line 5, characters 12-21
    (j#3 -> j)
    (i#2 -> i)
    (c#5 -> c) in file "rec.mligo", line 4, characters 12-13
    (c#1 -> c) in file "rec.mligo", line 4, characters 12-13
    (b#7 -> b) in file "rec.mligo", line 3, character 0 to line 9, character 10
    (b#6 -> b) in file "rec.mligo", line 8, characters 10-11
    (a#0 -> a) in file "rec.mligo", line 1, characters 0-9
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "shadowing.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ d#4 c#1 a#0 ] in file "shadowing.mligo", line 10, characters 2-11
    [ e#2 c#1 a#3 ] in file "shadowing.mligo", line 8, characters 4-13
    [ e#2 c#1 a#0 ] in file "shadowing.mligo", line 7, character 4 to line 8, character 13
    [ c#1 a#0 ] in file "shadowing.mligo", line 6, character 4 to line 8, character 13
    [ a#0 ] in file "shadowing.mligo", line 4, character 2 to line 10, character 11
    [ ] in file "shadowing.mligo", line 1, characters 0-9

    Variable definitions:
    (e#2 -> e) in file "shadowing.mligo", line 6, characters 12-17
    (d#4 -> d) in file "shadowing.mligo", line 6, character 4 to line 8, character 13
    (c#1 -> c) in file "shadowing.mligo", line 4, characters 10-15
    (b#5 -> b) in file "shadowing.mligo", line 3, character 0 to line 10, character 11
    (a#3 -> a) in file "shadowing.mligo", line 7, characters 12-21
    (a#0 -> a) in file "shadowing.mligo", line 1, characters 0-9
    Type definitions: |} ] ;