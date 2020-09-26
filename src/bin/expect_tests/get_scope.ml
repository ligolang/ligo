open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/"^s

let%expect_test _ =
  run_ligo_good [ "get-scope" ; gs "lambda_letin.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f#5 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 6-7:[0m
    [ f#5 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 4-5:[0m
    [ f#5 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 9, characters 2-3:[0m
    [ k#4 j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 20-21:[0m
    [ k#4 j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 16-17:[0m
    [ k#4 j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 12-13:[0m
    [ k#4 j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 8-9:[0m
    [ k#4 j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 7, characters 4-5:[0m
    [ j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 24-25:[0m
    [ j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 20-21:[0m
    [ j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 16-17:[0m
    [ j#2 i#1 g#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 12-13:[0m
    [ j#2 i#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 20-21:[0m
    [ j#2 i#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 16-17:[0m
    [ j#2 i#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 12-13:[0m
    [ ] [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 0-9:[0m

    Variable definitions:
    (k#4 -> k) [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 6, characters 8-9:[0m
    (j#2 -> j) [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 46-55:[0m
    (i#1 -> i) [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 36-45:[0m
    (g#3 -> g) [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 5, characters 8-9:[0m
    (f#5 -> f) [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 4, characters 6-7:[0m
    (b#6 -> b) [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 3, characters 4-5:[0m
    (a#0 -> a) [1mFile "../../test/contracts/get_scope_tests/lambda_letin.mligo", line 1, characters 4-5:[0m
    Type definitions: |} ];

  run_ligo_good [ "get-scope" ; gs "letin.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 10-11:[0m
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 6-7:[0m
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 10, characters 2-3:[0m
    [ f#3 e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 16-17:[0m
    [ f#3 e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 12-13:[0m
    [ f#3 e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 8-9:[0m
    [ f#3 e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 8, characters 4-5:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 20-21:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 16-17:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 12-13:[0m
    [ c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 16-17:[0m
    [ c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 12-13:[0m
    [ a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 14-15:[0m
    [ a#0 ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 10-11:[0m
    [ ] [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 0-9:[0m

    Variable definitions:
    (f#3 -> f) [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 7, characters 8-9:[0m
    (e#2 -> e) [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 6, characters 8-9:[0m
    (d#4 -> d) [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 5, characters 6-7:[0m
    (c#1 -> c) [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 4, characters 6-7:[0m
    (b#5 -> b) [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 3, characters 4-5:[0m
    (a#0 -> a) [1mFile "../../test/contracts/get_scope_tests/letin.mligo", line 1, characters 4-5:[0m
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "lambda.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 6-7:[0m
    [ f#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 4-5:[0m
    [ f#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 5, characters 2-3:[0m
    [ j#2 i#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 62-63:[0m
    [ j#2 i#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 58-59:[0m
    [ ] [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 0-9:[0m

    Variable definitions:
    (j#2 -> j) [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 45-54:[0m
    (i#1 -> i) [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 35-44:[0m
    (f#3 -> f) [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 4, characters 6-7:[0m
    (b#4 -> b) [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 3, characters 4-5:[0m
    (a#0 -> a) [1mFile "../../test/contracts/get_scope_tests/lambda.mligo", line 1, characters 4-5:[0m
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "match.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ s#11 mytype#0 c#9 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 20-21:[0m
    [ s#11 mytype#0 c#9 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 16-17:[0m
    [ mytype#0 c#9 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 20, characters 12-13:[0m
    [ mytype#0 d#10 c#9 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 30-31:[0m
    [ mytype#0 d#10 c#9 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 28-29:[0m
    [ mytype#0 c#9 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 9-32:[0m
    [ tl#8 mytype#0 hd#7 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 14-15:[0m
    [ mytype#0 c#6 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 14, characters 4-5:[0m
    [ mytype#0 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 13, character 4 to line 14, character 5:[0m
    [ mytype#0 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 18-19:[0m
    [ mytype#0 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 15-16:[0m
    [ mytype#0 b#5 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 11, characters 11-12:[0m
    [ y#4 mytype#0 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 17-18:[0m
    [ y#4 mytype#0 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 13-14:[0m
    [ x#3 mytype#0 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 17-18:[0m
    [ x#3 mytype#0 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 13-14:[0m
    [ mytype#0 c#2 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 26-27:[0m
    [ mytype#0 a#1 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 9-27:[0m
    [ mytype#0 ] [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 0-9:[0m

    Variable definitions:
    (y#4 -> y) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 8, characters 4-9:[0m
    (x#3 -> x) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 7, characters 4-9:[0m
    (tl#8 -> tl) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6:[0m
    (s#11 -> s) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 19, characters 4-12:[0m
    (hd#7 -> hd) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 15, characters 4-6:[0m
    (d#12 -> d) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 17, characters 4-5:[0m
    (d#10 -> d) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 18, characters 13-14:[0m
    (c#9 -> c) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 10, characters 4-5:[0m
    (c#6 -> c) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 13, characters 8-9:[0m
    (c#2 -> c) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 6, characters 13-14:[0m
    (b#5 -> b) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 5, characters 4-5:[0m
    (a#1 -> a) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 3, characters 4-5:[0m
    Type definitions:
    (mytype#0 -> mytype) [1mFile "../../test/contracts/get_scope_tests/match.mligo", line 1, characters 0-40:[0m |} ] ;

  run_ligo_good [ "get-scope" ; gs "rec.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ c#5 b#6 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 5-6:[0m
    [ c#5 b#6 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 8-9:[0m
    [ c#5 b#6 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 9, characters 2-3:[0m
    [ c#5 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 8, character 2 to line 9, character 10:[0m
    [ k#4 j#3 i#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 7-8:[0m
    [ k#4 j#3 i#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 9-10:[0m
    [ k#4 j#3 i#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 6, characters 4-5:[0m
    [ j#3 i#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 20-21:[0m
    [ j#3 i#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 16-17:[0m
    [ j#3 i#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 12-13:[0m
    [ i#2 c#1 a#0 ]
    [ c#1 a#0 ]
    [ ] [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 0-9:[0m

    Variable definitions:
    (k#4 -> k) [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 5, characters 8-9:[0m
    (j#3 -> j) [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 39-40:[0m
    (i#2 -> i) [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 37-38:[0m
    (c#5 -> c) [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11:[0m
    (c#1 -> c) [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 4, characters 10-11:[0m
    (b#7 -> b) [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 3, characters 4-5:[0m
    (b#6 -> b) [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 8, characters 6-7:[0m
    (a#0 -> a) [1mFile "../../test/contracts/get_scope_tests/rec.mligo", line 1, characters 4-5:[0m
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "shadowing.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 10-11:[0m
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 6-7:[0m
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 10, characters 2-3:[0m
    [ e#2 c#1 a#3 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 12-13:[0m
    [ e#2 c#1 a#3 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 8-9:[0m
    [ e#2 c#1 a#3 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 8, characters 4-5:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 20-21:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 16-17:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 12-13:[0m
    [ c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 16-17:[0m
    [ c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 12-13:[0m
    [ a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 14-15:[0m
    [ a#0 ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 10-11:[0m
    [ ] [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 0-9:[0m

    Variable definitions:
    (e#2 -> e) [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 6, characters 8-9:[0m
    (d#4 -> d) [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 5, characters 6-7:[0m
    (c#1 -> c) [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 4, characters 6-7:[0m
    (b#5 -> b) [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 3, characters 4-5:[0m
    (a#3 -> a) [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 7, characters 8-9:[0m
    (a#0 -> a) [1mFile "../../test/contracts/get_scope_tests/shadowing.mligo", line 1, characters 4-5:[0m
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "records.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ myrec#0 g#5 b#4 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 40-41:[0m
    [ myrec#0 g#5 b#4 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 32-37:[0m
    [ myrec#0 g#5 b#4 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 28-29:[0m
    [ myrec#0 b#4 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 15-41:[0m
    [ myrec#0 b#4 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 3-4:[0m
    [ myrec#0 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 53-55:[0m
    [ myrec#0 j#3 i#2 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 44-45:[0m
    [ myrec#0 j#3 i#2 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 42-43:[0m
    [ myrec#0 j#3 i#2 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 40-41:[0m
    [ myrec#0 i#2 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 27-45:[0m
    [ myrec#0 a#1 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 14-45:[0m
    [ myrec#0 ] [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 0-9:[0m

    Variable definitions:
    (j#3 -> j) [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 31-32:[0m
    (i#2 -> i) [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 18-19:[0m
    (g#5 -> g) [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 16, characters 19-20:[0m
    (e#6 -> e) [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 15, characters 4-5:[0m
    (b#4 -> b) [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 6, characters 4-5:[0m
    (a#1 -> a) [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 3, characters 4-5:[0m
    Type definitions:
    (myrec#0 -> myrec) [1mFile "../../test/contracts/get_scope_tests/records.mligo", line 1, characters 0-36:[0m |} ] ;

  run_ligo_good [ "get-scope" ; gs "constant.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ e#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30:[0m
    [ e#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28:[0m
    [ e#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23:[0m
    [ e#3 a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21:[0m
    [ a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 5-32:[0m
    [ d#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44:[0m
    [ d#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40:[0m
    [ d#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36:[0m
    [ c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44:[0m
    [ ] [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 0-9:[0m

    Variable definitions:
    (e#3 -> e) [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10:[0m
    (d#2 -> d) [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27:[0m
    (c#1 -> c) [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 9-18:[0m
    (b#4 -> b) [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5:[0m
    (a#0 -> a) [1mFile "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5:[0m
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "application.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ f#2 c#4 ] [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 35-36:[0m
    [ f#2 ] [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 22-36:[0m
    [ f#2 b#3 ] [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 18-19:[0m
    [ f#2 b#3 ] [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 16-17:[0m
    [ f#2 ] [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 3-19:[0m
    [ j#1 i#0 ] [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 62-63:[0m
    [ j#1 i#0 ] [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 58-59:[0m

    Variable definitions:
    (j#1 -> j) [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 45-54:[0m
    (i#0 -> i) [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 35-44:[0m
    (f#2 -> f) [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 2, characters 6-7:[0m
    (c#4 -> c) [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 26-27:[0m
    (b#3 -> b) [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 3, characters 7-8:[0m
    (a#5 -> a) [1mFile "../../test/contracts/get_scope_tests/application.mligo", line 1, characters 4-5:[0m
    Type definitions: |} ] ;

  run_ligo_good [ "get-scope" ; gs "include.mligo" ; "--format=dev" ] ;
  [%expect {|
    Scopes:
    [ x#6 b#5 a#0 ] [1mFile "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 12-13:[0m
    [ x#6 b#5 a#0 ] [1mFile "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 8-9:[0m
    [ b#5 a#0 ] [1mFile "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 0-9:[0m
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 10, characters 10-11:[0m
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 10, characters 6-7:[0m
    [ d#4 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 10, characters 2-3:[0m
    [ f#3 e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 8, characters 16-17:[0m
    [ f#3 e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 8, characters 12-13:[0m
    [ f#3 e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 8, characters 8-9:[0m
    [ f#3 e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 8, characters 4-5:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 7, characters 20-21:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 7, characters 16-17:[0m
    [ e#2 c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 7, characters 12-13:[0m
    [ c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 6, characters 16-17:[0m
    [ c#1 a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 6, characters 12-13:[0m
    [ a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 4, characters 14-15:[0m
    [ a#0 ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 4, characters 10-11:[0m
    [ ] [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 1, characters 0-9:[0m

    Variable definitions:
    (y#7 -> y) [1mFile "../../test/contracts/get_scope_tests/include.mligo", line 5, characters 4-5:[0m
    (x#6 -> x) [1mFile "../../test/contracts/get_scope_tests/include.mligo", line 3, characters 4-5:[0m
    (f#3 -> f) [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 7, characters 8-9:[0m
    (e#2 -> e) [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 6, characters 8-9:[0m
    (d#4 -> d) [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 5, characters 6-7:[0m
    (c#1 -> c) [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 4, characters 6-7:[0m
    (b#5 -> b) [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 3, characters 4-5:[0m
    (a#0 -> a) [1mFile "../../test/contracts/get_scope_tests/./letin.mligo", line 1, characters 4-5:[0m
    Type definitions: |} ] ;