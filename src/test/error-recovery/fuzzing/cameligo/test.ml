let%expect_test "error_recovery_fuzzing_cameligo" =
  printf "%s" @@ Error_recovery.test_cameligo ();
  [%expect {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 30, 133, 41, 252, 8, ./1annotated_michelson_record_tree.mligo
    PASS, 10, 57, 1, 148, 1, ./1condition.mligo
    PASS, 9, 103, 9, 59, 1, ./1incr_decr.mligo
    PASS, 16, 68, 26, 49, 3, ./1local_type_decl.mligo
    PASS, 9, 103, 9, 57, 1, ./1match_bis.mligo
    PASS, 127, 683, 55, 492, 17, ./1record.mligo
    PASS, 41, 207, 49, 364, 6, ./1ticket_builder.mligo
    PASS, 150, 1143, 131, 814, 23, ./1ticket_wallet.mligo
    PASS, 13, 118, 44, 91, 2, ./1voting.mligo
    PASS, 16, 76, 12, 41, 3, ./2address.mligo
    PASS, 15, 125, 9, 16, 4, ./2annotated_michelson_record_comb.mligo
    PASS, 149, 1083, 67, 769, 14, ./2let_destructuring.mligo
    PASS, 55, 608, 58, 483, 15, ./2let_multiple.mligo
    PASS, 18, 84, 14, 40, 2, ./2local_type_decl.mligo
    PASS, 15, 179, 17, 126, 4, ./2warning_unused.mligo
    PASS, 34, 366, 44, 199, 9, ./4assert.mligo
    PASS, 34, 120, 38, 78, 5, ./4check_signature.mligo
    PASS, 32, 126, 38, 143, 8, ./4fibo.mligo
    PASS, 25, 170, 18, 79, 3, ./4k.mligo
    PASS, 10, 126, 10, 14, 2, ./4michelson_or_tree.mligo
    PASS, 23, 158, 42, 100, 5, ./4super-counter.mligo
    PASS, 169, 1116, 198, 815, 40, ./4ticket_wallet.mligo |}]
