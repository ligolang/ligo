let%expect_test "error_recovery_fuzzing_cameligo" =
  printf "%s" @@ Error_recovery.test_cameligo ();
  [%expect
    {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 35, 133, 41, 244, 8, ./1annotated_michelson_record_tree.mligo
    PASS, 9, 109, 5, 155, 1, ./1condition.mligo
    PASS, 9, 103, 9, 59, 1, ./1incr_decr.mligo
    PASS, 16, 68, 26, 49, 3, ./1local_type_decl.mligo
    PASS, 9, 103, 9, 57, 1, ./1match_bis.mligo
    PASS, 127, 705, 57, 492, 17, ./1record.mligo
    PASS, 40, 261, 53, 359, 6, ./1ticket_builder.mligo
    PASS, 148, 1175, 135, 817, 23, ./1ticket_wallet.mligo
    PASS, 13, 118, 44, 91, 2, ./1voting.mligo
    PASS, 16, 76, 12, 41, 3, ./2address.mligo
    PASS, 17, 125, 9, 390, 4, ./2annotated_michelson_record_comb.mligo
    PASS, 149, 1083, 67, 769, 14, ./2let_destructuring.mligo
    PASS, 51, 640, 74, 485, 15, ./2let_multiple.mligo
    PASS, 18, 84, 14, 40, 2, ./2local_type_decl.mligo
    PASS, 15, 185, 17, 126, 4, ./2warning_unused.mligo
    PASS, 34, 366, 44, 199, 9, ./4assert.mligo
    PASS, 33, 126, 42, 79, 5, ./4check_signature.mligo
    PASS, 32, 126, 38, 143, 8, ./4fibo.mligo
    PASS, 25, 170, 18, 79, 3, ./4k.mligo
    PASS, 10, 128, 12, 16, 2, ./4michelson_or_tree.mligo
    PASS, 24, 158, 42, 102, 5, ./4super-counter.mligo
    PASS, 168, 1194, 202, 820, 40, ./4ticket_wallet.mligo |}]
