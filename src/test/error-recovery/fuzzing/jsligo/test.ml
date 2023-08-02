let%expect_test "error_recovery_fuzzing_jsligo" =
  printf "%s" @@ Error_recovery.test_jsligo ();
  [%expect
    {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 119, 715, 335, 461, 51, ./16tuples_sequences_functions.jsligo
    PASS, 71, 742, 298, 675, 45, ./1high-order.jsligo
    PASS, 26, 182, 84, 139, 15, ./1never.jsligo
    PASS, 16, 80, 30, 53, 4, ./2amount.jsligo
    PASS, 20, 200, 42, 173, 10, ./2bytes_unpack.jsligo
    PASS, 11, 94, 24, 72, 4, ./2counter.jsligo
    PASS, 17, 162, 22, 39, 4, ./2if_if_return.jsligo
    PASS, 16, 160, 48, 102, 7, ./2key_hash.jsligo
    PASS, 12, 124, 6, 12, 2, ./2local_type_decl.jsligo
    PASS, 29, 194, 118, 156, 9, ./2match_bis.jsligo
    PASS, 18, 84, 12, 114, 4, ./2super-counter.jsligo
    PASS, 22, 186, 62, 175, 10, ./4balance_constant.jsligo
    PASS, 17, 123, 45, 82, 10, ./4check_signature.jsligo
    PASS, 6, 0, 0, 2, 0, ./4if_if_return.jsligo
    PASS, 13, 96, 50, 90, 10, ./4lambda2.jsligo
    PASS, 19, 186, 68, 128, 17, ./4match.jsligo
    PASS, 29, 176, 90, 139, 17, ./4never.jsligo
    PASS, 31, 249, 75, 198, 16, ./4recursion.jsligo
    PASS, 18, 80, 32, 140, 6, ./8assert.jsligo
    PASS, 16, 89, 23, 70, 4, ./8eq_bool.jsligo
    PASS, 20, 67, 41, 198, 5, ./8recursion.jsligo |}]
