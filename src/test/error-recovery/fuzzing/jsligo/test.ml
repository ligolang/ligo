let%expect_test "error_recovery_fuzzing_jsligo" =
  printf "%s" @@ Error_recovery.test_jsligo ();
  [%expect
    {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 100, 474, 214, 417, 36, ./16tuples_sequences_functions.jsligo
    PASS, 40, 225, 47, 860, 4, ./1high-order.jsligo
    PASS, 36, 172, 18, 139, 4, ./1never.jsligo
    PASS, 16, 89, 7, 38, 1, ./2amount.jsligo
    PASS, 30, 224, 32, 208, 11, ./2bytes_unpack.jsligo
    PASS, 10, 90, 10, 58, 1, ./2counter.jsligo
    PASS, 12, 28, 6, 41, 0, ./2if_if_return.jsligo
    PASS, 17, 136, 24, 85, 4, ./2key_hash.jsligo
    PASS, 11, 4, 4, 30, 1, ./2local_type_decl.jsligo
    PASS, 38, 227, 69, 214, 12, ./2match_bis.jsligo
    PASS, 15, 6, 6, 139, 4, ./2super-counter.jsligo
    PASS, 22, 121, 37, 185, 6, ./4balance_constant.jsligo
    PASS, 12, 37, 19, 86, 1, ./4check_signature.jsligo
    PASS, 10, 2, 2, 13, 0, ./4if_if_return.jsligo
    PASS, 13, 114, 12, 117, 2, ./4lambda2.jsligo
    PASS, 24, 159, 57, 134, 14, ./4match.jsligo
    PASS, 39, 168, 58, 130, 12, ./4never.jsligo
    PASS, 33, 219, 41, 231, 12, ./4recursion.jsligo
    PASS, 18, 168, 40, 154, 7, ./8assert.jsligo
    PASS, 10, 86, 8, 46, 1, ./8eq_bool.jsligo
    PASS, 17, 65, 5, 183, 0, ./8recursion.jsligo |}]
