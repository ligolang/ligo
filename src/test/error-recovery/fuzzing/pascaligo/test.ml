let%expect_test "error_recovery_fuzzing_pascaligo" =
  printf "%s" @@ Error_recovery.test_pascaligo ();
  [%expect
    {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 86, 578, 302, 1695, 23, ./1loop.ligo
    PASS, 24, 166, 14, 133, 2, ./1loop7.ligo
    PASS, 44, 262, 48, 274, 6, ./1multisig.ligo
    PASS, 4, 54, 30, 39, 5, ./1recursion.ligo
    PASS, 20, 132, 44, 868, 11, ./1set_arithmetic.ligo
    PASS, 6, 99, 51, 68, 11, ./2condition.ligo
    PASS, 11, 60, 56, 45, 2, ./2function-complex.ligo
    PASS, 34, 307, 51, 574, 11, ./2hashlock.ligo
    PASS, 0, 0, 0, 0, 0, ./2lambda2.ligo
    PASS, 43, 582, 174, 1094, 32, ./2long_assign.ligo
    PASS, 18, 140, 32, 315, 10, ./2long_remove.ligo
    PASS, 0, 0, 0, 0, 0, ./2loop13.ligo
    PASS, 2, 18, 14, 26, 3, ./2loop18.ligo
    PASS, 9, 94, 36, 67, 4, ./2michelson_pair_tree_intermediary.ligo
    PASS, 18, 157, 57, 134, 5, ./2replaceable_id.ligo
    PASS, 4, 55, 9, 25, 2, ./2transpiler_nested.ligo
    PASS, 9, 115, 13, 87, 2, ./4closure.ligo
    PASS, 32, 302, 118, 334, 17, ./4failwith.ligo
    PASS, 13, 37, 19, 90, 5, ./4super-counter.ligo
    PASS, 7, 74, 16, 57, 2, ./8check_signature.ligo
    PASS, 4, 22, 10, 54, 3, ./8function-complex.ligo
    PASS, 6, 22, 14, 75, 4, ./8loop9.ligo |}]
