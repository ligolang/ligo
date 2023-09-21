let%expect_test "error_recovery_simple_pascaligo" =
  printf "%s" @@ Error_recovery.test_pascaligo ();
  [%expect
    {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 0, 0, 0, 0, 0, ./extra_arrow_in_case_expr.ligo
    PASS, 0, 0, 0, 0, 0, ./extra_colon_in_const_decl.ligo
    FAIL : can't recover test file./extra_colon_in_return_type.ligo
    PASS, 0, 0, 0, 0, 0, ./extra_eq_in_func_decl.ligo
    PASS, 0, 0, 0, 0, 0, ./extra_then_kw.ligo
    PASS, 0, 0, 0, 0, 0, ./extra_vertical_bar.ligo
    PASS, 3, 7, 7, 252, 0, ./lambda_with_missing_arguments.ligo
    PASS, 7, 70, 38, 132, 7, ./let_kw_instead_of_function_kw.ligo
    PASS, 5, 31, 21, 106, 4, ./match_kw_instead_of_case_kw.ligo
    PASS, 0, 0, 0, 0, 0, ./missing_argument_bracketR.ligo
    PASS, 0, 0, 0, 0, 0, ./missing_arrow_in_match_expr.ligo
    PASS, 0, 0, 0, 0, 0, ./missing_colon_in_var_decl.ligo
    PASS, 2, 9, 5, 6, 1, ./missing_cons_op_in_list_pat.ligo
    PASS, 0, 0, 0, 0, 0, ./missing_end_kw_in_the_nested_module.ligo
    PASS, 2, 10, 6, 12, 0, ./missing_eq_in_record_expr.ligo
    PASS, 2, 39, 5, 18, 0, ./missing_expr_parenthesesL.ligo
    PASS, 2, 22, 2, 6, 0, ./missing_expr_parenthesesR.ligo
    PASS, 2, 2, 2, 6, 0, ./missing_ident_in_type_decl.ligo
    PASS, 2, 5, 5, 14, 0, ./missing_int.ligo
    PASS, 0, 0, 0, 0, 0, ./missing_is_kw_in_lambda_expr.ligo
    PASS, 0, 0, 0, 0, 0, ./missing_is_kw_in_type_decl.ligo
    PASS, 12, 100, 4, 199, 1, ./missing_module_kw_in_module_decl.ligo
    PASS, 2, 2, 2, 24, 0, ./missing_name_of_argument.ligo
    PASS, 2, 10, 6, 7, 0, ./missing_of_kw.ligo
    PASS, 0, 0, 0, 0, 0, ./missing_semicolon_between_stmts.ligo
    PASS, 5, 44, 14, 70, 2, ./missing_semicolon_in_arguments.ligo
    PASS, 2, 4, 4, 24, 0, ./missing_semicolon_in_record_decl.ligo
    PASS, 2, 13, 5, 15, 0, ./missing_then_kw.ligo
    PASS, 2, 22, 10, 8, 1, ./missing_vertical_bar.ligo
    PASS, 0, 0, 0, 0, 0, ./missing_with_kw_in_block_expr.ligo
    PASS, 5, 32, 24, 107, 4, ./typo_in_case_kw.ligo
    PASS, 5, 54, 12, 257, 1, ./typo_in_function_kw.ligo
    PASS, 2, 23, 9, 12, 0, ./typo_in_with_kw_in_record_update.ligo
    PASS, 2, 6, 6, 399, 0, ./unfinished_code00.ligo
    PASS, 2, 2, 2, 2, 0, ./unfinished_code01.ligo
    PASS, 2, 11, 9, 7, 0, ./unfinished_code02.ligo
    PASS, 2, 8, 4, 6, 0, ./unfinished_code03.ligo
    PASS, 3, 28, 26, 223, 0, ./unfinished_code04.ligo
    PASS, 3, 8, 8, 183, 0, ./unfinished_code05.ligo
    PASS, 2, 6, 6, 5, 0, ./unfinished_code06.ligo
    PASS, 2, 2, 2, 28, 0, ./unfinished_code07.ligo
    PASS, 2, 2, 2, 10, 0, ./unfinished_code08.ligo
    PASS, 2, 2, 2, 2, 0, ./unfinished_code09.ligo
    PASS, 3, 29, 27, 304, 0, ./unfinished_code10.ligo
    PASS, 2, 4, 4, 4, 0, ./unfinished_code11.ligo
    PASS, 2, 5, 5, 6, 0, ./unfinished_code12.ligo
    PASS, 31, 314, 16, 237, 2, ./unfinished_code13.ligo
    FAIL : can't recover test file./unreadable_symbol.ligo |}]
