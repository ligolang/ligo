let%expect_test "error_recovery_simple_jsligo" =
  printf "%s" @@ Error_recovery.test_jsligo ();
  [%expect
    {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 0, 0, 0, 0, 0, ./extra_arrow_in_lambda.jsligo
    FAIL : can't recover test file./extra_colon_in_return_type.jsligo
    PASS, 4, 95, 21, 195, 3, ./extra_eq_in_func_decl.jsligo
    FAIL : can parse test file (but shouldn't)./extra_gt_zwsp.jsligo
    PASS, 3, 18, 8, 268, 0, ./lambda_with_missing_arguments.jsligo
    PASS, 6, 19, 13, 280, 2, ./missing_argument_bracketR.jsligo
    PASS, 0, 0, 0, 0, 0, ./missing_arrow_in_lambda_expr.jsligo
    PASS, 0, 0, 0, 0, 0, ./missing_colon_in_match.jsligo
    PASS, 2, 7, 3, 48, 0, ./missing_colon_in_record_expr.jsligo
    PASS, 4, 96, 26, 67, 5, ./missing_comma_in_arguments.jsligo
    PASS, 4, 26, 12, 22, 2, ./missing_comma_in_list_pat.jsligo
    PASS, 4, 32, 24, 347, 1, ./missing_comma_in_record_decl.jsligo
    PASS, 5, 110, 48, 90, 9, ./missing_comma_in_tuple_pat.jsligo
    PASS, 13, 229, 1, 142, 0, ./missing_curly_bracketR_in_the_nested_namespace.jsligo
    PASS, 2, 0, 0, 1, 0, ./missing_curly_bracket_in_record_decl.jsligo
    PASS, 2, 5, 1, 21, 0, ./missing_dots_in_list_pat.jsligo
    PASS, 2, 20, 6, 15, 0, ./missing_dots_in_record_update.jsligo
    PASS, 0, 0, 0, 0, 0, ./missing_eq_in_type_decl.jsligo
    PASS, 2, 39, 1, 28, 0, ./missing_expr_parenthesesL.jsligo
    PASS, 2, 22, 2, 6, 0, ./missing_expr_parenthesesR.jsligo
    PASS, 2, 2, 2, 8, 0, ./missing_ident_in_type_decl.jsligo
    PASS, 2, 5, 5, 12, 0, ./missing_int.jsligo
    PASS, 4, 95, 21, 195, 2, ./missing_name_of_argument.jsligo
    PASS, 16, 140, 12, 266, 2, ./missing_namespace_kw_in_namespace_decl.jsligo
    PASS, 2, 2, 2, 8, 0, ./missing_namespace_name.jsligo
    PASS, 16, 133, 1, 260, 1, ./missing_open_curly_bracket_in_namespace_decl.jsligo
    PASS, 2, 9, 5, 23, 0, ./missing_par_in_call.jsligo
    PASS, 0, 0, 0, 0, 1, ./missing_par_in_if_condition.jsligo
    FAIL : can parse test file (but shouldn't)./missing_semicolon_before_return_on_same_line.jsligo
    FAIL : can parse test file (but shouldn't)./missing_semicolon_in_top_level.jsligo
    PASS, 2, 2, 2, 6, 0, ./missing_string.jsligo
    FAIL : can parse test file (but shouldn't)./missing_type_annotation_in_lambda_in_match.jsligo
    PASS, 1, 9, 9, 113, 0, ./switch_with_empty_body.jsligo
    PASS, 2, 5, 5, 4, 0, ./switch_with_missing_case_value.jsligo
    PASS, 0, 0, 0, 0, 0, ./switch_with_missing_colon.jsligo
    FAIL : can parse test file (but shouldn't)./switch_with_missing_semicolon.jsligo
    PASS, 11, 21, 5, 114, 2, ./switch_with_not_last_default.jsligo
    PASS, 0, 0, 0, 0, 0, ./triple_eq_in_if_condition.jsligo
    PASS, 2, 10, 10, 467, 0, ./unfinished_code00.jsligo
    PASS, 5, 2, 2, 390, 0, ./unfinished_code01.jsligo
    PASS, 3, 279, 17, 753, 0, ./unfinished_code02.jsligo
    PASS, 3, 8, 8, 906, 0, ./unfinished_code03.jsligo
    PASS, 4, 31, 29, 25, 0, ./unfinished_code04.jsligo
    PASS, 3, 9, 9, 456, 0, ./unfinished_code05.jsligo
    PASS, 5, 8, 6, 473, 0, ./unfinished_code06.jsligo
    FAIL : can parse test file (but shouldn't)./unfinished_code07.jsligo
    PASS, 2, 7, 5, 20, 0, ./unfinished_code08.jsligo
    PASS, 3, 20, 6, 132, 0, ./unfinished_code09.jsligo
    PASS, 4, 11, 9, 301, 0, ./unfinished_code10.jsligo
    PASS, 5, 3, 3, 192, 0, ./unfinished_code11.jsligo
    PASS, 3, 7, 5, 257, 0, ./unfinished_code12.jsligo
    PASS, 3, 7, 5, 257, 0, ./unfinished_code13.jsligo
    FAIL : can't recover test file./unreadable_symbol.jsligo
    PASS, 6, 65, 5, 137, 1, ./using_then_in_if_expr.jsligo
    PASS, 15, 112, 12, 217, 0, ./var_kw_instead_of_let_kw.jsligo |}]
