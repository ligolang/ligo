let%expect_test "error_recovery_simple_jsligo" =
  printf "%s" @@ Error_recovery.test_jsligo ();
  [%expect {|
    STATUS, LOC, CST, SYMBOLS, TOKENS, ERRORS, FILE
    PASS, 8, 224, 50, 191, 11, ./extra_arrow_in_lambda.jsligo
    FAIL : can't recover test file./extra_colon_in_return_type.jsligo
    PASS, 0, 0, 0, 0, 0, ./extra_eq_in_func_decl.jsligo
    PASS, 2, 15, 3, 8, 0, ./extra_gt_zwsp.jsligo
    PASS, 3, 37, 17, 270, 1, ./lambda_with_missing_arguments.jsligo
    PASS, 35, 357, 101, 285, 15, ./missing_argument_bracketR.jsligo
    PASS, 5, 33, 13, 254, 2, ./missing_arrow_in_lambda_expr.jsligo
    PASS, 6, 101, 41, 163, 6, ./missing_arrow_in_lambda_in_match.jsligo
    PASS, 2, 9, 5, 14, 0, ./missing_colon_in_record_expr.jsligo
    PASS, 4, 81, 25, 66, 5, ./missing_comma_in_arguments.jsligo
    PASS, 3, 109, 21, 134, 3, ./missing_comma_in_list_pat.jsligo
    PASS, 1, 4, 4, 326, 0, ./missing_comma_in_record_decl.jsligo
    PASS, 4, 101, 15, 81, 4, ./missing_comma_in_tuple_pat.jsligo
    PASS, 19, 344, 38, 198, 8, ./missing_curly_bracketR_in_the_nested_namespace.jsligo
    PASS, 15, 284, 58, 224, 9, ./missing_curly_bracket_in_record_decl.jsligo
    PASS, 3, 120, 32, 128, 5, ./missing_dots_in_list_pat.jsligo
    PASS, 3, 33, 21, 259, 2, ./missing_dots_in_record_update.jsligo
    PASS, 0, 0, 0, 0, 0, ./missing_eq_in_type_decl.jsligo
    PASS, 9, 229, 37, 195, 8, ./missing_expr_parenthesesL.jsligo
    PASS, 9, 218, 38, 197, 8, ./missing_expr_parenthesesR.jsligo
    PASS, 2, 2, 2, 8, 0, ./missing_ident_in_type_decl.jsligo
    PASS, 2, 5, 5, 2, 0, ./missing_int.jsligo
    PASS, 5, 114, 12, 199, 3, ./missing_name_of_argument.jsligo
    PASS, 16, 127, 95, 86, 11, ./missing_namespace_kw_in_namespace_decl.jsligo
    PASS, 24, 287, 39, 240, 9, ./missing_open_curly_bracket_in_namespace_decl.jsligo
    PASS, 2, 8, 4, 25, 0, ./missing_par_in_call.jsligo
    PASS, 17, 280, 24, 103, 4, ./missing_par_in_if_condition.jsligo
    PASS, 3, 171, 7, 12, 1, ./missing_semicolon_before_return_on_same_line.jsligo
    FAIL : can parse test file (but shouldn't)./missing_semicolon_in_top_level.jsligo
    FAIL : can parse test file (but shouldn't)./missing_type_annotation_in_lambda_in_match.jsligo
    PASS, 3, 7, 7, 110, 0, ./switch_with_empty_body.jsligo
    PASS, 2, 5, 5, 2, 0, ./switch_with_missing_case_value.jsligo
    PASS, 0, 0, 0, 0, 0, ./switch_with_missing_colon.jsligo
    PASS, 0, 0, 0, 0, 0, ./switch_with_missing_semicolon.jsligo
    PASS, 2, 22, 8, 114, 1, ./switch_with_not_last_default.jsligo
    PASS, 0, 0, 0, 0, 0, ./triple_eq_in_if_condition.jsligo
    PASS, 3, 8, 8, 479, 0, ./unfinished_code00.jsligo
    PASS, 4, 2, 2, 386, 0, ./unfinished_code01.jsligo
    PASS, 3, 10, 8, 703, 0, ./unfinished_code02.jsligo
    PASS, 73, 854, 92, 816, 19, ./unfinished_code03.jsligo
    PASS, 5, 29, 27, 283, 0, ./unfinished_code04.jsligo
    PASS, 4, 8, 8, 243, 0, ./unfinished_code05.jsligo
    PASS, 6, 9, 7, 465, 0, ./unfinished_code06.jsligo
    PASS, 10, 2, 2, 58, 0, ./unfinished_code07.jsligo
    PASS, 6, 7, 5, 688, 0, ./unfinished_code08.jsligo
    PASS, 4, 2, 2, 122, 0, ./unfinished_code09.jsligo
    PASS, 4, 10, 8, 323, 0, ./unfinished_code10.jsligo
    PASS, 8, 2, 2, 192, 0, ./unfinished_code11.jsligo
    PASS, 5, 8, 6, 240, 0, ./unfinished_code12.jsligo
    PASS, 6, 67, 13, 12, 0, ./unfinished_code13.jsligo
    FAIL : can't recover test file./unreadable_symbol.jsligo
    PASS, 6, 45, 5, 131, 1, ./using_then_in_if_expr.jsligo
    PASS, 7, 146, 14, 213, 0, ./var_kw_instead_of_let_kw.jsligo |}]
