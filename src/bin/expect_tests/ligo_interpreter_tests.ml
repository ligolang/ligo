open Cli_expect

let test basename = "./" ^ basename
let pwd = Sys_unix.getcwd ()
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "tickets.mligo" ];
  [%expect {|
    Everything at the top-level was executed. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

(* tests for timelock primitives *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_timelock.mligo" ];
  [%expect
    {|
    (0x4141 , 0x4141)
    (0xff , 0xff)
    (0x , 0x)
    Everything at the top-level was executed.
    - test exited with value [() ; () ; () ; ()]. |}]

(* tests replacing Hashlock tests *)

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_hashlock.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_commit exited with value ().
    - test_reveal_no_commit exited with value ().
    - test_reveal_young_commit exited with value ().
    - test_reveal_breaks_commit exited with value ().
    - test_reveal_wrong_commit exited with value ().
    - test_reveal_no_reuse exited with value ().
    - test_reveal exited with value (). |}]

(* test comparison on sum/record types *)

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_compare.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_cmp exited with value ().
    - test_cmp_list exited with value ().
    - test_cmp_record exited with value (). |}]

(* test loops on maps *)

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_loop_map.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - testmap exited with value [0 -> 1 ; 1 -> 2 ; 2 -> 4].
    - test_entries exited with value [(2 , 4) ; (1 , 2) ; (0 , 1)].
    - test_unzipped_entries exited with value ([2 ; 1 ; 0] , [4 ; 2 ; 1]). |}]

(* events payload being records and not decompiled to pairs in the interpreter *)

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_events_pair_vs_record.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_foo exited with value 3n. |}]

(* decompilation of timestamp *)

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_timestamp_contract.mligo" ];
  [%expect
    {|
    Success (1279n)
    Everything at the top-level was executed.
    - test_timestamp exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpret_test.mligo" ];
  [%expect
    {|
  Everything at the top-level was executed.
  - test_lambda_call exited with value ().
  - test_higher_order1 exited with value ().
  - test_higher_order2 exited with value ().
  - test_higher_order3 exited with value ().
  - test_higher_order4 exited with value ().
  - test_concats exited with value ().
  - test_record_concat exited with value ().
  - test_record_patch exited with value ().
  - test_record_lambda exited with value ().
  - test_variant_match exited with value ().
  - test_bool_match exited with value ().
  - test_list_match exited with value ().
  - test_tuple_proj exited with value ().
  - test_list_const exited with value ().
  - test_options_match_some exited with value ().
  - test_options_match_none exited with value ().
  - test_is_nat_yes exited with value ().
  - test_is_nat_no exited with value ().
  - test_abs_int exited with value ().
  - test_nat_int exited with value ().
  - test_map_list exited with value ().
  - test_fold_list exited with value ().
  - test_comparison_int exited with value ().
  - test_comparison_string exited with value ().
  - test_divs_int exited with value ().
  - test_divs_nat exited with value ().
  - test_var_neg exited with value ().
  - test_sizes exited with value ().
  - test_modi exited with value ().
  - test_assertion_pass exited with value ().
  - test_map_finds exited with value ().
  - test_map_fold exited with value ().
  - test_map_map exited with value ().
  - test_map_mem exited with value ().
  - test_map_remove exited with value ().
  - test_map_update exited with value ().
  - test_set_add exited with value ().
  - test_set_mem exited with value ().
  - test_set_remove exited with value ().
  - test_recursion_let_rec_in exited with value ().
  - test_top_level_recursion exited with value ().
  - test_bitwise_ops exited with value ().
  - test_bitwise_module exited with value ().
  - test_bytes_bitwise_ops exited with value ().
  - test_bytes_bitwise_module exited with value ().
  - test_list_concat exited with value ().
  - test_list_head exited with value ().
  - test_list_tail exited with value ().
  - test_list_reverse exited with value ().
  - test_set_fold_desc exited with value ().
  - test_set_update exited with value ().
  - test_map_get_and_update exited with value ().
  - test_big_map_get_and_update exited with value ().
  - test_add_mutez exited with value ().
  - test_sub_mutez exited with value ().
  - test_div_mutez exited with value ().
  - test_sub_timestamp exited with value ().
  - test_list_fold_left_sum exited with value ().
  - test_bytes_sub exited with value ().
  - test_with_error exited with value ().
  - test_some exited with value ().
  - test_some_with_error exited with value ().
  - test_none exited with value ().
  - test_none_with_error exited with value ().
  - test_unopt exited with value ().
  - test_unopt_with_error exited with value ().
  - test_sha256 exited with value ().
  - test_sha512 exited with value ().
  - test_blake2b exited with value ().
  - test_keccak exited with value ().
  - test_sha3 exited with value ().
  - test_key_hash exited with value ().
  - test_check exited with value ().
  - test_int_bls exited with value ().
  - test_not exited with value ().
  - test_chain_id exited with value ().
  - test_concats exited with value ().
  - test_bytes_nat_int_conversion exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  (* This tests a possible regression on the way modules are evaluated. It is possible that the number of element in the environment explodes. *)
  run_ligo_good [ "run"; "test"; test "imported_modules/test.mligo"; "--format"; "dev" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test1 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "views_test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpret_test_log.mligo" ];
  [%expect
    {|
    {a = 1 ; b = 2n ; c = "aaa"}
    One (())
    Everything at the top-level was executed. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fail.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value "my contract always fail". |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fail_from_file.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value "my contract always fail". |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "compile_expr.mligo" ];
  [%expect
    {|
  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "compile_expr_from_file.mligo" ];
  [%expect
    {|
  Everything at the top-level was executed.
  - test1 exited with value ().
  - test2 exited with value ().
  - test3 exited with value ().
  - test4 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_example.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value 111.
    - test2 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_example.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value 111.
    - test2 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "catch_balance_too_low.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_subst_with_storage.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value 0. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_subst_with_storage_from_file.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "nesting_modules.mligo" ];
  [%expect
    {|
    111
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "map_map.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value ["one" -> "foo" ; "two" -> "foo"]. |}]

(* DEPRECATED
let%expect_test _ =
run_ligo_good ["run";"test" ; test "bootstrapped_contracts.mligo" ] ;
  [%expect {|
  "Initial states:"
  (Pair "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy" 12)
  (Pair "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG" 9)
  "Final states:"
  (Pair "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy" 3)
  (Pair "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG" 0)
  Everything at the top-level was executed.
  - test_transfer exited with value ().
  |}]
*)

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "override_function.mligo" ];
  [%expect
    {|
    4
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_fresh.mligo" ];
  [%expect {|
    Everything at the top-level was executed. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_rec_contract.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_importer.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap.mligo" ];
  [%expect
    {|
    [32 -> 42n]
    None (())
    [32 -> 42n]
    [3 -> 42n ; 21 -> 42n ; 32 -> 42n]
    None (())
    Some (42n)
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap_compare.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_bigmap_set.mligo" ];
  [%expect
    {|
    9n
    0n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_module.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value 1. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "interpreter_nested_comparison_test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value ().
    - test_equal exited with value ().
    - test_not_equal exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_no_mutation.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value ().
    - test_mutation exited with value ().
    - test_mutation_all exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_mutate_from_file.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value [(() , Mutation at: File "adder.mligo", line 2, characters 58-63:
      1 | [@entry]
      2 | let main (p : int) (k : int) : operation list * int = [], p + k
                                                                    ^^^^^

    Replacing by: p ^ k.
    )]. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_mutate_module.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value [(() , Mutation at: File "contract_under_test/module_adder.mligo", line 1, characters 66-71:
      1 | [@entry] let add (p : int) (k : int) : operation list * int = [], p + k
                                                                            ^^^^^

    Replacing by: p ^ k.
    )]. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_mutate_module.jsligo" ];
  [%expect
    {|
    [(() , Mutation at: File "contract_under_test/module_adder.mligo", line 1, characters 66-71:
      1 | [@entry] let add (p : int) (k : int) : operation list * int = [], p + k
                                                                            ^^^^^

    Replacing by: p ^ k.
    )]
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "iteration.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_set exited with value 3.
    - test_list exited with value 3. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "func_michelson.mligo" ];
  [%expect
    {|
    42
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "func_michelson_loop.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_many_imports.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_1.jsligo"; "--no-warn" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test1 exited with value ().
    - test2 exited with value ().
    - test3 exited with value ().
    - test4 exited with value ().
    - test5 exited with value ().
    - test6 exited with value ().
    - test7 exited with value ().
    - test8 exited with value ().
    - test9 exited with value ().
    - test10 exited with value ().
    - test11 exited with value ().
    - test12 exited with value ().
    - test13 exited with value ().
    - test14 exited with value ().
    - test15 exited with value ().
    - test16 exited with value ().
    - test17 exited with value ().
    - test18 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_2.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test1 exited with value ().
      - test2 exited with value ().
      - test3 exited with value ().
      - test4 exited with value ().
      - test5 exited with value ().
      - test6 exited with value ().
      - test7 exited with value ().
      - test8 exited with value ().
      - test9 exited with value ().
      - test10 exited with value ().
      - test11 exited with value ().
      - test12 exited with value ().
      - test13 exited with value ().
      - test14 exited with value ().
      - test15 exited with value ().
      - test16 exited with value ().
      - test17 exited with value ().
      - test18 exited with value ().
      - test19 exited with value ().
      - test20 exited with value ().
      - test21 exited with value ().
      - test22 exited with value ().
      - test23 exited with value ().
      - test24 exited with value ().
      - test25 exited with value ().
      - test26 exited with value ().
      - test27 exited with value ().
      - test28 exited with value ().
      - test29 exited with value ().
      - test30 exited with value ().
      - test31 exited with value ().
      - test32 exited with value ().
      - test33 exited with value ().
      - test34 exited with value ().
      - test35 exited with value ().
      - test36 exited with value ().
      - test37 exited with value ().
      - test38 exited with value ().
      - test39 exited with value ().
      - test40 exited with value ().
      - test41 exited with value ().
      - test42 exited with value ().
      - test43 exited with value ().
      - test44 exited with value ().
      - test45 exited with value ().
      - test46 exited with value ().
      - test47 exited with value ().
      - test48 exited with value ().
      - test49 exited with value ().
      - test50 exited with value ().
      - test51 exited with value ().
      - test52 exited with value ().
      - test53 exited with value ().
      - test54 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_part_3.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test1 exited with value ().
      - test2 exited with value ().
      - test3 exited with value ().
      - test4 exited with value ().
      - test5 exited with value ().
      - test6 exited with value ().
      - test7 exited with value ().
      - test8 exited with value ().
      - test9 exited with value ().
      - test10 exited with value ().
      - test11 exited with value ().
      - test12 exited with value ().
      - test13 exited with value ().
      - test14 exited with value ().
      - test15 exited with value ().
      - test16 exited with value ().
      - test17 exited with value ().
      - test18 exited with value ().
      - test19 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "switch_case_if_else.jsligo"; "--no-warn" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test_if_switch_break exited with value ().
      - test_if_switch_return exited with value ().
      - test_switch_if_break exited with value ().
      - test_switch_if_return exited with value ().
      - test_switch_switch_break exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "let_rec.mligo" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test exited with value true. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_negative_big_map_id.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_main exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_FA12.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_transfer exited with value ().
    - test_transfer_not_e_allowance exited with value ().
    - test_transfer_not_e_balance exited with value ().
    - test_approve exited with value ().
    - test_approve_unsafe exited with value ().
    - test_get_allowance exited with value ().
    - test_get_balance exited with value ().
    - test_get_total_supply exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "pack_unpack.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_string exited with value ().
    - test_int exited with value ().
    - test_string_int exited with value ().
    - test_string_string exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_pack_unpack.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "pairing_check.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "gas_consum.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (1305n , 1510n , 1510n). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_implicit_account.jsligo" ];
  [%expect
    {|
    0mutez
    123mutez
    Everything at the top-level was executed.
    - test_addresses exited with value [tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx]. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_accounts.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_new exited with value 110000000mutez.
    - test_add exited with value 110000000mutez.
    - test_new exited with value 110000000mutez.
    - test_add exited with value 110000000mutez. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_baker_account.mligo" ];
  [%expect
    {|
    File "./test_baker_account.mligo", line 7, characters 12-28:
      6 | let test =
      7 |   let acc = Test.new_account () in
                      ^^^^^^^^^^^^^^^^
      8 |   let () = Test.baker_account acc (None : tez option) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 8, characters 11-29:
      7 |   let acc = Test.new_account () in
      8 |   let () = Test.baker_account acc (None : tez option) in
                     ^^^^^^^^^^^^^^^^^^
      9 |   let () = Test.reset_state 2n ([] : tez list) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.Reset.add_baker` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 9, characters 11-27:
      8 |   let () = Test.baker_account acc (None : tez option) in
      9 |   let () = Test.reset_state 2n ([] : tez list) in
                     ^^^^^^^^^^^^^^^^
     10 |   let pkh = Crypto.hash_key acc.1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 13, characters 11-19:
     12 |   let a = Tezos.address c in
     13 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
                     ^^^^^^^^
     14 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 14, characters 11-19:
     13 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
     14 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     15 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 14, characters 20-47:
     13 |   let () = Test.log "STARTING BALANCE AND VOTING POWER" in
     14 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     15 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 15, characters 11-19:
     14 |   let () = Test.log(Test.get_balance_of_address a) in
     15 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     16 |   let () = Test.set_baker a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 15, characters 20-41:
     14 |   let () = Test.log(Test.get_balance_of_address a) in
     15 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     16 |   let () = Test.set_baker a in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 16, characters 11-25:
     15 |   let () = Test.log(Test.get_voting_power pkh) in
     16 |   let () = Test.set_baker a in
                     ^^^^^^^^^^^^^^
     17 |   let orig = Test.originate (contract_of C) 41 5tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 17, characters 13-27:
     16 |   let () = Test.set_baker a in
     17 |   let orig = Test.originate (contract_of C) 41 5tez in
                       ^^^^^^^^^^^^^^
     18 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 18, characters 11-19:
     17 |   let orig = Test.originate (contract_of C) 41 5tez in
     18 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
                     ^^^^^^^^
     19 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 19, characters 11-19:
     18 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
     19 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     20 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 19, characters 20-47:
     18 |   let () = Test.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
     19 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     20 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 20, characters 11-19:
     19 |   let () = Test.log(Test.get_balance_of_address a) in
     20 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     21 |   let cc = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 20, characters 20-41:
     19 |   let () = Test.log(Test.get_balance_of_address a) in
     20 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     21 |   let cc = Test.to_contract orig.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 21, characters 11-27:
     20 |   let () = Test.log(Test.get_voting_power pkh) in
     21 |   let cc = Test.to_contract orig.addr in
                     ^^^^^^^^^^^^^^^^
     22 |   let _ = Test.transfer_to_contract cc (Main 1) 3tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 22, characters 10-35:
     21 |   let cc = Test.to_contract orig.addr in
     22 |   let _ = Test.transfer_to_contract cc (Main 1) 3tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
     23 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 23, characters 11-19:
     22 |   let _ = Test.transfer_to_contract cc (Main 1) 3tez in
     23 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
                     ^^^^^^^^
     24 |   let () = Test.log(Test.get_balance_of_address a) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 24, characters 11-19:
     23 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
     24 |   let () = Test.log(Test.get_balance_of_address a) in
                     ^^^^^^^^
     25 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 24, characters 20-47:
     23 |   let () = Test.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
     24 |   let () = Test.log(Test.get_balance_of_address a) in
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     25 |   let () = Test.log(Test.get_voting_power pkh) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 25, characters 11-19:
     24 |   let () = Test.log(Test.get_balance_of_address a) in
     25 |   let () = Test.log(Test.get_voting_power pkh) in
                     ^^^^^^^^
     26 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "./test_baker_account.mligo", line 25, characters 20-41:
     24 |   let () = Test.log(Test.get_balance_of_address a) in
     25 |   let () = Test.log(Test.get_voting_power pkh) in
                              ^^^^^^^^^^^^^^^^^^^^^
     26 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

    "STARTING BALANCE AND VOTING POWER"
    3800000000000mutez
    4000000000000n
    "BALANCE AND VOTING POWER AFTER ORIGINATE"
    3800004166666mutez
    4000000000000n
    "BALANCE AND VOTING POWER AFTER TRANSFER"
    3800008333332mutez
    4000000000000n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_register_delegate.mligo" ];
  [%expect
    {|
    "STARTING BALANCE AND VOTING POWER"
    950000000000mutez
    1000000000000n
    "BALANCE AND VOTING POWER AFTER ORIGINATE"
    950004166666mutez
    1000000000000n
    "BALANCE AND VOTING POWER AFTER TRANSFER"
    950008333332mutez
    1000000000000n
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_global_constant.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_global_constant_2.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "recursion_uncurry.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value 112. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_timestamp.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_sub exited with value ().
    - test_get_time exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_decompile_failure.mligo" ];
  [%expect
    {|
    (4n , ("a" , 5))
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_context.mligo" ];
  [%expect
    {|
    "test_contract:"
    0
    10
    5
    0
    0
    "test_move:"
    3800000000000mutez
    3800100000000mutez
    3800000000000mutez
    "test_drop:"
    3800000000000mutez
    3800100000000mutez
    3800100000000mutez
    Everything at the top-level was executed.
    - test_contract exited with value ().
    - test_move exited with value ().
    - test_drop exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_error_balance.jsligo"; "--no-warn" ];
  [%expect
    {|
    100000000000000mutez
    3799997904750mutez
    Everything at the top-level was executed.
    - test exited with value {contract_balance = 3799997904750mutez ; contract_too_low = tz1hkMbkLPkvhxyqsQoBoLPqb1mruSzZx3zy ; spend_request = 100000000000000mutez}. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_inline.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_x exited with value {code = { parameter unit ;
      storage
        (pair (set %participants address)
              (map %secrets address bool)
              (big_map %metadata string bytes)) ;
      code { CDR ;
             PUSH bool True ;
             DUP 2 ;
             CAR ;
             ITER { SWAP ;
                    DUP 3 ;
                    GET 3 ;
                    DIG 2 ;
                    GET ;
                    IF_NONE { PUSH bool False ; AND } { DROP ; PUSH bool True ; AND } } ;
             DROP ;
             PUSH bool True ;
             DUP 2 ;
             CAR ;
             ITER { SWAP ;
                    EMPTY_MAP address bool ;
                    DIG 2 ;
                    GET ;
                    IF_NONE { PUSH bool False ; AND } { DROP ; PUSH bool True ; AND } } ;
             DROP ;
             NIL operation ;
             PAIR } } ; size = 226 ; taddr = KT1RCTMT7fm32ZVaTT5pqtNnPsRvbMxkpVMd}. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_read_contract.mligo" ];
  [%expect
    {|
    KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS
    [1 -> "hi"]
    Everything at the top-level was executed.
    - test_foo exited with value ().
    - test_bar exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "cli_arg.mligo"; "--arg"; "[ 1 ; 2 ; 3]" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_cli_arg exited with value [1 ; 2 ; 3]. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "reset_time.mligo" ];
  [%expect
    {|
  Everything at the top-level was executed.
  - test_x exited with value (timestamp(1970-01-01T00:00:00Z) , timestamp(2012-02-02T10:10:10Z)). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_get_account.mligo" ];
  [%expect
    {|
    (tz1MBWU1WkszFfkEER2pgn4ATKXE9ng7x1sR , edpkusHqa6fxkGPPL9YpgbcakvSTvcTBcwnLAmCdcevmws4Mh2MdHB , "edsk41aRaPPBpidY7w5xu54edk76uJJtJ6myTwYDEWhAwNHce9gKNo")
    3800000000000mutez
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_sign.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_create.mligo" ];
  [%expect
    {|
    42
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_create2.mligo" ];
  [%expect
    {|
    42
    Everything at the top-level was executed.
    - test exited with value (). |}]

(*
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_transfer_entrypoint.ligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]
*)

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_print.mligo" ];
  [%expect
    {|
    Hello world
    @42
    Everything at the top-level was executed.
    - test exited with value "(true , 42n)". |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_eprint.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value ().
    Ooops |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_random.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_untranspile_bls.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_fr exited with value ().
    - test_g1 exited with value ().
    - test_g2 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "get_contract.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_key.mligo" ];
  [%expect
    {|
    edpktom5rsehpEY6Kp2NShwsnpaaEjWxKFMJ3Rjp99VMJuHS93wxD6
    Everything at the top-level was executed.
    - test exited with value Success (1719n). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_tickets_and_bigmaps.mligo" ];
  [%expect
    {|
    Success (2672n)
    Everything at the top-level was executed.
    - test_one exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_chain_id.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value 0x050a0000000400000000. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_print_values.mligo" ];
  [%expect
    {|
    File "./test_print_values.mligo", line 1, characters 9-32:
      1 | let () = Test.unset_print_values ()
                   ^^^^^^^^^^^^^^^^^^^^^^^
      2 | let () = Test.println "aloh"
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.unset_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "./test_print_values.mligo", line 2, characters 9-21:
      1 | let () = Test.unset_print_values ()
      2 | let () = Test.println "aloh"
                   ^^^^^^^^^^^^
      3 | let test = 42
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    aloh |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_to_json.mligo" ];
  [%expect
    {|
    ["typed_address","KT19SRGEVxDMKdou6Fu7vZrtPy6X9GB7Dwna"]
    ["record",[[["Label","bar",["File",{"start":{"byte":{"pos_fname":"./test_to_json.mligo","pos_lnum":9,"pos_bol":0,"pos_cnum":31},"point_num":228,"point_bol":197},"stop":{"byte":{"pos_fname":"./test_to_json.mligo","pos_lnum":9,"pos_bol":0,"pos_cnum":34},"point_num":231,"point_bol":197}}]],["list",[["constant",["string","hello"]],["constant",["string","world"]]]]],[["Label","foo",["File",{"start":{"byte":{"pos_fname":"./test_to_json.mligo","pos_lnum":9,"pos_bol":0,"pos_cnum":19},"point_num":216,"point_bol":197},"stop":{"byte":{"pos_fname":"./test_to_json.mligo","pos_lnum":9,"pos_bol":0,"pos_cnum":22},"point_num":219,"point_bol":197}}]],["constant",["int","42"]]]]] |}]

(*
let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_imm.ligo" ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - test_orig exited with value (). |test}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_record.ligo" ];
  [%expect
    {test|
    0
    Everything at the top-level was executed.
    - test_reproducing exited with value "OK". |test}]
*)

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "tuple_long.mligo" ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - test exited with value (). |test}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_compare_setmap.mligo" ];
  [%expect
    {test|
    File "./test_compare_setmap.mligo", line 6, characters 23-37:
      5 |  let s = Set.add ("tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ" : address) s in
      6 |  let s : address set = Test.decompile (Test.eval s) in
                                 ^^^^^^^^^^^^^^
      7 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 6, characters 39-48:
      5 |  let s = Set.add ("tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ" : address) s in
      6 |  let s : address set = Test.decompile (Test.eval s) in
                                                 ^^^^^^^^^
      7 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 7, characters 1-10:
      6 |  let s : address set = Test.decompile (Test.eval s) in
      7 |  Test.eval s
           ^^^^^^^^^
      8 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 13, characters 19-33:
     12 |  let s = Set.add 3 s in
     13 |  let s : int set = Test.decompile (Test.eval s) in
                             ^^^^^^^^^^^^^^
     14 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 13, characters 35-44:
     12 |  let s = Set.add 3 s in
     13 |  let s : int set = Test.decompile (Test.eval s) in
                                             ^^^^^^^^^
     14 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 14, characters 1-10:
     13 |  let s : int set = Test.decompile (Test.eval s) in
     14 |  Test.eval s
           ^^^^^^^^^
     15 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 20, characters 30-44:
     19 |  let s = Map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
     20 |  let s : (address, int) map = Test.decompile (Test.eval s) in
                                        ^^^^^^^^^^^^^^
     21 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 20, characters 46-55:
     19 |  let s = Map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
     20 |  let s : (address, int) map = Test.decompile (Test.eval s) in
                                                        ^^^^^^^^^
     21 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 21, characters 1-10:
     20 |  let s : (address, int) map = Test.decompile (Test.eval s) in
     21 |  Test.eval s
           ^^^^^^^^^
     22 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 27, characters 34-48:
     26 |  let s = Big_map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
     27 |  let s : (address, int) big_map = Test.decompile (Test.eval s) in
                                            ^^^^^^^^^^^^^^
     28 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 27, characters 50-59:
     26 |  let s = Big_map.add ("tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" : address) 900 s in
     27 |  let s : (address, int) big_map = Test.decompile (Test.eval s) in
                                                            ^^^^^^^^^
     28 |  Test.eval s
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    File "./test_compare_setmap.mligo", line 28, characters 1-10:
     27 |  let s : (address, int) big_map = Test.decompile (Test.eval s) in
     28 |  Test.eval s
           ^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_address_set exited with value { "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" ;
      "tz1TDZG4vFoA2xutZMYauUnS4HVucnAGQSpZ" }.
    - test_int_set exited with value { 3 ; 4 }.
    - test_map exited with value { Elt "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" 900 ;
      Elt "KT1WoTZUkky48v3QqZWzkeJCYfhWhNaVFYuC" 100 }.
    - test_big_map exited with value { Elt "tz1KeYsjjSCLEELMuiq1oXzVZmuJrZ15W4mv" 900 ;
      Elt "KT1WoTZUkky48v3QqZWzkeJCYfhWhNaVFYuC" 100 }. |test}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "test-expr"
    ; "cameligo"
    ; "type t = [@layout:comb] { num : int ; num_nat : nat ; str : string } in let v = \
       Test.parse_michelson {| { Elt 1 (Pair 1 1 \"q\") } |} in ((Test.decompile v : \
       (nat, t) big_map))"
    ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - eval exited with value [1n -> {num = 1 ; num_nat = 1n ; str = "q"}]. |test}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good
    [ "run"; "test"; test "display_format_json.mligo"; "--display-format"; "json" ];
  [%expect
    {xxx|
    [
      [ "test_x", [ "constant", [ "int", "65" ] ] ],
      [ "test_y", [ "constant", [ "string", "hello" ] ] ]
    ] |xxx}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "record_field_assign.jsligo" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - test_simple_record_assign exited with value ().
      - test_nested_record_assign_level1 exited with value ().
      - test_nested_record_assign_level2 exited with value ().
      - test_record_assign_var exited with value ().
      - test_nested_record_assign_var_level1 exited with value ().
      - test_nested_record_assign_var_level2 exited with value ().
      - test_nested_record_assign_var_level2_expr exited with value ().
      - test_nested_record_assign_var_level2_record_access exited with value ().
      - test_nested_record_assign_var_level2_module_member exited with value ().
      - test_nested_record_assign_var_level2_module_record_member exited with value ().
      - test_nested_record_assign_var_level2_lambda exited with value ().
      - test_nested_record_assign_var_level2_lambda_app exited with value ().
      - test_simple_tuple_field_assign exited with value ().
      - test_simple_record_field_with_array_notation_assign exited with value ().
      - test_nested_record_assign_array_notation_level1 exited with value ().
      - test_nested_record_assign_array_notation_level2 exited with value ().
      - test_nested_record_assign_tuple_assign_array_notation_level2 exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_originate_module.mligo" ];
  [%expect
    {|
    Deployed the contract:
    { parameter (or (int %sub) (int %add)) ;
      storage int ;
      code { UNPAIR ; IF_LEFT { SWAP ; SUB } { ADD } ; NIL operation ; PAIR } ;
      view "get" unit int { CDR } ;
      view "get_diff" int int { UNPAIR ; SWAP ; SUB } }
    With storage: 0
    Storage after call: 42 |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_originate_module.jsligo" ];
  [%expect
    {test|
    Everything at the top-level was executed.
    - test_increment exited with value (). |test}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_originate_single_view.mligo" ];
  [%expect {| |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "contract_with_ticket_storage.mligo" ];
  [%expect
    {|
    ("unforged_ticket" , Some ({amount = 15n ; ticketer = KT1CDHnKFHBMFtyzC92oTfi4Z5wthR4Yk3LW ; value = 0x0202}))
    Everything at the top-level was executed.
    - test_originate_contract exited with value (). |}];
  run_ligo_good [ "run"; "test"; test "contract_with_ticket_param.mligo" ];
  [%expect
    {|
    File "./contract_with_ticket_param.mligo", line 7, characters 24-41:
      6 |     let (_,ticket) = p in
      7 |     let (_,(v,_)) , _ = Tezos.read_ticket ticket in
                                  ^^^^^^^^^^^^^^^^^
      8 |     [] , (v, Tezos.get_sender ())
    :
    Warning: deprecated value.
    In a future version, `Tezos` will be replaced by `Tezos.Next`, and using `Ticket.read` from `Tezos.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 12, characters 13-27:
     11 | let test_transfer_to_contract =
     12 |   let orig = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
                       ^^^^^^^^^^^^^^
     13 |   let main_addr = Tezos.address (Test.to_contract orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 12, characters 51-77:
     11 | let test_transfer_to_contract =
     12 |   let orig = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
                                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |   let main_addr = Tezos.address (Test.to_contract orig.addr) in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 13, characters 33-49:
     12 |   let orig = Test.originate (contract_of C) ("bye",Test.nth_bootstrap_account 1) 1mutez in
     13 |   let main_addr = Tezos.address (Test.to_contract orig.addr) in
                                           ^^^^^^^^^^^^^^^^
     14 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 32, characters 16-43:
     31 |   in
     32 |   let s, addr = Test.get_storage_of_address main_addr in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
     33 |   let p_addr = proxy_taddr |> Test.to_contract |> Tezos.address in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 33, characters 30-46:
     32 |   let s, addr = Test.get_storage_of_address main_addr in
     33 |   let p_addr = proxy_taddr |> Test.to_contract |> Tezos.address in
                                        ^^^^^^^^^^^^^^^^
     34 |   assert (s = "world" && addr = p_addr)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "./contract_with_ticket_param.mligo", line 34, characters 2-8:
     33 |   let p_addr = proxy_taddr |> Test.to_contract |> Tezos.address in
     34 |   assert (s = "world" && addr = p_addr)
            ^^^^^^
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_transfer_to_contract exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_prefix_posfix_ops.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_prefix_ops exited with value ().
    - test_postfix_ops exited with value ().
    - test1 exited with value ().
    - test2 exited with value ().
    - test3 exited with value ().
    - test4 exited with value ().
    - test5 exited with value ().
    - test6 exited with value ().
    - test7 exited with value ().
    - test8 exited with value ().
    - test9 exited with value ().
    - test10 exited with value ().
    - test11 exited with value ().
    - test12 exited with value ().
    - test1_ exited with value ().
    - test2_ exited with value ().
    - test3_ exited with value ().
    - test4_ exited with value ().
    - test5_ exited with value ().
    - test6_ exited with value ().
    - test7_ exited with value ().
    - test8_ exited with value ().
    - test9_ exited with value ().
    - test10_ exited with value ().
    - test11_ exited with value ().
    - test12_ exited with value (). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test "for loops" =
  run_ligo_good [ "run"; "test"; test "/for_loop/single_loop.jsligo" ];
  [%expect
    {|
      Everything at the top-level was executed.
      - testPalindrome exited with value (). |}];
  run_ligo_good [ "run"; "test"; test "/for_loop/pascal_triangle.jsligo" ];
  [%expect
    {|
    File ".//for_loop/pascal_triangle.jsligo", line 13, characters 35-45:
     12 | const printSpaces = (n: int): unit => {
     13 |     for (let i = n ; i >= 0 ; i--) Test.print(" ")
                                             ^^^^^^^^^^
     14 | }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 20, characters 8-18:
     19 |     for (const x of xs) {
     20 |         Test.print(x > 9 ? "  " : "   ");
                  ^^^^^^^^^^
     21 |         Test.print(Test.to_string(x));
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 21, characters 8-18:
     20 |         Test.print(x > 9 ? "  " : "   ");
     21 |         Test.print(Test.to_string(x));
                  ^^^^^^^^^^
     22 |     }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 21, characters 19-33:
     20 |         Test.print(x > 9 ? "  " : "   ");
     21 |         Test.print(Test.to_string(x));
                             ^^^^^^^^^^^^^^
     22 |     }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 36, characters 8-20:
     35 |         printNums(nums);
     36 |         Test.println("")
                  ^^^^^^^^^^^^
     37 |     };
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/pascal_triangle.jsligo", line 42, characters 18-30:
     41 | const test8 = printPascalTriangle(8)
     42 | const testspace = Test.println("")
                            ^^^^^^^^^^^^
     43 | const test10 = printPascalTriangle(9)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

                      1
                    1   1
                  1   2   1
                1   3   3   1
              1   4   6   4   1
            1   5  10  10   5   1
          1   6  15  20  15   6   1
        1   7  21  35  35  21   7   1

                        1
                      1   1
                    1   2   1
                  1   3   3   1
                1   4   6   4   1
              1   5  10  10   5   1
            1   6  15  20  15   6   1
          1   7  21  35  35  21   7   1
        1   8  28  56  70  56  28   8   1
    Everything at the top-level was executed.
    - test8 exited with value "".
    - testspace exited with value ().
    - test10 exited with value "". |}];
  run_ligo_good [ "run"; "test"; test "/for_loop/matrix_multiplication.jsligo" ];
  [%expect
    {|
    File ".//for_loop/matrix_multiplication.jsligo", line 15, characters 4-26:
     14 | {
     15 |     Test.assert_with_error(m1c == m2r,
              ^^^^^^^^^^^^^^^^^^^^^^
     16 |     "Error: The number of columns in 1st matrix must be equal to number of rows in 2nd matrix");
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.Error.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/matrix_multiplication.jsligo", line 39, characters 4-15:
     38 |         Map.literal([[[0, 0], 4], [[0, 1], 5], [[0, 2], 6]]), 1, 3);
     39 |     Test.assert(m == Map.literal(list([
              ^^^^^^^^^^^
     40 |         [[0, 0], 4] , [[0, 1], 5] , [[0, 2], 6],
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/matrix_multiplication.jsligo", line 55, characters 4-15:
     54 |                           [[2, 0], 16], [[2, 1], 17], [[2, 2], 18]])), 3, 3);
     55 |     Test.assert(m == Map.literal(list([
              ^^^^^^^^^^^
     56 |         [[0, 0], 84] , [[0, 1], 90] , [[0, 2], 96] ,
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/matrix_multiplication.jsligo", line 69, characters 4-15:
     68 |                           [[2, 0], 6]])), 3, 1);
     69 |     Test.assert(m == Map.literal(list([
              ^^^^^^^^^^^
     70 |         [[0, 0], 32]
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/matrix_multiplication.jsligo", line 84, characters 4-15:
     83 |                           [[2, 0], 16], [[2, 1], 17], [[2, 2], 18]])), 3, 3);
     84 |     Test.assert(m == Map.literal(list([
              ^^^^^^^^^^^
     85 |         [[0, 0], 10], [[0, 1], 11], [[0, 2], 12] ,
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_3x1_1x3 exited with value ().
    - test_3x3_3x3 exited with value ().
    - test_1x3_3x1 exited with value ().
    - test_I3x3_3x3 exited with value (). |}];
  run_ligo_good [ "run"; "test"; test "/for_loop/for_loops.jsligo" ];
  [%expect
    {|
    File ".//for_loop/for_loops.jsligo", line 7, characters 2-13:
      6 |   };
      7 |   Test.assert(a == 10)
            ^^^^^^^^^^^
      8 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 13, characters 2-13:
     12 |   };
     13 |   Test.assert(b == 12);
            ^^^^^^^^^^^
     14 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 23, characters 2-13:
     22 |   };
     23 |   Test.assert(d == 15);
            ^^^^^^^^^^^
     24 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 33, characters 2-13:
     32 |   };
     33 |   Test.assert(f == 10);
            ^^^^^^^^^^^
     34 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 37, characters 2-13:
     36 |   for ( ; g < 10 ; ) ;
     37 |   Test.assert(g == 11);
            ^^^^^^^^^^^
     38 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 41, characters 2-13:
     40 |   for ( ;h < 10; h++) ;
     41 |   Test.assert(h == 10);
            ^^^^^^^^^^^
     42 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 47, characters 2-13:
     46 |   for (j = 11 ; j < 10 ; ) ;
     47 |   Test.assert(j == 11);
            ^^^^^^^^^^^
     48 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    File ".//for_loop/for_loops.jsligo", line 53, characters 2-13:
     52 |   for (l = 0 ;l < 10; l++) ;
     53 |   Test.assert(l == 10);
            ^^^^^^^^^^^
     54 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - testLoop exited with value (). |}];
  run_ligo_good [ "run"; "test"; test "/for_loop/for_map.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value Some ((2 , "goodbye")). |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test "aggregation regression" =
  run_ligo_good [ "run"; "test"; test "agg_bar.mligo" ];
  [%expect
    {|
    Running <A simple list extension>
    Everything at the top-level was executed. |}]

(* do not remove that :) *)

let () = Sys_unix.chdir pwd

let () =
  Sys_unix.chdir
    "../../test/contracts/interpreter_tests/originate_from_relative_path/test/a/b/"


let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate_from_file_relative_path exited with value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS.
    - test_originate_from_file_relative_path_w_r_t_imported_file exited with value true. |}];
  run_ligo_good [ "run"; "test"; test "test.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate_from_file_relative_path exited with value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS.
    - test_originate_from_file_relative_path_w_r_t_imported_file exited with value true. |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test_record_update.jsligo" ];
  [%expect {|
    Everything at the top-level was executed. |}]

let () = Sys_unix.chdir pwd

let () =
  Sys_unix.chdir "../../test/contracts/interpreter_tests/originate_from_relative_path/"


let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test/a/b/test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate_from_file_relative_path exited with value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS.
    - test_originate_from_file_relative_path_w_r_t_imported_file exited with value true. |}];
  run_ligo_good [ "run"; "test"; test "test/a/b/test.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_originate_from_file_relative_path exited with value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS.
    - test_originate_from_file_relative_path_w_r_t_imported_file exited with value true. |}]

let () = Sys_unix.chdir pwd
let bad_test n = bad_test ("/interpreter_tests/" ^ n)

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_capture_meta_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_capture_meta_type.mligo", line 15, characters 26-27:
     14 |
     15 | let f = fun (_ : unit) -> v.x
                                    ^
     16 |

    Invalid usage of a Test type: typed_address (sum[Main -> unit] ,
    unit) in record[x -> int , y -> typed_address (sum[Main -> unit] , unit)] cannot be translated to Michelson. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_random.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_random.mligo", line 6, characters 46-49:
      5 |   (* We generate the property *)
      6 |   let test = PBT.make_test (PBT.gen_small : ((int contract) list) pbt_gen) (fun (xs : (int contract) list) -> List.length xs = 42n) in
                                                        ^^^
      7 |   (* And run it *)

    Generator for type contract (int) is not implemented. For now, only unit, string, bytes, address, int, nat, tez, records, sums, lists, sets, maps and big_maps can be generated. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25:
      1 | let test : unit =
      2 |   failwith "I am failing"
            ^^^^^^^^^^^^^^^^^^^^^^^

    An uncaught error occured:
    Failwith: "I am failing"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 1, characters 11-30:
      1 | let test = Assert.assert false
                     ^^^^^^^^^^^^^^^^^^^

    An uncaught error occured:
    Failwith: "failed assertion"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 1, characters 11-30 ,
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 1, characters 11-30 ,
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 1, characters 11-30 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "bad_balances_reset.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/bad_balances_reset.mligo", line 3, characters 11-48:
      2 |
      3 | let test = Test.State.reset 2n [4000tez;4000tez]
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

     baker account initial balance must at least reach 6000 tez |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_failure3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_failure3.mligo", line 1, characters 31-32:
      1 | module C = struct [@entry] let f = (fun () () -> ()) end
                                         ^
      2 | let test =

    Not an entrypoint: [_, _]unit -> unit -> unit |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_trace.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 3, characters 4-31:
      2 |   if x < 0 then
      3 |     (failwith "negative" : int)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   else

    An uncaught error occured:
    Failwith: "negative"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 3, characters 4-31 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 5, characters 4-13 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace.mligo", line 9, characters 14-49 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_trace2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 9, characters 11-86:
      8 | let make_call (contr : C parameter_of contract) =
      9 |   let () = Test.Address.get_storage ("KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL" : address) in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |   Test.Contract.transfer_exn contr (Main ()) 10tez

    An uncaught error occured:
    Did not find service: GET ocaml:context/contracts/KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL/storage
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 9, characters 11-86 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 9, characters 11-86 ,
    File "../../test/contracts/negative//interpreter_tests/test_trace2.mligo", line 15, characters 2-55 |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_mutation_loop.mligo"; "--steps"; "1000" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 15, characters 8-55:
     14 |         let () = Test.IO.log(mutation) in
     15 |         failwith "Some mutation also passes the tests!"
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    An uncaught error occured:
    Failwith: "Some mutation also passes the tests!"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 15, characters 8-55
    Mutation at: File "../../test/contracts/negative//interpreter_tests/test_mutation_loop.mligo", line 4, characters 43-44:
      3 | let rec my_rec_fun (rounds : int) : unit =
      4 |   if rounds > 0 then  my_rec_fun (rounds - 1)
                                                     ^
      5 |

    Replacing by: 2. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_source1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_source1.mligo", line 15, characters 10-57:
     14 |   let () = Test.State.set_source addr in
     15 |   let _ = Test.Originate.contract (contract_of C) () 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     16 |   ()

    The source address is not an implicit account
    KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_source2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_source2.mligo", line 13, characters 10-67:
     12 |              (Test.Typed_address.to_address orig.taddr) in
     13 |   let _ = Test.Typed_address.transfer_exn orig.taddr (Main ()) 0tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     14 |   ()

    The source address is not an implicit account
    KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types.jsligo", line 4, characters 36-54:
      3 | const foo = (x: {field: int}): {field: int} => {return x};
      4 | const bar = Test.Michelson.run(foo, {property: "toto"});
                                              ^^^^^^^^^^^^^^^^^^

    Mismatching record labels. Expected record of type "record[field -> int]". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types2.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types2.jsligo", line 4, characters 36-42:
      3 | const foo = (x:  {b:int}):  {b:int} => {return x};
      4 | const bar = Test.Michelson.run(foo, "toto");
                                              ^^^^^^

    This expression has type "string", but an expression was expected of type
    "record[b -> int]".
    Type "string" is not compatible with type "record[b -> int]". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_run_types3.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_run_types3.jsligo", line 4, characters 36-51:
      3 | const foo = (x: int): int => {return x};
      4 | const bar = Test.Michelson.run(foo, {field: "toto"});
                                              ^^^^^^^^^^^^^^^

    This expression has type "record[field -> string]", but an expression was expected of type
    "int".
    Type "record[field -> string]" is not compatible with type "int". |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_decompile.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_decompile.mligo", line 5, characters 2-39:
      4 |   let x = Test.Michelson.eval 4n in
      5 |   (Test.Michelson.decompile x : string)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This Michelson value has assigned type 'nat', which does not coincide with expected type 'string'. |}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_register_delegate.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_register_delegate.mligo", line 27, characters 4-51:
     26 |   let {taddr=ta; code=_; size=_} =
     27 |     Test.Originate.contract (contract_of C) 41 5tez in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     28 |

    Baker cannot bake. Enough rolls? Enough cycles passed?
    "STARTING BALANCE AND VOTING POWER"
    95000000000mutez
    100000000000n |}];
  run_ligo_bad [ "run"; "test"; bad_test "test_register_delegate_stake.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 6, characters 12-28:
      5 | let test =
      6 |   let acc = Test.new_account () in
                      ^^^^^^^^^^^^^^^^
      7 |   let pkh = Crypto.hash_key acc.1 in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 10, characters 10-39:
      9 |
     10 |   let _ = Test.transfer_to_contract_exn c () 1000000tez in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     11 |   let () = Test.register_delegate pkh in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 11, characters 11-33:
     10 |   let _ = Test.transfer_to_contract_exn c () 1000000tez in
     11 |   let () = Test.register_delegate pkh in
                     ^^^^^^^^^^^^^^^^^^^^^^
     12 |   let () = Test.stake pkh 1000000tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_delegate` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 12, characters 11-21:
     11 |   let () = Test.register_delegate pkh in
     12 |   let () = Test.stake pkh 1000000tez in
                     ^^^^^^^^^^
     13 |   ()
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `State.stake` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 12, characters 11-36:
     11 |   let () = Test.register_delegate pkh in
     12 |   let () = Test.stake pkh 1000000tez in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |   ()

    An uncaught error occured:
    { "id": "proto.019-PtParisB.operation.manual_staking_forbidden",
      "description":
        "Manual staking operations are forbidden because staking is currently automated.",
      "data": {} }
    Trace:
    File "../../test/contracts/negative//interpreter_tests/test_register_delegate_stake.mligo", line 12, characters 11-36 |}]

let () = Sys_unix.chdir pwd
let () = Sys_unix.chdir "../../test/contracts/negative/interpreter_tests/"

(* using typed_address in Bytes.pack *)
let%expect_test _ =
  run_ligo_bad [ "run"; "test"; "typed_addr_in_bytes_pack.mligo" ];
  [%expect
    {|
  File "typed_addr_in_bytes_pack.mligo", line 15, character 17 to line 19, character 5:
   14 |     let r = originate_record () in
   15 |     let packed = Bytes.pack (fun() ->
                         ^^^^^^^^^^^^^^^^^^^^
   16 |         match (Tezos.get_entrypoint_opt "%transfer" r.addr : unit contract option) with
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   17 |           Some(c) -> let op = Tezos.Next.Operation.transaction () 0mutez c in [op]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   18 |         | None ->  ([] : operation list)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   19 |     ) in
        ^^^^^
   20 |     let () = Test.IO.log(packed) in

  Cannot decompile value KT1MoPRoithHNa7i6LYHqeQfZB4oyWThinnS of type typed_address (unit ,
  unit) |}]

let () = Sys_unix.chdir pwd

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "test_michelson_non_func.mligo" ];
  [%expect
    {test|
    File "../../test/contracts/negative//interpreter_tests/test_michelson_non_func.mligo", line 5, characters 16-55:
      4 | let test =
      5 |   let x : int = [%Michelson ({|{ PUSH int 1 }|} : int)] in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      6 |   begin

    Embedded raw code can only have a functional type |test}]

let%expect_test _ =
  run_ligo_bad [ "run"; "test"; bad_test "get_contract.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 21, characters 10-66:
     20 |   let _ = (Tezos.get_contract a : (C parameter_of contract)) in
     21 |   let _ = (Tezos.get_contract_with_error a "foo" : (int contract)) in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     22 |   ()

    An uncaught error occured:
    Failwith: "foo"
    Trace:
    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 21, characters 10-66 ,
    File "../../test/contracts/negative//interpreter_tests/get_contract.mligo", line 21, characters 10-66 |}]
