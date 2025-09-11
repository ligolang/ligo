open Cli_expect

let contract = test

(* warning unused variables example *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_unused.mligo" ];
  [%expect
    {|
    File "../../test/contracts/warning_unused.mligo", line 14, characters 6-7:
     13 |   let x = s.x + 3 in
     14 |   let x = foo x in
                ^
     15 |   let x = bar s.x in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    { parameter int ;
      storage (pair (int %x) (int %y)) ;
      code { CDR ;
             PUSH int 3 ;
             DUP 2 ;
             CAR ;
             ADD ;
             DROP ;
             PUSH int 3 ;
             PUSH int 9 ;
             DUP 3 ;
             CAR ;
             MUL ;
             ADD ;
             UPDATE 1 ;
             NIL operation ;
             PAIR } } |}]

(* warning non-duplicable variable used examples *)
let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "warning_duplicate.mligo"
    ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Instruction.Slot.lookup: slot not found" (stack (Value))
    (slot (Ident Foo.x#437)))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 21-37
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 26-47
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen.compile_to_micheline in file "vendors/lltz/lib/lltz_codegen/lltz_codegen.ml", line 860, characters 36-52
  Called from Ligo_compile__Of_mini_c.compile_expression.(fun) in file "src/main/compile/of_mini_c.ml", line 176, characters 4-81
  Called from Build.build_expression.(fun) in file "src/main/build/build.ml", line 645, characters 9-77
  Called from Ligo_api__Compile.expression.(fun) in file "src/main/api/common/compile.ml", line 144, characters 8-74
  Called from Simple_utils__Trace.to_stdlib_result_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", line 129, characters 20-28
  Called from Lwt.Sequential_composition.backtrace_catch in file "src/core/lwt.ml", line 2077, characters 10-14
  Re-raised at Simple_utils__Trace.try_with_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", lines 75-76, characters 2-29
  Re-raised at Simple_utils__Trace.try_with_lwt.(fun) in file "_none_", lines 0-7, characters 62-127
  Called from Cli_helpers.return_result_lwt.(fun).get_formatted_result in file "src/main/helpers/cli_helpers.ml", line 185, characters 35-84
  Re-raised at Cli.run in file "src/bin/cli.ml", line 3760, characters 21-30
  Called from Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 46, characters 18-31
  Called from Cli_expect_tests__Warnings.(fun) in file "src/bin/expect_tests/warnings.ml", lines 39-46, characters 2-5
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "warning_duplicate2.mligo"
    ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Instruction.Slot.lookup: slot not found" (stack (Value))
    (slot (Ident x#404)))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 21-37
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 26-47
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen.compile_to_micheline in file "vendors/lltz/lib/lltz_codegen/lltz_codegen.ml", line 860, characters 36-52
  Called from Ligo_compile__Of_mini_c.compile_expression.(fun) in file "src/main/compile/of_mini_c.ml", line 176, characters 4-81
  Called from Build.build_expression.(fun) in file "src/main/build/build.ml", line 645, characters 9-77
  Called from Ligo_api__Compile.expression.(fun) in file "src/main/api/common/compile.ml", line 144, characters 8-74
  Called from Simple_utils__Trace.to_stdlib_result_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", line 129, characters 20-28
  Called from Lwt.Sequential_composition.backtrace_catch in file "src/core/lwt.ml", line 2077, characters 10-14
  Re-raised at Simple_utils__Trace.try_with_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", lines 75-76, characters 2-29
  Re-raised at Simple_utils__Trace.try_with_lwt.(fun) in file "_none_", lines 0-7, characters 62-127
  Called from Cli_helpers.return_result_lwt.(fun).get_formatted_result in file "src/main/helpers/cli_helpers.ml", line 185, characters 35-84
  Re-raised at Cli.run in file "src/bin/cli.ml", line 3760, characters 21-30
  Called from Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 46, characters 18-31
  Called from Cli_expect_tests__Warnings.(fun) in file "src/bin/expect_tests/warnings.ml", lines 104-111, characters 2-5
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "duplicate_ticket_local_module.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Instruction.Slot.lookup: slot not found" (stack (Value))
    (slot (Ident LOCAL#in.B.ticket#554)))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 21-37
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 26-47
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Lltz_codegen.compile_contract_to_micheline in file "vendors/lltz/lib/lltz_codegen/lltz_codegen.ml", line 875, characters 36-52
  Called from Ligo_compile__Of_mini_c.compile_contract.(fun) in file "src/main/compile/of_mini_c.ml", line 124, characters 6-70
  Called from Build.build_contract_stacking.(fun) in file "src/main/build/build.ml", line 751, characters 22-84
  Called from Build.build_contract in file "src/main/build/build.ml", line 760, characters 4-58
  Called from Ligo_api__Compile.contract.(fun) in file "src/main/api/common/compile.ml", line 93, characters 8-59
  Called from Simple_utils__Trace.to_stdlib_result_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", line 129, characters 20-28
  Called from Lwt.Sequential_composition.backtrace_catch in file "src/core/lwt.ml", line 2077, characters 10-14
  Re-raised at Simple_utils__Trace.try_with_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", lines 75-76, characters 2-29
  Re-raised at Simple_utils__Trace.try_with_lwt.(fun) in file "_none_", lines 0-7, characters 62-127
  Called from Cli_helpers.return_result_lwt.(fun).get_formatted_result in file "src/main/helpers/cli_helpers.ml", line 185, characters 35-84
  Re-raised at Cli.run in file "src/bin/cli.ml", line 3760, characters 21-30
  Called from Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 46, characters 18-31
  Called from Cli_expect_tests__Warnings.(fun) in file "src/bin/expect_tests/warnings.ml", line 159, characters 2-88
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

(* some check about the warnings of the E_constructor cases *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_ambiguous_ctor.mligo" ];
  [%expect
    {|
    File "../../test/contracts/warning_ambiguous_ctor.mligo", line 12, characters 67-70:
     11 | [@entry]
     12 | let main = fun (() : unit) (_ : union_b) -> ([] : operation list), A 1
                                                                             ^^^

    Warning: The type of "A(1)" is ambiguous: Inferred type is "union_b" but could be of type "union_a".
    Hint: You might want to add a type annotation.

    { parameter unit ;
      storage (or (int %a) (nat %b)) ;
      code { DROP ; PUSH int 1 ; LEFT nat ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile"; "contract"; contract "not_ambiguous_ctor.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (or (nat %a) (nat %b)) ;
      code { DROP ; PUSH nat 1 ; LEFT nat ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_sum_types.mligo" ];
  [%expect
    {|
    File "../../test/contracts/warning_sum_types.mligo", line 86, characters 14-23:
     85 |
     86 | let warn_me = TopTop 42
                        ^^^^^^^^^
     87 |

    Warning: The type of "TopTop(42)" is ambiguous: Inferred type is "ttop2" but could be of type "ttop".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 88, characters 14-21:
     87 |
     88 | let warn_me = TopA 42
                        ^^^^^^^
     89 |

    Warning: The type of "TopA(42)" is ambiguous: Inferred type is "ttop" but could be of type "ta".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 90, characters 14-21:
     89 |
     90 | let warn_me = TopB 42
                        ^^^^^^^
     91 |

    Warning: The type of "TopB(42)" is ambiguous: Inferred type is "ttop" but could be of type "tb".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 92, characters 14-19:
     91 |
     92 | let warn_me = BA 42
                        ^^^^^
     93 |

    Warning: The type of "BA(42)" is ambiguous: Inferred type is "tb" but could be of type "ta".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 94, characters 14-19:
     93 |
     94 | let warn_me = BB 42
                        ^^^^^
     95 |

    Warning: The type of "BB(42)" is ambiguous: Inferred type is "tb" but could be of type "tb2".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 96, characters 14-19:
     95 |
     96 | let warn_me = AA 42
                        ^^^^^
     97 |

    Warning: The type of "AA(42)" is ambiguous: Inferred type is "ta" but could be of type "ta2".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 98, characters 14-19:
     97 |
     98 | let warn_me = BN 42
                        ^^^^^
     99 |

    Warning: The type of "BN(42)" is ambiguous: Inferred type is "tb" but could be of type "tn".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 100, characters 14-19:
     99 |
    100 | let warn_me = AN 42
                        ^^^^^
    101 |

    Warning: The type of "AN(42)" is ambiguous: Inferred type is "tn" but could be of type "ta".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 104, characters 14-19:
    103 |
    104 | let warn_me = NN 42
                        ^^^^^
    105 |

    Warning: The type of "NN(42)" is ambiguous: Inferred type is "tn" but could be of type "tn2".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 106, characters 14-22:
    105 |
    106 | let warn_me = TopS1 42
                        ^^^^^^^^
    107 |

    Warning: The type of "TopS1(42)" is ambiguous: Inferred type is "ttop" but could be of type "ts1".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 108, characters 14-22:
    107 |
    108 | let warn_me = TopS2 42
                        ^^^^^^^^
    109 |

    Warning: The type of "TopS2(42)" is ambiguous: Inferred type is "ttop" but could be of type "ts2".
    Hint: You might want to add a type annotation.

    { parameter int ;
      storage int ;
      code { DROP ; PUSH int 42 ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_sum_types_shadowed.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage int ;
      code { DROP ; PUSH int 42 ; NIL operation ; PAIR } } |}]
