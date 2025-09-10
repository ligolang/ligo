open Cli_expect

(* Testing *)

let test_ file = test ("top_level_patterns/interpreter/" ^ file)

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/nested_record.jsligo" ] ;
  [%expect{| TODO |}] *)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 1, characters 10-31:
      1 | const _ = Test.set_print_values ()
                    ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 4, characters 4-12:
      3 | const f = () => {
      4 |     Test.log("Once");
              ^^^^^^^^
      5 |     return [[1 as nat, 1, "H"], [2 as nat, 2, "E"], [3 as nat, 3, "L"]]
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 13, characters 4-10:
     12 | const _test = () => {
     13 |     assert ([a1 + b1 + c1] == [a4 + b4 + c4]);
              ^^^^^^
     14 |     assert ([a2 + b2 + c2] == [a5 + b5 + c5]);
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 14, characters 4-10:
     13 |     assert ([a1 + b1 + c1] == [a4 + b4 + c4]);
     14 |     assert ([a2 + b2 + c2] == [a5 + b5 + c5]);
              ^^^^^^
     15 |     assert ([a3 + b3 + c3] == [a6 + b6 + c6])
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/nested_tuple.jsligo", line 15, characters 4-10:
     14 |     assert ([a2 + b2 + c2] == [a5 + b5 + c5]);
     15 |     assert ([a3 + b3 + c3] == [a6 + b6 + c6])
              ^^^^^^
     16 | }
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/record_tuple.jsligo" ] ;
  [%expect{| Everything at the top-level was executed. |}] *)

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/tuple_record.jsligo" ] ;
  [%expect{| Everything at the top-level was executed. |}] *)

(* let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; test_ "jsligo/record.jsligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {| TODO |}] *)

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test_ "jsligo/tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 1, characters 10-31:
      1 | const _ = Test.set_print_values ()
                    ^^^^^^^^^^^^^^^^^^^^^
      2 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 4, characters 2-10:
      3 | const f = () => {
      4 |   Test.log("Once");
            ^^^^^^^^
      5 |   return [1 as nat, 1, "Hello"]
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 12, characters 4-10:
     11 | const _test = () => {
     12 |     assert (a == a1);
              ^^^^^^
     13 |     assert (b == b1);
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 13, characters 4-10:
     12 |     assert (a == a1);
     13 |     assert (b == b1);
              ^^^^^^
     14 |     assert (c == c1)
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/top_level_patterns/interpreter/jsligo/tuple.jsligo", line 14, characters 4-10:
     13 |     assert (b == b1);
     14 |     assert (c == c1)
              ^^^^^^
     15 | }
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    "Once"
    Everything at the top-level was executed.
    - test exited with value (). |}]

(* Negative - linearity *)

let contract file = test ("top_level_patterns/negative/" ^ file)

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/nested_record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "jsligo/nested_tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/nested_tuple.jsligo", line 2, characters 26-28:
      1 | const r = [[1 as nat, 1, "H"], [2 as nat, 2, "E"], [3 as nat, 3, "Hello"]]
      2 | const [[a1, a2, a3], [b1, a2, b3], [c1, c2, c3]] = r
                                    ^^

    Duplicate identifier. |}]

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "jsligo/tuple.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/top_level_patterns/negative/jsligo/tuple.jsligo", line 2, characters 10-11:
      1 | const r = [1 as nat, 1, "Hello"]
      2 | const [a, a, c] = r
                    ^

    Duplicate identifier. |}]

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/record_tuple.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "jsligo/tuple_record.jsligo" ] ;
  [%expect{|
    Internal error: Entrypoint main does not exist |}] *)

(* Negative - much use *)

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "jsligo/ticket_record.jsligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Instruction.Slot.lookup: slot not found" (stack (Value))
    (slot (Ident gen#507)))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen.compile_contract_to_micheline in file "vendors/lltz/lib/lltz_codegen/lltz_codegen.ml", line 875, characters 36-52
  Called from Ligo_compile__Of_mini_c.compile_contract in file "src/main/compile/of_mini_c.ml", line 124, characters 6-70
  Called from Build.build_contract_stacking in file "src/main/build/build.ml", line 751, characters 22-84
  Called from Build.build_contract in file "src/main/build/build.ml", line 760, characters 4-58
  Called from Ligo_api__Compile.contract.(fun) in file "src/main/api/common/compile.ml", line 93, characters 8-59
  Called from Simple_utils__Trace.to_stdlib_result_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", line 129, characters 20-28
  Called from Lwt.Sequential_composition.backtrace_catch in file "src/core/lwt.ml", line 2077, characters 10-14
  Re-raised at Simple_utils__Trace.try_with_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", line 75, characters 2-60
  Re-raised at Lwt.Miscellaneous.poll in file "src/core/lwt.ml", line 3123, characters 20-29
  Called from Lwt_main.run.run_loop in file "src/unix/lwt_main.ml", line 27, characters 10-20
  Called from Lwt_main.run in file "src/unix/lwt_main.ml", line 48, characters 2-13
  Re-raised at Lwt_main.run in file "src/unix/lwt_main.ml", line 112, characters 4-13
  Called from Cli_helpers.return_result_lwt.get_formatted_result in file "src/main/helpers/cli_helpers.ml", line 185, characters 19-84
  Re-raised at Cli.run in file "src/bin/cli.ml", line 3757, characters 21-30
  Called from Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 46, characters 18-31
  Called from Cli_expect_tests__Top_level_binding_pattern_jsligo.(fun) in file "src/bin/expect_tests/top_level_binding_pattern_jsligo.ml", line 177, characters 2-156
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "contract"
    ; contract "jsligo/ticket_tuple.jsligo"
    ; "--werror"
    ; "--disable-michelson-typechecking"
    ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Instruction.Slot.lookup: slot not found" (stack (Value))
    (slot (Ident gen#515)))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen__Instruction.seq.(fun) in file "vendors/lltz/lib/lltz_codegen/instruction.ml", line 27, characters 55-62
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Lltz_codegen.compile_contract_to_micheline in file "vendors/lltz/lib/lltz_codegen/lltz_codegen.ml", line 875, characters 36-52
  Called from Ligo_compile__Of_mini_c.compile_contract in file "src/main/compile/of_mini_c.ml", line 124, characters 6-70
  Called from Build.build_contract_stacking in file "src/main/build/build.ml", line 751, characters 22-84
  Called from Build.build_contract in file "src/main/build/build.ml", line 760, characters 4-58
  Called from Ligo_api__Compile.contract.(fun) in file "src/main/api/common/compile.ml", line 93, characters 8-59
  Called from Simple_utils__Trace.to_stdlib_result_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", line 129, characters 20-28
  Called from Lwt.Sequential_composition.backtrace_catch in file "src/core/lwt.ml", line 2077, characters 10-14
  Re-raised at Simple_utils__Trace.try_with_lwt.(fun) in file "vendored-dune/ligo-utils/simple-utils/trace.ml", line 75, characters 2-60
  Re-raised at Lwt.Miscellaneous.poll in file "src/core/lwt.ml", line 3123, characters 20-29
  Called from Lwt_main.run.run_loop in file "src/unix/lwt_main.ml", line 27, characters 10-20
  Called from Lwt_main.run in file "src/unix/lwt_main.ml", line 48, characters 2-13
  Re-raised at Lwt_main.run in file "src/unix/lwt_main.ml", line 112, characters 4-13
  Called from Cli_helpers.return_result_lwt.get_formatted_result in file "src/main/helpers/cli_helpers.ml", line 185, characters 19-84
  Re-raised at Cli.run in file "src/bin/cli.ml", line 3757, characters 21-30
  Called from Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 46, characters 18-31
  Called from Cli_expect_tests__Top_level_binding_pattern_jsligo.(fun) in file "src/bin/expect_tests/top_level_binding_pattern_jsligo.ml", line 275, characters 2-155
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
