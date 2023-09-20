open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "dynamic_entry_wrong_storage.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/dynamic_entry_wrong_storage.mligo", line 1, character 0 to line 9, character 67:
      1 | type storage =
          ^^^^^^^^^^^^^^
      2 |   {
          ^^^
      3 |     storage : int;
          ^^^^^^^^^^^^^^^^^^
      4 |     dynamic_entrypoints;
          ^^^^^^^^^^^^^^^^^^^^^^^^
      5 |     extra : int
          ^^^^^^^^^^^^^^^
      6 |   }
          ^^^
      7 |

      8 | [@entry]
          ^^^^^^^^
      9 |   let foo () (_ : storage) : operation list * storage = failwith ()
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

     Wrong dynamic entrypoints storage definition record[dynamic_entrypoints -> big_map (nat , bytes) ,
                                                         extra -> int ,
                                                         storage -> int].
     We expect two fields "dynamic_entrypoint" and "storage" |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "dynamic_entry_wrong_storage.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/dynamic_entry_wrong_storage.jsligo", line 1, character 0 to line 9, character 89:
        1 | type storage =
            ^^^^^^^^^^^^^^
        2 |   {
            ^^^
        3 |     storage : int;
            ^^^^^^^^^^^^^^^^^^
        4 |     dynamic_entrypoints;
            ^^^^^^^^^^^^^^^^^^^^^^^^
        5 |     extra : int
            ^^^^^^^^^^^^^^^
        6 |   }
            ^^^
        7 |
  
        8 | @entry
            ^^^^^^
        9 |   const foo = (_u : unit, _storage : storage) : [list<operation>, storage] => failwith ()
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
       Wrong dynamic entrypoints storage definition record[dynamic_entrypoints -> big_map (nat , bytes) ,
                                                           extra -> int ,
                                                           storage -> int].
       We expect two fields "dynamic_entrypoint" and "storage" |}]

let%expect_test "compile storage with initials (mligo)" =
  run_ligo_good [ "compile"; "storage"; test "dynamic_entrypoints.mligo"; "42" ];
  [%expect
    {|
    (Pair 42
          { Elt 0
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0001053d036d034200000000 ;
            Elt 1
                0x05020000002f03200931000000230765035b035b096500000008055f036d035b035b000000000200000006053d036d034200000000 }) |}]

let%expect_test "compile storage with initials (jsligo)" =
  run_ligo_good [ "compile"; "storage"; test "dynamic_entrypoints.jsligo"; "42" ];
  [%expect
    {|
    (Pair 42
          { Elt 0
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0001053d036d034200000000 ;
            Elt 1
                0x05020000002f03200931000000230765035b035b096500000008055f036d035b035b000000000200000006053d036d034200000000 }) |}]

let%expect_test "dynamic entrypoints with tickets (mligo)" =
  run_ligo_good [ "compile"; "contract"; test "dynamic_entrypoints.mligo" ];
  shrink_output [%expect.output];
  [%expect
    {|
    { parameter
        (or (lambda %set_one unit (lambda int (pair (list operation) int)))
            (or (ticket %call_tick int) (unit %call_one))) ;
      storage (pair (int %storage) (big_map %dynamic_entrypoints nat bytes)) ; |}]

let%expect_test "dynamic entrypoints with tickets (jsligo)" =
  run_ligo_good [ "compile"; "contract"; test "dynamic_entrypoints.mligo" ];
  shrink_output [%expect.output];
  [%expect
    {|
        { parameter
            (or (lambda %set_one unit (lambda int (pair (list operation) int)))
                (or (ticket %call_tick int) (unit %call_one))) ;
          storage (pair (int %storage) (big_map %dynamic_entrypoints nat bytes)) ; |}]

let%expect_test "dynamic entrypoints test (mligo)" =
  run_ligo_good [ "run"; "test"; test "dynamic_entrypoints_tests.mligo" ];
  [%expect
    {|
  Everything at the top-level was executed.
  - test_dyn exited with value (). |}]

let%expect_test "dynamic entrypoints test (jsligo)" =
  run_ligo_good [ "run"; "test"; test "dynamic_entrypoints_tests.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_dyn exited with value (). |}]

let%expect_test "opt out dynamic_entrypoints (mligo)" =
  (* Here we expect ONLY TWO ENTRIES (with keys 0 and 2) in the generated big map *)
  run_ligo_good [ "compile"; "storage"; test "opt_out_dynamic_entrypoints.mligo"; "1" ];
  [%expect
    {|
    (Pair 1
          { Elt 0
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0001053d036d034200000000 ;
            Elt 2
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0003053d036d034200000000 }) |}]

let%expect_test "opt out dynamic_entrypoints (jsligo)" =
  (* Here we expect ONLY TWO ENTRIES (with keys 0 and 2) in the generated big map *)
  run_ligo_good [ "compile"; "storage"; test "opt_out_dynamic_entrypoints.jsligo"; "1" ];
  [%expect
    {|
    (Pair 1
          { Elt 0
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0001053d036d034200000000 ;
            Elt 2
                0x050200000029032009310000001d035b0765055f036d035b020000000e03200743035b0003053d036d034200000000 }) |}]

let%expect_test "opt out (mligo)" =
  run_ligo_bad [ "compile"; "contract"; bad_test "opt_out_dynamic_entrypoints.mligo" ];
  [%expect
    {|
  File "../../test/contracts/negative/opt_out_dynamic_entrypoints.mligo", line 7, characters 53-82:
    6 |
    7 | let two () () : operation list * unit = let _ = 1 in [%external ("OPT_OUT_ENTRY")]
                                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    8 |

  Illegal position for opted out entry.
   Only allowed in contracts "@dyn_entry" top-level declarations right-end side. |}]

let%expect_test "opt out (jsligo)" =
  run_ligo_bad [ "compile"; "contract"; bad_test "opt_out_dynamic_entrypoints.jsligo" ];
  [%expect
    {|
   File "../../test/contracts/negative/opt_out_dynamic_entrypoints.jsligo", line 9, characters 3-27:
     8 |   let _i = 1;
     9 |   (External `OPT_OUT_ENTRY`)
            ^^^^^^^^^^^^^^^^^^^^^^^^
    10 | }
 
   Illegal position for opted out entry.
    Only allowed in contracts "@dyn_entry" top-level declarations right-end side. |}]
