open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "dynamic_entrypoints_top_stdlib.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (unit %storage) (big_map %dynamic_entrypoints nat bytes)) ;
      code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; test "dynamic_entrypoints_context.mligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %storage) (big_map %dynamic_entrypoints nat bytes)) ;
      code { CDR ; PUSH int -1 ; UPDATE 1 ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "storage"; test "dynamic_entrypoints_context.mligo"; "0"; "-m"; "C" ];
  [%expect
    {|
    (Pair 0
          { Elt 0
                0x05020000004103200931000000350765035b07610362036909650000000c055f036d035b0761036203690000000002000000100743035b002a05500001053d036d034200000000 }) |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "dynamic_entry_wrong_storage.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/dynamic_entry_wrong_storage.mligo", line 9, characters 18-25:
      8 | [@entry]
      9 |   let foo () (_ : storage) : operation list * storage = failwith ()
                            ^^^^^^^

     Wrong dynamic entrypoints storage definition record[dynamic_entrypoints -> big_map (nat , bytes) ,
                                                         extra -> int ,
                                                         storage -> int].
     We expect two fields "dynamic_entrypoint" and "storage" |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "dynamic_entry_wrong_storage.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/dynamic_entry_wrong_storage.jsligo", line 9, characters 37-44:
        8 | @entry
        9 |   const foo = (_u : unit, _storage : storage) : [list<operation>, storage] => failwith ()
                                                 ^^^^^^^

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
  File "../../test/contracts/dynamic_entrypoints_tests.mligo", line 30, characters 13-27:
   29 |   let init_storage = Test.storage_with_dynamic_entrypoints (contract_of  C) 42 in
   30 |   let orig = Test.originate (contract_of C) init_storage 0mutez in
                     ^^^^^^^^^^^^^^
   31 |   (* Call initial one *)
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

  File "../../test/contracts/dynamic_entrypoints_tests.mligo", line 32, characters 10-27:
   31 |   (* Call initial one *)
   32 |   let _ = Test.transfer_exn orig.addr (Call_one ()) 1mutez in
                  ^^^^^^^^^^^^^^^^^
   33 |   let () = assert ((Test.get_storage orig.addr).storage = 1) in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

  File "../../test/contracts/dynamic_entrypoints_tests.mligo", line 33, characters 20-36:
   32 |   let _ = Test.transfer_exn orig.addr (Call_one ()) 1mutez in
   33 |   let () = assert ((Test.get_storage orig.addr).storage = 1) in
                            ^^^^^^^^^^^^^^^^
   34 |   (* Change initial one and call it *)
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

  File "../../test/contracts/dynamic_entrypoints_tests.mligo", line 36, characters 10-27:
   35 |   let f = fun () (i : int) : operation list * int -> [], i + 1 in
   36 |   let _ = Test.transfer_exn orig.addr (Set_one f) 1mutez in
                  ^^^^^^^^^^^^^^^^^
   37 |   let _ = Test.transfer_exn orig.addr (Call_one ()) 1mutez in
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

  File "../../test/contracts/dynamic_entrypoints_tests.mligo", line 37, characters 10-27:
   36 |   let _ = Test.transfer_exn orig.addr (Set_one f) 1mutez in
   37 |   let _ = Test.transfer_exn orig.addr (Call_one ()) 1mutez in
                  ^^^^^^^^^^^^^^^^^
   38 |   assert ((Test.get_storage orig.addr).storage = 2)
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

  File "../../test/contracts/dynamic_entrypoints_tests.mligo", line 38, characters 11-27:
   37 |   let _ = Test.transfer_exn orig.addr (Call_one ()) 1mutez in
   38 |   assert ((Test.get_storage orig.addr).storage = 2)
                   ^^^^^^^^^^^^^^^^
  :
  Warning: deprecated value.
  In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

  Everything at the top-level was executed.
  - test_dyn exited with value (). |}]

let%expect_test "dynamic entrypoints test (jsligo)" =
  run_ligo_good [ "run"; "test"; test "dynamic_entrypoints_tests.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/dynamic_entrypoints_tests.jsligo", line 33, characters 15-29:
     32 |   const init_storage = Test.storage_with_dynamic_entrypoints(contract_of(C), 42);
     33 |   const orig = Test.originate (contract_of(C), init_storage, 0mutez);
                         ^^^^^^^^^^^^^^
     34 |   /* Call initial one */
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/dynamic_entrypoints_tests.jsligo", line 35, characters 2-19:
     34 |   /* Call initial one */
     35 |   Test.transfer_exn (orig.addr, Call_one(), 1mutez);
            ^^^^^^^^^^^^^^^^^
     36 |   assert ((Test.get_storage(orig.addr)).storage == 1);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/dynamic_entrypoints_tests.jsligo", line 36, characters 11-27:
     35 |   Test.transfer_exn (orig.addr, Call_one(), 1mutez);
     36 |   assert ((Test.get_storage(orig.addr)).storage == 1);
                     ^^^^^^^^^^^^^^^^
     37 |   /* Change initial one and call it */
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/dynamic_entrypoints_tests.jsligo", line 39, characters 2-19:
     38 |   const f = (_ : unit, i : int) : [list<operation>, int] => [list([]), i + 1];
     39 |   Test.transfer_exn (orig.addr, (Set_one(f)), 1mutez);
            ^^^^^^^^^^^^^^^^^
     40 |   Test.transfer_exn (orig.addr, (Call_one()), 1mutez);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/dynamic_entrypoints_tests.jsligo", line 40, characters 2-19:
     39 |   Test.transfer_exn (orig.addr, (Set_one(f)), 1mutez);
     40 |   Test.transfer_exn (orig.addr, (Call_one()), 1mutez);
            ^^^^^^^^^^^^^^^^^
     41 |   assert ((Test.get_storage(orig.addr)).storage == 2);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/dynamic_entrypoints_tests.jsligo", line 41, characters 11-27:
     40 |   Test.transfer_exn (orig.addr, (Call_one()), 1mutez);
     41 |   assert ((Test.get_storage(orig.addr)).storage == 2);
                     ^^^^^^^^^^^^^^^^
     42 |   return []
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

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
