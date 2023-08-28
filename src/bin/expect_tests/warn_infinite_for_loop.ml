open Cli_expect

let%expect_test "for loop negative tests" =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop1.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop1.jsligo", line 4, character 2 to line 6, character 3:
      3 |   let x = 1;
      4 |   for ( ;;) {
            ^^^^^^^^^^^
      5 |     let _ = unit
          ^^^^^^^^^^^^^^^^
      6 |   };
          ^^^
      7 |   return [list([]), x]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF {} {} } ;
             PUSH int 1 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop2.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop2.jsligo", line 4, character 2 to line 6, character 3:
      3 |   let y = 0;
      4 |   for ( ; ; y++) {
            ^^^^^^^^^^^^^^^^
      5 |     let _ = unit;
          ^^^^^^^^^^^^^^^^^
      6 |   }
          ^^^
      7 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF { PUSH int 1 ; DIG 2 ; ADD ; SWAP } {} } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop3.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop3.jsligo", line 4, character 2 to line 6, character 3:
      3 |   let y = 0;
      4 |   for (let b = 0; ; ) {
            ^^^^^^^^^^^^^^^^^^^^^
      5 |     let _ = b;
          ^^^^^^^^^^^^^^
      6 |   }
          ^^^
      7 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF {} {} } ;
             PUSH int 0 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop4.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop4.jsligo", line 4, character 2 to line 6, character 3:
      3 |   let y = 0;
      4 |   for (y = 0; ; ) {
            ^^^^^^^^^^^^^^^^^
      5 |     let _ = y;
          ^^^^^^^^^^^^^^
      6 |   }
          ^^^
      7 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF {} {} } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop5.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop5.jsligo", line 4, character 2 to line 6, character 3:
      3 |   let x = 0;
      4 |   for (let y = 0 ; ; y++) {
            ^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |     let _ = y;
          ^^^^^^^^^^^^^^
      6 |   }
          ^^^
      7 |   return [list([]), x]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF { PUSH int 1 ; DIG 2 ; ADD ; SWAP } {} } ;
             DROP ;
             PUSH int 0 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop6.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop6.jsligo", line 4, character 2 to line 6, character 3:
      3 |   let y = 0;
      4 |   for (y = 0 ; ; y++) {
            ^^^^^^^^^^^^^^^^^^^^^
      5 |     let _ = y;
          ^^^^^^^^^^^^^^
      6 |   }
          ^^^
      7 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF { PUSH int 1 ; DIG 2 ; ADD ; SWAP } {} } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop7.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop7.jsligo", line 4, characters 2-13:
      3 |   let y = 0;
      4 |   for ( ; ; ) ;
            ^^^^^^^^^^^
      5 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF {} {} } ;
             PUSH int 0 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop8.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop8.jsligo", line 4, characters 2-16:
      3 |   let y = 0;
      4 |   for ( ; ; y++) ;
            ^^^^^^^^^^^^^^
      5 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF { PUSH int 1 ; DIG 2 ; ADD ; SWAP } {} } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop9.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop9.jsligo", line 4, characters 2-22:
      3 |   let y = 0;
      4 |   for (let _b = 0; ; ) ;
            ^^^^^^^^^^^^^^^^^^^^
      5 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF {} {} } ;
             PUSH int 0 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop10.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop10.jsligo", line 4, characters 2-17:
      3 |   let y = 0;
      4 |   for (y = 0; ; ) ;
            ^^^^^^^^^^^^^^^
      5 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF {} {} } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop11.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop11.jsligo", line 4, characters 2-25:
      3 |   let x = 0;
      4 |   for (let y = 0 ; ; y++) ;
            ^^^^^^^^^^^^^^^^^^^^^^^
      5 |   return [list([]), x]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF { PUSH int 1 ; DIG 2 ; ADD ; SWAP } {} } ;
             DROP ;
             PUSH int 0 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good
    [ "compile"
    ; "contract"
    ; test "for_loop/infinite_for_loop12.jsligo"
    ; "--warn-infinite-loop"
    ];
  [%expect
    {|
    File "../../test/contracts/for_loop/infinite_for_loop12.jsligo", line 4, characters 2-21:
      3 |   let y = 0;
      4 |   for (y = 0 ; ; y++) ;
            ^^^^^^^^^^^^^^^^^^^
      5 |   return [list([]), y]

    Warning: A boolean conditional expression is expected.
    Otherwise this leads to an infinte loop.

    { parameter unit ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH bool True ;
             LOOP { PUSH bool True ; DUP ; IF { PUSH int 1 ; DIG 2 ; ADD ; SWAP } {} } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_bad
    [ "compile"; "contract"; test "for_loop/for_loop_initialiser_scope.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/for_loop/for_loop_initialiser_scope.jsligo", line 6, characters 20-21:
      5 |   };
      6 |   return [list([]), c]
                              ^
      7 | };

    Variable "c" not found. |}]
