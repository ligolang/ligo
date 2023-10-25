open Cli_expect

let contract basename = "../../test/contracts/signature/" ^ basename

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "BI.f 42n"
    ; "--init-file"
    ; contract "simple.mligo"
    ];
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "Bar.Foo.x"
    ; "--init-file"
    ; contract "spath.mligo"
    ];
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "X.x"
    ; "--init-file"
    ; contract "interface.jsligo"
    ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl0" ];
  [%expect
    {|
    { parameter int ;
      storage nat ;
      code { UNPAIR ; ADD ; ABS ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl1" ];
  [%expect
    {|
    { parameter (or (int %extra) (int %add)) ;
      storage int ;
      code { UNPAIR ; IF_LEFT { SUB } { ADD } ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl2" ];
  [%expect
    {|
    { parameter (or (int %extra) (int %add)) ;
      storage nat ;
      code { UNPAIR ; IF_LEFT { SUB } { ADD } ; ABS ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl3" ];
  [%expect
    {|
    { parameter int ;
      storage nat ;
      code { UNPAIR ; ADD ; ABS ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl4" ];
  [%expect
    {|
    { parameter int ;
      storage nat ;
      code { UNPAIR ; ADD ; ABS ; NIL operation ; PAIR } } |}]
