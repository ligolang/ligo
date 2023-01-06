open Cli_expect

let base path =
  "../../../gitlab-pages/website/src/components/HomepageCodeExamples/" ^ path


let%expect_test _ =
  run_ligo_good [ "run"; "test"; base "cameligo.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_initial_storage exited with value ().
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; base "jsligo.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_initial_storage exited with value ().
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; base "pascaligo.ligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_initial_storage exited with value ().
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; base "cameligo.mligo" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; base "jsligo.jsligo" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; base "pascaligo.ligo" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]
