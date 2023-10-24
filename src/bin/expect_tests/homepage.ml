open Cli_expect

let base path =
  "../../../gitlab-pages/website/src/components/HomepageCodeExamples/" ^ path


let%expect_test _ =
  run_ligo_good [ "run"; "test"; base "cameligo.mligo" ];
  [%expect {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; base "jsligo.jsligo" ];
  [%expect {|
    Everything at the top-level was executed. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; base "cameligo.mligo"; ];
  [%expect
    {|
    { parameter (or (unit %reset) (or (int %decrement) (int %increment))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { DROP 2 ; PUSH int 0 } { IF_LEFT { SWAP ; SUB } { ADD } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; base "jsligo.jsligo"; ];
  [%expect
    {|
    { parameter (or (unit %reset) (or (int %decrement) (int %increment))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { DROP 2 ; PUSH int 0 } { IF_LEFT { SWAP ; SUB } { ADD } } ;
             NIL operation ;
             PAIR } } |}]
