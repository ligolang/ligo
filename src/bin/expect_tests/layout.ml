open Cli_expect

let layout = "../../test/contracts/negative/layout.mligo"

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; layout ];
  [%expect {|
    File "../../test/contracts/negative/layout.mligo", line 17, character 2 to line 23, character 6:
     16 | let main ((p,s) : unit * storage) : return =
     17 |   let s : storage1 = {
     18 |     tata = 0;
     19 |     toto = 1;
     20 |     titi = 2;
     21 |     tutu = 3;
     22 |   } in
     23 |   [],s

    Invalid type(s)
    Cannot unify "storage1" with "storage" due to differing layouts (tree and comb). |}]