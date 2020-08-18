open Cli_expect

(* evaluate-value *)
let%expect_test _ =
  run_ligo_good [ "evaluate-value" ; "../../test/contracts/evaluation_tests.ligo" ; "a" ] ;
  [%expect {|
    record[bar -> "bar" , foo -> +0] |} ];

  run_ligo_good [ "evaluate-value" ; "../../test/contracts/evaluation_tests.ligo" ; "b" ] ;
  [%expect {|
    2 |} ]

(* list-declarations *)
let%expect_test _ =
  run_ligo_good [ "list-declarations" ; "../../test/contracts/loop.ligo" ] ;
  [%expect {|
    ../../test/contracts/loop.ligo declarations:
    inner_capture_in_conditional_block
    dummy
    nested_for_collection_local_var
    nested_for_collection
    for_collection_map_kv
    for_collection_empty
    for_collection_with_patches
    for_collection_comp_with_acc
    for_collection_proc_call
    for_collection_rhs_capture
    for_collection_if_and_local_var
    for_collection_set
    for_collection_list
    for_sum_step
    for_sum
    while_sum
    counter |} ];

  run_ligo_good [ "list-declarations" ; "../../test/contracts/loop.mligo" ; "--format=json" ] ;
  [%expect {|
    {"source_file":"../../test/contracts/loop.mligo","declarations":["counter_nest","aux_nest","counter","counter_simple","aux_simple"]} |} ];


  run_ligo_good [ "list-declarations" ; "../../test/contracts/loop.mligo" ] ;
  [%expect {|
    ../../test/contracts/loop.mligo declarations:
    counter_nest
    aux_nest
    counter
    counter_simple
    aux_simple |} ];

  run_ligo_good [ "list-declarations" ; "../../test/contracts/loop.religo" ] ;
  [%expect {|
    ../../test/contracts/loop.religo declarations:
    counter_nest
    aux_nest
    counter
    counter_simple
    aux_simple |} ];

  run_ligo_bad [ "compile-storage" ; "../../test/contracts/coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ; "--brief" ] ;
  [%expect {|
    ligo: error
          Invalid command line argument.
          The provided storage does not have the correct type for the contract.

          Invalid type(s).
          Expected: "record[card_patterns -> Map (nat , record[coefficient -> mutez , quantity -> nat]) , cards -> Map (nat , record[card_owner -> address , card_pattern -> nat]) , next_id -> nat]", but got: "
          sum[Buy_single -> record[card_to_buy -> nat] , Sell_single -> record[card_to_sell -> nat] , Transfer_single -> record[card_to_transfer -> nat , destination -> address]]". |}] ;