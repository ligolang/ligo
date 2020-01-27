open Cli_expect

(* evaluate-value *)
let%expect_test _ =
  run_ligo_good [ "evaluate-value" ; "../../test/contracts/evaluation_tests.ligo" ; "a" ] ;
  [%expect {|
    {foo = +0 , bar = "bar"} |} ];

  run_ligo_good [ "evaluate-value" ; "../../test/contracts/evaluation_tests.ligo" ; "b" ] ;
  [%expect {|
    2 |} ]

(* list-declarations *)
let%expect_test _ =
  run_ligo_good [ "list-declarations" ; "../../test/contracts/loop.ligo" ] ;
  [%expect {| {"source_file":"../../test/contracts/loop.ligo","declarations":["inner_capture_in_conditional_block","dummy","nested_for_collection_local_var","nested_for_collection","for_collection_map_k","for_collection_map_kv","for_collection_empty","for_collection_with_patches","for_collection_comp_with_acc","for_collection_proc_call","for_collection_rhs_capture","for_collection_if_and_local_var","for_collection_set","for_collection_list","for_sum","while_sum","counter"]} |} ];

  run_ligo_good [ "list-declarations" ; "../../test/contracts/loop.mligo" ] ;
  [%expect {| {"source_file":"../../test/contracts/loop.mligo","declarations":["counter_nest","aux_nest","counter","counter_simple","aux_simple"]} |} ];

  run_ligo_good [ "list-declarations" ; "../../test/contracts/loop.religo" ] ;
  [%expect {| {"source_file":"../../test/contracts/loop.religo","declarations":["counter_nest","aux_nest","counter","counter_simple","aux_simple"]} |} ];