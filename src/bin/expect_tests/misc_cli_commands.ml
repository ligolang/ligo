open Cli_expect

(* evaluate-expr *)
let%expect_test _ =
  run_ligo_good ["run"; "evaluate-expr" ; "../../test/contracts/evaluation_tests.ligo" ; "--entry-point" ; "a" ] ;
  [%expect {|
    record[bar -> "bar" , foo -> +0] |} ];

  run_ligo_good ["run"; "evaluate-expr" ; "../../test/contracts/evaluation_tests.ligo" ; "--entry-point" ; "b" ] ;
  [%expect {|
    2 |} ]

(* list-declarations *)
let%expect_test _ =
  run_ligo_good [ "info"; "list-declarations" ; "../../test/contracts/loop.ligo" ] ;
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

  run_ligo_good [ "info"; "list-declarations" ; "../../test/contracts/loop.mligo" ; "--format" ;"json" ] ;
  [%expect {|
    {
      "source_file": "../../test/contracts/loop.mligo",
      "declarations": [
        {
          "name": "counter_nest",
          "counter": null,
          "location": [
            "File",
            {
              "start": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 31,
                  "pos_bol": 777,
                  "pos_cnum": 781
                },
                "point_num": 781,
                "point_bol": 777
              },
              "stop": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 31,
                  "pos_bol": 777,
                  "pos_cnum": 793
                },
                "point_num": 793,
                "point_bol": 777
              }
            }
          ]
        },
        {
          "name": "aux_nest",
          "counter": null,
          "location": [
            "File",
            {
              "start": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 24,
                  "pos_bol": 571,
                  "pos_cnum": 579
                },
                "point_num": 579,
                "point_bol": 571
              },
              "stop": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 24,
                  "pos_bol": 571,
                  "pos_cnum": 587
                },
                "point_num": 587,
                "point_bol": 571
              }
            }
          ]
        },
        {
          "name": "counter",
          "counter": null,
          "location": [
            "File",
            {
              "start": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 13,
                  "pos_bol": 234,
                  "pos_cnum": 238
                },
                "point_num": 238,
                "point_bol": 234
              },
              "stop": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 13,
                  "pos_bol": 234,
                  "pos_cnum": 245
                },
                "point_num": 245,
                "point_bol": 234
              }
            }
          ]
        },
        {
          "name": "counter_simple",
          "counter": null,
          "location": [
            "File",
            {
              "start": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 6,
                  "pos_bol": 127,
                  "pos_cnum": 131
                },
                "point_num": 131,
                "point_bol": 127
              },
              "stop": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 6,
                  "pos_bol": 127,
                  "pos_cnum": 145
                },
                "point_num": 145,
                "point_bol": 127
              }
            }
          ]
        },
        {
          "name": "aux_simple",
          "counter": null,
          "location": [
            "File",
            {
              "start": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 3,
                  "pos_bol": 45,
                  "pos_cnum": 53
                },
                "point_num": 53,
                "point_bol": 45
              },
              "stop": {
                "byte": {
                  "pos_fname": "../../test/contracts/loop.mligo",
                  "pos_lnum": 3,
                  "pos_bol": 45,
                  "pos_cnum": 63
                },
                "point_num": 63,
                "point_bol": 45
              }
            }
          ]
        }
      ]
    } |} ];


  run_ligo_good [ "info"; "list-declarations" ; "../../test/contracts/loop.mligo" ] ;
  [%expect {|
    ../../test/contracts/loop.mligo declarations:
    counter_nest
    aux_nest
    counter
    counter_simple
    aux_simple |} ];

  run_ligo_good ["info"; "list-declarations" ; "../../test/contracts/loop.religo" ] ;
  [%expect {|
    ../../test/contracts/loop.religo declarations:
    counter_nest
    aux_nest
    counter
    counter_simple
    aux_simple |} ];

  run_ligo_bad ["run" ; "interpret" ; "1" ; "--syntax"; "cameligo" ; "--protocol"; "do_not_exist" ] ;
  [%expect {|
    Invalid protocol version 'do_not_exist'. Available versions: edo , hangzhou |}] ;