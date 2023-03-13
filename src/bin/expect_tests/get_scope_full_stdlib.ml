open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s
let () = Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:"true"

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Test#694 assert_none_with_error#693 assert_some_with_error#690 assert_with_error#687 assert_none#684 assert_some#682 assert#680 originate_from_file_and_mutate_all#678 originate_from_file_and_mutate#651 mutation_test_all#625 mutation_test#609 originate_from_file#594 compile_contract_from_file#585 originate_module#580 originate_uncurried#570 compile_contract_with_views#561 originate#557 originate_contract#548 to_entrypoint#544 michelson_equal#540 transfer_to_contract_exn#537 transfer_to_contract#530 create_chest_key#523 create_chest#520 set_big_map#517 baker_account#514 add_account#511 sign#508 save_mutation#505 mutate_value#502 bootstrap_contract#499 reset_state_at#495 reset_state#491 log#488 transfer_exn#484 transfer#480 get_last_events_from#476 PBT#467 run#466 make_test#458 gen_small#455 gen#454 unset_print_values#453 set_print_values#452 println#451 nl#449 chr#448 read_contract_from_file#445 compile_contract#443 size#439 set_baker#437 set_baker_policy#435 get_storage#433 to_json#428 to_string#426 drop_context#424 save_context#422 restore_context#420 parse_michelson#418 constant_to_michelson_program#416 to_typed_address#414 register_constant#412 register_delegate#410 cast_address#408 get_time#406 bake_until_n_cycle_end#404 decompile#402 new_account#400 random#398 last_originations#395 nth_bootstrap_typed_address#393 get_bootstrap_account#391 nth_bootstrap_account#389 nth_bootstrap_contract#386 get_voting_power#384 eprint#382 print#380 get_balance#378 get_storage_of_address#376 set_source#374 to_contract#372 failwith#370 get_total_voting_power#368 compile_value#366 eval#364 run#361 unforged_ticket#358 pbt_result#357 pbt_test#356 test_baker_policy#355 test_exec_result#354 test_exec_error#353 test_exec_error_balance_too_low#352 ediv#351 assert_none_with_error#348 assert_some_with_error#345 assert_with_error#342 uncurry#339 curry#336 ignore#332 int#331 unit#329 false#328 true#327 is_nat#326 abs#324 assert_none#322 assert_some#320 assert#318 Crypto#316 check#315 hash_key#311 keccak#309 sha3#307 sha512#305 sha256#303 blake2b#301 Bytes#299 sub#298 concat#294 length#291 unpack#289 pack#287 concats#285 Option#283 is_some#282 is_none#280 value_exn#278 value#274 map#270 unopt_with_error#267 unopt#263 String#260 sub#259 concat#255 concats#252 length#250 List#248 find_opt#247 cons#243 fold_right#240 fold_left#236 fold#232 iter#228 map#225 tail_opt#222 head_opt#219 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ a#695 Test#694 assert_none_with_error#693 assert_some_with_error#690 assert_with_error#687 assert_none#684 assert_some#682 assert#680 originate_from_file_and_mutate_all#678 originate_from_file_and_mutate#651 mutation_test_all#625 mutation_test#609 originate_from_file#594 compile_contract_from_file#585 originate_module#580 originate_uncurried#570 compile_contract_with_views#561 originate#557 originate_contract#548 to_entrypoint#544 michelson_equal#540 transfer_to_contract_exn#537 transfer_to_contract#530 create_chest_key#523 create_chest#520 set_big_map#517 baker_account#514 add_account#511 sign#508 save_mutation#505 mutate_value#502 bootstrap_contract#499 reset_state_at#495 reset_state#491 log#488 transfer_exn#484 transfer#480 get_last_events_from#476 PBT#467 run#466 make_test#458 gen_small#455 gen#454 unset_print_values#453 set_print_values#452 println#451 nl#449 chr#448 read_contract_from_file#445 compile_contract#443 size#439 set_baker#437 set_baker_policy#435 get_storage#433 to_json#428 to_string#426 drop_context#424 save_context#422 restore_context#420 parse_michelson#418 constant_to_michelson_program#416 to_typed_address#414 register_constant#412 register_delegate#410 cast_address#408 get_time#406 bake_until_n_cycle_end#404 decompile#402 new_account#400 random#398 last_originations#395 nth_bootstrap_typed_address#393 get_bootstrap_account#391 nth_bootstrap_account#389 nth_bootstrap_contract#386 get_voting_power#384 eprint#382 print#380 get_balance#378 get_storage_of_address#376 set_source#374 to_contract#372 failwith#370 get_total_voting_power#368 compile_value#366 eval#364 run#361 unforged_ticket#358 pbt_result#357 pbt_test#356 test_baker_policy#355 test_exec_result#354 test_exec_error#353 test_exec_error_balance_too_low#352 ediv#351 assert_none_with_error#348 assert_some_with_error#345 assert_with_error#342 uncurry#339 curry#336 ignore#332 int#331 unit#329 false#328 true#327 is_nat#326 abs#324 assert_none#322 assert_some#320 assert#318 Crypto#316 check#315 hash_key#311 keccak#309 sha3#307 sha512#305 sha256#303 blake2b#301 Bytes#299 sub#298 concat#294 length#291 unpack#289 pack#287 concats#285 Option#283 is_some#282 is_none#280 value_exn#278 value#274 map#270 unopt_with_error#267 unopt#263 String#260 sub#259 concat#255 concats#252 length#250 List#248 find_opt#247 cons#243 fold_right#240 fold_left#236 fold#232 iter#228 map#225 tail_opt#222 head_opt#219 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 14
    [ c#696 a#695 Test#694 assert_none_with_error#693 assert_some_with_error#690 assert_with_error#687 assert_none#684 assert_some#682 assert#680 originate_from_file_and_mutate_all#678 originate_from_file_and_mutate#651 mutation_test_all#625 mutation_test#609 originate_from_file#594 compile_contract_from_file#585 originate_module#580 originate_uncurried#570 compile_contract_with_views#561 originate#557 originate_contract#548 to_entrypoint#544 michelson_equal#540 transfer_to_contract_exn#537 transfer_to_contract#530 create_chest_key#523 create_chest#520 set_big_map#517 baker_account#514 add_account#511 sign#508 save_mutation#505 mutate_value#502 bootstrap_contract#499 reset_state_at#495 reset_state#491 log#488 transfer_exn#484 transfer#480 get_last_events_from#476 PBT#467 run#466 make_test#458 gen_small#455 gen#454 unset_print_values#453 set_print_values#452 println#451 nl#449 chr#448 read_contract_from_file#445 compile_contract#443 size#439 set_baker#437 set_baker_policy#435 get_storage#433 to_json#428 to_string#426 drop_context#424 save_context#422 restore_context#420 parse_michelson#418 constant_to_michelson_program#416 to_typed_address#414 register_constant#412 register_delegate#410 cast_address#408 get_time#406 bake_until_n_cycle_end#404 decompile#402 new_account#400 random#398 last_originations#395 nth_bootstrap_typed_address#393 get_bootstrap_account#391 nth_bootstrap_account#389 nth_bootstrap_contract#386 get_voting_power#384 eprint#382 print#380 get_balance#378 get_storage_of_address#376 set_source#374 to_contract#372 failwith#370 get_total_voting_power#368 compile_value#366 eval#364 run#361 unforged_ticket#358 pbt_result#357 pbt_test#356 test_baker_policy#355 test_exec_result#354 test_exec_error#353 test_exec_error_balance_too_low#352 ediv#351 assert_none_with_error#348 assert_some_with_error#345 assert_with_error#342 uncurry#339 curry#336 ignore#332 int#331 unit#329 false#328 true#327 is_nat#326 abs#324 assert_none#322 assert_some#320 assert#318 Crypto#316 check#315 hash_key#311 keccak#309 sha3#307 sha512#305 sha256#303 blake2b#301 Bytes#299 sub#298 concat#294 length#291 unpack#289 pack#287 concats#285 Option#283 is_some#282 is_none#280 value_exn#278 value#274 map#270 unopt_with_error#267 unopt#263 String#260 sub#259 concat#255 concats#252 length#250 List#248 find_opt#247 cons#243 fold_right#240 fold_left#236 fold#232 iter#228 map#225 tail_opt#222 head_opt#219 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ d#697 c#696 a#695 Test#694 assert_none_with_error#693 assert_some_with_error#690 assert_with_error#687 assert_none#684 assert_some#682 assert#680 originate_from_file_and_mutate_all#678 originate_from_file_and_mutate#651 mutation_test_all#625 mutation_test#609 originate_from_file#594 compile_contract_from_file#585 originate_module#580 originate_uncurried#570 compile_contract_with_views#561 originate#557 originate_contract#548 to_entrypoint#544 michelson_equal#540 transfer_to_contract_exn#537 transfer_to_contract#530 create_chest_key#523 create_chest#520 set_big_map#517 baker_account#514 add_account#511 sign#508 save_mutation#505 mutate_value#502 bootstrap_contract#499 reset_state_at#495 reset_state#491 log#488 transfer_exn#484 transfer#480 get_last_events_from#476 PBT#467 run#466 make_test#458 gen_small#455 gen#454 unset_print_values#453 set_print_values#452 println#451 nl#449 chr#448 read_contract_from_file#445 compile_contract#443 size#439 set_baker#437 set_baker_policy#435 get_storage#433 to_json#428 to_string#426 drop_context#424 save_context#422 restore_context#420 parse_michelson#418 constant_to_michelson_program#416 to_typed_address#414 register_constant#412 register_delegate#410 cast_address#408 get_time#406 bake_until_n_cycle_end#404 decompile#402 new_account#400 random#398 last_originations#395 nth_bootstrap_typed_address#393 get_bootstrap_account#391 nth_bootstrap_account#389 nth_bootstrap_contract#386 get_voting_power#384 eprint#382 print#380 get_balance#378 get_storage_of_address#376 set_source#374 to_contract#372 failwith#370 get_total_voting_power#368 compile_value#366 eval#364 run#361 unforged_ticket#358 pbt_result#357 pbt_test#356 test_baker_policy#355 test_exec_result#354 test_exec_error#353 test_exec_error_balance_too_low#352 ediv#351 assert_none_with_error#348 assert_some_with_error#345 assert_with_error#342 uncurry#339 curry#336 ignore#332 int#331 unit#329 false#328 true#327 is_nat#326 abs#324 assert_none#322 assert_some#320 assert#318 Crypto#316 check#315 hash_key#311 keccak#309 sha3#307 sha512#305 sha256#303 blake2b#301 Bytes#299 sub#298 concat#294 length#291 unpack#289 pack#287 concats#285 Option#283 is_some#282 is_none#280 value_exn#278 value#274 map#270 unopt_with_error#267 unopt#263 String#260 sub#259 concat#255 concats#252 length#250 List#248 find_opt#247 cons#243 fold_right#240 fold_left#236 fold#232 iter#228 map#225 tail_opt#222 head_opt#219 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ e#698 a#695 Test#694 assert_none_with_error#693 assert_some_with_error#690 assert_with_error#687 assert_none#684 assert_some#682 assert#680 originate_from_file_and_mutate_all#678 originate_from_file_and_mutate#651 mutation_test_all#625 mutation_test#609 originate_from_file#594 compile_contract_from_file#585 originate_module#580 originate_uncurried#570 compile_contract_with_views#561 originate#557 originate_contract#548 to_entrypoint#544 michelson_equal#540 transfer_to_contract_exn#537 transfer_to_contract#530 create_chest_key#523 create_chest#520 set_big_map#517 baker_account#514 add_account#511 sign#508 save_mutation#505 mutate_value#502 bootstrap_contract#499 reset_state_at#495 reset_state#491 log#488 transfer_exn#484 transfer#480 get_last_events_from#476 PBT#467 run#466 make_test#458 gen_small#455 gen#454 unset_print_values#453 set_print_values#452 println#451 nl#449 chr#448 read_contract_from_file#445 compile_contract#443 size#439 set_baker#437 set_baker_policy#435 get_storage#433 to_json#428 to_string#426 drop_context#424 save_context#422 restore_context#420 parse_michelson#418 constant_to_michelson_program#416 to_typed_address#414 register_constant#412 register_delegate#410 cast_address#408 get_time#406 bake_until_n_cycle_end#404 decompile#402 new_account#400 random#398 last_originations#395 nth_bootstrap_typed_address#393 get_bootstrap_account#391 nth_bootstrap_account#389 nth_bootstrap_contract#386 get_voting_power#384 eprint#382 print#380 get_balance#378 get_storage_of_address#376 set_source#374 to_contract#372 failwith#370 get_total_voting_power#368 compile_value#366 eval#364 run#361 unforged_ticket#358 pbt_result#357 pbt_test#356 test_baker_policy#355 test_exec_result#354 test_exec_error#353 test_exec_error_balance_too_low#352 ediv#351 assert_none_with_error#348 assert_some_with_error#345 assert_with_error#342 uncurry#339 curry#336 ignore#332 int#331 unit#329 false#328 true#327 is_nat#326 abs#324 assert_none#322 assert_some#320 assert#318 Crypto#316 check#315 hash_key#311 keccak#309 sha3#307 sha512#305 sha256#303 blake2b#301 Bytes#299 sub#298 concat#294 length#291 unpack#289 pack#287 concats#285 Option#283 is_some#282 is_none#280 value_exn#278 value#274 map#270 unopt_with_error#267 unopt#263 String#260 sub#259 concat#255 concats#252 length#250 List#248 find_opt#247 cons#243 fold_right#240 fold_left#236 fold#232 iter#228 map#225 tail_opt#222 head_opt#219 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30

    Variable definitions:
    (a#695 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#324 -> abs)
    Range: File "", line 191, characters 4-7
    Body Range: File "", line 191, characters 9-10
    Content: |core: int -> nat|
    references: File "", line 355, characters 31-34
    (assert#318 -> assert)
    Range: File "", line 188, characters 4-10
    Body Range: File "", line 188, characters 12-13
    Content: |core: bool -> unit|
    references: []
    (assert_none#322 -> assert_none)
    Range: File "", line 190, characters 4-15
    Body Range: File "", line 190, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#348 -> assert_none_with_error)
    Range: File "", line 203, characters 4-26
    Body Range: File "", line 203, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#320 -> assert_some)
    Range: File "", line 189, characters 4-15
    Body Range: File "", line 189, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#345 -> assert_some_with_error)
    Range: File "", line 202, characters 4-26
    Body Range: File "", line 202, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#342 -> assert_with_error)
    Range: File "", line 201, characters 4-21
    Body Range: File "", line 201, characters 23-24
    Content: |unresolved|
    references: []
    (b#699 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#696 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (curry#336 -> curry)
    Range: File "", line 198, characters 4-9
    Body Range: File "", line 198, characters 10-22
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    (d#697 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#698 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    (ediv#351 -> ediv)
    Range: File "", line 204, characters 4-8
    Body Range: File "", line 204, characters 9-19
    Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_ediv (a ,
    b)|
    references: []
    (failwith#0 -> failwith)
    Range: File "", line 2, characters 4-12
    Body Range: File "", line 2, characters 13-23
    Content: |unresolved|
    references:
      File "", line 38, characters 27-35 ,
      File "", line 42, characters 27-35 ,
      File "", line 69, characters 27-35 ,
      File "", line 157, characters 79-87 ,
      File "", line 159, characters 103-111 ,
      File "", line 162, characters 83-91 ,
      File "", line 188, characters 49-57 ,
      File "", line 189, characters 72-80 ,
      File "", line 190, characters 87-95 ,
      File "", line 201, characters 66-74 ,
      File "", line 202, characters 96-104 ,
      File "", line 203, characters 111-119
    (false#328 -> false)
    Range: File "", line 194, characters 4-9
    Body Range: File "", line 194, characters 19-24
    Content: |core: bool|
    references:
      File "", line 250, characters 51-56 ,
      File "", line 295, characters 90-95 ,
      File "", line 298, characters 62-67
    (ignore#332 -> ignore)
    Range: File "", line 197, characters 4-10
    Body Range: File "", line 197, characters 11-19
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#331 -> int)
    Range: File "", line 196, characters 4-7
    Body Range: File "", line 196, characters 8-16
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 246, characters 97-100 ,
      File "", line 283, characters 79-82 ,
      File "", line 285, characters 78-81 ,
      File "", line 287, characters 72-75
    (is_nat#326 -> is_nat)
    Range: File "", line 192, characters 4-10
    Body Range: File "", line 192, characters 12-13
    Content: |core: int -> option (nat)|
    references: []
    (true#327 -> true)
    Range: File "", line 193, characters 4-8
    Body Range: File "", line 193, characters 18-22
    Content: |core: bool|
    references:
      File "", line 294, characters 88-92 ,
      File "", line 299, characters 68-72
    (uncurry#339 -> uncurry)
    Range: File "", line 199, characters 4-11
    Body Range: File "", line 199, characters 12-24
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 361, characters 30-37
    (unit#329 -> unit)
    Range: File "", line 195, characters 4-8
    Body Range: File "", line 195, characters 18-38
    Content: |core: unit|
    references: []
    Type definitions:
    (bool#1 -> bool)
    Range: File "", line 4, characters 5-9
    Body Range: File "", line 4, characters 12-24
    Content: : |sum[False -> unit , True -> unit]|
    references:
      File "", line 26, characters 63-67 ,
      File "", line 26, characters 147-151 ,
      File "", line 89, characters 52-56 ,
      File "", line 104, characters 48-52 ,
      File "", line 123, characters 41-45 ,
      File "", line 126, characters 35-39 ,
      File "", line 144, characters 34-38 ,
      File "", line 163, characters 40-44 ,
      File "", line 164, characters 40-44 ,
      File "", line 185, characters 52-56 ,
      File "", line 185, characters 145-149 ,
      File "", line 188, characters 16-20 ,
      File "", line 193, characters 11-15 ,
      File "", line 194, characters 12-16 ,
      File "", line 201, characters 27-31 ,
      File "", line 222, characters 41-45 ,
      File "", line 300, characters 53-57 ,
      File "", line 350, characters 74-78 ,
      File "", line 457, characters 18-22 ,
      File "", line 461, characters 29-33
    (option#2 -> option)
    Range: File "", line 5, characters 8-14
    Body Range: File "", line 5, characters 0-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 22, characters 56-73 ,
      File "", line 22, characters 137-152 ,
      File "", line 27, characters 24-39 ,
      File "", line 27, characters 93-108 ,
      File "", line 29, characters 12-20 ,
      File "", line 34, characters 67-86 ,
      File "", line 35, characters 60-79 ,
      File "", line 47, characters 49-66 ,
      File "", line 47, characters 121-138 ,
      File "", line 55, characters 84-92 ,
      File "", line 56, characters 81-89 ,
      File "", line 57, characters 61-89 ,
      File "", line 58, characters 77-105 ,
      File "", line 59, characters 93-108 ,
      File "", line 62, characters 102-117 ,
      File "", line 64, characters 82-99 ,
      File "", line 66, characters 71-90 ,
      File "", line 73, characters 120-164 ,
      File "", line 73, characters 278-322 ,
      File "", line 92, characters 37-45 ,
      File "", line 93, characters 45-53 ,
      File "", line 93, characters 78-86 ,
      File "", line 94, characters 57-65 ,
      File "", line 107, characters 37-45 ,
      File "", line 108, characters 45-53 ,
      File "", line 108, characters 74-82 ,
      File "", line 110, characters 53-61 ,
      File "", line 135, characters 40-48 ,
      File "", line 136, characters 40-55 ,
      File "", line 144, characters 56-64 ,
      File "", line 145, characters 29-37 ,
      File "", line 157, characters 26-34 ,
      File "", line 159, characters 37-45 ,
      File "", line 160, characters 48-56 ,
      File "", line 160, characters 60-68 ,
      File "", line 161, characters 40-48 ,
      File "", line 162, characters 46-54 ,
      File "", line 163, characters 28-36 ,
      File "", line 164, characters 28-36 ,
      File "", line 170, characters 36-44 ,
      File "", line 170, characters 99-107 ,
      File "", line 189, characters 30-38 ,
      File "", line 190, characters 30-38 ,
      File "", line 192, characters 23-33 ,
      File "", line 192, characters 74-84 ,
      File "", line 202, characters 41-49 ,
      File "", line 203, characters 41-49 ,
      File "", line 280, characters 22-35 ,
      File "", line 323, characters 140-153 ,
      File "", line 324, characters 135-148 ,
      File "", line 329, characters 92-108 ,
      File "", line 332, characters 48-69 ,
      File "", line 333, characters 50-63 ,
      File "", line 336, characters 44-54 ,
      File "", line 342, characters 12-25 ,
      File "", line 347, characters 14-27 ,
      File "", line 385, characters 96-106 ,
      File "", line 392, characters 59-80 ,
      File "", line 395, characters 37-58 ,
      File "", line 417, characters 90-111 ,
      File "", line 423, characters 96-106 ,
      File "", line 426, characters 37-58 ,
      File "", line 443, characters 96-106 ,
      File "", line 458, characters 32-40 ,
      File "", line 459, characters 32-40 ,
      File "", line 462, characters 43-51 ,
      File "", line 463, characters 43-51
    (pbt_result#357 -> pbt_result)
    Range: File "", line 223, characters 8-18
    Body Range: File "", line 223, characters 0-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 301, characters 55-67 ,
      File "", line 302, characters 37-49 ,
      File "", line 304, characters 82-94 ,
      File "", line 308, characters 94-106 ,
      File "", line 311, characters 66-78
    (pbt_test#356 -> pbt_test)
    Range: File "", line 222, characters 8-16
    Body Range: File "", line 222, characters 0-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 300, characters 61-71 ,
      File "", line 301, characters 31-41
    (test_baker_policy#355 -> test_baker_policy)
    Range: File "", line 217, characters 5-22
    Body Range: File "", line 218, character 4 to line 220, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 272, characters 29-46
    (test_exec_error#353 -> test_exec_error)
    Range: File "", line 210, characters 5-20
    Body Range: File "", line 211, character 4 to line 213, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 215, characters 49-64
    (test_exec_error_balance_too_low#352 -> test_exec_error_balance_too_low)
    Range: File "", line 207, characters 5-36
    Body Range: File "", line 208, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 212, characters 23-54
    (test_exec_result#354 -> test_exec_result)
    Range: File "", line 215, characters 5-21
    Body Range: File "", line 215, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 323, characters 65-81 ,
      File "", line 340, characters 73-89
    (unforged_ticket#358 -> unforged_ticket)
    Range: File "", line 225, characters 8-23
    Body Range: File "", line 225, characters 0-91
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    Module definitions:
    (Big_map#140 -> Big_map)
    Range: File "", line 85, characters 7-14
    Body Range: File "", line 85, character 0 to line 97, character 3
    Content: Members: Variable definitions:
                      (add#122 -> add)
                      Range: File "", line 90, characters 6-9
                      Body Range: File "", line 90, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (empty#113 -> empty)
                      Range: File "", line 86, characters 16-21
                      Body Range: File "", line 86, characters 22-32
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#139 -> find)
                      Range: File "", line 95, characters 6-10
                      Body Range: File "", line 95, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      (find_opt#136 -> find_opt)
                      Range: File "", line 94, characters 6-14
                      Body Range: File "", line 94, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (get_and_update#133 -> get_and_update)
                      Range: File "", line 93, characters 6-20
                      Body Range: File "", line 93, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#115 -> literal)
                      Range: File "", line 87, characters 25-32
                      Body Range: File "", line 87, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#118 -> mem)
                      Range: File "", line 89, characters 6-9
                      Body Range: File "", line 89, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (remove#125 -> remove)
                      Range: File "", line 91, characters 6-12
                      Body Range: File "", line 91, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#129 -> update)
                      Range: File "", line 92, characters 6-12
                      Body Range: File "", line 92, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#112 -> Bitwise)
    Range: File "", line 77, characters 7-14
    Body Range: File "", line 77, character 0 to line 83, character 3
    Content: Members: Variable definitions:
                      (and#99 -> and)
                      Range: File "", line 78, characters 6-10
                      Body Range: File "", line 78, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (or#105 -> or)
                      Range: File "", line 80, characters 6-9
                      Body Range: File "", line 80, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_left#108 -> shift_left)
                      Range: File "", line 81, characters 6-16
                      Body Range: File "", line 81, characters 18-19
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_right#111 -> shift_right)
                      Range: File "", line 82, characters 6-17
                      Body Range: File "", line 82, characters 19-20
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (xor#102 -> xor)
                      Range: File "", line 79, characters 6-9
                      Body Range: File "", line 79, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#299 -> Bytes)
    Range: File "", line 167, characters 7-12
    Body Range: File "", line 167, character 0 to line 175, character 3
    Content: Members: Variable definitions:
                      (concat#294 -> concat)
                      Range: File "", line 173, characters 6-12
                      Body Range: File "", line 173, characters 14-16
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#285 -> concats)
                      Range: File "", line 168, characters 6-13
                      Body Range: File "", line 168, characters 15-17
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#291 -> length)
                      Range: File "", line 171, characters 6-12
                      Body Range: File "", line 171, characters 14-15
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#287 -> pack)
                      Range: File "", line 169, characters 6-10
                      Body Range: File "", line 169, characters 11-19
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#298 -> sub)
                      Range: File "", line 174, characters 6-9
                      Body Range: File "", line 174, characters 11-12
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#289 -> unpack)
                      Range: File "", line 170, characters 6-12
                      Body Range: File "", line 170, characters 13-21
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#316 -> Crypto)
    Range: File "", line 177, characters 7-13
    Body Range: File "", line 177, character 0 to line 186, character 3
    Content: Members: Variable definitions:
                      (blake2b#301 -> blake2b)
                      Range: File "", line 178, characters 6-13
                      Body Range: File "", line 178, characters 15-16
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#315 -> check)
                      Range: File "", line 185, characters 6-11
                      Body Range: File "", line 185, characters 13-14
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#311 -> hash_key)
                      Range: File "", line 183, characters 6-14
                      Body Range: File "", line 183, characters 16-17
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#309 -> keccak)
                      Range: File "", line 182, characters 6-12
                      Body Range: File "", line 182, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#303 -> sha256)
                      Range: File "", line 179, characters 6-12
                      Body Range: File "", line 179, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#307 -> sha3)
                      Range: File "", line 181, characters 6-10
                      Body Range: File "", line 181, characters 12-13
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#305 -> sha512)
                      Range: File "", line 180, characters 6-12
                      Body Range: File "", line 180, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#248 -> List)
    Range: File "", line 132, characters 7-11
    Body Range: File "", line 132, character 0 to line 146, character 3
    Content: Members: Variable definitions:
                      (cons#243 -> cons)
                      Range: File "", line 143, characters 6-10
                      Body Range: File "", line 143, characters 11-19
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (find_opt#247 -> find_opt)
                      Range: File "", line 144, characters 6-14
                      Body Range: File "", line 144, characters 15-23
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#232 -> fold)
                      Range: File "", line 140, characters 6-10
                      Body Range: File "", line 140, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 322, characters 9-13
                      (fold_left#236 -> fold_left)
                      Range: File "", line 141, characters 6-15
                      Body Range: File "", line 141, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#240 -> fold_right)
                      Range: File "", line 142, characters 6-16
                      Body Range: File "", line 142, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references: File "", line 145, characters 4-14
                      (head_opt#219 -> head_opt)
                      Range: File "", line 135, characters 6-14
                      Body Range: File "", line 135, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#228 -> iter)
                      Range: File "", line 139, characters 6-10
                      Body Range: File "", line 139, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#214 -> length)
                      Range: File "", line 133, characters 6-12
                      Body Range: File "", line 133, characters 13-21
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#225 -> map)
                      Range: File "", line 138, characters 6-9
                      Body Range: File "", line 138, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10
                      (size#216 -> size)
                      Range: File "", line 134, characters 6-10
                      Body Range: File "", line 134, characters 11-19
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#222 -> tail_opt)
                      Range: File "", line 136, characters 6-14
                      Body Range: File "", line 136, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 322, characters 4-8

    (Map#180 -> Map)
    Range: File "", line 99, characters 7-10
    Body Range: File "", line 99, character 0 to line 115, character 3
    Content: Members: Variable definitions:
                      (add#152 -> add)
                      Range: File "", line 105, characters 6-9
                      Body Range: File "", line 105, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (empty#141 -> empty)
                      Range: File "", line 100, characters 6-11
                      Body Range: File "", line 100, characters 12-22
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#166 -> find)
                      Range: File "", line 109, characters 6-10
                      Body Range: File "", line 109, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#169 -> find_opt)
                      Range: File "", line 110, characters 6-14
                      Body Range: File "", line 110, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (fold#179 -> fold)
                      Range: File "", line 113, characters 6-10
                      Body Range: File "", line 113, characters 11-23
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      (get_and_update#163 -> get_and_update)
                      Range: File "", line 108, characters 6-20
                      Body Range: File "", line 108, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (iter#172 -> iter)
                      Range: File "", line 111, characters 6-10
                      Body Range: File "", line 111, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (literal#145 -> literal)
                      Range: File "", line 102, characters 25-32
                      Body Range: File "", line 102, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#175 -> map)
                      Range: File "", line 112, characters 6-9
                      Body Range: File "", line 112, characters 10-22
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (mem#148 -> mem)
                      Range: File "", line 104, characters 6-9
                      Body Range: File "", line 104, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (remove#155 -> remove)
                      Range: File "", line 106, characters 6-12
                      Body Range: File "", line 106, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (size#143 -> size)
                      Range: File "", line 101, characters 6-10
                      Body Range: File "", line 101, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#159 -> update)
                      Range: File "", line 107, characters 6-12
                      Body Range: File "", line 107, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#283 -> Option)
    Range: File "", line 156, characters 7-13
    Body Range: File "", line 156, character 0 to line 165, character 3
    Content: Members: Variable definitions:
                      (is_none#280 -> is_none)
                      Range: File "", line 163, characters 6-13
                      Body Range: File "", line 163, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#282 -> is_some)
                      Range: File "", line 164, characters 6-13
                      Body Range: File "", line 164, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#270 -> map)
                      Range: File "", line 160, characters 15-18
                      Body Range: File "", line 160, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#263 -> unopt)
                      Range: File "", line 157, characters 6-11
                      Body Range: File "", line 157, characters 12-20
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#267 -> unopt_with_error)
                      Range: File "", line 159, characters 6-22
                      Body Range: File "", line 159, characters 23-31
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#274 -> value)
                      Range: File "", line 161, characters 6-11
                      Body Range: File "", line 161, characters 12-20
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#278 -> value_exn)
                      Range: File "", line 162, characters 6-15
                      Body Range: File "", line 162, characters 16-28
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#212 -> Set)
    Range: File "", line 117, characters 7-10
    Body Range: File "", line 117, character 0 to line 130, character 3
    Content: Members: Variable definitions:
                      (add#193 -> add)
                      Range: File "", line 124, characters 6-9
                      Body Range: File "", line 124, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (cardinal#185 -> cardinal)
                      Range: File "", line 120, characters 6-14
                      Body Range: File "", line 120, characters 15-23
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#181 -> empty)
                      Range: File "", line 118, characters 6-11
                      Body Range: File "", line 118, characters 12-20
                      Content: |core: ∀ a : * . set (a)|
                      references: []
                      (fold#207 -> fold)
                      Range: File "", line 128, characters 6-10
                      Body Range: File "", line 128, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#211 -> fold_desc)
                      Range: File "", line 129, characters 6-15
                      Body Range: File "", line 129, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: []
                      (iter#203 -> iter)
                      Range: File "", line 127, characters 6-10
                      Body Range: File "", line 127, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#187 -> literal)
                      Range: File "", line 121, characters 25-32
                      Body Range: File "", line 121, characters 33-41
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#190 -> mem)
                      Range: File "", line 123, characters 6-9
                      Body Range: File "", line 123, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#196 -> remove)
                      Range: File "", line 125, characters 6-12
                      Body Range: File "", line 125, characters 13-21
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#183 -> size)
                      Range: File "", line 119, characters 6-10
                      Body Range: File "", line 119, characters 11-19
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#200 -> update)
                      Range: File "", line 126, characters 6-12
                      Body Range: File "", line 126, characters 13-21
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#260 -> String)
    Range: File "", line 148, characters 7-13
    Body Range: File "", line 148, character 0 to line 154, character 3
    Content: Members: Variable definitions:
                      (concat#255 -> concat)
                      Range: File "", line 152, characters 6-12
                      Body Range: File "", line 152, characters 14-16
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#252 -> concats)
                      Range: File "", line 150, characters 6-13
                      Body Range: File "", line 150, characters 15-17
                      Content: |core: list (string) -> string|
                      references: []
                      (length#250 -> length)
                      Range: File "", line 149, characters 6-12
                      Body Range: File "", line 149, characters 14-15
                      Content: |core: string -> nat|
                      references:
                        File "", line 352, characters 22-28 ,
                        File "", line 355, characters 43-49
                      (sub#259 -> sub)
                      Range: File "", line 153, characters 6-9
                      Body Range: File "", line 153, characters 11-12
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 353, characters 24-27 ,
                        File "", line 355, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 352, characters 15-21 ,
      File "", line 353, characters 17-23 ,
      File "", line 355, characters 16-22 ,
      File "", line 355, characters 36-42

    (Test#694 -> Test)
    Range: File "", line 227, characters 7-11
    Body Range: File "", line 227, character 0 to line 465, character 3
    Content: Members: Variable definitions:
                      (add_account#511 -> add_account)
                      Range: File "", line 335, characters 6-17
                      Body Range: File "", line 335, characters 19-20
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#680 -> assert)
                      Range: File "", line 457, characters 6-12
                      Body Range: File "", line 457, characters 14-15
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#684 -> assert_none)
                      Range: File "", line 459, characters 6-17
                      Body Range: File "", line 459, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#693 -> assert_none_with_error)
                      Range: File "", line 463, characters 6-28
                      Body Range: File "", line 463, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#682 -> assert_some)
                      Range: File "", line 458, characters 6-17
                      Body Range: File "", line 458, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#690 -> assert_some_with_error)
                      Range: File "", line 462, characters 6-28
                      Body Range: File "", line 462, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#687 -> assert_with_error)
                      Range: File "", line 461, characters 6-23
                      Body Range: File "", line 461, characters 25-26
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#404 -> bake_until_n_cycle_end)
                      Range: File "", line 254, characters 6-28
                      Body Range: File "", line 254, characters 30-31
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#514 -> baker_account)
                      Range: File "", line 336, characters 6-19
                      Body Range: File "", line 336, characters 21-22
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#499 -> bootstrap_contract)
                      Range: File "", line 331, characters 6-24
                      Body Range: File "", line 331, characters 25-35
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#408 -> cast_address)
                      Range: File "", line 256, characters 6-18
                      Body Range: File "", line 256, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 365, characters 35-47 ,
                        File "", line 375, characters 35-47 ,
                        File "", line 382, characters 35-47
                      (chr#448 -> chr)
                      Range: File "", line 280, characters 6-9
                      Body Range: File "", line 280, characters 11-12
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#443 -> compile_contract)
                      Range: File "", line 275, characters 6-22
                      Body Range: File "", line 275, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 361, characters 12-28 ,
                        File "", line 371, characters 12-28
                      (compile_contract_from_file#585 -> compile_contract_from_file)
                      Range: File "", line 384, characters 6-32
                      Body Range: File "", line 384, characters 34-36
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 388, characters 12-38
                      (compile_contract_with_views#561 -> compile_contract_with_views)
                      Range: File "", line 367, characters 8-35
                      Body Range: File "", line 367, characters 36-46
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 378, characters 12-39
                      (compile_value#366 -> compile_value)
                      Range: File "", line 232, characters 6-19
                      Body Range: File "", line 232, characters 20-28
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#416 -> constant_to_michelson_program)
                      Range: File "", line 260, characters 6-35
                      Body Range: File "", line 260, characters 37-38
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#520 -> create_chest)
                      Range: File "", line 338, characters 6-18
                      Body Range: File "", line 338, characters 20-21
                      Content: |core: bytes -> nat -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#523 -> create_chest_key)
                      Range: File "", line 339, characters 6-22
                      Body Range: File "", line 339, characters 24-25
                      Content: |core: chest -> nat -> chest_key|
                      references: []
                      (decompile#402 -> decompile)
                      Range: File "", line 253, characters 6-15
                      Body Range: File "", line 253, characters 16-24
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 271, characters 5-14
                      (drop_context#424 -> drop_context)
                      Range: File "", line 264, characters 6-18
                      Body Range: File "", line 264, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#382 -> eprint)
                      Range: File "", line 240, characters 6-12
                      Body Range: File "", line 240, characters 14-15
                      Content: |core: string -> unit|
                      references: []
                      (eval#364 -> eval)
                      Range: File "", line 230, characters 6-10
                      Body Range: File "", line 230, characters 11-19
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 232, characters 59-63 ,
                        File "", line 343, characters 32-36 ,
                        File "", line 348, characters 34-38 ,
                        File "", line 362, characters 12-16 ,
                        File "", line 372, characters 12-16 ,
                        File "", line 379, characters 12-16
                      (failwith#370 -> failwith)
                      Range: File "", line 234, characters 6-14
                      Body Range: File "", line 234, characters 15-25
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 457, characters 51-59 ,
                        File "", line 458, characters 74-82 ,
                        File "", line 459, characters 89-97 ,
                        File "", line 461, characters 68-76 ,
                        File "", line 462, characters 98-106 ,
                        File "", line 463, characters 113-121
                      (get_balance#378 -> get_balance)
                      Range: File "", line 238, characters 6-17
                      Body Range: File "", line 238, characters 19-20
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#391 -> get_bootstrap_account)
                      Range: File "", line 246, characters 6-27
                      Body Range: File "", line 246, characters 29-30
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#476 -> get_last_events_from)
                      Range: File "", line 315, characters 6-26
                      Body Range: File "", line 315, characters 27-39
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#433 -> get_storage)
                      Range: File "", line 267, characters 6-17
                      Body Range: File "", line 267, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#376 -> get_storage_of_address)
                      Range: File "", line 237, characters 6-28
                      Body Range: File "", line 237, characters 30-31
                      Content: |core: address -> michelson_program|
                      references: File "", line 270, characters 32-54
                      (get_time#406 -> get_time)
                      Range: File "", line 255, characters 6-14
                      Body Range: File "", line 255, characters 16-18
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#368 -> get_total_voting_power)
                      Range: File "", line 233, characters 6-28
                      Body Range: File "", line 233, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#384 -> get_voting_power)
                      Range: File "", line 241, characters 6-22
                      Body Range: File "", line 241, characters 24-26
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#395 -> last_originations)
                      Range: File "", line 248, characters 6-23
                      Body Range: File "", line 248, characters 25-26
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#488 -> log)
                      Range: File "", line 325, characters 6-9
                      Body Range: File "", line 325, characters 10-18
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 354, characters 25-28
                      (michelson_equal#540 -> michelson_equal)
                      Range: File "", line 350, characters 6-21
                      Body Range: File "", line 350, characters 23-25
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#502 -> mutate_value)
                      Range: File "", line 332, characters 6-18
                      Body Range: File "", line 332, characters 19-27
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 396, characters 23-35 ,
                        File "", line 408, characters 23-35
                      (mutation_test#609 -> mutation_test)
                      Range: File "", line 392, characters 6-19
                      Body Range: File "", line 392, characters 20-30
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#625 -> mutation_test_all)
                      Range: File "", line 404, characters 6-23
                      Body Range: File "", line 404, characters 24-34
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (new_account#400 -> new_account)
                      Range: File "", line 252, characters 6-17
                      Body Range: File "", line 252, characters 19-20
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#449 -> nl)
                      Range: File "", line 290, characters 6-8
                      Body Range: File "", line 290, characters 11-53
                      Content: |unresolved|
                      references: File "", line 292, characters 15-17
                      (nth_bootstrap_account#389 -> nth_bootstrap_account)
                      Range: File "", line 243, characters 6-27
                      Body Range: File "", line 243, characters 29-30
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#386 -> nth_bootstrap_contract)
                      Range: File "", line 242, characters 6-28
                      Body Range: File "", line 242, characters 30-31
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#393 -> nth_bootstrap_typed_address)
                      Range: File "", line 247, characters 6-33
                      Body Range: File "", line 247, characters 34-44
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#557 -> originate)
                      Range: File "", line 360, characters 6-15
                      Body Range: File "", line 360, characters 16-26
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_contract#548 -> originate_contract)
                      Range: File "", line 359, characters 6-24
                      Body Range: File "", line 359, characters 26-27
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 363, characters 12-30 ,
                        File "", line 373, characters 12-30 ,
                        File "", line 380, characters 12-30 ,
                        File "", line 389, characters 12-30 ,
                        File "", line 420, characters 14-32 ,
                        File "", line 440, characters 14-32
                      (originate_from_file#594 -> originate_from_file)
                      Range: File "", line 387, characters 6-25
                      Body Range: File "", line 387, characters 27-29
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_from_file_and_mutate#651 -> originate_from_file_and_mutate)
                      Range: File "", line 416, characters 6-36
                      Body Range: File "", line 416, characters 37-45
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#678 -> originate_from_file_and_mutate_all)
                      Range: File "", line 436, characters 6-40
                      Body Range: File "", line 436, characters 41-49
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module#580 -> originate_module)
                      Range: File "", line 377, characters 6-22
                      Body Range: File "", line 377, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) ->
                                                                ( list (operation) *
                                                                  s ) *
                                                                views (s) ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_uncurried#570 -> originate_uncurried)
                      Range: File "", line 370, characters 6-25
                      Body Range: File "", line 370, characters 26-36
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (parse_michelson#418 -> parse_michelson)
                      Range: File "", line 261, characters 6-21
                      Body Range: File "", line 261, characters 23-24
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#380 -> print)
                      Range: File "", line 239, characters 6-11
                      Body Range: File "", line 239, characters 13-14
                      Content: |core: string -> unit|
                      references:
                        File "", line 292, characters 4-9 ,
                        File "", line 328, characters 4-9
                      (println#451 -> println)
                      Range: File "", line 291, characters 6-13
                      Body Range: File "", line 291, characters 15-16
                      Content: |core: string -> unit|
                      references: []
                      (random#398 -> random)
                      Range: File "", line 249, characters 6-12
                      Body Range: File "", line 249, characters 13-21
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#445 -> read_contract_from_file)
                      Range: File "", line 279, characters 6-29
                      Body Range: File "", line 279, characters 31-33
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#412 -> register_constant)
                      Range: File "", line 258, characters 6-23
                      Body Range: File "", line 258, characters 25-26
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#410 -> register_delegate)
                      Range: File "", line 257, characters 6-23
                      Body Range: File "", line 257, characters 25-27
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#491 -> reset_state)
                      Range: File "", line 329, characters 6-17
                      Body Range: File "", line 329, characters 19-20
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#495 -> reset_state_at)
                      Range: File "", line 330, characters 6-20
                      Body Range: File "", line 330, characters 22-23
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#420 -> restore_context)
                      Range: File "", line 262, characters 6-21
                      Body Range: File "", line 262, characters 23-24
                      Content: |core: unit -> unit|
                      references: []
                      (run#361 -> run)
                      Range: File "", line 229, characters 6-9
                      Body Range: File "", line 229, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 230, characters 50-53
                      (save_context#422 -> save_context)
                      Range: File "", line 263, characters 6-18
                      Body Range: File "", line 263, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#505 -> save_mutation)
                      Range: File "", line 333, characters 6-19
                      Body Range: File "", line 333, characters 21-22
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#437 -> set_baker)
                      Range: File "", line 273, characters 6-15
                      Body Range: File "", line 273, characters 17-18
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#435 -> set_baker_policy)
                      Range: File "", line 272, characters 6-22
                      Body Range: File "", line 272, characters 24-26
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 273, characters 39-55
                      (set_big_map#517 -> set_big_map)
                      Range: File "", line 337, characters 6-17
                      Body Range: File "", line 337, characters 18-28
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#452 -> set_print_values)
                      Range: File "", line 294, characters 6-22
                      Body Range: File "", line 294, characters 24-25
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#374 -> set_source)
                      Range: File "", line 236, characters 6-16
                      Body Range: File "", line 236, characters 18-19
                      Content: |core: address -> unit|
                      references: []
                      (sign#508 -> sign)
                      Range: File "", line 334, characters 6-10
                      Body Range: File "", line 334, characters 12-14
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#439 -> size)
                      Range: File "", line 274, characters 6-10
                      Body Range: File "", line 274, characters 12-13
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 364, characters 12-16 ,
                        File "", line 374, characters 12-16 ,
                        File "", line 381, characters 12-16 ,
                        File "", line 390, characters 12-16 ,
                        File "", line 421, characters 14-18 ,
                        File "", line 441, characters 14-18
                      (to_contract#372 -> to_contract)
                      Range: File "", line 235, characters 6-17
                      Body Range: File "", line 235, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 268, characters 25-36 ,
                        File "", line 316, characters 30-41
                      (to_entrypoint#544 -> to_entrypoint)
                      Range: File "", line 351, characters 6-19
                      Body Range: File "", line 351, characters 20-32
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#428 -> to_json)
                      Range: File "", line 266, characters 6-13
                      Body Range: File "", line 266, characters 14-22
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#426 -> to_string)
                      Range: File "", line 265, characters 6-15
                      Body Range: File "", line 265, characters 16-24
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 283, characters 68-77 ,
                        File "", line 285, characters 67-76 ,
                        File "", line 287, characters 61-70 ,
                        File "", line 327, characters 12-21
                      (to_typed_address#414 -> to_typed_address)
                      Range: File "", line 259, characters 6-22
                      Body Range: File "", line 259, characters 23-33
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#480 -> transfer)
                      Range: File "", line 323, characters 6-14
                      Body Range: File "", line 323, characters 16-17
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#484 -> transfer_exn)
                      Range: File "", line 324, characters 6-18
                      Body Range: File "", line 324, characters 20-21
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#530 -> transfer_to_contract)
                      Range: File "", line 340, characters 6-26
                      Body Range: File "", line 340, characters 27-35
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#537 -> transfer_to_contract_exn)
                      Range: File "", line 345, characters 6-30
                      Body Range: File "", line 345, characters 31-39
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#453 -> unset_print_values)
                      Range: File "", line 295, characters 6-24
                      Body Range: File "", line 295, characters 26-27
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#467 -> PBT)
                      Range: File "", line 297, characters 9-12
                      Body Range: File "", line 297, character 2 to line 313, character 5
                      Content: Members: Variable definitions:
                                        (gen#454 -> gen)
                                        Range: File "", line 298, characters 8-11
                                        Body Range: File "", line 298, characters 12-20
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references:
                                          File "", line 301, characters 23-27 ,
                                          File "", line 302, characters 23-27
                                        (gen_small#455 -> gen_small)
                                        Range: File "", line 299, characters 8-17
                                        Body Range: File "", line 299, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#458 -> make_test)
                                        Range: File "", line 300, characters 8-17
                                        Body Range: File "", line 300, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#466 -> run)
                                        Range: File "", line 301, characters 8-11
                                        Body Range: File "", line 301, characters 12-20
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#96 -> Tezos)
    Range: File "", line 7, characters 7-12
    Body Range: File "", line 7, character 0 to line 75, character 3
    Content: Members: Variable definitions:
                      (address#26 -> address)
                      Range: File "", line 20, characters 6-13
                      Body Range: File "", line 20, characters 14-22
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 316, characters 21-28
                      (call_view#65 -> call_view)
                      Range: File "", line 55, characters 25-34
                      Body Range: File "", line 55, characters 35-45
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (constant#42 -> constant)
                      Range: File "", line 31, characters 25-33
                      Body Range: File "", line 31, characters 34-42
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (create_contract#76 -> create_contract)
                      Range: File "", line 59, characters 25-40
                      Body Range: File "", line 59, characters 41-51
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: []
                      (create_contract_uncurried#81 -> create_contract_uncurried)
                      Range: File "", line 62, characters 25-50
                      Body Range: File "", line 62, characters 51-61
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      (create_ticket#57 -> create_ticket)
                      Range: File "", line 47, characters 6-19
                      Body Range: File "", line 47, characters 20-28
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references: []
                      (emit#92 -> emit)
                      Range: File "", line 70, characters 25-29
                      Body Range: File "", line 70, characters 30-38
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      (get_amount#6 -> get_amount)
                      Range: File "", line 10, characters 6-16
                      Body Range: File "", line 10, characters 18-20
                      Content: |core: unit -> tez|
                      references: []
                      (get_balance#4 -> get_balance)
                      Range: File "", line 9, characters 6-17
                      Body Range: File "", line 9, characters 19-21
                      Content: |core: unit -> tez|
                      references: []
                      (get_chain_id#18 -> get_chain_id)
                      Range: File "", line 16, characters 6-18
                      Body Range: File "", line 16, characters 20-22
                      Content: |core: unit -> chain_id|
                      references: []
                      (get_contract#49 -> get_contract)
                      Range: File "", line 36, characters 25-37
                      Body Range: File "", line 36, characters 38-46
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      (get_contract_opt#45 -> get_contract_opt)
                      Range: File "", line 34, characters 25-41
                      Body Range: File "", line 34, characters 42-50
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 37, characters 12-28 ,
                        File "", line 41, characters 12-28
                      (get_contract_with_error#54 -> get_contract_with_error)
                      Range: File "", line 40, characters 6-29
                      Body Range: File "", line 40, characters 30-38
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: []
                      (get_entrypoint#89 -> get_entrypoint)
                      Range: File "", line 67, characters 25-39
                      Body Range: File "", line 67, characters 40-48
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (get_entrypoint_opt#84 -> get_entrypoint_opt)
                      Range: File "", line 64, characters 25-43
                      Body Range: File "", line 64, characters 44-52
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 68, characters 12-30
                      (get_level#14 -> get_level)
                      Range: File "", line 14, characters 6-15
                      Body Range: File "", line 14, characters 17-19
                      Content: |core: unit -> nat|
                      references: []
                      (get_min_block_time#22 -> get_min_block_time)
                      Range: File "", line 18, characters 6-24
                      Body Range: File "", line 18, characters 26-28
                      Content: |core: unit -> nat|
                      references: []
                      (get_now#8 -> get_now)
                      Range: File "", line 11, characters 6-13
                      Body Range: File "", line 11, characters 15-17
                      Content: |core: unit -> timestamp|
                      references: File "", line 255, characters 47-54
                      (get_self_address#16 -> get_self_address)
                      Range: File "", line 15, characters 6-22
                      Body Range: File "", line 15, characters 24-26
                      Content: |core: unit -> address|
                      references: []
                      (get_sender#10 -> get_sender)
                      Range: File "", line 12, characters 6-16
                      Body Range: File "", line 12, characters 18-20
                      Content: |core: unit -> address|
                      references: []
                      (get_source#12 -> get_source)
                      Range: File "", line 13, characters 6-16
                      Body Range: File "", line 13, characters 18-20
                      Content: |core: unit -> address|
                      references: []
                      (get_total_voting_power#20 -> get_total_voting_power)
                      Range: File "", line 17, characters 6-28
                      Body Range: File "", line 17, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (implicit_account#28 -> implicit_account)
                      Range: File "", line 21, characters 6-22
                      Body Range: File "", line 21, characters 24-26
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      (join_tickets#30 -> join_tickets)
                      Range: File "", line 22, characters 6-18
                      Body Range: File "", line 22, characters 19-27
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      (never#34 -> never)
                      Range: File "", line 25, characters 6-11
                      Body Range: File "", line 25, characters 12-20
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      (pairing_check#36 -> pairing_check)
                      Range: File "", line 26, characters 6-19
                      Body Range: File "", line 26, characters 21-22
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      (read_ticket#32 -> read_ticket)
                      Range: File "", line 23, characters 6-17
                      Body Range: File "", line 23, characters 18-26
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address *
                                                                    ( a * nat ) ) *
                                                                  ticket (a) )|
                      references: []
                      (sapling_empty_state#43 -> sapling_empty_state)
                      Range: File "", line 32, characters 25-44
                      Body Range: File "", line 32, characters 45-57
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      (sapling_verify_update#95 -> sapling_verify_update)
                      Range: File "", line 73, characters 25-46
                      Body Range: File "", line 73, characters 47-59
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes *
                        ( int * sapling_state (sap_a) ) ))|
                      references: []
                      (self#40 -> self)
                      Range: File "", line 28, characters 25-29
                      Body Range: File "", line 28, characters 30-38
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      (set_delegate#38 -> set_delegate)
                      Range: File "", line 27, characters 6-18
                      Body Range: File "", line 27, characters 20-21
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      (split_ticket#68 -> split_ticket)
                      Range: File "", line 57, characters 6-18
                      Body Range: File "", line 57, characters 19-27
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      (transaction#61 -> transaction)
                      Range: File "", line 49, characters 6-17
                      Body Range: File "", line 49, characters 18-26
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: []
                      (voting_power#24 -> voting_power)
                      Range: File "", line 19, characters 6-18
                      Body Range: File "", line 19, characters 20-22
                      Content: |core: key_hash -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 255, characters 41-46 ,
      File "", line 316, characters 15-20 |}]

let () = Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:""
