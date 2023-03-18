open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s
let () = Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:"true"

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Test#705 assert_none_with_error#704 assert_some_with_error#701 assert_with_error#698 assert_none#695 assert_some#693 assert#691 originate_from_file_and_mutate_all#689 originate_from_file_and_mutate#662 mutation_test_all#636 mutation_test#620 originate_from_file#605 compile_contract_from_file#596 originate_module#591 originate_uncurried#581 compile_contract_with_views#572 originate#568 originate_contract#559 to_entrypoint#555 michelson_equal#551 transfer_to_contract_exn#548 transfer_to_contract#541 create_chest_key#534 create_chest#531 set_big_map#528 baker_account#525 add_account#522 sign#519 save_mutation#516 mutate_value#513 bootstrap_contract#510 reset_state_at#506 reset_state#502 log#499 transfer_exn#495 transfer#491 get_last_events_from#487 PBT#478 run#477 make_test#469 gen_small#466 gen#465 unset_print_values#464 set_print_values#463 println#462 nl#460 chr#459 read_contract_from_file#456 compile_contract#454 size#450 set_baker#448 set_baker_policy#446 get_storage#444 to_json#439 to_string#437 drop_context#435 save_context#433 restore_context#431 parse_michelson#429 constant_to_michelson_program#427 to_typed_address#425 register_constant#423 register_delegate#421 cast_address#419 get_time#417 bake_until_n_cycle_end#415 decompile#413 new_account#411 random#409 last_originations#406 nth_bootstrap_typed_address#404 get_bootstrap_account#402 nth_bootstrap_account#400 nth_bootstrap_contract#397 get_voting_power#395 eprint#393 print#391 get_balance#389 get_storage_of_address#387 set_source#385 to_contract#383 failwith#381 get_total_voting_power#379 compile_value#377 eval#375 run#372 unforged_ticket#369 pbt_result#368 pbt_test#367 test_baker_policy#366 test_exec_result#365 test_exec_error#364 test_exec_error_balance_too_low#363 ediv#362 assert_none_with_error#359 assert_some_with_error#356 assert_with_error#353 uncurry#350 curry#347 ignore#343 int#342 unit#340 false#339 true#338 is_nat#337 abs#335 assert_none#333 assert_some#331 assert#329 Crypto#327 check#326 hash_key#322 keccak#320 sha3#318 sha512#316 sha256#314 blake2b#312 Bytes#310 sub#309 concat#305 length#302 unpack#300 pack#298 concats#296 Option#294 is_some#293 is_none#291 value_exn#289 value#285 map#281 unopt_with_error#278 unopt#274 String#271 sub#270 concat#266 concats#263 length#261 List#259 find_opt#258 cons#254 fold_right#251 fold_left#247 fold#243 iter#239 map#236 tail_opt#233 head_opt#230 size#227 length#225 Set#223 fold_desc#222 fold#218 iter#214 update#211 remove#207 add#204 mem#201 literal#198 cardinal#196 size#194 empty#192 Transpiled#191 map_remove#190 map_add#187 map_find_opt#183 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ a#706 Test#705 assert_none_with_error#704 assert_some_with_error#701 assert_with_error#698 assert_none#695 assert_some#693 assert#691 originate_from_file_and_mutate_all#689 originate_from_file_and_mutate#662 mutation_test_all#636 mutation_test#620 originate_from_file#605 compile_contract_from_file#596 originate_module#591 originate_uncurried#581 compile_contract_with_views#572 originate#568 originate_contract#559 to_entrypoint#555 michelson_equal#551 transfer_to_contract_exn#548 transfer_to_contract#541 create_chest_key#534 create_chest#531 set_big_map#528 baker_account#525 add_account#522 sign#519 save_mutation#516 mutate_value#513 bootstrap_contract#510 reset_state_at#506 reset_state#502 log#499 transfer_exn#495 transfer#491 get_last_events_from#487 PBT#478 run#477 make_test#469 gen_small#466 gen#465 unset_print_values#464 set_print_values#463 println#462 nl#460 chr#459 read_contract_from_file#456 compile_contract#454 size#450 set_baker#448 set_baker_policy#446 get_storage#444 to_json#439 to_string#437 drop_context#435 save_context#433 restore_context#431 parse_michelson#429 constant_to_michelson_program#427 to_typed_address#425 register_constant#423 register_delegate#421 cast_address#419 get_time#417 bake_until_n_cycle_end#415 decompile#413 new_account#411 random#409 last_originations#406 nth_bootstrap_typed_address#404 get_bootstrap_account#402 nth_bootstrap_account#400 nth_bootstrap_contract#397 get_voting_power#395 eprint#393 print#391 get_balance#389 get_storage_of_address#387 set_source#385 to_contract#383 failwith#381 get_total_voting_power#379 compile_value#377 eval#375 run#372 unforged_ticket#369 pbt_result#368 pbt_test#367 test_baker_policy#366 test_exec_result#365 test_exec_error#364 test_exec_error_balance_too_low#363 ediv#362 assert_none_with_error#359 assert_some_with_error#356 assert_with_error#353 uncurry#350 curry#347 ignore#343 int#342 unit#340 false#339 true#338 is_nat#337 abs#335 assert_none#333 assert_some#331 assert#329 Crypto#327 check#326 hash_key#322 keccak#320 sha3#318 sha512#316 sha256#314 blake2b#312 Bytes#310 sub#309 concat#305 length#302 unpack#300 pack#298 concats#296 Option#294 is_some#293 is_none#291 value_exn#289 value#285 map#281 unopt_with_error#278 unopt#274 String#271 sub#270 concat#266 concats#263 length#261 List#259 find_opt#258 cons#254 fold_right#251 fold_left#247 fold#243 iter#239 map#236 tail_opt#233 head_opt#230 size#227 length#225 Set#223 fold_desc#222 fold#218 iter#214 update#211 remove#207 add#204 mem#201 literal#198 cardinal#196 size#194 empty#192 Transpiled#191 map_remove#190 map_add#187 map_find_opt#183 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 14
    [ c#707 a#706 Test#705 assert_none_with_error#704 assert_some_with_error#701 assert_with_error#698 assert_none#695 assert_some#693 assert#691 originate_from_file_and_mutate_all#689 originate_from_file_and_mutate#662 mutation_test_all#636 mutation_test#620 originate_from_file#605 compile_contract_from_file#596 originate_module#591 originate_uncurried#581 compile_contract_with_views#572 originate#568 originate_contract#559 to_entrypoint#555 michelson_equal#551 transfer_to_contract_exn#548 transfer_to_contract#541 create_chest_key#534 create_chest#531 set_big_map#528 baker_account#525 add_account#522 sign#519 save_mutation#516 mutate_value#513 bootstrap_contract#510 reset_state_at#506 reset_state#502 log#499 transfer_exn#495 transfer#491 get_last_events_from#487 PBT#478 run#477 make_test#469 gen_small#466 gen#465 unset_print_values#464 set_print_values#463 println#462 nl#460 chr#459 read_contract_from_file#456 compile_contract#454 size#450 set_baker#448 set_baker_policy#446 get_storage#444 to_json#439 to_string#437 drop_context#435 save_context#433 restore_context#431 parse_michelson#429 constant_to_michelson_program#427 to_typed_address#425 register_constant#423 register_delegate#421 cast_address#419 get_time#417 bake_until_n_cycle_end#415 decompile#413 new_account#411 random#409 last_originations#406 nth_bootstrap_typed_address#404 get_bootstrap_account#402 nth_bootstrap_account#400 nth_bootstrap_contract#397 get_voting_power#395 eprint#393 print#391 get_balance#389 get_storage_of_address#387 set_source#385 to_contract#383 failwith#381 get_total_voting_power#379 compile_value#377 eval#375 run#372 unforged_ticket#369 pbt_result#368 pbt_test#367 test_baker_policy#366 test_exec_result#365 test_exec_error#364 test_exec_error_balance_too_low#363 ediv#362 assert_none_with_error#359 assert_some_with_error#356 assert_with_error#353 uncurry#350 curry#347 ignore#343 int#342 unit#340 false#339 true#338 is_nat#337 abs#335 assert_none#333 assert_some#331 assert#329 Crypto#327 check#326 hash_key#322 keccak#320 sha3#318 sha512#316 sha256#314 blake2b#312 Bytes#310 sub#309 concat#305 length#302 unpack#300 pack#298 concats#296 Option#294 is_some#293 is_none#291 value_exn#289 value#285 map#281 unopt_with_error#278 unopt#274 String#271 sub#270 concat#266 concats#263 length#261 List#259 find_opt#258 cons#254 fold_right#251 fold_left#247 fold#243 iter#239 map#236 tail_opt#233 head_opt#230 size#227 length#225 Set#223 fold_desc#222 fold#218 iter#214 update#211 remove#207 add#204 mem#201 literal#198 cardinal#196 size#194 empty#192 Transpiled#191 map_remove#190 map_add#187 map_find_opt#183 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ d#708 c#707 a#706 Test#705 assert_none_with_error#704 assert_some_with_error#701 assert_with_error#698 assert_none#695 assert_some#693 assert#691 originate_from_file_and_mutate_all#689 originate_from_file_and_mutate#662 mutation_test_all#636 mutation_test#620 originate_from_file#605 compile_contract_from_file#596 originate_module#591 originate_uncurried#581 compile_contract_with_views#572 originate#568 originate_contract#559 to_entrypoint#555 michelson_equal#551 transfer_to_contract_exn#548 transfer_to_contract#541 create_chest_key#534 create_chest#531 set_big_map#528 baker_account#525 add_account#522 sign#519 save_mutation#516 mutate_value#513 bootstrap_contract#510 reset_state_at#506 reset_state#502 log#499 transfer_exn#495 transfer#491 get_last_events_from#487 PBT#478 run#477 make_test#469 gen_small#466 gen#465 unset_print_values#464 set_print_values#463 println#462 nl#460 chr#459 read_contract_from_file#456 compile_contract#454 size#450 set_baker#448 set_baker_policy#446 get_storage#444 to_json#439 to_string#437 drop_context#435 save_context#433 restore_context#431 parse_michelson#429 constant_to_michelson_program#427 to_typed_address#425 register_constant#423 register_delegate#421 cast_address#419 get_time#417 bake_until_n_cycle_end#415 decompile#413 new_account#411 random#409 last_originations#406 nth_bootstrap_typed_address#404 get_bootstrap_account#402 nth_bootstrap_account#400 nth_bootstrap_contract#397 get_voting_power#395 eprint#393 print#391 get_balance#389 get_storage_of_address#387 set_source#385 to_contract#383 failwith#381 get_total_voting_power#379 compile_value#377 eval#375 run#372 unforged_ticket#369 pbt_result#368 pbt_test#367 test_baker_policy#366 test_exec_result#365 test_exec_error#364 test_exec_error_balance_too_low#363 ediv#362 assert_none_with_error#359 assert_some_with_error#356 assert_with_error#353 uncurry#350 curry#347 ignore#343 int#342 unit#340 false#339 true#338 is_nat#337 abs#335 assert_none#333 assert_some#331 assert#329 Crypto#327 check#326 hash_key#322 keccak#320 sha3#318 sha512#316 sha256#314 blake2b#312 Bytes#310 sub#309 concat#305 length#302 unpack#300 pack#298 concats#296 Option#294 is_some#293 is_none#291 value_exn#289 value#285 map#281 unopt_with_error#278 unopt#274 String#271 sub#270 concat#266 concats#263 length#261 List#259 find_opt#258 cons#254 fold_right#251 fold_left#247 fold#243 iter#239 map#236 tail_opt#233 head_opt#230 size#227 length#225 Set#223 fold_desc#222 fold#218 iter#214 update#211 remove#207 add#204 mem#201 literal#198 cardinal#196 size#194 empty#192 Transpiled#191 map_remove#190 map_add#187 map_find_opt#183 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ e#709 a#706 Test#705 assert_none_with_error#704 assert_some_with_error#701 assert_with_error#698 assert_none#695 assert_some#693 assert#691 originate_from_file_and_mutate_all#689 originate_from_file_and_mutate#662 mutation_test_all#636 mutation_test#620 originate_from_file#605 compile_contract_from_file#596 originate_module#591 originate_uncurried#581 compile_contract_with_views#572 originate#568 originate_contract#559 to_entrypoint#555 michelson_equal#551 transfer_to_contract_exn#548 transfer_to_contract#541 create_chest_key#534 create_chest#531 set_big_map#528 baker_account#525 add_account#522 sign#519 save_mutation#516 mutate_value#513 bootstrap_contract#510 reset_state_at#506 reset_state#502 log#499 transfer_exn#495 transfer#491 get_last_events_from#487 PBT#478 run#477 make_test#469 gen_small#466 gen#465 unset_print_values#464 set_print_values#463 println#462 nl#460 chr#459 read_contract_from_file#456 compile_contract#454 size#450 set_baker#448 set_baker_policy#446 get_storage#444 to_json#439 to_string#437 drop_context#435 save_context#433 restore_context#431 parse_michelson#429 constant_to_michelson_program#427 to_typed_address#425 register_constant#423 register_delegate#421 cast_address#419 get_time#417 bake_until_n_cycle_end#415 decompile#413 new_account#411 random#409 last_originations#406 nth_bootstrap_typed_address#404 get_bootstrap_account#402 nth_bootstrap_account#400 nth_bootstrap_contract#397 get_voting_power#395 eprint#393 print#391 get_balance#389 get_storage_of_address#387 set_source#385 to_contract#383 failwith#381 get_total_voting_power#379 compile_value#377 eval#375 run#372 unforged_ticket#369 pbt_result#368 pbt_test#367 test_baker_policy#366 test_exec_result#365 test_exec_error#364 test_exec_error_balance_too_low#363 ediv#362 assert_none_with_error#359 assert_some_with_error#356 assert_with_error#353 uncurry#350 curry#347 ignore#343 int#342 unit#340 false#339 true#338 is_nat#337 abs#335 assert_none#333 assert_some#331 assert#329 Crypto#327 check#326 hash_key#322 keccak#320 sha3#318 sha512#316 sha256#314 blake2b#312 Bytes#310 sub#309 concat#305 length#302 unpack#300 pack#298 concats#296 Option#294 is_some#293 is_none#291 value_exn#289 value#285 map#281 unopt_with_error#278 unopt#274 String#271 sub#270 concat#266 concats#263 length#261 List#259 find_opt#258 cons#254 fold_right#251 fold_left#247 fold#243 iter#239 map#236 tail_opt#233 head_opt#230 size#227 length#225 Set#223 fold_desc#222 fold#218 iter#214 update#211 remove#207 add#204 mem#201 literal#198 cardinal#196 size#194 empty#192 Transpiled#191 map_remove#190 map_add#187 map_find_opt#183 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30

    Variable definitions:
    (a#706 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#335 -> abs)
    Range: File "", line 197, characters 4-7
    Body Range: File "", line 197, characters 9-10
    Content: |core: int -> nat|
    references: File "", line 361, characters 31-34
    (assert#329 -> assert)
    Range: File "", line 194, characters 4-10
    Body Range: File "", line 194, characters 12-13
    Content: |core: bool -> unit|
    references: []
    (assert_none#333 -> assert_none)
    Range: File "", line 196, characters 4-15
    Body Range: File "", line 196, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#359 -> assert_none_with_error)
    Range: File "", line 209, characters 4-26
    Body Range: File "", line 209, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#331 -> assert_some)
    Range: File "", line 195, characters 4-15
    Body Range: File "", line 195, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#356 -> assert_some_with_error)
    Range: File "", line 208, characters 4-26
    Body Range: File "", line 208, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#353 -> assert_with_error)
    Range: File "", line 207, characters 4-21
    Body Range: File "", line 207, characters 23-24
    Content: |unresolved|
    references: []
    (b#710 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#707 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (curry#347 -> curry)
    Range: File "", line 204, characters 4-9
    Body Range: File "", line 204, characters 10-22
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    (d#708 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#709 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    (ediv#362 -> ediv)
    Range: File "", line 210, characters 4-8
    Body Range: File "", line 210, characters 9-19
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
      File "", line 163, characters 79-87 ,
      File "", line 165, characters 103-111 ,
      File "", line 168, characters 83-91 ,
      File "", line 194, characters 49-57 ,
      File "", line 195, characters 72-80 ,
      File "", line 196, characters 87-95 ,
      File "", line 207, characters 66-74 ,
      File "", line 208, characters 96-104 ,
      File "", line 209, characters 111-119
    (false#339 -> false)
    Range: File "", line 200, characters 4-9
    Body Range: File "", line 200, characters 19-24
    Content: |core: bool|
    references:
      File "", line 256, characters 51-56 ,
      File "", line 301, characters 90-95 ,
      File "", line 304, characters 62-67
    (ignore#343 -> ignore)
    Range: File "", line 203, characters 4-10
    Body Range: File "", line 203, characters 11-19
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#342 -> int)
    Range: File "", line 202, characters 4-7
    Body Range: File "", line 202, characters 8-16
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 252, characters 97-100 ,
      File "", line 289, characters 79-82 ,
      File "", line 291, characters 78-81 ,
      File "", line 293, characters 72-75
    (is_nat#337 -> is_nat)
    Range: File "", line 198, characters 4-10
    Body Range: File "", line 198, characters 12-13
    Content: |core: int -> option (nat)|
    references: []
    (true#338 -> true)
    Range: File "", line 199, characters 4-8
    Body Range: File "", line 199, characters 18-22
    Content: |core: bool|
    references:
      File "", line 300, characters 88-92 ,
      File "", line 305, characters 68-72
    (uncurry#350 -> uncurry)
    Range: File "", line 205, characters 4-11
    Body Range: File "", line 205, characters 12-24
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 367, characters 30-37
    (unit#340 -> unit)
    Range: File "", line 201, characters 4-8
    Body Range: File "", line 201, characters 18-38
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
      File "", line 129, characters 41-45 ,
      File "", line 132, characters 35-39 ,
      File "", line 150, characters 34-38 ,
      File "", line 169, characters 40-44 ,
      File "", line 170, characters 40-44 ,
      File "", line 191, characters 52-56 ,
      File "", line 191, characters 145-149 ,
      File "", line 194, characters 16-20 ,
      File "", line 199, characters 11-15 ,
      File "", line 200, characters 12-16 ,
      File "", line 207, characters 27-31 ,
      File "", line 228, characters 41-45 ,
      File "", line 306, characters 53-57 ,
      File "", line 356, characters 74-78 ,
      File "", line 463, characters 18-22 ,
      File "", line 467, characters 29-33
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
      File "", line 141, characters 40-48 ,
      File "", line 142, characters 40-55 ,
      File "", line 150, characters 56-64 ,
      File "", line 151, characters 29-37 ,
      File "", line 163, characters 26-34 ,
      File "", line 165, characters 37-45 ,
      File "", line 166, characters 48-56 ,
      File "", line 166, characters 60-68 ,
      File "", line 167, characters 40-48 ,
      File "", line 168, characters 46-54 ,
      File "", line 169, characters 28-36 ,
      File "", line 170, characters 28-36 ,
      File "", line 176, characters 36-44 ,
      File "", line 176, characters 99-107 ,
      File "", line 195, characters 30-38 ,
      File "", line 196, characters 30-38 ,
      File "", line 198, characters 23-33 ,
      File "", line 198, characters 74-84 ,
      File "", line 208, characters 41-49 ,
      File "", line 209, characters 41-49 ,
      File "", line 286, characters 22-35 ,
      File "", line 329, characters 140-153 ,
      File "", line 330, characters 135-148 ,
      File "", line 335, characters 92-108 ,
      File "", line 338, characters 48-69 ,
      File "", line 339, characters 50-63 ,
      File "", line 342, characters 44-54 ,
      File "", line 348, characters 12-25 ,
      File "", line 353, characters 14-27 ,
      File "", line 391, characters 96-106 ,
      File "", line 398, characters 59-80 ,
      File "", line 401, characters 37-58 ,
      File "", line 423, characters 90-111 ,
      File "", line 429, characters 96-106 ,
      File "", line 432, characters 37-58 ,
      File "", line 449, characters 96-106 ,
      File "", line 464, characters 32-40 ,
      File "", line 465, characters 32-40 ,
      File "", line 468, characters 43-51 ,
      File "", line 469, characters 43-51
    (pbt_result#368 -> pbt_result)
    Range: File "", line 229, characters 8-18
    Body Range: File "", line 229, characters 0-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 307, characters 55-67 ,
      File "", line 308, characters 37-49 ,
      File "", line 310, characters 82-94 ,
      File "", line 314, characters 94-106 ,
      File "", line 317, characters 66-78
    (pbt_test#367 -> pbt_test)
    Range: File "", line 228, characters 8-16
    Body Range: File "", line 228, characters 0-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 306, characters 61-71 ,
      File "", line 307, characters 31-41
    (test_baker_policy#366 -> test_baker_policy)
    Range: File "", line 223, characters 5-22
    Body Range: File "", line 224, character 4 to line 226, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 278, characters 29-46
    (test_exec_error#364 -> test_exec_error)
    Range: File "", line 216, characters 5-20
    Body Range: File "", line 217, character 4 to line 219, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 221, characters 49-64
    (test_exec_error_balance_too_low#363 -> test_exec_error_balance_too_low)
    Range: File "", line 213, characters 5-36
    Body Range: File "", line 214, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 218, characters 23-54
    (test_exec_result#365 -> test_exec_result)
    Range: File "", line 221, characters 5-21
    Body Range: File "", line 221, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 329, characters 65-81 ,
      File "", line 346, characters 73-89
    (unforged_ticket#369 -> unforged_ticket)
    Range: File "", line 231, characters 8-23
    Body Range: File "", line 231, characters 0-91
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

    (Bytes#310 -> Bytes)
    Range: File "", line 173, characters 7-12
    Body Range: File "", line 173, character 0 to line 181, character 3
    Content: Members: Variable definitions:
                      (concat#305 -> concat)
                      Range: File "", line 179, characters 6-12
                      Body Range: File "", line 179, characters 14-16
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#296 -> concats)
                      Range: File "", line 174, characters 6-13
                      Body Range: File "", line 174, characters 15-17
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#302 -> length)
                      Range: File "", line 177, characters 6-12
                      Body Range: File "", line 177, characters 14-15
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#298 -> pack)
                      Range: File "", line 175, characters 6-10
                      Body Range: File "", line 175, characters 11-19
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#309 -> sub)
                      Range: File "", line 180, characters 6-9
                      Body Range: File "", line 180, characters 11-12
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#300 -> unpack)
                      Range: File "", line 176, characters 6-12
                      Body Range: File "", line 176, characters 13-21
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#327 -> Crypto)
    Range: File "", line 183, characters 7-13
    Body Range: File "", line 183, character 0 to line 192, character 3
    Content: Members: Variable definitions:
                      (blake2b#312 -> blake2b)
                      Range: File "", line 184, characters 6-13
                      Body Range: File "", line 184, characters 15-16
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#326 -> check)
                      Range: File "", line 191, characters 6-11
                      Body Range: File "", line 191, characters 13-14
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#322 -> hash_key)
                      Range: File "", line 189, characters 6-14
                      Body Range: File "", line 189, characters 16-17
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#320 -> keccak)
                      Range: File "", line 188, characters 6-12
                      Body Range: File "", line 188, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#314 -> sha256)
                      Range: File "", line 185, characters 6-12
                      Body Range: File "", line 185, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#318 -> sha3)
                      Range: File "", line 187, characters 6-10
                      Body Range: File "", line 187, characters 12-13
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#316 -> sha512)
                      Range: File "", line 186, characters 6-12
                      Body Range: File "", line 186, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#259 -> List)
    Range: File "", line 138, characters 7-11
    Body Range: File "", line 138, character 0 to line 152, character 3
    Content: Members: Variable definitions:
                      (cons#254 -> cons)
                      Range: File "", line 149, characters 6-10
                      Body Range: File "", line 149, characters 11-19
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (find_opt#258 -> find_opt)
                      Range: File "", line 150, characters 6-14
                      Body Range: File "", line 150, characters 15-23
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#243 -> fold)
                      Range: File "", line 146, characters 6-10
                      Body Range: File "", line 146, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 328, characters 9-13
                      (fold_left#247 -> fold_left)
                      Range: File "", line 147, characters 6-15
                      Body Range: File "", line 147, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#251 -> fold_right)
                      Range: File "", line 148, characters 6-16
                      Body Range: File "", line 148, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references: File "", line 151, characters 4-14
                      (head_opt#230 -> head_opt)
                      Range: File "", line 141, characters 6-14
                      Body Range: File "", line 141, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#239 -> iter)
                      Range: File "", line 145, characters 6-10
                      Body Range: File "", line 145, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#225 -> length)
                      Range: File "", line 139, characters 6-12
                      Body Range: File "", line 139, characters 13-21
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#236 -> map)
                      Range: File "", line 144, characters 6-9
                      Body Range: File "", line 144, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10
                      (size#227 -> size)
                      Range: File "", line 140, characters 6-10
                      Body Range: File "", line 140, characters 11-19
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#233 -> tail_opt)
                      Range: File "", line 142, characters 6-14
                      Body Range: File "", line 142, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 328, characters 4-8

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

    (Option#294 -> Option)
    Range: File "", line 162, characters 7-13
    Body Range: File "", line 162, character 0 to line 171, character 3
    Content: Members: Variable definitions:
                      (is_none#291 -> is_none)
                      Range: File "", line 169, characters 6-13
                      Body Range: File "", line 169, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#293 -> is_some)
                      Range: File "", line 170, characters 6-13
                      Body Range: File "", line 170, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#281 -> map)
                      Range: File "", line 166, characters 15-18
                      Body Range: File "", line 166, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#274 -> unopt)
                      Range: File "", line 163, characters 6-11
                      Body Range: File "", line 163, characters 12-20
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#278 -> unopt_with_error)
                      Range: File "", line 165, characters 6-22
                      Body Range: File "", line 165, characters 23-31
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#285 -> value)
                      Range: File "", line 167, characters 6-11
                      Body Range: File "", line 167, characters 12-20
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#289 -> value_exn)
                      Range: File "", line 168, characters 6-15
                      Body Range: File "", line 168, characters 16-28
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#223 -> Set)
    Range: File "", line 123, characters 7-10
    Body Range: File "", line 123, character 0 to line 136, character 3
    Content: Members: Variable definitions:
                      (add#204 -> add)
                      Range: File "", line 130, characters 6-9
                      Body Range: File "", line 130, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (cardinal#196 -> cardinal)
                      Range: File "", line 126, characters 6-14
                      Body Range: File "", line 126, characters 15-23
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#192 -> empty)
                      Range: File "", line 124, characters 6-11
                      Body Range: File "", line 124, characters 12-20
                      Content: |core: ∀ a : * . set (a)|
                      references: []
                      (fold#218 -> fold)
                      Range: File "", line 134, characters 6-10
                      Body Range: File "", line 134, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#222 -> fold_desc)
                      Range: File "", line 135, characters 6-15
                      Body Range: File "", line 135, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: []
                      (iter#214 -> iter)
                      Range: File "", line 133, characters 6-10
                      Body Range: File "", line 133, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#198 -> literal)
                      Range: File "", line 127, characters 25-32
                      Body Range: File "", line 127, characters 33-41
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#201 -> mem)
                      Range: File "", line 129, characters 6-9
                      Body Range: File "", line 129, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#207 -> remove)
                      Range: File "", line 131, characters 6-12
                      Body Range: File "", line 131, characters 13-21
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#194 -> size)
                      Range: File "", line 125, characters 6-10
                      Body Range: File "", line 125, characters 11-19
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#211 -> update)
                      Range: File "", line 132, characters 6-12
                      Body Range: File "", line 132, characters 13-21
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#271 -> String)
    Range: File "", line 154, characters 7-13
    Body Range: File "", line 154, character 0 to line 160, character 3
    Content: Members: Variable definitions:
                      (concat#266 -> concat)
                      Range: File "", line 158, characters 6-12
                      Body Range: File "", line 158, characters 14-16
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#263 -> concats)
                      Range: File "", line 156, characters 6-13
                      Body Range: File "", line 156, characters 15-17
                      Content: |core: list (string) -> string|
                      references: []
                      (length#261 -> length)
                      Range: File "", line 155, characters 6-12
                      Body Range: File "", line 155, characters 14-15
                      Content: |core: string -> nat|
                      references:
                        File "", line 358, characters 22-28 ,
                        File "", line 361, characters 43-49
                      (sub#270 -> sub)
                      Range: File "", line 159, characters 6-9
                      Body Range: File "", line 159, characters 11-12
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 359, characters 24-27 ,
                        File "", line 361, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 358, characters 15-21 ,
      File "", line 359, characters 17-23 ,
      File "", line 361, characters 16-22 ,
      File "", line 361, characters 36-42

    (Test#705 -> Test)
    Range: File "", line 233, characters 7-11
    Body Range: File "", line 233, character 0 to line 471, character 3
    Content: Members: Variable definitions:
                      (add_account#522 -> add_account)
                      Range: File "", line 341, characters 6-17
                      Body Range: File "", line 341, characters 19-20
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#691 -> assert)
                      Range: File "", line 463, characters 6-12
                      Body Range: File "", line 463, characters 14-15
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#695 -> assert_none)
                      Range: File "", line 465, characters 6-17
                      Body Range: File "", line 465, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#704 -> assert_none_with_error)
                      Range: File "", line 469, characters 6-28
                      Body Range: File "", line 469, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#693 -> assert_some)
                      Range: File "", line 464, characters 6-17
                      Body Range: File "", line 464, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#701 -> assert_some_with_error)
                      Range: File "", line 468, characters 6-28
                      Body Range: File "", line 468, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#698 -> assert_with_error)
                      Range: File "", line 467, characters 6-23
                      Body Range: File "", line 467, characters 25-26
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#415 -> bake_until_n_cycle_end)
                      Range: File "", line 260, characters 6-28
                      Body Range: File "", line 260, characters 30-31
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#525 -> baker_account)
                      Range: File "", line 342, characters 6-19
                      Body Range: File "", line 342, characters 21-22
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#510 -> bootstrap_contract)
                      Range: File "", line 337, characters 6-24
                      Body Range: File "", line 337, characters 25-35
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#419 -> cast_address)
                      Range: File "", line 262, characters 6-18
                      Body Range: File "", line 262, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 371, characters 35-47 ,
                        File "", line 381, characters 35-47 ,
                        File "", line 388, characters 35-47
                      (chr#459 -> chr)
                      Range: File "", line 286, characters 6-9
                      Body Range: File "", line 286, characters 11-12
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#454 -> compile_contract)
                      Range: File "", line 281, characters 6-22
                      Body Range: File "", line 281, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 367, characters 12-28 ,
                        File "", line 377, characters 12-28
                      (compile_contract_from_file#596 -> compile_contract_from_file)
                      Range: File "", line 390, characters 6-32
                      Body Range: File "", line 390, characters 34-36
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 394, characters 12-38
                      (compile_contract_with_views#572 -> compile_contract_with_views)
                      Range: File "", line 373, characters 8-35
                      Body Range: File "", line 373, characters 36-46
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 384, characters 12-39
                      (compile_value#377 -> compile_value)
                      Range: File "", line 238, characters 6-19
                      Body Range: File "", line 238, characters 20-28
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#427 -> constant_to_michelson_program)
                      Range: File "", line 266, characters 6-35
                      Body Range: File "", line 266, characters 37-38
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#531 -> create_chest)
                      Range: File "", line 344, characters 6-18
                      Body Range: File "", line 344, characters 20-21
                      Content: |core: bytes -> nat -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#534 -> create_chest_key)
                      Range: File "", line 345, characters 6-22
                      Body Range: File "", line 345, characters 24-25
                      Content: |core: chest -> nat -> chest_key|
                      references: []
                      (decompile#413 -> decompile)
                      Range: File "", line 259, characters 6-15
                      Body Range: File "", line 259, characters 16-24
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 277, characters 5-14
                      (drop_context#435 -> drop_context)
                      Range: File "", line 270, characters 6-18
                      Body Range: File "", line 270, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#393 -> eprint)
                      Range: File "", line 246, characters 6-12
                      Body Range: File "", line 246, characters 14-15
                      Content: |core: string -> unit|
                      references: []
                      (eval#375 -> eval)
                      Range: File "", line 236, characters 6-10
                      Body Range: File "", line 236, characters 11-19
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 238, characters 59-63 ,
                        File "", line 349, characters 32-36 ,
                        File "", line 354, characters 34-38 ,
                        File "", line 368, characters 12-16 ,
                        File "", line 378, characters 12-16 ,
                        File "", line 385, characters 12-16
                      (failwith#381 -> failwith)
                      Range: File "", line 240, characters 6-14
                      Body Range: File "", line 240, characters 15-25
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 463, characters 51-59 ,
                        File "", line 464, characters 74-82 ,
                        File "", line 465, characters 89-97 ,
                        File "", line 467, characters 68-76 ,
                        File "", line 468, characters 98-106 ,
                        File "", line 469, characters 113-121
                      (get_balance#389 -> get_balance)
                      Range: File "", line 244, characters 6-17
                      Body Range: File "", line 244, characters 19-20
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#402 -> get_bootstrap_account)
                      Range: File "", line 252, characters 6-27
                      Body Range: File "", line 252, characters 29-30
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#487 -> get_last_events_from)
                      Range: File "", line 321, characters 6-26
                      Body Range: File "", line 321, characters 27-39
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#444 -> get_storage)
                      Range: File "", line 273, characters 6-17
                      Body Range: File "", line 273, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#387 -> get_storage_of_address)
                      Range: File "", line 243, characters 6-28
                      Body Range: File "", line 243, characters 30-31
                      Content: |core: address -> michelson_program|
                      references: File "", line 276, characters 32-54
                      (get_time#417 -> get_time)
                      Range: File "", line 261, characters 6-14
                      Body Range: File "", line 261, characters 16-18
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#379 -> get_total_voting_power)
                      Range: File "", line 239, characters 6-28
                      Body Range: File "", line 239, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#395 -> get_voting_power)
                      Range: File "", line 247, characters 6-22
                      Body Range: File "", line 247, characters 24-26
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#406 -> last_originations)
                      Range: File "", line 254, characters 6-23
                      Body Range: File "", line 254, characters 25-26
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#499 -> log)
                      Range: File "", line 331, characters 6-9
                      Body Range: File "", line 331, characters 10-18
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 360, characters 25-28
                      (michelson_equal#551 -> michelson_equal)
                      Range: File "", line 356, characters 6-21
                      Body Range: File "", line 356, characters 23-25
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#513 -> mutate_value)
                      Range: File "", line 338, characters 6-18
                      Body Range: File "", line 338, characters 19-27
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 402, characters 23-35 ,
                        File "", line 414, characters 23-35
                      (mutation_test#620 -> mutation_test)
                      Range: File "", line 398, characters 6-19
                      Body Range: File "", line 398, characters 20-30
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#636 -> mutation_test_all)
                      Range: File "", line 410, characters 6-23
                      Body Range: File "", line 410, characters 24-34
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (new_account#411 -> new_account)
                      Range: File "", line 258, characters 6-17
                      Body Range: File "", line 258, characters 19-20
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#460 -> nl)
                      Range: File "", line 296, characters 6-8
                      Body Range: File "", line 296, characters 11-53
                      Content: |unresolved|
                      references: File "", line 298, characters 15-17
                      (nth_bootstrap_account#400 -> nth_bootstrap_account)
                      Range: File "", line 249, characters 6-27
                      Body Range: File "", line 249, characters 29-30
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#397 -> nth_bootstrap_contract)
                      Range: File "", line 248, characters 6-28
                      Body Range: File "", line 248, characters 30-31
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#404 -> nth_bootstrap_typed_address)
                      Range: File "", line 253, characters 6-33
                      Body Range: File "", line 253, characters 34-44
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#568 -> originate)
                      Range: File "", line 366, characters 6-15
                      Body Range: File "", line 366, characters 16-26
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_contract#559 -> originate_contract)
                      Range: File "", line 365, characters 6-24
                      Body Range: File "", line 365, characters 26-27
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 369, characters 12-30 ,
                        File "", line 379, characters 12-30 ,
                        File "", line 386, characters 12-30 ,
                        File "", line 395, characters 12-30 ,
                        File "", line 426, characters 14-32 ,
                        File "", line 446, characters 14-32
                      (originate_from_file#605 -> originate_from_file)
                      Range: File "", line 393, characters 6-25
                      Body Range: File "", line 393, characters 27-29
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_from_file_and_mutate#662 -> originate_from_file_and_mutate)
                      Range: File "", line 422, characters 6-36
                      Body Range: File "", line 422, characters 37-45
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#689 -> originate_from_file_and_mutate_all)
                      Range: File "", line 442, characters 6-40
                      Body Range: File "", line 442, characters 41-49
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module#591 -> originate_module)
                      Range: File "", line 383, characters 6-22
                      Body Range: File "", line 383, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) ->
                                                                ( list (operation) *
                                                                  s ) *
                                                                views (s) ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_uncurried#581 -> originate_uncurried)
                      Range: File "", line 376, characters 6-25
                      Body Range: File "", line 376, characters 26-36
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (parse_michelson#429 -> parse_michelson)
                      Range: File "", line 267, characters 6-21
                      Body Range: File "", line 267, characters 23-24
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#391 -> print)
                      Range: File "", line 245, characters 6-11
                      Body Range: File "", line 245, characters 13-14
                      Content: |core: string -> unit|
                      references:
                        File "", line 298, characters 4-9 ,
                        File "", line 334, characters 4-9
                      (println#462 -> println)
                      Range: File "", line 297, characters 6-13
                      Body Range: File "", line 297, characters 15-16
                      Content: |core: string -> unit|
                      references: []
                      (random#409 -> random)
                      Range: File "", line 255, characters 6-12
                      Body Range: File "", line 255, characters 13-21
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#456 -> read_contract_from_file)
                      Range: File "", line 285, characters 6-29
                      Body Range: File "", line 285, characters 31-33
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#423 -> register_constant)
                      Range: File "", line 264, characters 6-23
                      Body Range: File "", line 264, characters 25-26
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#421 -> register_delegate)
                      Range: File "", line 263, characters 6-23
                      Body Range: File "", line 263, characters 25-27
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#502 -> reset_state)
                      Range: File "", line 335, characters 6-17
                      Body Range: File "", line 335, characters 19-20
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#506 -> reset_state_at)
                      Range: File "", line 336, characters 6-20
                      Body Range: File "", line 336, characters 22-23
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#431 -> restore_context)
                      Range: File "", line 268, characters 6-21
                      Body Range: File "", line 268, characters 23-24
                      Content: |core: unit -> unit|
                      references: []
                      (run#372 -> run)
                      Range: File "", line 235, characters 6-9
                      Body Range: File "", line 235, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 236, characters 50-53
                      (save_context#433 -> save_context)
                      Range: File "", line 269, characters 6-18
                      Body Range: File "", line 269, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#516 -> save_mutation)
                      Range: File "", line 339, characters 6-19
                      Body Range: File "", line 339, characters 21-22
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#448 -> set_baker)
                      Range: File "", line 279, characters 6-15
                      Body Range: File "", line 279, characters 17-18
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#446 -> set_baker_policy)
                      Range: File "", line 278, characters 6-22
                      Body Range: File "", line 278, characters 24-26
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 279, characters 39-55
                      (set_big_map#528 -> set_big_map)
                      Range: File "", line 343, characters 6-17
                      Body Range: File "", line 343, characters 18-28
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#463 -> set_print_values)
                      Range: File "", line 300, characters 6-22
                      Body Range: File "", line 300, characters 24-25
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#385 -> set_source)
                      Range: File "", line 242, characters 6-16
                      Body Range: File "", line 242, characters 18-19
                      Content: |core: address -> unit|
                      references: []
                      (sign#519 -> sign)
                      Range: File "", line 340, characters 6-10
                      Body Range: File "", line 340, characters 12-14
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#450 -> size)
                      Range: File "", line 280, characters 6-10
                      Body Range: File "", line 280, characters 12-13
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 370, characters 12-16 ,
                        File "", line 380, characters 12-16 ,
                        File "", line 387, characters 12-16 ,
                        File "", line 396, characters 12-16 ,
                        File "", line 427, characters 14-18 ,
                        File "", line 447, characters 14-18
                      (to_contract#383 -> to_contract)
                      Range: File "", line 241, characters 6-17
                      Body Range: File "", line 241, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 274, characters 25-36 ,
                        File "", line 322, characters 30-41
                      (to_entrypoint#555 -> to_entrypoint)
                      Range: File "", line 357, characters 6-19
                      Body Range: File "", line 357, characters 20-32
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#439 -> to_json)
                      Range: File "", line 272, characters 6-13
                      Body Range: File "", line 272, characters 14-22
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#437 -> to_string)
                      Range: File "", line 271, characters 6-15
                      Body Range: File "", line 271, characters 16-24
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 289, characters 68-77 ,
                        File "", line 291, characters 67-76 ,
                        File "", line 293, characters 61-70 ,
                        File "", line 333, characters 12-21
                      (to_typed_address#425 -> to_typed_address)
                      Range: File "", line 265, characters 6-22
                      Body Range: File "", line 265, characters 23-33
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#491 -> transfer)
                      Range: File "", line 329, characters 6-14
                      Body Range: File "", line 329, characters 16-17
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#495 -> transfer_exn)
                      Range: File "", line 330, characters 6-18
                      Body Range: File "", line 330, characters 20-21
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#541 -> transfer_to_contract)
                      Range: File "", line 346, characters 6-26
                      Body Range: File "", line 346, characters 27-35
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#548 -> transfer_to_contract_exn)
                      Range: File "", line 351, characters 6-30
                      Body Range: File "", line 351, characters 31-39
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#464 -> unset_print_values)
                      Range: File "", line 301, characters 6-24
                      Body Range: File "", line 301, characters 26-27
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#478 -> PBT)
                      Range: File "", line 303, characters 9-12
                      Body Range: File "", line 303, character 2 to line 319, character 5
                      Content: Members: Variable definitions:
                                        (gen#465 -> gen)
                                        Range: File "", line 304, characters 8-11
                                        Body Range: File "", line 304, characters 12-20
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (gen_small#466 -> gen_small)
                                        Range: File "", line 305, characters 8-17
                                        Body Range: File "", line 305, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#469 -> make_test)
                                        Range: File "", line 306, characters 8-17
                                        Body Range: File "", line 306, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#477 -> run)
                                        Range: File "", line 307, characters 8-11
                                        Body Range: File "", line 307, characters 12-20
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
                      references: File "", line 322, characters 21-28
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
                      references: File "", line 261, characters 47-54
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
      File "", line 261, characters 41-46 ,
      File "", line 322, characters 15-20

    (Transpiled#191 -> Transpiled)
    Range: File "", line 117, characters 7-17
    Body Range: File "", line 117, character 0 to line 121, character 3
    Content: Members: Variable definitions:
                      (map_add#187 -> map_add)
                      Range: File "", line 119, characters 6-13
                      Body Range: File "", line 119, characters 14-26
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      (map_find_opt#183 -> map_find_opt)
                      Range: File "", line 118, characters 6-18
                      Body Range: File "", line 118, characters 19-29
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      (map_remove#190 -> map_remove)
                      Range: File "", line 120, characters 6-16
                      Body Range: File "", line 120, characters 17-27
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: [] |}]

let () = Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:""
