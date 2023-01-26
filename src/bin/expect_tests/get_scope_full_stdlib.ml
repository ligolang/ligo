open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Test#669 assert_none_with_error#668 assert_some_with_error#664 assert_with_error#660 assert_none#657 assert_some#654 assert#651 originate_from_file_and_mutate_all#649 originate_from_file_and_mutate#622 mutation_test_all#596 mutation_test#580 originate_from_file#565 compile_contract_from_file#556 originate#551 originate_contract#542 to_entrypoint#538 michelson_equal#534 transfer_to_contract_exn#531 transfer_to_contract#524 create_chest_key#517 create_chest#514 set_big_map#511 baker_account#508 add_account#505 sign#502 save_mutation#499 mutate_value#496 bootstrap_contract#493 reset_state_at#489 reset_state#485 log#482 transfer_exn#478 transfer#474 get_last_events_from#470 PBT#460 run#459 make_test#450 gen_small#447 gen#446 unset_print_values#445 set_print_values#444 println#443 nl#441 chr#440 read_contract_from_file#437 compile_contract#435 size#432 set_baker#430 set_baker_policy#428 get_storage#426 to_json#421 to_string#419 drop_context#417 save_context#415 restore_context#413 parse_michelson#411 constant_to_michelson_program#409 to_typed_address#407 register_constant#405 register_delegate#403 cast_address#401 get_time#399 bake_until_n_cycle_end#397 decompile#395 new_account#393 random#391 last_originations#388 nth_bootstrap_typed_address#386 get_bootstrap_account#384 nth_bootstrap_account#382 nth_bootstrap_contract#379 get_voting_power#377 eprint#375 print#373 get_balance#371 get_storage_of_address#369 set_source#367 to_contract#365 failwith#363 get_total_voting_power#361 compile_value#359 eval#357 run#354 unforged_ticket#351 pbt_result#350 pbt_test#349 test_baker_policy#348 test_exec_result#347 test_exec_error#346 test_exec_error_balance_too_low#345 ediv#344 assert_none_with_error#341 assert_some_with_error#337 assert_with_error#333 ignore#330 int#329 unit#327 false#326 true#325 is_nat#324 abs#322 assert_none#320 assert_some#317 assert#314 Crypto#312 check#311 hash_key#307 keccak#305 sha3#303 sha512#301 sha256#299 blake2b#297 Bytes#295 sub#294 concat#290 length#287 unpack#285 pack#283 concats#281 Option#279 is_some#278 is_none#275 value_exn#272 value#268 map#264 unopt_with_error#261 unopt#257 String#254 sub#253 concat#249 concats#246 length#244 List#242 find_opt#241 cons#237 fold_right#234 fold_left#230 fold#226 iter#222 map#219 tail_opt#216 head_opt#212 size#208 length#206 Set#204 fold_desc#203 fold#199 iter#195 update#192 remove#188 add#185 mem#182 literal#179 cardinal#177 size#175 empty#173 Map#172 fold#171 map#167 iter#164 find_opt#161 find#158 get_and_update#155 update#151 remove#147 add#144 mem#140 literal#137 size#135 empty#133 Big_map#132 find#131 find_opt#128 get_and_update#125 update#121 remove#117 add#114 mem#110 literal#107 empty#105 Bitwise#104 shift_right#103 shift_left#100 or#97 xor#94 and#91 Tezos#88 sapling_verify_update#87 emit#84 get_entrypoint#81 get_entrypoint_opt#76 create_contract#73 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ a#670 Test#669 assert_none_with_error#668 assert_some_with_error#664 assert_with_error#660 assert_none#657 assert_some#654 assert#651 originate_from_file_and_mutate_all#649 originate_from_file_and_mutate#622 mutation_test_all#596 mutation_test#580 originate_from_file#565 compile_contract_from_file#556 originate#551 originate_contract#542 to_entrypoint#538 michelson_equal#534 transfer_to_contract_exn#531 transfer_to_contract#524 create_chest_key#517 create_chest#514 set_big_map#511 baker_account#508 add_account#505 sign#502 save_mutation#499 mutate_value#496 bootstrap_contract#493 reset_state_at#489 reset_state#485 log#482 transfer_exn#478 transfer#474 get_last_events_from#470 PBT#460 run#459 make_test#450 gen_small#447 gen#446 unset_print_values#445 set_print_values#444 println#443 nl#441 chr#440 read_contract_from_file#437 compile_contract#435 size#432 set_baker#430 set_baker_policy#428 get_storage#426 to_json#421 to_string#419 drop_context#417 save_context#415 restore_context#413 parse_michelson#411 constant_to_michelson_program#409 to_typed_address#407 register_constant#405 register_delegate#403 cast_address#401 get_time#399 bake_until_n_cycle_end#397 decompile#395 new_account#393 random#391 last_originations#388 nth_bootstrap_typed_address#386 get_bootstrap_account#384 nth_bootstrap_account#382 nth_bootstrap_contract#379 get_voting_power#377 eprint#375 print#373 get_balance#371 get_storage_of_address#369 set_source#367 to_contract#365 failwith#363 get_total_voting_power#361 compile_value#359 eval#357 run#354 unforged_ticket#351 pbt_result#350 pbt_test#349 test_baker_policy#348 test_exec_result#347 test_exec_error#346 test_exec_error_balance_too_low#345 ediv#344 assert_none_with_error#341 assert_some_with_error#337 assert_with_error#333 ignore#330 int#329 unit#327 false#326 true#325 is_nat#324 abs#322 assert_none#320 assert_some#317 assert#314 Crypto#312 check#311 hash_key#307 keccak#305 sha3#303 sha512#301 sha256#299 blake2b#297 Bytes#295 sub#294 concat#290 length#287 unpack#285 pack#283 concats#281 Option#279 is_some#278 is_none#275 value_exn#272 value#268 map#264 unopt_with_error#261 unopt#257 String#254 sub#253 concat#249 concats#246 length#244 List#242 find_opt#241 cons#237 fold_right#234 fold_left#230 fold#226 iter#222 map#219 tail_opt#216 head_opt#212 size#208 length#206 Set#204 fold_desc#203 fold#199 iter#195 update#192 remove#188 add#185 mem#182 literal#179 cardinal#177 size#175 empty#173 Map#172 fold#171 map#167 iter#164 find_opt#161 find#158 get_and_update#155 update#151 remove#147 add#144 mem#140 literal#137 size#135 empty#133 Big_map#132 find#131 find_opt#128 get_and_update#125 update#121 remove#117 add#114 mem#110 literal#107 empty#105 Bitwise#104 shift_right#103 shift_left#100 or#97 xor#94 and#91 Tezos#88 sapling_verify_update#87 emit#84 get_entrypoint#81 get_entrypoint_opt#76 create_contract#73 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 14
    [ c#671 a#670 Test#669 assert_none_with_error#668 assert_some_with_error#664 assert_with_error#660 assert_none#657 assert_some#654 assert#651 originate_from_file_and_mutate_all#649 originate_from_file_and_mutate#622 mutation_test_all#596 mutation_test#580 originate_from_file#565 compile_contract_from_file#556 originate#551 originate_contract#542 to_entrypoint#538 michelson_equal#534 transfer_to_contract_exn#531 transfer_to_contract#524 create_chest_key#517 create_chest#514 set_big_map#511 baker_account#508 add_account#505 sign#502 save_mutation#499 mutate_value#496 bootstrap_contract#493 reset_state_at#489 reset_state#485 log#482 transfer_exn#478 transfer#474 get_last_events_from#470 PBT#460 run#459 make_test#450 gen_small#447 gen#446 unset_print_values#445 set_print_values#444 println#443 nl#441 chr#440 read_contract_from_file#437 compile_contract#435 size#432 set_baker#430 set_baker_policy#428 get_storage#426 to_json#421 to_string#419 drop_context#417 save_context#415 restore_context#413 parse_michelson#411 constant_to_michelson_program#409 to_typed_address#407 register_constant#405 register_delegate#403 cast_address#401 get_time#399 bake_until_n_cycle_end#397 decompile#395 new_account#393 random#391 last_originations#388 nth_bootstrap_typed_address#386 get_bootstrap_account#384 nth_bootstrap_account#382 nth_bootstrap_contract#379 get_voting_power#377 eprint#375 print#373 get_balance#371 get_storage_of_address#369 set_source#367 to_contract#365 failwith#363 get_total_voting_power#361 compile_value#359 eval#357 run#354 unforged_ticket#351 pbt_result#350 pbt_test#349 test_baker_policy#348 test_exec_result#347 test_exec_error#346 test_exec_error_balance_too_low#345 ediv#344 assert_none_with_error#341 assert_some_with_error#337 assert_with_error#333 ignore#330 int#329 unit#327 false#326 true#325 is_nat#324 abs#322 assert_none#320 assert_some#317 assert#314 Crypto#312 check#311 hash_key#307 keccak#305 sha3#303 sha512#301 sha256#299 blake2b#297 Bytes#295 sub#294 concat#290 length#287 unpack#285 pack#283 concats#281 Option#279 is_some#278 is_none#275 value_exn#272 value#268 map#264 unopt_with_error#261 unopt#257 String#254 sub#253 concat#249 concats#246 length#244 List#242 find_opt#241 cons#237 fold_right#234 fold_left#230 fold#226 iter#222 map#219 tail_opt#216 head_opt#212 size#208 length#206 Set#204 fold_desc#203 fold#199 iter#195 update#192 remove#188 add#185 mem#182 literal#179 cardinal#177 size#175 empty#173 Map#172 fold#171 map#167 iter#164 find_opt#161 find#158 get_and_update#155 update#151 remove#147 add#144 mem#140 literal#137 size#135 empty#133 Big_map#132 find#131 find_opt#128 get_and_update#125 update#121 remove#117 add#114 mem#110 literal#107 empty#105 Bitwise#104 shift_right#103 shift_left#100 or#97 xor#94 and#91 Tezos#88 sapling_verify_update#87 emit#84 get_entrypoint#81 get_entrypoint_opt#76 create_contract#73 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ d#672 c#671 a#670 Test#669 assert_none_with_error#668 assert_some_with_error#664 assert_with_error#660 assert_none#657 assert_some#654 assert#651 originate_from_file_and_mutate_all#649 originate_from_file_and_mutate#622 mutation_test_all#596 mutation_test#580 originate_from_file#565 compile_contract_from_file#556 originate#551 originate_contract#542 to_entrypoint#538 michelson_equal#534 transfer_to_contract_exn#531 transfer_to_contract#524 create_chest_key#517 create_chest#514 set_big_map#511 baker_account#508 add_account#505 sign#502 save_mutation#499 mutate_value#496 bootstrap_contract#493 reset_state_at#489 reset_state#485 log#482 transfer_exn#478 transfer#474 get_last_events_from#470 PBT#460 run#459 make_test#450 gen_small#447 gen#446 unset_print_values#445 set_print_values#444 println#443 nl#441 chr#440 read_contract_from_file#437 compile_contract#435 size#432 set_baker#430 set_baker_policy#428 get_storage#426 to_json#421 to_string#419 drop_context#417 save_context#415 restore_context#413 parse_michelson#411 constant_to_michelson_program#409 to_typed_address#407 register_constant#405 register_delegate#403 cast_address#401 get_time#399 bake_until_n_cycle_end#397 decompile#395 new_account#393 random#391 last_originations#388 nth_bootstrap_typed_address#386 get_bootstrap_account#384 nth_bootstrap_account#382 nth_bootstrap_contract#379 get_voting_power#377 eprint#375 print#373 get_balance#371 get_storage_of_address#369 set_source#367 to_contract#365 failwith#363 get_total_voting_power#361 compile_value#359 eval#357 run#354 unforged_ticket#351 pbt_result#350 pbt_test#349 test_baker_policy#348 test_exec_result#347 test_exec_error#346 test_exec_error_balance_too_low#345 ediv#344 assert_none_with_error#341 assert_some_with_error#337 assert_with_error#333 ignore#330 int#329 unit#327 false#326 true#325 is_nat#324 abs#322 assert_none#320 assert_some#317 assert#314 Crypto#312 check#311 hash_key#307 keccak#305 sha3#303 sha512#301 sha256#299 blake2b#297 Bytes#295 sub#294 concat#290 length#287 unpack#285 pack#283 concats#281 Option#279 is_some#278 is_none#275 value_exn#272 value#268 map#264 unopt_with_error#261 unopt#257 String#254 sub#253 concat#249 concats#246 length#244 List#242 find_opt#241 cons#237 fold_right#234 fold_left#230 fold#226 iter#222 map#219 tail_opt#216 head_opt#212 size#208 length#206 Set#204 fold_desc#203 fold#199 iter#195 update#192 remove#188 add#185 mem#182 literal#179 cardinal#177 size#175 empty#173 Map#172 fold#171 map#167 iter#164 find_opt#161 find#158 get_and_update#155 update#151 remove#147 add#144 mem#140 literal#137 size#135 empty#133 Big_map#132 find#131 find_opt#128 get_and_update#125 update#121 remove#117 add#114 mem#110 literal#107 empty#105 Bitwise#104 shift_right#103 shift_left#100 or#97 xor#94 and#91 Tezos#88 sapling_verify_update#87 emit#84 get_entrypoint#81 get_entrypoint_opt#76 create_contract#73 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ e#673 a#670 Test#669 assert_none_with_error#668 assert_some_with_error#664 assert_with_error#660 assert_none#657 assert_some#654 assert#651 originate_from_file_and_mutate_all#649 originate_from_file_and_mutate#622 mutation_test_all#596 mutation_test#580 originate_from_file#565 compile_contract_from_file#556 originate#551 originate_contract#542 to_entrypoint#538 michelson_equal#534 transfer_to_contract_exn#531 transfer_to_contract#524 create_chest_key#517 create_chest#514 set_big_map#511 baker_account#508 add_account#505 sign#502 save_mutation#499 mutate_value#496 bootstrap_contract#493 reset_state_at#489 reset_state#485 log#482 transfer_exn#478 transfer#474 get_last_events_from#470 PBT#460 run#459 make_test#450 gen_small#447 gen#446 unset_print_values#445 set_print_values#444 println#443 nl#441 chr#440 read_contract_from_file#437 compile_contract#435 size#432 set_baker#430 set_baker_policy#428 get_storage#426 to_json#421 to_string#419 drop_context#417 save_context#415 restore_context#413 parse_michelson#411 constant_to_michelson_program#409 to_typed_address#407 register_constant#405 register_delegate#403 cast_address#401 get_time#399 bake_until_n_cycle_end#397 decompile#395 new_account#393 random#391 last_originations#388 nth_bootstrap_typed_address#386 get_bootstrap_account#384 nth_bootstrap_account#382 nth_bootstrap_contract#379 get_voting_power#377 eprint#375 print#373 get_balance#371 get_storage_of_address#369 set_source#367 to_contract#365 failwith#363 get_total_voting_power#361 compile_value#359 eval#357 run#354 unforged_ticket#351 pbt_result#350 pbt_test#349 test_baker_policy#348 test_exec_result#347 test_exec_error#346 test_exec_error_balance_too_low#345 ediv#344 assert_none_with_error#341 assert_some_with_error#337 assert_with_error#333 ignore#330 int#329 unit#327 false#326 true#325 is_nat#324 abs#322 assert_none#320 assert_some#317 assert#314 Crypto#312 check#311 hash_key#307 keccak#305 sha3#303 sha512#301 sha256#299 blake2b#297 Bytes#295 sub#294 concat#290 length#287 unpack#285 pack#283 concats#281 Option#279 is_some#278 is_none#275 value_exn#272 value#268 map#264 unopt_with_error#261 unopt#257 String#254 sub#253 concat#249 concats#246 length#244 List#242 find_opt#241 cons#237 fold_right#234 fold_left#230 fold#226 iter#222 map#219 tail_opt#216 head_opt#212 size#208 length#206 Set#204 fold_desc#203 fold#199 iter#195 update#192 remove#188 add#185 mem#182 literal#179 cardinal#177 size#175 empty#173 Map#172 fold#171 map#167 iter#164 find_opt#161 find#158 get_and_update#155 update#151 remove#147 add#144 mem#140 literal#137 size#135 empty#133 Big_map#132 find#131 find_opt#128 get_and_update#125 update#121 remove#117 add#114 mem#110 literal#107 empty#105 Bitwise#104 shift_right#103 shift_left#100 or#97 xor#94 and#91 Tezos#88 sapling_verify_update#87 emit#84 get_entrypoint#81 get_entrypoint_opt#76 create_contract#73 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30

    Variable definitions:
    (a#670 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#322 -> abs)
    Range: File "", line 322, characters 4-7
    Body Range: File "", line 322, characters 9-10
    Content: |core: int -> nat|
    references: File "", line 520, characters 31-34
    (assert#314 -> assert)
    Range: File "", line 319, characters 4-10
    Body Range: File "", line 319, characters 12-13
    Content: |core: bool -> unit|
    references: []
    (assert_none#320 -> assert_none)
    Range: File "", line 321, characters 4-15
    Body Range: File "", line 321, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#341 -> assert_none_with_error)
    Range: File "", line 333, characters 4-26
    Body Range: File "", line 333, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#317 -> assert_some)
    Range: File "", line 320, characters 4-15
    Body Range: File "", line 320, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#337 -> assert_some_with_error)
    Range: File "", line 332, characters 4-26
    Body Range: File "", line 332, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#333 -> assert_with_error)
    Range: File "", line 331, characters 4-21
    Body Range: File "", line 331, characters 23-24
    Content: |unresolved|
    references: []
    (b#674 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#671 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#672 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#673 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    (ediv#344 -> ediv)
    Range: File "", line 334, characters 4-8
    Body Range: File "", line 334, characters 9-19
    Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_ediv (a ,
    b)|
    references: []
    (failwith#0 -> failwith)
    Range: File "", line 3, characters 4-12
    Body Range: File "", line 3, characters 13-23
    Content: |unresolved|
    references:
      File "", line 39, characters 27-35 ,
      File "", line 44, characters 27-35 ,
      File "", line 68, characters 27-35 ,
      File "", line 263, characters 79-87 ,
      File "", line 266, characters 103-111 ,
      File "", line 269, characters 83-91 ,
      File "", line 319, characters 49-57 ,
      File "", line 320, characters 72-80 ,
      File "", line 321, characters 87-95 ,
      File "", line 331, characters 66-74 ,
      File "", line 332, characters 96-104 ,
      File "", line 333, characters 111-119
    (false#326 -> false)
    Range: File "", line 325, characters 4-9
    Body Range: File "", line 325, characters 19-24
    Content: |core: bool|
    references:
      File "", line 398, characters 51-56 ,
      File "", line 442, characters 90-95 ,
      File "", line 445, characters 62-67
    (ignore#330 -> ignore)
    Range: File "", line 328, characters 4-10
    Body Range: File "", line 328, characters 11-19
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#329 -> int)
    Range: File "", line 327, characters 4-7
    Body Range: File "", line 327, characters 8-16
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 394, characters 97-100 ,
      File "", line 430, characters 79-82 ,
      File "", line 432, characters 78-81 ,
      File "", line 434, characters 72-75
    (is_nat#324 -> is_nat)
    Range: File "", line 323, characters 4-10
    Body Range: File "", line 323, characters 12-13
    Content: |core: int -> option (nat)|
    references: []
    (true#325 -> true)
    Range: File "", line 324, characters 4-8
    Body Range: File "", line 324, characters 18-22
    Content: |core: bool|
    references:
      File "", line 441, characters 88-92 ,
      File "", line 446, characters 68-72
    (unit#327 -> unit)
    Range: File "", line 326, characters 4-8
    Body Range: File "", line 326, characters 18-38
    Content: |core: unit|
    references: []
    Type definitions:
    (bool#1 -> bool)
    Range: File "", line 5, characters 5-9
    Body Range: File "", line 5, characters 12-24
    Content: : |sum[False -> unit , True -> unit]|
    references:
      File "", line 27, characters 63-67 ,
      File "", line 134, characters 52-56 ,
      File "", line 161, characters 48-52 ,
      File "", line 195, characters 41-45 ,
      File "", line 198, characters 35-39 ,
      File "", line 229, characters 34-38 ,
      File "", line 279, characters 40-44 ,
      File "", line 280, characters 40-44 ,
      File "", line 310, characters 52-56 ,
      File "", line 319, characters 16-20 ,
      File "", line 324, characters 11-15 ,
      File "", line 325, characters 12-16 ,
      File "", line 331, characters 27-31 ,
      File "", line 361, characters 41-45 ,
      File "", line 448, characters 53-57 ,
      File "", line 515, characters 74-78 ,
      File "", line 731, characters 18-22 ,
      File "", line 736, characters 29-33
    (option#2 -> option)
    Range: File "", line 6, characters 8-14
    Body Range: File "", line 6, characters 0-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 23, characters 56-73 ,
      File "", line 28, characters 24-39 ,
      File "", line 30, characters 12-20 ,
      File "", line 35, characters 67-86 ,
      File "", line 49, characters 49-66 ,
      File "", line 57, characters 84-92 ,
      File "", line 59, characters 61-89 ,
      File "", line 61, characters 92-107 ,
      File "", line 63, characters 82-99 ,
      File "", line 72, characters 120-164 ,
      File "", line 137, characters 37-45 ,
      File "", line 138, characters 45-53 ,
      File "", line 138, characters 78-86 ,
      File "", line 139, characters 57-65 ,
      File "", line 164, characters 37-45 ,
      File "", line 165, characters 45-53 ,
      File "", line 165, characters 74-82 ,
      File "", line 167, characters 53-61 ,
      File "", line 219, characters 40-48 ,
      File "", line 220, characters 40-55 ,
      File "", line 229, characters 56-64 ,
      File "", line 230, characters 29-37 ,
      File "", line 263, characters 26-34 ,
      File "", line 266, characters 37-45 ,
      File "", line 267, characters 48-56 ,
      File "", line 267, characters 60-68 ,
      File "", line 268, characters 40-48 ,
      File "", line 269, characters 46-54 ,
      File "", line 279, characters 28-36 ,
      File "", line 280, characters 28-36 ,
      File "", line 286, characters 36-44 ,
      File "", line 320, characters 30-38 ,
      File "", line 321, characters 30-38 ,
      File "", line 323, characters 23-33 ,
      File "", line 332, characters 41-49 ,
      File "", line 333, characters 41-49 ,
      File "", line 427, characters 22-35 ,
      File "", line 488, characters 140-153 ,
      File "", line 489, characters 135-148 ,
      File "", line 494, characters 92-108 ,
      File "", line 497, characters 48-69 ,
      File "", line 498, characters 50-63 ,
      File "", line 501, characters 44-54 ,
      File "", line 507, characters 12-25 ,
      File "", line 512, characters 14-27 ,
      File "", line 533, characters 96-106 ,
      File "", line 540, characters 59-80 ,
      File "", line 543, characters 37-58 ,
      File "", line 565, characters 90-111 ,
      File "", line 571, characters 96-106 ,
      File "", line 574, characters 37-58 ,
      File "", line 591, characters 96-106 ,
      File "", line 732, characters 32-40 ,
      File "", line 733, characters 32-40 ,
      File "", line 737, characters 43-51 ,
      File "", line 738, characters 43-51
    (pbt_result#350 -> pbt_result)
    Range: File "", line 362, characters 8-18
    Body Range: File "", line 362, characters 0-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 449, characters 55-67 ,
      File "", line 450, characters 37-49 ,
      File "", line 452, characters 82-94 ,
      File "", line 456, characters 94-106 ,
      File "", line 459, characters 66-78
    (pbt_test#349 -> pbt_test)
    Range: File "", line 361, characters 8-16
    Body Range: File "", line 361, characters 0-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 448, characters 61-71 ,
      File "", line 449, characters 31-41
    (test_baker_policy#348 -> test_baker_policy)
    Range: File "", line 356, characters 5-22
    Body Range: File "", line 357, character 4 to line 359, character 29
    Content: : |sum[By_account -> address , By_round -> int , Excluding -> list (address)]|
    references: File "", line 420, characters 29-46
    (test_exec_error#346 -> test_exec_error)
    Range: File "", line 349, characters 5-20
    Body Range: File "", line 350, character 4 to line 352, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low , Other -> string , Rejected -> ( michelson_program * address )]|
    references: File "", line 354, characters 49-64
    (test_exec_error_balance_too_low#345 -> test_exec_error_balance_too_low)
    Range: File "", line 346, characters 5-36
    Body Range: File "", line 347, characters 2-79
    Content: : |record[contract_balance -> tez , contract_too_low -> address , spend_request -> tez]|
    references: File "", line 351, characters 23-54
    (test_exec_result#347 -> test_exec_result)
    Range: File "", line 354, characters 5-21
    Body Range: File "", line 354, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 488, characters 65-81 ,
      File "", line 505, characters 73-89
    (unforged_ticket#351 -> unforged_ticket)
    Range: File "", line 364, characters 8-23
    Body Range: File "", line 364, characters 0-91
    Content: : |funtype 's : * . record[amount -> nat , ticketer -> address , value -> 's]|
    references: []
    Module definitions:
    (Big_map#132 -> Big_map)
    Range: File "", line 129, characters 7-14
    Body Range: File "", line 129, character 0 to line 153, character 3
    Content: Members: Variable definitions:
                      (add#114 -> add)
                      Range: File "", line 135, characters 6-9
                      Body Range: File "", line 135, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (empty#105 -> empty)
                      Range: File "", line 130, characters 16-21
                      Body Range: File "", line 130, characters 22-32
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#131 -> find)
                      Range: File "", line 140, characters 6-10
                      Body Range: File "", line 140, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      (find_opt#128 -> find_opt)
                      Range: File "", line 139, characters 6-14
                      Body Range: File "", line 139, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (get_and_update#125 -> get_and_update)
                      Range: File "", line 138, characters 6-20
                      Body Range: File "", line 138, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#107 -> literal)
                      Range: File "", line 131, characters 25-32
                      Body Range: File "", line 131, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#110 -> mem)
                      Range: File "", line 134, characters 6-9
                      Body Range: File "", line 134, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (remove#117 -> remove)
                      Range: File "", line 136, characters 6-12
                      Body Range: File "", line 136, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#121 -> update)
                      Range: File "", line 137, characters 6-12
                      Body Range: File "", line 137, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#104 -> Bitwise)
    Range: File "", line 111, characters 7-14
    Body Range: File "", line 111, character 0 to line 127, character 3
    Content: Members: Variable definitions:
                      (and#91 -> and)
                      Range: File "", line 113, characters 6-10
                      Body Range: File "", line 113, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (or#97 -> or)
                      Range: File "", line 115, characters 6-9
                      Body Range: File "", line 115, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_left#100 -> shift_left)
                      Range: File "", line 116, characters 6-16
                      Body Range: File "", line 116, characters 18-19
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_right#103 -> shift_right)
                      Range: File "", line 117, characters 6-17
                      Body Range: File "", line 117, characters 19-20
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (xor#94 -> xor)
                      Range: File "", line 114, characters 6-9
                      Body Range: File "", line 114, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#295 -> Bytes)
    Range: File "", line 283, characters 7-12
    Body Range: File "", line 283, character 0 to line 299, character 3
    Content: Members: Variable definitions:
                      (concat#290 -> concat)
                      Range: File "", line 290, characters 6-12
                      Body Range: File "", line 290, characters 14-16
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#281 -> concats)
                      Range: File "", line 284, characters 6-13
                      Body Range: File "", line 284, characters 15-17
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#287 -> length)
                      Range: File "", line 287, characters 6-12
                      Body Range: File "", line 287, characters 14-15
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#283 -> pack)
                      Range: File "", line 285, characters 6-10
                      Body Range: File "", line 285, characters 11-19
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#294 -> sub)
                      Range: File "", line 291, characters 6-9
                      Body Range: File "", line 291, characters 11-12
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#285 -> unpack)
                      Range: File "", line 286, characters 6-12
                      Body Range: File "", line 286, characters 13-21
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#312 -> Crypto)
    Range: File "", line 301, characters 7-13
    Body Range: File "", line 301, character 0 to line 317, character 3
    Content: Members: Variable definitions:
                      (blake2b#297 -> blake2b)
                      Range: File "", line 302, characters 6-13
                      Body Range: File "", line 302, characters 15-16
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#311 -> check)
                      Range: File "", line 310, characters 6-11
                      Body Range: File "", line 310, characters 13-14
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#307 -> hash_key)
                      Range: File "", line 307, characters 6-14
                      Body Range: File "", line 307, characters 16-17
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#305 -> keccak)
                      Range: File "", line 306, characters 6-12
                      Body Range: File "", line 306, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#299 -> sha256)
                      Range: File "", line 303, characters 6-12
                      Body Range: File "", line 303, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#303 -> sha3)
                      Range: File "", line 305, characters 6-10
                      Body Range: File "", line 305, characters 12-13
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#301 -> sha512)
                      Range: File "", line 304, characters 6-12
                      Body Range: File "", line 304, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#242 -> List)
    Range: File "", line 216, characters 7-11
    Body Range: File "", line 216, character 0 to line 244, character 3
    Content: Members: Variable definitions:
                      (cons#237 -> cons)
                      Range: File "", line 228, characters 6-10
                      Body Range: File "", line 228, characters 11-19
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (find_opt#241 -> find_opt)
                      Range: File "", line 229, characters 6-14
                      Body Range: File "", line 229, characters 15-23
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#226 -> fold)
                      Range: File "", line 225, characters 6-10
                      Body Range: File "", line 225, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 487, characters 9-13
                      (fold_left#230 -> fold_left)
                      Range: File "", line 226, characters 6-15
                      Body Range: File "", line 226, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#234 -> fold_right)
                      Range: File "", line 227, characters 6-16
                      Body Range: File "", line 227, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references: File "", line 230, characters 4-14
                      (head_opt#212 -> head_opt)
                      Range: File "", line 219, characters 6-14
                      Body Range: File "", line 219, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#222 -> iter)
                      Range: File "", line 224, characters 6-10
                      Body Range: File "", line 224, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#206 -> length)
                      Range: File "", line 217, characters 6-12
                      Body Range: File "", line 217, characters 13-21
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#219 -> map)
                      Range: File "", line 223, characters 6-9
                      Body Range: File "", line 223, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10
                      (size#208 -> size)
                      Range: File "", line 218, characters 6-10
                      Body Range: File "", line 218, characters 11-19
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#216 -> tail_opt)
                      Range: File "", line 220, characters 6-14
                      Body Range: File "", line 220, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 487, characters 4-8

    (Map#172 -> Map)
    Range: File "", line 155, characters 7-10
    Body Range: File "", line 155, character 0 to line 186, character 3
    Content: Members: Variable definitions:
                      (add#144 -> add)
                      Range: File "", line 162, characters 6-9
                      Body Range: File "", line 162, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (empty#133 -> empty)
                      Range: File "", line 156, characters 6-11
                      Body Range: File "", line 156, characters 12-22
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#158 -> find)
                      Range: File "", line 166, characters 6-10
                      Body Range: File "", line 166, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#161 -> find_opt)
                      Range: File "", line 167, characters 6-14
                      Body Range: File "", line 167, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (fold#171 -> fold)
                      Range: File "", line 170, characters 6-10
                      Body Range: File "", line 170, characters 11-23
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c * ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      (get_and_update#155 -> get_and_update)
                      Range: File "", line 165, characters 6-20
                      Body Range: File "", line 165, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (iter#164 -> iter)
                      Range: File "", line 168, characters 6-10
                      Body Range: File "", line 168, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (literal#137 -> literal)
                      Range: File "", line 158, characters 25-32
                      Body Range: File "", line 158, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#167 -> map)
                      Range: File "", line 169, characters 6-9
                      Body Range: File "", line 169, characters 10-22
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k * v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (mem#140 -> mem)
                      Range: File "", line 161, characters 6-9
                      Body Range: File "", line 161, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (remove#147 -> remove)
                      Range: File "", line 163, characters 6-12
                      Body Range: File "", line 163, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (size#135 -> size)
                      Range: File "", line 157, characters 6-10
                      Body Range: File "", line 157, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#151 -> update)
                      Range: File "", line 164, characters 6-12
                      Body Range: File "", line 164, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#279 -> Option)
    Range: File "", line 262, characters 7-13
    Body Range: File "", line 262, character 0 to line 281, character 3
    Content: Members: Variable definitions:
                      (is_none#275 -> is_none)
                      Range: File "", line 279, characters 6-13
                      Body Range: File "", line 279, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#278 -> is_some)
                      Range: File "", line 280, characters 6-13
                      Body Range: File "", line 280, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#264 -> map)
                      Range: File "", line 267, characters 15-18
                      Body Range: File "", line 267, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#257 -> unopt)
                      Range: File "", line 263, characters 6-11
                      Body Range: File "", line 263, characters 12-20
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#261 -> unopt_with_error)
                      Range: File "", line 266, characters 6-22
                      Body Range: File "", line 266, characters 23-31
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#268 -> value)
                      Range: File "", line 268, characters 6-11
                      Body Range: File "", line 268, characters 12-20
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#272 -> value_exn)
                      Range: File "", line 269, characters 6-15
                      Body Range: File "", line 269, characters 16-28
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#204 -> Set)
    Range: File "", line 188, characters 7-10
    Body Range: File "", line 188, character 0 to line 214, character 3
    Content: Members: Variable definitions:
                      (add#185 -> add)
                      Range: File "", line 196, characters 6-9
                      Body Range: File "", line 196, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (cardinal#177 -> cardinal)
                      Range: File "", line 191, characters 6-14
                      Body Range: File "", line 191, characters 15-23
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#173 -> empty)
                      Range: File "", line 189, characters 6-11
                      Body Range: File "", line 189, characters 12-20
                      Content: |core: ∀ a : * . set (a)|
                      references: []
                      (fold#199 -> fold)
                      Range: File "", line 200, characters 6-10
                      Body Range: File "", line 200, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#203 -> fold_desc)
                      Range: File "", line 201, characters 6-15
                      Body Range: File "", line 201, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: []
                      (iter#195 -> iter)
                      Range: File "", line 199, characters 6-10
                      Body Range: File "", line 199, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#179 -> literal)
                      Range: File "", line 192, characters 25-32
                      Body Range: File "", line 192, characters 33-41
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#182 -> mem)
                      Range: File "", line 195, characters 6-9
                      Body Range: File "", line 195, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#188 -> remove)
                      Range: File "", line 197, characters 6-12
                      Body Range: File "", line 197, characters 13-21
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#175 -> size)
                      Range: File "", line 190, characters 6-10
                      Body Range: File "", line 190, characters 11-19
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#192 -> update)
                      Range: File "", line 198, characters 6-12
                      Body Range: File "", line 198, characters 13-21
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#254 -> String)
    Range: File "", line 246, characters 7-13
    Body Range: File "", line 246, character 0 to line 260, character 3
    Content: Members: Variable definitions:
                      (concat#249 -> concat)
                      Range: File "", line 251, characters 6-12
                      Body Range: File "", line 251, characters 14-16
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#246 -> concats)
                      Range: File "", line 248, characters 6-13
                      Body Range: File "", line 248, characters 15-17
                      Content: |core: list (string) -> string|
                      references: []
                      (length#244 -> length)
                      Range: File "", line 247, characters 6-12
                      Body Range: File "", line 247, characters 14-15
                      Content: |core: string -> nat|
                      references:
                        File "", line 517, characters 22-28 ,
                        File "", line 520, characters 43-49
                      (sub#253 -> sub)
                      Range: File "", line 252, characters 6-9
                      Body Range: File "", line 252, characters 11-12
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 518, characters 24-27 ,
                        File "", line 520, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 517, characters 15-21 ,
      File "", line 518, characters 17-23 ,
      File "", line 520, characters 16-22 ,
      File "", line 520, characters 36-42

    (Test#669 -> Test)
    Range: File "", line 366, characters 7-11
    Body Range: File "", line 366, character 0 to line 747, character 3
    Content: Members: Variable definitions:
                      (add_account#505 -> add_account)
                      Range: File "", line 500, characters 6-17
                      Body Range: File "", line 500, characters 19-20
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#651 -> assert)
                      Range: File "", line 731, characters 6-12
                      Body Range: File "", line 731, characters 14-15
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#657 -> assert_none)
                      Range: File "", line 733, characters 6-17
                      Body Range: File "", line 733, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#668 -> assert_none_with_error)
                      Range: File "", line 738, characters 6-28
                      Body Range: File "", line 738, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#654 -> assert_some)
                      Range: File "", line 732, characters 6-17
                      Body Range: File "", line 732, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#664 -> assert_some_with_error)
                      Range: File "", line 737, characters 6-28
                      Body Range: File "", line 737, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#660 -> assert_with_error)
                      Range: File "", line 736, characters 6-23
                      Body Range: File "", line 736, characters 25-26
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#397 -> bake_until_n_cycle_end)
                      Range: File "", line 402, characters 6-28
                      Body Range: File "", line 402, characters 30-31
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#508 -> baker_account)
                      Range: File "", line 501, characters 6-19
                      Body Range: File "", line 501, characters 21-22
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#493 -> bootstrap_contract)
                      Range: File "", line 496, characters 6-24
                      Body Range: File "", line 496, characters 25-35
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#401 -> cast_address)
                      Range: File "", line 404, characters 6-18
                      Body Range: File "", line 404, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references: File "", line 530, characters 35-47
                      (chr#440 -> chr)
                      Range: File "", line 427, characters 6-9
                      Body Range: File "", line 427, characters 11-12
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#435 -> compile_contract)
                      Range: File "", line 423, characters 6-22
                      Body Range: File "", line 423, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> michelson_contract|
                      references: File "", line 526, characters 12-28
                      (compile_contract_from_file#556 -> compile_contract_from_file)
                      Range: File "", line 532, characters 6-32
                      Body Range: File "", line 532, characters 34-36
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 536, characters 12-38
                      (compile_value#359 -> compile_value)
                      Range: File "", line 380, characters 6-19
                      Body Range: File "", line 380, characters 20-28
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#409 -> constant_to_michelson_program)
                      Range: File "", line 408, characters 6-35
                      Body Range: File "", line 408, characters 37-38
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#514 -> create_chest)
                      Range: File "", line 503, characters 6-18
                      Body Range: File "", line 503, characters 20-21
                      Content: |core: bytes -> nat -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#517 -> create_chest_key)
                      Range: File "", line 504, characters 6-22
                      Body Range: File "", line 504, characters 24-25
                      Content: |core: chest -> nat -> chest_key|
                      references: []
                      (decompile#395 -> decompile)
                      Range: File "", line 401, characters 6-15
                      Body Range: File "", line 401, characters 16-24
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 419, characters 5-14
                      (drop_context#417 -> drop_context)
                      Range: File "", line 412, characters 6-18
                      Body Range: File "", line 412, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#375 -> eprint)
                      Range: File "", line 388, characters 6-12
                      Body Range: File "", line 388, characters 14-15
                      Content: |core: string -> unit|
                      references: []
                      (eval#357 -> eval)
                      Range: File "", line 370, characters 6-10
                      Body Range: File "", line 370, characters 11-19
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 380, characters 59-63 ,
                        File "", line 508, characters 32-36 ,
                        File "", line 513, characters 34-38 ,
                        File "", line 527, characters 12-16
                      (failwith#363 -> failwith)
                      Range: File "", line 382, characters 6-14
                      Body Range: File "", line 382, characters 15-25
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 731, characters 51-59 ,
                        File "", line 732, characters 74-82 ,
                        File "", line 733, characters 89-97 ,
                        File "", line 736, characters 68-76 ,
                        File "", line 737, characters 98-106 ,
                        File "", line 738, characters 113-121
                      (get_balance#371 -> get_balance)
                      Range: File "", line 386, characters 6-17
                      Body Range: File "", line 386, characters 19-20
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#384 -> get_bootstrap_account)
                      Range: File "", line 394, characters 6-27
                      Body Range: File "", line 394, characters 29-30
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#470 -> get_last_events_from)
                      Range: File "", line 480, characters 6-26
                      Body Range: File "", line 480, characters 27-39
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#426 -> get_storage)
                      Range: File "", line 415, characters 6-17
                      Body Range: File "", line 415, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#369 -> get_storage_of_address)
                      Range: File "", line 385, characters 6-28
                      Body Range: File "", line 385, characters 30-31
                      Content: |core: address -> michelson_program|
                      references: File "", line 418, characters 32-54
                      (get_time#399 -> get_time)
                      Range: File "", line 403, characters 6-14
                      Body Range: File "", line 403, characters 16-18
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#361 -> get_total_voting_power)
                      Range: File "", line 381, characters 6-28
                      Body Range: File "", line 381, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#377 -> get_voting_power)
                      Range: File "", line 389, characters 6-22
                      Body Range: File "", line 389, characters 24-26
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#388 -> last_originations)
                      Range: File "", line 396, characters 6-23
                      Body Range: File "", line 396, characters 25-26
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#482 -> log)
                      Range: File "", line 490, characters 6-9
                      Body Range: File "", line 490, characters 10-18
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 519, characters 25-28
                      (michelson_equal#534 -> michelson_equal)
                      Range: File "", line 515, characters 6-21
                      Body Range: File "", line 515, characters 23-25
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#496 -> mutate_value)
                      Range: File "", line 497, characters 6-18
                      Body Range: File "", line 497, characters 19-27
                      Content: |core: ∀ a : * . nat -> a -> option (( a * mutation ))|
                      references:
                        File "", line 544, characters 23-35 ,
                        File "", line 556, characters 23-35
                      (mutation_test#580 -> mutation_test)
                      Range: File "", line 540, characters 6-19
                      Body Range: File "", line 540, characters 20-30
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b * mutation ))|
                      references: []
                      (mutation_test_all#596 -> mutation_test_all)
                      Range: File "", line 552, characters 6-23
                      Body Range: File "", line 552, characters 24-34
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b * mutation ))|
                      references: []
                      (new_account#393 -> new_account)
                      Range: File "", line 400, characters 6-17
                      Body Range: File "", line 400, characters 19-20
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#441 -> nl)
                      Range: File "", line 437, characters 6-8
                      Body Range: File "", line 437, characters 11-53
                      Content: |unresolved|
                      references: File "", line 439, characters 15-17
                      (nth_bootstrap_account#382 -> nth_bootstrap_account)
                      Range: File "", line 391, characters 6-27
                      Body Range: File "", line 391, characters 29-30
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#379 -> nth_bootstrap_contract)
                      Range: File "", line 390, characters 6-28
                      Body Range: File "", line 390, characters 30-31
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#386 -> nth_bootstrap_typed_address)
                      Range: File "", line 395, characters 6-33
                      Body Range: File "", line 395, characters 34-44
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#551 -> originate)
                      Range: File "", line 525, characters 6-15
                      Body Range: File "", line 525, characters 16-26
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> s -> tez -> ( typed_address (p , s) * michelson_contract * int )|
                      references: []
                      (originate_contract#542 -> originate_contract)
                      Range: File "", line 524, characters 6-24
                      Body Range: File "", line 524, characters 26-27
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 528, characters 12-30 ,
                        File "", line 537, characters 12-30 ,
                        File "", line 568, characters 14-32 ,
                        File "", line 588, characters 14-32
                      (originate_from_file#565 -> originate_from_file)
                      Range: File "", line 535, characters 6-25
                      Body Range: File "", line 535, characters 27-29
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address * michelson_contract * int )|
                      references: []
                      (originate_from_file_and_mutate#622 -> originate_from_file_and_mutate)
                      Range: File "", line 564, characters 6-36
                      Body Range: File "", line 564, characters 37-45
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address * michelson_contract * int ) -> b -> option (
                      ( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#649 -> originate_from_file_and_mutate_all)
                      Range: File "", line 584, characters 6-40
                      Body Range: File "", line 584, characters 41-49
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address * michelson_contract * int ) -> b -> list (
                      ( b * mutation ))|
                      references: []
                      (parse_michelson#411 -> parse_michelson)
                      Range: File "", line 409, characters 6-21
                      Body Range: File "", line 409, characters 23-24
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#373 -> print)
                      Range: File "", line 387, characters 6-11
                      Body Range: File "", line 387, characters 13-14
                      Content: |core: string -> unit|
                      references:
                        File "", line 439, characters 4-9 ,
                        File "", line 493, characters 4-9
                      (println#443 -> println)
                      Range: File "", line 438, characters 6-13
                      Body Range: File "", line 438, characters 15-16
                      Content: |core: string -> unit|
                      references: []
                      (random#391 -> random)
                      Range: File "", line 397, characters 6-12
                      Body Range: File "", line 397, characters 13-21
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#437 -> read_contract_from_file)
                      Range: File "", line 426, characters 6-29
                      Body Range: File "", line 426, characters 31-33
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#405 -> register_constant)
                      Range: File "", line 406, characters 6-23
                      Body Range: File "", line 406, characters 25-26
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#403 -> register_delegate)
                      Range: File "", line 405, characters 6-23
                      Body Range: File "", line 405, characters 25-27
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#485 -> reset_state)
                      Range: File "", line 494, characters 6-17
                      Body Range: File "", line 494, characters 19-20
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#489 -> reset_state_at)
                      Range: File "", line 495, characters 6-20
                      Body Range: File "", line 495, characters 22-23
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#413 -> restore_context)
                      Range: File "", line 410, characters 6-21
                      Body Range: File "", line 410, characters 23-24
                      Content: |core: unit -> unit|
                      references: []
                      (run#354 -> run)
                      Range: File "", line 369, characters 6-9
                      Body Range: File "", line 369, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 370, characters 50-53
                      (save_context#415 -> save_context)
                      Range: File "", line 411, characters 6-18
                      Body Range: File "", line 411, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#499 -> save_mutation)
                      Range: File "", line 498, characters 6-19
                      Body Range: File "", line 498, characters 21-22
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#430 -> set_baker)
                      Range: File "", line 421, characters 6-15
                      Body Range: File "", line 421, characters 17-18
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#428 -> set_baker_policy)
                      Range: File "", line 420, characters 6-22
                      Body Range: File "", line 420, characters 24-26
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 421, characters 39-55
                      (set_big_map#511 -> set_big_map)
                      Range: File "", line 502, characters 6-17
                      Body Range: File "", line 502, characters 18-28
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#444 -> set_print_values)
                      Range: File "", line 441, characters 6-22
                      Body Range: File "", line 441, characters 24-25
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#367 -> set_source)
                      Range: File "", line 384, characters 6-16
                      Body Range: File "", line 384, characters 18-19
                      Content: |core: address -> unit|
                      references: []
                      (sign#502 -> sign)
                      Range: File "", line 499, characters 6-10
                      Body Range: File "", line 499, characters 12-14
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#432 -> size)
                      Range: File "", line 422, characters 6-10
                      Body Range: File "", line 422, characters 12-13
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 529, characters 12-16 ,
                        File "", line 538, characters 12-16 ,
                        File "", line 569, characters 14-18 ,
                        File "", line 589, characters 14-18
                      (to_contract#365 -> to_contract)
                      Range: File "", line 383, characters 6-17
                      Body Range: File "", line 383, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 416, characters 25-36 ,
                        File "", line 481, characters 30-41
                      (to_entrypoint#538 -> to_entrypoint)
                      Range: File "", line 516, characters 6-19
                      Body Range: File "", line 516, characters 20-32
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#421 -> to_json)
                      Range: File "", line 414, characters 6-13
                      Body Range: File "", line 414, characters 14-22
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#419 -> to_string)
                      Range: File "", line 413, characters 6-15
                      Body Range: File "", line 413, characters 16-24
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 430, characters 68-77 ,
                        File "", line 432, characters 67-76 ,
                        File "", line 434, characters 61-70 ,
                        File "", line 492, characters 12-21
                      (to_typed_address#407 -> to_typed_address)
                      Range: File "", line 407, characters 6-22
                      Body Range: File "", line 407, characters 23-33
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#474 -> transfer)
                      Range: File "", line 488, characters 6-14
                      Body Range: File "", line 488, characters 16-17
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#478 -> transfer_exn)
                      Range: File "", line 489, characters 6-18
                      Body Range: File "", line 489, characters 20-21
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#524 -> transfer_to_contract)
                      Range: File "", line 505, characters 6-26
                      Body Range: File "", line 505, characters 27-35
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#531 -> transfer_to_contract_exn)
                      Range: File "", line 510, characters 6-30
                      Body Range: File "", line 510, characters 31-39
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#445 -> unset_print_values)
                      Range: File "", line 442, characters 6-24
                      Body Range: File "", line 442, characters 26-27
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#460 -> PBT)
                      Range: File "", line 444, characters 9-12
                      Body Range: File "", line 444, character 2 to line 477, character 5
                      Content: Members: Variable definitions:
                                        (gen#446 -> gen)
                                        Range: File "", line 445, characters 8-11
                                        Body Range: File "", line 445, characters 12-20
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references:
                                          File "", line 449, characters 23-27 ,
                                          File "", line 450, characters 23-27
                                        (gen_small#447 -> gen_small)
                                        Range: File "", line 446, characters 8-17
                                        Body Range: File "", line 446, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#450 -> make_test)
                                        Range: File "", line 448, characters 8-17
                                        Body Range: File "", line 448, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#459 -> run)
                                        Range: File "", line 449, characters 8-11
                                        Body Range: File "", line 449, characters 12-20
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#88 -> Tezos)
    Range: File "", line 8, characters 7-12
    Body Range: File "", line 8, character 0 to line 109, character 3
    Content: Members: Variable definitions:
                      (address#26 -> address)
                      Range: File "", line 21, characters 6-13
                      Body Range: File "", line 21, characters 14-22
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 481, characters 21-28
                      (call_view#65 -> call_view)
                      Range: File "", line 57, characters 25-34
                      Body Range: File "", line 57, characters 35-45
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (constant#42 -> constant)
                      Range: File "", line 32, characters 25-33
                      Body Range: File "", line 32, characters 34-42
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (create_contract#73 -> create_contract)
                      Range: File "", line 61, characters 25-40
                      Body Range: File "", line 61, characters 41-51
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> option (key_hash) -> tez -> s ->
                      ( operation * address )|
                      references: []
                      (create_ticket#57 -> create_ticket)
                      Range: File "", line 49, characters 6-19
                      Body Range: File "", line 49, characters 20-28
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references: []
                      (emit#84 -> emit)
                      Range: File "", line 69, characters 25-29
                      Body Range: File "", line 69, characters 30-38
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      (get_amount#6 -> get_amount)
                      Range: File "", line 11, characters 6-16
                      Body Range: File "", line 11, characters 18-20
                      Content: |core: unit -> tez|
                      references: []
                      (get_balance#4 -> get_balance)
                      Range: File "", line 10, characters 6-17
                      Body Range: File "", line 10, characters 19-21
                      Content: |core: unit -> tez|
                      references: []
                      (get_chain_id#18 -> get_chain_id)
                      Range: File "", line 17, characters 6-18
                      Body Range: File "", line 17, characters 20-22
                      Content: |core: unit -> chain_id|
                      references: []
                      (get_contract#49 -> get_contract)
                      Range: File "", line 37, characters 25-37
                      Body Range: File "", line 37, characters 38-46
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      (get_contract_opt#45 -> get_contract_opt)
                      Range: File "", line 35, characters 25-41
                      Body Range: File "", line 35, characters 42-50
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 38, characters 12-28 ,
                        File "", line 43, characters 12-28
                      (get_contract_with_error#54 -> get_contract_with_error)
                      Range: File "", line 42, characters 6-29
                      Body Range: File "", line 42, characters 30-38
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: []
                      (get_entrypoint#81 -> get_entrypoint)
                      Range: File "", line 66, characters 25-39
                      Body Range: File "", line 66, characters 40-48
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (get_entrypoint_opt#76 -> get_entrypoint_opt)
                      Range: File "", line 63, characters 25-43
                      Body Range: File "", line 63, characters 44-52
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 67, characters 12-30
                      (get_level#14 -> get_level)
                      Range: File "", line 15, characters 6-15
                      Body Range: File "", line 15, characters 17-19
                      Content: |core: unit -> nat|
                      references: []
                      (get_min_block_time#22 -> get_min_block_time)
                      Range: File "", line 19, characters 6-24
                      Body Range: File "", line 19, characters 26-28
                      Content: |core: unit -> nat|
                      references: []
                      (get_now#8 -> get_now)
                      Range: File "", line 12, characters 6-13
                      Body Range: File "", line 12, characters 15-17
                      Content: |core: unit -> timestamp|
                      references: File "", line 403, characters 47-54
                      (get_self_address#16 -> get_self_address)
                      Range: File "", line 16, characters 6-22
                      Body Range: File "", line 16, characters 24-26
                      Content: |core: unit -> address|
                      references: []
                      (get_sender#10 -> get_sender)
                      Range: File "", line 13, characters 6-16
                      Body Range: File "", line 13, characters 18-20
                      Content: |core: unit -> address|
                      references: []
                      (get_source#12 -> get_source)
                      Range: File "", line 14, characters 6-16
                      Body Range: File "", line 14, characters 18-20
                      Content: |core: unit -> address|
                      references: []
                      (get_total_voting_power#20 -> get_total_voting_power)
                      Range: File "", line 18, characters 6-28
                      Body Range: File "", line 18, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (implicit_account#28 -> implicit_account)
                      Range: File "", line 22, characters 6-22
                      Body Range: File "", line 22, characters 24-26
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      (join_tickets#30 -> join_tickets)
                      Range: File "", line 23, characters 6-18
                      Body Range: File "", line 23, characters 19-27
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      (never#34 -> never)
                      Range: File "", line 26, characters 6-11
                      Body Range: File "", line 26, characters 12-20
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      (pairing_check#36 -> pairing_check)
                      Range: File "", line 27, characters 6-19
                      Body Range: File "", line 27, characters 21-22
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      (read_ticket#32 -> read_ticket)
                      Range: File "", line 24, characters 6-17
                      Body Range: File "", line 24, characters 18-26
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) )|
                      references: []
                      (sapling_empty_state#43 -> sapling_empty_state)
                      Range: File "", line 33, characters 25-44
                      Body Range: File "", line 33, characters 45-57
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      (sapling_verify_update#87 -> sapling_verify_update)
                      Range: File "", line 72, characters 25-46
                      Body Range: File "", line 72, characters 47-59
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes * ( int * sapling_state (sap_a) ) ))|
                      references: []
                      (self#40 -> self)
                      Range: File "", line 29, characters 25-29
                      Body Range: File "", line 29, characters 30-38
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      (set_delegate#38 -> set_delegate)
                      Range: File "", line 28, characters 6-18
                      Body Range: File "", line 28, characters 20-21
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      (split_ticket#68 -> split_ticket)
                      Range: File "", line 59, characters 6-18
                      Body Range: File "", line 59, characters 19-27
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) * ticket (a) ))|
                      references: []
                      (transaction#61 -> transaction)
                      Range: File "", line 51, characters 6-17
                      Body Range: File "", line 51, characters 18-26
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: []
                      (voting_power#24 -> voting_power)
                      Range: File "", line 20, characters 6-18
                      Body Range: File "", line 20, characters 20-22
                      Content: |core: key_hash -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 403, characters 41-46 ,
      File "", line 481, characters 15-20 |}]
