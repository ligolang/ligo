open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Test#693 assert_none_with_error#692 assert_some_with_error#688 assert_with_error#684 assert_none#681 assert_some#678 assert#675 originate_from_file_and_mutate_all#673 originate_from_file_and_mutate#646 mutation_test_all#620 mutation_test#604 originate_from_file#589 compile_contract_from_file#580 originate_uncurried#575 originate#566 originate_contract#557 to_entrypoint#553 michelson_equal#549 transfer_to_contract_exn#546 transfer_to_contract#539 create_chest_key#532 create_chest#529 set_big_map#526 baker_account#523 add_account#520 sign#517 save_mutation#514 mutate_value#511 bootstrap_contract#508 reset_state_at#504 reset_state#500 log#497 transfer_exn#493 transfer#489 get_last_events_from#485 PBT#475 run#474 make_test#465 gen_small#462 gen#461 unset_print_values#460 set_print_values#459 println#458 nl#456 chr#455 read_contract_from_file#452 compile_contract#450 size#447 set_baker#445 set_baker_policy#443 get_storage#441 to_json#436 to_string#434 drop_context#432 save_context#430 restore_context#428 parse_michelson#426 constant_to_michelson_program#424 to_typed_address#422 register_constant#420 register_delegate#418 cast_address#416 get_time#414 bake_until_n_cycle_end#412 decompile#410 new_account#408 random#406 last_originations#403 nth_bootstrap_typed_address#401 get_bootstrap_account#399 nth_bootstrap_account#397 nth_bootstrap_contract#394 get_voting_power#392 eprint#390 print#388 get_balance#386 get_storage_of_address#384 set_source#382 to_contract#380 failwith#378 get_total_voting_power#376 compile_value#374 eval#372 run#369 unforged_ticket#366 pbt_result#365 pbt_test#364 test_baker_policy#363 test_exec_result#362 test_exec_error#361 test_exec_error_balance_too_low#360 ediv#359 assert_none_with_error#356 assert_some_with_error#352 assert_with_error#348 uncurry#345 curry#342 ignore#338 int#337 unit#335 false#334 true#333 is_nat#332 abs#330 assert_none#328 assert_some#325 assert#322 Crypto#320 check#319 hash_key#315 keccak#313 sha3#311 sha512#309 sha256#307 blake2b#305 Bytes#303 sub#302 concat#298 length#295 unpack#293 pack#291 concats#289 Option#287 is_some#286 is_none#283 value_exn#280 value#276 map#272 unopt_with_error#269 unopt#265 String#262 sub#261 concat#257 concats#254 length#252 List#250 find_opt#249 cons#245 fold_right#242 fold_left#238 fold#234 iter#230 map#227 tail_opt#224 head_opt#220 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ a#694 Test#693 assert_none_with_error#692 assert_some_with_error#688 assert_with_error#684 assert_none#681 assert_some#678 assert#675 originate_from_file_and_mutate_all#673 originate_from_file_and_mutate#646 mutation_test_all#620 mutation_test#604 originate_from_file#589 compile_contract_from_file#580 originate_uncurried#575 originate#566 originate_contract#557 to_entrypoint#553 michelson_equal#549 transfer_to_contract_exn#546 transfer_to_contract#539 create_chest_key#532 create_chest#529 set_big_map#526 baker_account#523 add_account#520 sign#517 save_mutation#514 mutate_value#511 bootstrap_contract#508 reset_state_at#504 reset_state#500 log#497 transfer_exn#493 transfer#489 get_last_events_from#485 PBT#475 run#474 make_test#465 gen_small#462 gen#461 unset_print_values#460 set_print_values#459 println#458 nl#456 chr#455 read_contract_from_file#452 compile_contract#450 size#447 set_baker#445 set_baker_policy#443 get_storage#441 to_json#436 to_string#434 drop_context#432 save_context#430 restore_context#428 parse_michelson#426 constant_to_michelson_program#424 to_typed_address#422 register_constant#420 register_delegate#418 cast_address#416 get_time#414 bake_until_n_cycle_end#412 decompile#410 new_account#408 random#406 last_originations#403 nth_bootstrap_typed_address#401 get_bootstrap_account#399 nth_bootstrap_account#397 nth_bootstrap_contract#394 get_voting_power#392 eprint#390 print#388 get_balance#386 get_storage_of_address#384 set_source#382 to_contract#380 failwith#378 get_total_voting_power#376 compile_value#374 eval#372 run#369 unforged_ticket#366 pbt_result#365 pbt_test#364 test_baker_policy#363 test_exec_result#362 test_exec_error#361 test_exec_error_balance_too_low#360 ediv#359 assert_none_with_error#356 assert_some_with_error#352 assert_with_error#348 uncurry#345 curry#342 ignore#338 int#337 unit#335 false#334 true#333 is_nat#332 abs#330 assert_none#328 assert_some#325 assert#322 Crypto#320 check#319 hash_key#315 keccak#313 sha3#311 sha512#309 sha256#307 blake2b#305 Bytes#303 sub#302 concat#298 length#295 unpack#293 pack#291 concats#289 Option#287 is_some#286 is_none#283 value_exn#280 value#276 map#272 unopt_with_error#269 unopt#265 String#262 sub#261 concat#257 concats#254 length#252 List#250 find_opt#249 cons#245 fold_right#242 fold_left#238 fold#234 iter#230 map#227 tail_opt#224 head_opt#220 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 14
    [ c#695 a#694 Test#693 assert_none_with_error#692 assert_some_with_error#688 assert_with_error#684 assert_none#681 assert_some#678 assert#675 originate_from_file_and_mutate_all#673 originate_from_file_and_mutate#646 mutation_test_all#620 mutation_test#604 originate_from_file#589 compile_contract_from_file#580 originate_uncurried#575 originate#566 originate_contract#557 to_entrypoint#553 michelson_equal#549 transfer_to_contract_exn#546 transfer_to_contract#539 create_chest_key#532 create_chest#529 set_big_map#526 baker_account#523 add_account#520 sign#517 save_mutation#514 mutate_value#511 bootstrap_contract#508 reset_state_at#504 reset_state#500 log#497 transfer_exn#493 transfer#489 get_last_events_from#485 PBT#475 run#474 make_test#465 gen_small#462 gen#461 unset_print_values#460 set_print_values#459 println#458 nl#456 chr#455 read_contract_from_file#452 compile_contract#450 size#447 set_baker#445 set_baker_policy#443 get_storage#441 to_json#436 to_string#434 drop_context#432 save_context#430 restore_context#428 parse_michelson#426 constant_to_michelson_program#424 to_typed_address#422 register_constant#420 register_delegate#418 cast_address#416 get_time#414 bake_until_n_cycle_end#412 decompile#410 new_account#408 random#406 last_originations#403 nth_bootstrap_typed_address#401 get_bootstrap_account#399 nth_bootstrap_account#397 nth_bootstrap_contract#394 get_voting_power#392 eprint#390 print#388 get_balance#386 get_storage_of_address#384 set_source#382 to_contract#380 failwith#378 get_total_voting_power#376 compile_value#374 eval#372 run#369 unforged_ticket#366 pbt_result#365 pbt_test#364 test_baker_policy#363 test_exec_result#362 test_exec_error#361 test_exec_error_balance_too_low#360 ediv#359 assert_none_with_error#356 assert_some_with_error#352 assert_with_error#348 uncurry#345 curry#342 ignore#338 int#337 unit#335 false#334 true#333 is_nat#332 abs#330 assert_none#328 assert_some#325 assert#322 Crypto#320 check#319 hash_key#315 keccak#313 sha3#311 sha512#309 sha256#307 blake2b#305 Bytes#303 sub#302 concat#298 length#295 unpack#293 pack#291 concats#289 Option#287 is_some#286 is_none#283 value_exn#280 value#276 map#272 unopt_with_error#269 unopt#265 String#262 sub#261 concat#257 concats#254 length#252 List#250 find_opt#249 cons#245 fold_right#242 fold_left#238 fold#234 iter#230 map#227 tail_opt#224 head_opt#220 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ d#696 c#695 a#694 Test#693 assert_none_with_error#692 assert_some_with_error#688 assert_with_error#684 assert_none#681 assert_some#678 assert#675 originate_from_file_and_mutate_all#673 originate_from_file_and_mutate#646 mutation_test_all#620 mutation_test#604 originate_from_file#589 compile_contract_from_file#580 originate_uncurried#575 originate#566 originate_contract#557 to_entrypoint#553 michelson_equal#549 transfer_to_contract_exn#546 transfer_to_contract#539 create_chest_key#532 create_chest#529 set_big_map#526 baker_account#523 add_account#520 sign#517 save_mutation#514 mutate_value#511 bootstrap_contract#508 reset_state_at#504 reset_state#500 log#497 transfer_exn#493 transfer#489 get_last_events_from#485 PBT#475 run#474 make_test#465 gen_small#462 gen#461 unset_print_values#460 set_print_values#459 println#458 nl#456 chr#455 read_contract_from_file#452 compile_contract#450 size#447 set_baker#445 set_baker_policy#443 get_storage#441 to_json#436 to_string#434 drop_context#432 save_context#430 restore_context#428 parse_michelson#426 constant_to_michelson_program#424 to_typed_address#422 register_constant#420 register_delegate#418 cast_address#416 get_time#414 bake_until_n_cycle_end#412 decompile#410 new_account#408 random#406 last_originations#403 nth_bootstrap_typed_address#401 get_bootstrap_account#399 nth_bootstrap_account#397 nth_bootstrap_contract#394 get_voting_power#392 eprint#390 print#388 get_balance#386 get_storage_of_address#384 set_source#382 to_contract#380 failwith#378 get_total_voting_power#376 compile_value#374 eval#372 run#369 unforged_ticket#366 pbt_result#365 pbt_test#364 test_baker_policy#363 test_exec_result#362 test_exec_error#361 test_exec_error_balance_too_low#360 ediv#359 assert_none_with_error#356 assert_some_with_error#352 assert_with_error#348 uncurry#345 curry#342 ignore#338 int#337 unit#335 false#334 true#333 is_nat#332 abs#330 assert_none#328 assert_some#325 assert#322 Crypto#320 check#319 hash_key#315 keccak#313 sha3#311 sha512#309 sha256#307 blake2b#305 Bytes#303 sub#302 concat#298 length#295 unpack#293 pack#291 concats#289 Option#287 is_some#286 is_none#283 value_exn#280 value#276 map#272 unopt_with_error#269 unopt#265 String#262 sub#261 concat#257 concats#254 length#252 List#250 find_opt#249 cons#245 fold_right#242 fold_left#238 fold#234 iter#230 map#227 tail_opt#224 head_opt#220 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ e#697 a#694 Test#693 assert_none_with_error#692 assert_some_with_error#688 assert_with_error#684 assert_none#681 assert_some#678 assert#675 originate_from_file_and_mutate_all#673 originate_from_file_and_mutate#646 mutation_test_all#620 mutation_test#604 originate_from_file#589 compile_contract_from_file#580 originate_uncurried#575 originate#566 originate_contract#557 to_entrypoint#553 michelson_equal#549 transfer_to_contract_exn#546 transfer_to_contract#539 create_chest_key#532 create_chest#529 set_big_map#526 baker_account#523 add_account#520 sign#517 save_mutation#514 mutate_value#511 bootstrap_contract#508 reset_state_at#504 reset_state#500 log#497 transfer_exn#493 transfer#489 get_last_events_from#485 PBT#475 run#474 make_test#465 gen_small#462 gen#461 unset_print_values#460 set_print_values#459 println#458 nl#456 chr#455 read_contract_from_file#452 compile_contract#450 size#447 set_baker#445 set_baker_policy#443 get_storage#441 to_json#436 to_string#434 drop_context#432 save_context#430 restore_context#428 parse_michelson#426 constant_to_michelson_program#424 to_typed_address#422 register_constant#420 register_delegate#418 cast_address#416 get_time#414 bake_until_n_cycle_end#412 decompile#410 new_account#408 random#406 last_originations#403 nth_bootstrap_typed_address#401 get_bootstrap_account#399 nth_bootstrap_account#397 nth_bootstrap_contract#394 get_voting_power#392 eprint#390 print#388 get_balance#386 get_storage_of_address#384 set_source#382 to_contract#380 failwith#378 get_total_voting_power#376 compile_value#374 eval#372 run#369 unforged_ticket#366 pbt_result#365 pbt_test#364 test_baker_policy#363 test_exec_result#362 test_exec_error#361 test_exec_error_balance_too_low#360 ediv#359 assert_none_with_error#356 assert_some_with_error#352 assert_with_error#348 uncurry#345 curry#342 ignore#338 int#337 unit#335 false#334 true#333 is_nat#332 abs#330 assert_none#328 assert_some#325 assert#322 Crypto#320 check#319 hash_key#315 keccak#313 sha3#311 sha512#309 sha256#307 blake2b#305 Bytes#303 sub#302 concat#298 length#295 unpack#293 pack#291 concats#289 Option#287 is_some#286 is_none#283 value_exn#280 value#276 map#272 unopt_with_error#269 unopt#265 String#262 sub#261 concat#257 concats#254 length#252 List#250 find_opt#249 cons#245 fold_right#242 fold_left#238 fold#234 iter#230 map#227 tail_opt#224 head_opt#220 size#216 length#214 Set#212 fold_desc#211 fold#207 iter#203 update#200 remove#196 add#193 mem#190 literal#187 cardinal#185 size#183 empty#181 Map#180 fold#179 map#175 iter#172 find_opt#169 find#166 get_and_update#163 update#159 remove#155 add#152 mem#148 literal#145 size#143 empty#141 Big_map#140 find#139 find_opt#136 get_and_update#133 update#129 remove#125 add#122 mem#118 literal#115 empty#113 Bitwise#112 shift_right#111 shift_left#108 or#105 xor#102 and#99 Tezos#96 sapling_verify_update#95 emit#92 get_entrypoint#89 get_entrypoint_opt#84 create_contract_uncurried#81 create_contract#76 split_ticket#68 call_view#65 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30

    Variable definitions:
    (a#694 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#330 -> abs)
    Range: File "", line 328, characters 4-7
    Body Range: File "", line 328, characters 9-10
    Content: |core: int -> nat|
    references: File "", line 528, characters 31-34
    (assert#322 -> assert)
    Range: File "", line 325, characters 4-10
    Body Range: File "", line 325, characters 12-13
    Content: |core: bool -> unit|
    references: []
    (assert_none#328 -> assert_none)
    Range: File "", line 327, characters 4-15
    Body Range: File "", line 327, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#356 -> assert_none_with_error)
    Range: File "", line 341, characters 4-26
    Body Range: File "", line 341, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#325 -> assert_some)
    Range: File "", line 326, characters 4-15
    Body Range: File "", line 326, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#352 -> assert_some_with_error)
    Range: File "", line 340, characters 4-26
    Body Range: File "", line 340, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#348 -> assert_with_error)
    Range: File "", line 339, characters 4-21
    Body Range: File "", line 339, characters 23-24
    Content: |unresolved|
    references: []
    (b#698 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#695 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (curry#342 -> curry)
    Range: File "", line 335, characters 4-9
    Body Range: File "", line 335, characters 10-22
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    (d#696 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#697 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    (ediv#359 -> ediv)
    Range: File "", line 342, characters 4-8
    Body Range: File "", line 342, characters 9-19
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
      File "", line 71, characters 27-35 ,
      File "", line 269, characters 79-87 ,
      File "", line 272, characters 103-111 ,
      File "", line 275, characters 83-91 ,
      File "", line 325, characters 49-57 ,
      File "", line 326, characters 72-80 ,
      File "", line 327, characters 87-95 ,
      File "", line 339, characters 66-74 ,
      File "", line 340, characters 96-104 ,
      File "", line 341, characters 111-119
    (false#334 -> false)
    Range: File "", line 331, characters 4-9
    Body Range: File "", line 331, characters 19-24
    Content: |core: bool|
    references:
      File "", line 406, characters 51-56 ,
      File "", line 450, characters 90-95 ,
      File "", line 453, characters 62-67
    (ignore#338 -> ignore)
    Range: File "", line 334, characters 4-10
    Body Range: File "", line 334, characters 11-19
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#337 -> int)
    Range: File "", line 333, characters 4-7
    Body Range: File "", line 333, characters 8-16
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 402, characters 97-100 ,
      File "", line 438, characters 79-82 ,
      File "", line 440, characters 78-81 ,
      File "", line 442, characters 72-75
    (is_nat#332 -> is_nat)
    Range: File "", line 329, characters 4-10
    Body Range: File "", line 329, characters 12-13
    Content: |core: int -> option (nat)|
    references: []
    (true#333 -> true)
    Range: File "", line 330, characters 4-8
    Body Range: File "", line 330, characters 18-22
    Content: |core: bool|
    references:
      File "", line 449, characters 88-92 ,
      File "", line 454, characters 68-72
    (uncurry#345 -> uncurry)
    Range: File "", line 336, characters 4-11
    Body Range: File "", line 336, characters 12-24
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 534, characters 30-37
    (unit#335 -> unit)
    Range: File "", line 332, characters 4-8
    Body Range: File "", line 332, characters 18-38
    Content: |core: unit|
    references: []
    Type definitions:
    (bool#1 -> bool)
    Range: File "", line 5, characters 5-9
    Body Range: File "", line 5, characters 12-24
    Content: : |sum[False -> unit , True -> unit]|
    references:
      File "", line 27, characters 63-67 ,
      File "", line 140, characters 52-56 ,
      File "", line 167, characters 48-52 ,
      File "", line 201, characters 41-45 ,
      File "", line 204, characters 35-39 ,
      File "", line 235, characters 34-38 ,
      File "", line 285, characters 40-44 ,
      File "", line 286, characters 40-44 ,
      File "", line 316, characters 52-56 ,
      File "", line 325, characters 16-20 ,
      File "", line 330, characters 11-15 ,
      File "", line 331, characters 12-16 ,
      File "", line 339, characters 27-31 ,
      File "", line 369, characters 41-45 ,
      File "", line 456, characters 53-57 ,
      File "", line 523, characters 74-78 ,
      File "", line 753, characters 18-22 ,
      File "", line 758, characters 29-33
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
      File "", line 61, characters 93-108 ,
      File "", line 64, characters 102-117 ,
      File "", line 66, characters 82-99 ,
      File "", line 75, characters 120-164 ,
      File "", line 143, characters 37-45 ,
      File "", line 144, characters 45-53 ,
      File "", line 144, characters 78-86 ,
      File "", line 145, characters 57-65 ,
      File "", line 170, characters 37-45 ,
      File "", line 171, characters 45-53 ,
      File "", line 171, characters 74-82 ,
      File "", line 173, characters 53-61 ,
      File "", line 225, characters 40-48 ,
      File "", line 226, characters 40-55 ,
      File "", line 235, characters 56-64 ,
      File "", line 236, characters 29-37 ,
      File "", line 269, characters 26-34 ,
      File "", line 272, characters 37-45 ,
      File "", line 273, characters 48-56 ,
      File "", line 273, characters 60-68 ,
      File "", line 274, characters 40-48 ,
      File "", line 275, characters 46-54 ,
      File "", line 285, characters 28-36 ,
      File "", line 286, characters 28-36 ,
      File "", line 292, characters 36-44 ,
      File "", line 326, characters 30-38 ,
      File "", line 327, characters 30-38 ,
      File "", line 329, characters 23-33 ,
      File "", line 340, characters 41-49 ,
      File "", line 341, characters 41-49 ,
      File "", line 435, characters 22-35 ,
      File "", line 496, characters 140-153 ,
      File "", line 497, characters 135-148 ,
      File "", line 502, characters 92-108 ,
      File "", line 505, characters 48-69 ,
      File "", line 506, characters 50-63 ,
      File "", line 509, characters 44-54 ,
      File "", line 515, characters 12-25 ,
      File "", line 520, characters 14-27 ,
      File "", line 548, characters 96-106 ,
      File "", line 555, characters 59-80 ,
      File "", line 558, characters 37-58 ,
      File "", line 580, characters 90-111 ,
      File "", line 586, characters 96-106 ,
      File "", line 589, characters 37-58 ,
      File "", line 606, characters 96-106 ,
      File "", line 754, characters 32-40 ,
      File "", line 755, characters 32-40 ,
      File "", line 759, characters 43-51 ,
      File "", line 760, characters 43-51
    (pbt_result#365 -> pbt_result)
    Range: File "", line 370, characters 8-18
    Body Range: File "", line 370, characters 0-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 457, characters 55-67 ,
      File "", line 458, characters 37-49 ,
      File "", line 460, characters 82-94 ,
      File "", line 464, characters 94-106 ,
      File "", line 467, characters 66-78
    (pbt_test#364 -> pbt_test)
    Range: File "", line 369, characters 8-16
    Body Range: File "", line 369, characters 0-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 456, characters 61-71 ,
      File "", line 457, characters 31-41
    (test_baker_policy#363 -> test_baker_policy)
    Range: File "", line 364, characters 5-22
    Body Range: File "", line 365, character 4 to line 367, character 29
    Content: : |sum[By_account -> address , By_round -> int , Excluding -> list (address)]|
    references: File "", line 428, characters 29-46
    (test_exec_error#361 -> test_exec_error)
    Range: File "", line 357, characters 5-20
    Body Range: File "", line 358, character 4 to line 360, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low , Other -> string , Rejected -> ( michelson_program * address )]|
    references: File "", line 362, characters 49-64
    (test_exec_error_balance_too_low#360 -> test_exec_error_balance_too_low)
    Range: File "", line 354, characters 5-36
    Body Range: File "", line 355, characters 2-79
    Content: : |record[contract_balance -> tez , contract_too_low -> address , spend_request -> tez]|
    references: File "", line 359, characters 23-54
    (test_exec_result#362 -> test_exec_result)
    Range: File "", line 362, characters 5-21
    Body Range: File "", line 362, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 496, characters 65-81 ,
      File "", line 513, characters 73-89
    (unforged_ticket#366 -> unforged_ticket)
    Range: File "", line 372, characters 8-23
    Body Range: File "", line 372, characters 0-91
    Content: : |funtype 's : * . record[amount -> nat , ticketer -> address , value -> 's]|
    references: []
    Module definitions:
    (Big_map#140 -> Big_map)
    Range: File "", line 135, characters 7-14
    Body Range: File "", line 135, character 0 to line 159, character 3
    Content: Members: Variable definitions:
                      (add#122 -> add)
                      Range: File "", line 141, characters 6-9
                      Body Range: File "", line 141, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (empty#113 -> empty)
                      Range: File "", line 136, characters 16-21
                      Body Range: File "", line 136, characters 22-32
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#139 -> find)
                      Range: File "", line 146, characters 6-10
                      Body Range: File "", line 146, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      (find_opt#136 -> find_opt)
                      Range: File "", line 145, characters 6-14
                      Body Range: File "", line 145, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (get_and_update#133 -> get_and_update)
                      Range: File "", line 144, characters 6-20
                      Body Range: File "", line 144, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#115 -> literal)
                      Range: File "", line 137, characters 25-32
                      Body Range: File "", line 137, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#118 -> mem)
                      Range: File "", line 140, characters 6-9
                      Body Range: File "", line 140, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (remove#125 -> remove)
                      Range: File "", line 142, characters 6-12
                      Body Range: File "", line 142, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#129 -> update)
                      Range: File "", line 143, characters 6-12
                      Body Range: File "", line 143, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#112 -> Bitwise)
    Range: File "", line 117, characters 7-14
    Body Range: File "", line 117, character 0 to line 133, character 3
    Content: Members: Variable definitions:
                      (and#99 -> and)
                      Range: File "", line 119, characters 6-10
                      Body Range: File "", line 119, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (or#105 -> or)
                      Range: File "", line 121, characters 6-9
                      Body Range: File "", line 121, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_left#108 -> shift_left)
                      Range: File "", line 122, characters 6-16
                      Body Range: File "", line 122, characters 18-19
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_right#111 -> shift_right)
                      Range: File "", line 123, characters 6-17
                      Body Range: File "", line 123, characters 19-20
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (xor#102 -> xor)
                      Range: File "", line 120, characters 6-9
                      Body Range: File "", line 120, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#303 -> Bytes)
    Range: File "", line 289, characters 7-12
    Body Range: File "", line 289, character 0 to line 305, character 3
    Content: Members: Variable definitions:
                      (concat#298 -> concat)
                      Range: File "", line 296, characters 6-12
                      Body Range: File "", line 296, characters 14-16
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#289 -> concats)
                      Range: File "", line 290, characters 6-13
                      Body Range: File "", line 290, characters 15-17
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#295 -> length)
                      Range: File "", line 293, characters 6-12
                      Body Range: File "", line 293, characters 14-15
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#291 -> pack)
                      Range: File "", line 291, characters 6-10
                      Body Range: File "", line 291, characters 11-19
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#302 -> sub)
                      Range: File "", line 297, characters 6-9
                      Body Range: File "", line 297, characters 11-12
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#293 -> unpack)
                      Range: File "", line 292, characters 6-12
                      Body Range: File "", line 292, characters 13-21
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#320 -> Crypto)
    Range: File "", line 307, characters 7-13
    Body Range: File "", line 307, character 0 to line 323, character 3
    Content: Members: Variable definitions:
                      (blake2b#305 -> blake2b)
                      Range: File "", line 308, characters 6-13
                      Body Range: File "", line 308, characters 15-16
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#319 -> check)
                      Range: File "", line 316, characters 6-11
                      Body Range: File "", line 316, characters 13-14
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#315 -> hash_key)
                      Range: File "", line 313, characters 6-14
                      Body Range: File "", line 313, characters 16-17
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#313 -> keccak)
                      Range: File "", line 312, characters 6-12
                      Body Range: File "", line 312, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#307 -> sha256)
                      Range: File "", line 309, characters 6-12
                      Body Range: File "", line 309, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#311 -> sha3)
                      Range: File "", line 311, characters 6-10
                      Body Range: File "", line 311, characters 12-13
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#309 -> sha512)
                      Range: File "", line 310, characters 6-12
                      Body Range: File "", line 310, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#250 -> List)
    Range: File "", line 222, characters 7-11
    Body Range: File "", line 222, character 0 to line 250, character 3
    Content: Members: Variable definitions:
                      (cons#245 -> cons)
                      Range: File "", line 234, characters 6-10
                      Body Range: File "", line 234, characters 11-19
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (find_opt#249 -> find_opt)
                      Range: File "", line 235, characters 6-14
                      Body Range: File "", line 235, characters 15-23
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#234 -> fold)
                      Range: File "", line 231, characters 6-10
                      Body Range: File "", line 231, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 495, characters 9-13
                      (fold_left#238 -> fold_left)
                      Range: File "", line 232, characters 6-15
                      Body Range: File "", line 232, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#242 -> fold_right)
                      Range: File "", line 233, characters 6-16
                      Body Range: File "", line 233, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references: File "", line 236, characters 4-14
                      (head_opt#220 -> head_opt)
                      Range: File "", line 225, characters 6-14
                      Body Range: File "", line 225, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#230 -> iter)
                      Range: File "", line 230, characters 6-10
                      Body Range: File "", line 230, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#214 -> length)
                      Range: File "", line 223, characters 6-12
                      Body Range: File "", line 223, characters 13-21
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#227 -> map)
                      Range: File "", line 229, characters 6-9
                      Body Range: File "", line 229, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10
                      (size#216 -> size)
                      Range: File "", line 224, characters 6-10
                      Body Range: File "", line 224, characters 11-19
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#224 -> tail_opt)
                      Range: File "", line 226, characters 6-14
                      Body Range: File "", line 226, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 495, characters 4-8

    (Map#180 -> Map)
    Range: File "", line 161, characters 7-10
    Body Range: File "", line 161, character 0 to line 192, character 3
    Content: Members: Variable definitions:
                      (add#152 -> add)
                      Range: File "", line 168, characters 6-9
                      Body Range: File "", line 168, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (empty#141 -> empty)
                      Range: File "", line 162, characters 6-11
                      Body Range: File "", line 162, characters 12-22
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#166 -> find)
                      Range: File "", line 172, characters 6-10
                      Body Range: File "", line 172, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#169 -> find_opt)
                      Range: File "", line 173, characters 6-14
                      Body Range: File "", line 173, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (fold#179 -> fold)
                      Range: File "", line 176, characters 6-10
                      Body Range: File "", line 176, characters 11-23
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c * ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      (get_and_update#163 -> get_and_update)
                      Range: File "", line 171, characters 6-20
                      Body Range: File "", line 171, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (iter#172 -> iter)
                      Range: File "", line 174, characters 6-10
                      Body Range: File "", line 174, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (literal#145 -> literal)
                      Range: File "", line 164, characters 25-32
                      Body Range: File "", line 164, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#175 -> map)
                      Range: File "", line 175, characters 6-9
                      Body Range: File "", line 175, characters 10-22
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k * v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (mem#148 -> mem)
                      Range: File "", line 167, characters 6-9
                      Body Range: File "", line 167, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (remove#155 -> remove)
                      Range: File "", line 169, characters 6-12
                      Body Range: File "", line 169, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (size#143 -> size)
                      Range: File "", line 163, characters 6-10
                      Body Range: File "", line 163, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#159 -> update)
                      Range: File "", line 170, characters 6-12
                      Body Range: File "", line 170, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#287 -> Option)
    Range: File "", line 268, characters 7-13
    Body Range: File "", line 268, character 0 to line 287, character 3
    Content: Members: Variable definitions:
                      (is_none#283 -> is_none)
                      Range: File "", line 285, characters 6-13
                      Body Range: File "", line 285, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#286 -> is_some)
                      Range: File "", line 286, characters 6-13
                      Body Range: File "", line 286, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#272 -> map)
                      Range: File "", line 273, characters 15-18
                      Body Range: File "", line 273, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#265 -> unopt)
                      Range: File "", line 269, characters 6-11
                      Body Range: File "", line 269, characters 12-20
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#269 -> unopt_with_error)
                      Range: File "", line 272, characters 6-22
                      Body Range: File "", line 272, characters 23-31
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#276 -> value)
                      Range: File "", line 274, characters 6-11
                      Body Range: File "", line 274, characters 12-20
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#280 -> value_exn)
                      Range: File "", line 275, characters 6-15
                      Body Range: File "", line 275, characters 16-28
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#212 -> Set)
    Range: File "", line 194, characters 7-10
    Body Range: File "", line 194, character 0 to line 220, character 3
    Content: Members: Variable definitions:
                      (add#193 -> add)
                      Range: File "", line 202, characters 6-9
                      Body Range: File "", line 202, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (cardinal#185 -> cardinal)
                      Range: File "", line 197, characters 6-14
                      Body Range: File "", line 197, characters 15-23
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#181 -> empty)
                      Range: File "", line 195, characters 6-11
                      Body Range: File "", line 195, characters 12-20
                      Content: |core: ∀ a : * . set (a)|
                      references: []
                      (fold#207 -> fold)
                      Range: File "", line 206, characters 6-10
                      Body Range: File "", line 206, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#211 -> fold_desc)
                      Range: File "", line 207, characters 6-15
                      Body Range: File "", line 207, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: []
                      (iter#203 -> iter)
                      Range: File "", line 205, characters 6-10
                      Body Range: File "", line 205, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#187 -> literal)
                      Range: File "", line 198, characters 25-32
                      Body Range: File "", line 198, characters 33-41
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#190 -> mem)
                      Range: File "", line 201, characters 6-9
                      Body Range: File "", line 201, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#196 -> remove)
                      Range: File "", line 203, characters 6-12
                      Body Range: File "", line 203, characters 13-21
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#183 -> size)
                      Range: File "", line 196, characters 6-10
                      Body Range: File "", line 196, characters 11-19
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#200 -> update)
                      Range: File "", line 204, characters 6-12
                      Body Range: File "", line 204, characters 13-21
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#262 -> String)
    Range: File "", line 252, characters 7-13
    Body Range: File "", line 252, character 0 to line 266, character 3
    Content: Members: Variable definitions:
                      (concat#257 -> concat)
                      Range: File "", line 257, characters 6-12
                      Body Range: File "", line 257, characters 14-16
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#254 -> concats)
                      Range: File "", line 254, characters 6-13
                      Body Range: File "", line 254, characters 15-17
                      Content: |core: list (string) -> string|
                      references: []
                      (length#252 -> length)
                      Range: File "", line 253, characters 6-12
                      Body Range: File "", line 253, characters 14-15
                      Content: |core: string -> nat|
                      references:
                        File "", line 525, characters 22-28 ,
                        File "", line 528, characters 43-49
                      (sub#261 -> sub)
                      Range: File "", line 258, characters 6-9
                      Body Range: File "", line 258, characters 11-12
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 526, characters 24-27 ,
                        File "", line 528, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 525, characters 15-21 ,
      File "", line 526, characters 17-23 ,
      File "", line 528, characters 16-22 ,
      File "", line 528, characters 36-42

    (Test#693 -> Test)
    Range: File "", line 374, characters 7-11
    Body Range: File "", line 374, character 0 to line 769, character 3
    Content: Members: Variable definitions:
                      (add_account#520 -> add_account)
                      Range: File "", line 508, characters 6-17
                      Body Range: File "", line 508, characters 19-20
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#675 -> assert)
                      Range: File "", line 753, characters 6-12
                      Body Range: File "", line 753, characters 14-15
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#681 -> assert_none)
                      Range: File "", line 755, characters 6-17
                      Body Range: File "", line 755, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#692 -> assert_none_with_error)
                      Range: File "", line 760, characters 6-28
                      Body Range: File "", line 760, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#678 -> assert_some)
                      Range: File "", line 754, characters 6-17
                      Body Range: File "", line 754, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#688 -> assert_some_with_error)
                      Range: File "", line 759, characters 6-28
                      Body Range: File "", line 759, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#684 -> assert_with_error)
                      Range: File "", line 758, characters 6-23
                      Body Range: File "", line 758, characters 25-26
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#412 -> bake_until_n_cycle_end)
                      Range: File "", line 410, characters 6-28
                      Body Range: File "", line 410, characters 30-31
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#523 -> baker_account)
                      Range: File "", line 509, characters 6-19
                      Body Range: File "", line 509, characters 21-22
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#508 -> bootstrap_contract)
                      Range: File "", line 504, characters 6-24
                      Body Range: File "", line 504, characters 25-35
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#416 -> cast_address)
                      Range: File "", line 412, characters 6-18
                      Body Range: File "", line 412, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 538, characters 35-47 ,
                        File "", line 545, characters 35-47
                      (chr#455 -> chr)
                      Range: File "", line 435, characters 6-9
                      Body Range: File "", line 435, characters 11-12
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#450 -> compile_contract)
                      Range: File "", line 431, characters 6-22
                      Body Range: File "", line 431, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> michelson_contract|
                      references:
                        File "", line 534, characters 12-28 ,
                        File "", line 541, characters 12-28
                      (compile_contract_from_file#580 -> compile_contract_from_file)
                      Range: File "", line 547, characters 6-32
                      Body Range: File "", line 547, characters 34-36
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 551, characters 12-38
                      (compile_value#374 -> compile_value)
                      Range: File "", line 388, characters 6-19
                      Body Range: File "", line 388, characters 20-28
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#424 -> constant_to_michelson_program)
                      Range: File "", line 416, characters 6-35
                      Body Range: File "", line 416, characters 37-38
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#529 -> create_chest)
                      Range: File "", line 511, characters 6-18
                      Body Range: File "", line 511, characters 20-21
                      Content: |core: bytes -> nat -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#532 -> create_chest_key)
                      Range: File "", line 512, characters 6-22
                      Body Range: File "", line 512, characters 24-25
                      Content: |core: chest -> nat -> chest_key|
                      references: []
                      (decompile#410 -> decompile)
                      Range: File "", line 409, characters 6-15
                      Body Range: File "", line 409, characters 16-24
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 427, characters 5-14
                      (drop_context#432 -> drop_context)
                      Range: File "", line 420, characters 6-18
                      Body Range: File "", line 420, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#390 -> eprint)
                      Range: File "", line 396, characters 6-12
                      Body Range: File "", line 396, characters 14-15
                      Content: |core: string -> unit|
                      references: []
                      (eval#372 -> eval)
                      Range: File "", line 378, characters 6-10
                      Body Range: File "", line 378, characters 11-19
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 388, characters 59-63 ,
                        File "", line 516, characters 32-36 ,
                        File "", line 521, characters 34-38 ,
                        File "", line 535, characters 12-16 ,
                        File "", line 542, characters 12-16
                      (failwith#378 -> failwith)
                      Range: File "", line 390, characters 6-14
                      Body Range: File "", line 390, characters 15-25
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 753, characters 51-59 ,
                        File "", line 754, characters 74-82 ,
                        File "", line 755, characters 89-97 ,
                        File "", line 758, characters 68-76 ,
                        File "", line 759, characters 98-106 ,
                        File "", line 760, characters 113-121
                      (get_balance#386 -> get_balance)
                      Range: File "", line 394, characters 6-17
                      Body Range: File "", line 394, characters 19-20
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#399 -> get_bootstrap_account)
                      Range: File "", line 402, characters 6-27
                      Body Range: File "", line 402, characters 29-30
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#485 -> get_last_events_from)
                      Range: File "", line 488, characters 6-26
                      Body Range: File "", line 488, characters 27-39
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#441 -> get_storage)
                      Range: File "", line 423, characters 6-17
                      Body Range: File "", line 423, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#384 -> get_storage_of_address)
                      Range: File "", line 393, characters 6-28
                      Body Range: File "", line 393, characters 30-31
                      Content: |core: address -> michelson_program|
                      references: File "", line 426, characters 32-54
                      (get_time#414 -> get_time)
                      Range: File "", line 411, characters 6-14
                      Body Range: File "", line 411, characters 16-18
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#376 -> get_total_voting_power)
                      Range: File "", line 389, characters 6-28
                      Body Range: File "", line 389, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#392 -> get_voting_power)
                      Range: File "", line 397, characters 6-22
                      Body Range: File "", line 397, characters 24-26
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#403 -> last_originations)
                      Range: File "", line 404, characters 6-23
                      Body Range: File "", line 404, characters 25-26
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#497 -> log)
                      Range: File "", line 498, characters 6-9
                      Body Range: File "", line 498, characters 10-18
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 527, characters 25-28
                      (michelson_equal#549 -> michelson_equal)
                      Range: File "", line 523, characters 6-21
                      Body Range: File "", line 523, characters 23-25
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#511 -> mutate_value)
                      Range: File "", line 505, characters 6-18
                      Body Range: File "", line 505, characters 19-27
                      Content: |core: ∀ a : * . nat -> a -> option (( a * mutation ))|
                      references:
                        File "", line 559, characters 23-35 ,
                        File "", line 571, characters 23-35
                      (mutation_test#604 -> mutation_test)
                      Range: File "", line 555, characters 6-19
                      Body Range: File "", line 555, characters 20-30
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b * mutation ))|
                      references: []
                      (mutation_test_all#620 -> mutation_test_all)
                      Range: File "", line 567, characters 6-23
                      Body Range: File "", line 567, characters 24-34
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b * mutation ))|
                      references: []
                      (new_account#408 -> new_account)
                      Range: File "", line 408, characters 6-17
                      Body Range: File "", line 408, characters 19-20
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#456 -> nl)
                      Range: File "", line 445, characters 6-8
                      Body Range: File "", line 445, characters 11-53
                      Content: |unresolved|
                      references: File "", line 447, characters 15-17
                      (nth_bootstrap_account#397 -> nth_bootstrap_account)
                      Range: File "", line 399, characters 6-27
                      Body Range: File "", line 399, characters 29-30
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#394 -> nth_bootstrap_contract)
                      Range: File "", line 398, characters 6-28
                      Body Range: File "", line 398, characters 30-31
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#401 -> nth_bootstrap_typed_address)
                      Range: File "", line 403, characters 6-33
                      Body Range: File "", line 403, characters 34-44
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#566 -> originate)
                      Range: File "", line 533, characters 6-15
                      Body Range: File "", line 533, characters 16-26
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) * s ) -> s -> tez ->
                      ( typed_address (p , s) * michelson_contract * int )|
                      references: []
                      (originate_contract#557 -> originate_contract)
                      Range: File "", line 532, characters 6-24
                      Body Range: File "", line 532, characters 26-27
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 536, characters 12-30 ,
                        File "", line 543, characters 12-30 ,
                        File "", line 552, characters 12-30 ,
                        File "", line 583, characters 14-32 ,
                        File "", line 603, characters 14-32
                      (originate_from_file#589 -> originate_from_file)
                      Range: File "", line 550, characters 6-25
                      Body Range: File "", line 550, characters 27-29
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address * michelson_contract * int )|
                      references: []
                      (originate_from_file_and_mutate#646 -> originate_from_file_and_mutate)
                      Range: File "", line 579, characters 6-36
                      Body Range: File "", line 579, characters 37-45
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address * michelson_contract * int ) -> b -> option (
                      ( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#673 -> originate_from_file_and_mutate_all)
                      Range: File "", line 599, characters 6-40
                      Body Range: File "", line 599, characters 41-49
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address * michelson_contract * int ) -> b -> list (
                      ( b * mutation ))|
                      references: []
                      (originate_uncurried#575 -> originate_uncurried)
                      Range: File "", line 540, characters 6-25
                      Body Range: File "", line 540, characters 26-36
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> s -> tez -> ( typed_address (p , s) * michelson_contract * int )|
                      references: []
                      (parse_michelson#426 -> parse_michelson)
                      Range: File "", line 417, characters 6-21
                      Body Range: File "", line 417, characters 23-24
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#388 -> print)
                      Range: File "", line 395, characters 6-11
                      Body Range: File "", line 395, characters 13-14
                      Content: |core: string -> unit|
                      references:
                        File "", line 447, characters 4-9 ,
                        File "", line 501, characters 4-9
                      (println#458 -> println)
                      Range: File "", line 446, characters 6-13
                      Body Range: File "", line 446, characters 15-16
                      Content: |core: string -> unit|
                      references: []
                      (random#406 -> random)
                      Range: File "", line 405, characters 6-12
                      Body Range: File "", line 405, characters 13-21
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#452 -> read_contract_from_file)
                      Range: File "", line 434, characters 6-29
                      Body Range: File "", line 434, characters 31-33
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#420 -> register_constant)
                      Range: File "", line 414, characters 6-23
                      Body Range: File "", line 414, characters 25-26
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#418 -> register_delegate)
                      Range: File "", line 413, characters 6-23
                      Body Range: File "", line 413, characters 25-27
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#500 -> reset_state)
                      Range: File "", line 502, characters 6-17
                      Body Range: File "", line 502, characters 19-20
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#504 -> reset_state_at)
                      Range: File "", line 503, characters 6-20
                      Body Range: File "", line 503, characters 22-23
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#428 -> restore_context)
                      Range: File "", line 418, characters 6-21
                      Body Range: File "", line 418, characters 23-24
                      Content: |core: unit -> unit|
                      references: []
                      (run#369 -> run)
                      Range: File "", line 377, characters 6-9
                      Body Range: File "", line 377, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 378, characters 50-53
                      (save_context#430 -> save_context)
                      Range: File "", line 419, characters 6-18
                      Body Range: File "", line 419, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#514 -> save_mutation)
                      Range: File "", line 506, characters 6-19
                      Body Range: File "", line 506, characters 21-22
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#445 -> set_baker)
                      Range: File "", line 429, characters 6-15
                      Body Range: File "", line 429, characters 17-18
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#443 -> set_baker_policy)
                      Range: File "", line 428, characters 6-22
                      Body Range: File "", line 428, characters 24-26
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 429, characters 39-55
                      (set_big_map#526 -> set_big_map)
                      Range: File "", line 510, characters 6-17
                      Body Range: File "", line 510, characters 18-28
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#459 -> set_print_values)
                      Range: File "", line 449, characters 6-22
                      Body Range: File "", line 449, characters 24-25
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#382 -> set_source)
                      Range: File "", line 392, characters 6-16
                      Body Range: File "", line 392, characters 18-19
                      Content: |core: address -> unit|
                      references: []
                      (sign#517 -> sign)
                      Range: File "", line 507, characters 6-10
                      Body Range: File "", line 507, characters 12-14
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#447 -> size)
                      Range: File "", line 430, characters 6-10
                      Body Range: File "", line 430, characters 12-13
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 537, characters 12-16 ,
                        File "", line 544, characters 12-16 ,
                        File "", line 553, characters 12-16 ,
                        File "", line 584, characters 14-18 ,
                        File "", line 604, characters 14-18
                      (to_contract#380 -> to_contract)
                      Range: File "", line 391, characters 6-17
                      Body Range: File "", line 391, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 424, characters 25-36 ,
                        File "", line 489, characters 30-41
                      (to_entrypoint#553 -> to_entrypoint)
                      Range: File "", line 524, characters 6-19
                      Body Range: File "", line 524, characters 20-32
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#436 -> to_json)
                      Range: File "", line 422, characters 6-13
                      Body Range: File "", line 422, characters 14-22
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#434 -> to_string)
                      Range: File "", line 421, characters 6-15
                      Body Range: File "", line 421, characters 16-24
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 438, characters 68-77 ,
                        File "", line 440, characters 67-76 ,
                        File "", line 442, characters 61-70 ,
                        File "", line 500, characters 12-21
                      (to_typed_address#422 -> to_typed_address)
                      Range: File "", line 415, characters 6-22
                      Body Range: File "", line 415, characters 23-33
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#489 -> transfer)
                      Range: File "", line 496, characters 6-14
                      Body Range: File "", line 496, characters 16-17
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#493 -> transfer_exn)
                      Range: File "", line 497, characters 6-18
                      Body Range: File "", line 497, characters 20-21
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#539 -> transfer_to_contract)
                      Range: File "", line 513, characters 6-26
                      Body Range: File "", line 513, characters 27-35
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#546 -> transfer_to_contract_exn)
                      Range: File "", line 518, characters 6-30
                      Body Range: File "", line 518, characters 31-39
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#460 -> unset_print_values)
                      Range: File "", line 450, characters 6-24
                      Body Range: File "", line 450, characters 26-27
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#475 -> PBT)
                      Range: File "", line 452, characters 9-12
                      Body Range: File "", line 452, character 2 to line 485, character 5
                      Content: Members: Variable definitions:
                                        (gen#461 -> gen)
                                        Range: File "", line 453, characters 8-11
                                        Body Range: File "", line 453, characters 12-20
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references:
                                          File "", line 457, characters 23-27 ,
                                          File "", line 458, characters 23-27
                                        (gen_small#462 -> gen_small)
                                        Range: File "", line 454, characters 8-17
                                        Body Range: File "", line 454, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#465 -> make_test)
                                        Range: File "", line 456, characters 8-17
                                        Body Range: File "", line 456, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#474 -> run)
                                        Range: File "", line 457, characters 8-11
                                        Body Range: File "", line 457, characters 12-20
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#96 -> Tezos)
    Range: File "", line 8, characters 7-12
    Body Range: File "", line 8, character 0 to line 115, character 3
    Content: Members: Variable definitions:
                      (address#26 -> address)
                      Range: File "", line 21, characters 6-13
                      Body Range: File "", line 21, characters 14-22
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 489, characters 21-28
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
                      (create_contract#76 -> create_contract)
                      Range: File "", line 61, characters 25-40
                      Body Range: File "", line 61, characters 41-51
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) * s ) -> option (key_hash) -> tez -> s ->
                      ( operation * address )|
                      references: []
                      (create_contract_uncurried#81 -> create_contract_uncurried)
                      Range: File "", line 64, characters 25-50
                      Body Range: File "", line 64, characters 51-61
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> option (key_hash) -> tez -> s ->
                      ( operation * address )|
                      references: []
                      (create_ticket#57 -> create_ticket)
                      Range: File "", line 49, characters 6-19
                      Body Range: File "", line 49, characters 20-28
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references: []
                      (emit#92 -> emit)
                      Range: File "", line 72, characters 25-29
                      Body Range: File "", line 72, characters 30-38
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
                      (get_entrypoint#89 -> get_entrypoint)
                      Range: File "", line 69, characters 25-39
                      Body Range: File "", line 69, characters 40-48
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (get_entrypoint_opt#84 -> get_entrypoint_opt)
                      Range: File "", line 66, characters 25-43
                      Body Range: File "", line 66, characters 44-52
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 70, characters 12-30
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
                      references: File "", line 411, characters 47-54
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
                      (sapling_verify_update#95 -> sapling_verify_update)
                      Range: File "", line 75, characters 25-46
                      Body Range: File "", line 75, characters 47-59
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
      File "", line 411, characters 41-46 ,
      File "", line 489, characters 15-20 |}]
