open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Big_map#85:7-14 Bitwise#77:7-14 Bytes#181:7-12 Crypto#191:7-13 List#140:7-11 Map#99:7-10 Option#170:7-13 PBT#311:9-12 Set#123:7-10 String#162:7-13 Test#241:7-11 Tezos#7:7-12 Transpiled#117:7-17 abs#205:4-7 add#105:6-9 add#130:6-9 add#90:6-9 add_account#349:6-17 address#20:6-13 and#78:6-10 assert#202:4-10 assert#471:6-12 assert_none#204:4-15 assert_none#473:6-17 assert_none_with_error#217:4-26 assert_none_with_error#477:6-28 assert_some#203:4-15 assert_some#472:6-17 assert_some_with_error#216:4-26 assert_some_with_error#476:6-28 assert_with_error#215:4-21 assert_with_error#475:6-23 bake_until_n_cycle_end#268:6-28 baker_account#350:6-19 blake2b#192:6-13 bool#4:5-9 bootstrap_contract#345:6-24 call_view#55:25-34 cardinal#126:6-14 cast_address#270:6-18 check#199:6-11 chr#294:6-9 compile_contract#289:6-22 compile_contract_from_file#398:6-32 compile_contract_with_views#381:8-35 compile_value#246:6-19 concat#166:6-12 concat#187:6-12 concats#164:6-13 concats#182:6-13 cons#151:6-10 constant#31:25-33 constant_to_michelson_program#274:6-35 create_chest#352:6-18 create_chest_key#353:6-22 create_contract#59:25-40 create_contract_uncurried#62:25-50 create_ticket#47:6-19 curry#212:4-9 decompile#267:6-15 drop_context#278:6-18 ediv#218:4-8 emit#70:25-29 empty#100:6-11 empty#124:6-11 empty#86:16-21 eprint#254:6-12 eval#244:6-10 failwith#248:6-14 failwith#2:4-12 false#208:4-9 filter_map#136:6-16 filter_map#154:6-16 find#109:6-10 find#95:6-10 find_opt#110:6-14 find_opt#152:6-14 find_opt#94:6-14 fold#113:6-10 fold#134:6-10 fold#148:6-10 fold_desc#135:6-15 fold_left#149:6-15 fold_right#150:6-16 gen#312:8-11 gen_small#313:8-17 get_amount#10:6-16 get_and_update#108:6-20 get_and_update#93:6-20 get_balance#252:6-17 get_balance#9:6-17 get_bootstrap_account#260:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#67:25-39 get_entrypoint_opt#64:25-43 get_last_events_from#329:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#281:6-17 get_storage_of_address#251:6-28 get_time#269:6-14 get_total_voting_power#17:6-28 get_total_voting_power#247:6-28 get_voting_power#255:6-22 hash_key#197:6-14 head_opt#143:6-14 ignore#211:4-10 implicit_account#21:6-22 int#210:4-7 is_nat#206:4-10 is_none#177:6-13 is_some#178:6-13 iter#111:6-10 iter#133:6-10 iter#147:6-10 join_tickets#22:6-18 keccak#196:6-12 last_originations#262:6-23 length#141:6-12 length#163:6-12 length#185:6-12 literal#102:25-32 literal#127:25-32 literal#87:25-32 log#339:6-9 make_test#314:8-17 map#112:6-9 map#146:6-9 map#174:15-18 map_add#119:6-13 map_find_opt#118:6-18 map_remove#120:6-16 mem#104:6-9 mem#129:6-9 mem#89:6-9 michelson_equal#364:6-21 mutate_value#346:6-18 mutation_test#406:6-19 mutation_test_all#418:6-23 never#25:6-11 new_account#266:6-17 nl#304:6-8 nth_bootstrap_account#257:6-27 nth_bootstrap_contract#256:6-28 nth_bootstrap_typed_address#261:6-33 option#5:8-14 or#80:6-9 originate#374:6-15 originate_contract#373:6-24 originate_from_file#401:6-25 originate_from_file_and_mutate#430:6-36 originate_from_file_and_mutate_all#450:6-40 originate_module#391:6-22 originate_uncurried#384:6-25 pack#183:6-10 pairing_check#26:6-19 parse_michelson#275:6-21 pbt_result#237:8-18 pbt_test#236:8-16 print#253:6-11 println#305:6-13 random#263:6-12 read_contract_from_file#293:6-29 read_ticket#23:6-17 register_constant#272:6-23 register_delegate#271:6-23 remove#106:6-12 remove#131:6-12 remove#91:6-12 reset_state#343:6-17 reset_state_at#344:6-20 restore_context#276:6-21 run#243:6-9 run#315:8-11 sapling_empty_state#32:25-44 sapling_verify_update#73:25-46 save_context#277:6-18 save_mutation#347:6-19 self#28:25-29 set_baker#287:6-15 set_baker_policy#286:6-22 set_big_map#351:6-17 set_delegate#27:6-18 set_print_values#308:6-22 set_source#250:6-16 sha256#193:6-12 sha3#195:6-10 sha512#194:6-12 shift_left#81:6-16 shift_right#82:6-17 sign#348:6-10 size#101:6-10 size#125:6-10 size#142:6-10 size#288:6-10 split_ticket#57:6-18 sub#167:6-9 sub#188:6-9 tail_opt#144:6-14 test_baker_policy#231:5-22 test_exec_error#224:5-20 test_exec_error_balance_too_low#221:5-36 test_exec_result#229:5-21 to_contract#249:6-17 to_entrypoint#365:6-19 to_json#280:6-13 to_string#279:6-15 to_typed_address#273:6-22 transaction#49:6-17 transfer#337:6-14 transfer_exn#338:6-18 transfer_to_contract#354:6-26 transfer_to_contract_exn#359:6-30 true#207:4-8 uncurry#213:4-11 unforged_ticket#239:8-23 unit#209:4-8 unopt#171:6-11 unopt_with_error#173:6-22 unpack#184:6-12 unset_print_values#309:6-24 update#107:6-12 update#132:6-12 update#156:6-12 update#92:6-12 update_with#158:6-17 value#175:6-11 value_exn#176:6-15 voting_power#19:6-18 xor#79:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ Big_map#85:7-14 Bitwise#77:7-14 Bytes#181:7-12 Crypto#191:7-13 List#140:7-11 Map#99:7-10 Option#170:7-13 PBT#311:9-12 Set#123:7-10 String#162:7-13 Test#241:7-11 Tezos#7:7-12 Transpiled#117:7-17 a#1:4-5 abs#205:4-7 add#105:6-9 add#130:6-9 add#90:6-9 add_account#349:6-17 address#20:6-13 and#78:6-10 assert#202:4-10 assert#471:6-12 assert_none#204:4-15 assert_none#473:6-17 assert_none_with_error#217:4-26 assert_none_with_error#477:6-28 assert_some#203:4-15 assert_some#472:6-17 assert_some_with_error#216:4-26 assert_some_with_error#476:6-28 assert_with_error#215:4-21 assert_with_error#475:6-23 bake_until_n_cycle_end#268:6-28 baker_account#350:6-19 blake2b#192:6-13 bool#4:5-9 bootstrap_contract#345:6-24 c#5:10-11 call_view#55:25-34 cardinal#126:6-14 cast_address#270:6-18 check#199:6-11 chr#294:6-9 compile_contract#289:6-22 compile_contract_from_file#398:6-32 compile_contract_with_views#381:8-35 compile_value#246:6-19 concat#166:6-12 concat#187:6-12 concats#164:6-13 concats#182:6-13 cons#151:6-10 constant#31:25-33 constant_to_michelson_program#274:6-35 create_chest#352:6-18 create_chest_key#353:6-22 create_contract#59:25-40 create_contract_uncurried#62:25-50 create_ticket#47:6-19 curry#212:4-9 decompile#267:6-15 drop_context#278:6-18 ediv#218:4-8 emit#70:25-29 empty#100:6-11 empty#124:6-11 empty#86:16-21 eprint#254:6-12 eval#244:6-10 failwith#248:6-14 failwith#2:4-12 false#208:4-9 filter_map#136:6-16 filter_map#154:6-16 find#109:6-10 find#95:6-10 find_opt#110:6-14 find_opt#152:6-14 find_opt#94:6-14 fold#113:6-10 fold#134:6-10 fold#148:6-10 fold_desc#135:6-15 fold_left#149:6-15 fold_right#150:6-16 gen#312:8-11 gen_small#313:8-17 get_amount#10:6-16 get_and_update#108:6-20 get_and_update#93:6-20 get_balance#252:6-17 get_balance#9:6-17 get_bootstrap_account#260:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#67:25-39 get_entrypoint_opt#64:25-43 get_last_events_from#329:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#281:6-17 get_storage_of_address#251:6-28 get_time#269:6-14 get_total_voting_power#17:6-28 get_total_voting_power#247:6-28 get_voting_power#255:6-22 hash_key#197:6-14 head_opt#143:6-14 ignore#211:4-10 implicit_account#21:6-22 int#210:4-7 is_nat#206:4-10 is_none#177:6-13 is_some#178:6-13 iter#111:6-10 iter#133:6-10 iter#147:6-10 join_tickets#22:6-18 keccak#196:6-12 last_originations#262:6-23 length#141:6-12 length#163:6-12 length#185:6-12 literal#102:25-32 literal#127:25-32 literal#87:25-32 log#339:6-9 make_test#314:8-17 map#112:6-9 map#146:6-9 map#174:15-18 map_add#119:6-13 map_find_opt#118:6-18 map_remove#120:6-16 mem#104:6-9 mem#129:6-9 mem#89:6-9 michelson_equal#364:6-21 mutate_value#346:6-18 mutation_test#406:6-19 mutation_test_all#418:6-23 never#25:6-11 new_account#266:6-17 nl#304:6-8 nth_bootstrap_account#257:6-27 nth_bootstrap_contract#256:6-28 nth_bootstrap_typed_address#261:6-33 option#5:8-14 or#80:6-9 originate#374:6-15 originate_contract#373:6-24 originate_from_file#401:6-25 originate_from_file_and_mutate#430:6-36 originate_from_file_and_mutate_all#450:6-40 originate_module#391:6-22 originate_uncurried#384:6-25 pack#183:6-10 pairing_check#26:6-19 parse_michelson#275:6-21 pbt_result#237:8-18 pbt_test#236:8-16 print#253:6-11 println#305:6-13 random#263:6-12 read_contract_from_file#293:6-29 read_ticket#23:6-17 register_constant#272:6-23 register_delegate#271:6-23 remove#106:6-12 remove#131:6-12 remove#91:6-12 reset_state#343:6-17 reset_state_at#344:6-20 restore_context#276:6-21 run#243:6-9 run#315:8-11 sapling_empty_state#32:25-44 sapling_verify_update#73:25-46 save_context#277:6-18 save_mutation#347:6-19 self#28:25-29 set_baker#287:6-15 set_baker_policy#286:6-22 set_big_map#351:6-17 set_delegate#27:6-18 set_print_values#308:6-22 set_source#250:6-16 sha256#193:6-12 sha3#195:6-10 sha512#194:6-12 shift_left#81:6-16 shift_right#82:6-17 sign#348:6-10 size#101:6-10 size#125:6-10 size#142:6-10 size#288:6-10 split_ticket#57:6-18 sub#167:6-9 sub#188:6-9 tail_opt#144:6-14 test_baker_policy#231:5-22 test_exec_error#224:5-20 test_exec_error_balance_too_low#221:5-36 test_exec_result#229:5-21 to_contract#249:6-17 to_entrypoint#365:6-19 to_json#280:6-13 to_string#279:6-15 to_typed_address#273:6-22 transaction#49:6-17 transfer#337:6-14 transfer_exn#338:6-18 transfer_to_contract#354:6-26 transfer_to_contract_exn#359:6-30 true#207:4-8 uncurry#213:4-11 unforged_ticket#239:8-23 unit#209:4-8 unopt#171:6-11 unopt_with_error#173:6-22 unpack#184:6-12 unset_print_values#309:6-24 update#107:6-12 update#132:6-12 update#156:6-12 update#92:6-12 update_with#158:6-17 value#175:6-11 value_exn#176:6-15 voting_power#19:6-18 xor#79:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ Big_map#85:7-14 Bitwise#77:7-14 Bytes#181:7-12 Crypto#191:7-13 List#140:7-11 Map#99:7-10 Option#170:7-13 PBT#311:9-12 Set#123:7-10 String#162:7-13 Test#241:7-11 Tezos#7:7-12 Transpiled#117:7-17 a#1:4-5 abs#205:4-7 add#105:6-9 add#130:6-9 add#90:6-9 add_account#349:6-17 address#20:6-13 and#78:6-10 assert#202:4-10 assert#471:6-12 assert_none#204:4-15 assert_none#473:6-17 assert_none_with_error#217:4-26 assert_none_with_error#477:6-28 assert_some#203:4-15 assert_some#472:6-17 assert_some_with_error#216:4-26 assert_some_with_error#476:6-28 assert_with_error#215:4-21 assert_with_error#475:6-23 bake_until_n_cycle_end#268:6-28 baker_account#350:6-19 blake2b#192:6-13 bool#4:5-9 bootstrap_contract#345:6-24 c#5:10-11 call_view#55:25-34 cardinal#126:6-14 cast_address#270:6-18 check#199:6-11 chr#294:6-9 compile_contract#289:6-22 compile_contract_from_file#398:6-32 compile_contract_with_views#381:8-35 compile_value#246:6-19 concat#166:6-12 concat#187:6-12 concats#164:6-13 concats#182:6-13 cons#151:6-10 constant#31:25-33 constant_to_michelson_program#274:6-35 create_chest#352:6-18 create_chest_key#353:6-22 create_contract#59:25-40 create_contract_uncurried#62:25-50 create_ticket#47:6-19 curry#212:4-9 d#5:26-27 decompile#267:6-15 drop_context#278:6-18 ediv#218:4-8 emit#70:25-29 empty#100:6-11 empty#124:6-11 empty#86:16-21 eprint#254:6-12 eval#244:6-10 failwith#248:6-14 failwith#2:4-12 false#208:4-9 filter_map#136:6-16 filter_map#154:6-16 find#109:6-10 find#95:6-10 find_opt#110:6-14 find_opt#152:6-14 find_opt#94:6-14 fold#113:6-10 fold#134:6-10 fold#148:6-10 fold_desc#135:6-15 fold_left#149:6-15 fold_right#150:6-16 gen#312:8-11 gen_small#313:8-17 get_amount#10:6-16 get_and_update#108:6-20 get_and_update#93:6-20 get_balance#252:6-17 get_balance#9:6-17 get_bootstrap_account#260:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#67:25-39 get_entrypoint_opt#64:25-43 get_last_events_from#329:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#281:6-17 get_storage_of_address#251:6-28 get_time#269:6-14 get_total_voting_power#17:6-28 get_total_voting_power#247:6-28 get_voting_power#255:6-22 hash_key#197:6-14 head_opt#143:6-14 ignore#211:4-10 implicit_account#21:6-22 int#210:4-7 is_nat#206:4-10 is_none#177:6-13 is_some#178:6-13 iter#111:6-10 iter#133:6-10 iter#147:6-10 join_tickets#22:6-18 keccak#196:6-12 last_originations#262:6-23 length#141:6-12 length#163:6-12 length#185:6-12 literal#102:25-32 literal#127:25-32 literal#87:25-32 log#339:6-9 make_test#314:8-17 map#112:6-9 map#146:6-9 map#174:15-18 map_add#119:6-13 map_find_opt#118:6-18 map_remove#120:6-16 mem#104:6-9 mem#129:6-9 mem#89:6-9 michelson_equal#364:6-21 mutate_value#346:6-18 mutation_test#406:6-19 mutation_test_all#418:6-23 never#25:6-11 new_account#266:6-17 nl#304:6-8 nth_bootstrap_account#257:6-27 nth_bootstrap_contract#256:6-28 nth_bootstrap_typed_address#261:6-33 option#5:8-14 or#80:6-9 originate#374:6-15 originate_contract#373:6-24 originate_from_file#401:6-25 originate_from_file_and_mutate#430:6-36 originate_from_file_and_mutate_all#450:6-40 originate_module#391:6-22 originate_uncurried#384:6-25 pack#183:6-10 pairing_check#26:6-19 parse_michelson#275:6-21 pbt_result#237:8-18 pbt_test#236:8-16 print#253:6-11 println#305:6-13 random#263:6-12 read_contract_from_file#293:6-29 read_ticket#23:6-17 register_constant#272:6-23 register_delegate#271:6-23 remove#106:6-12 remove#131:6-12 remove#91:6-12 reset_state#343:6-17 reset_state_at#344:6-20 restore_context#276:6-21 run#243:6-9 run#315:8-11 sapling_empty_state#32:25-44 sapling_verify_update#73:25-46 save_context#277:6-18 save_mutation#347:6-19 self#28:25-29 set_baker#287:6-15 set_baker_policy#286:6-22 set_big_map#351:6-17 set_delegate#27:6-18 set_print_values#308:6-22 set_source#250:6-16 sha256#193:6-12 sha3#195:6-10 sha512#194:6-12 shift_left#81:6-16 shift_right#82:6-17 sign#348:6-10 size#101:6-10 size#125:6-10 size#142:6-10 size#288:6-10 split_ticket#57:6-18 sub#167:6-9 sub#188:6-9 tail_opt#144:6-14 test_baker_policy#231:5-22 test_exec_error#224:5-20 test_exec_error_balance_too_low#221:5-36 test_exec_result#229:5-21 to_contract#249:6-17 to_entrypoint#365:6-19 to_json#280:6-13 to_string#279:6-15 to_typed_address#273:6-22 transaction#49:6-17 transfer#337:6-14 transfer_exn#338:6-18 transfer_to_contract#354:6-26 transfer_to_contract_exn#359:6-30 true#207:4-8 uncurry#213:4-11 unforged_ticket#239:8-23 unit#209:4-8 unopt#171:6-11 unopt_with_error#173:6-22 unpack#184:6-12 unset_print_values#309:6-24 update#107:6-12 update#132:6-12 update#156:6-12 update#92:6-12 update_with#158:6-17 value#175:6-11 value_exn#176:6-15 voting_power#19:6-18 xor#79:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Big_map#85:7-14 Bitwise#77:7-14 Bytes#181:7-12 Crypto#191:7-13 List#140:7-11 Map#99:7-10 Option#170:7-13 PBT#311:9-12 Set#123:7-10 String#162:7-13 Test#241:7-11 Tezos#7:7-12 Transpiled#117:7-17 a#1:4-5 abs#205:4-7 add#105:6-9 add#130:6-9 add#90:6-9 add_account#349:6-17 address#20:6-13 and#78:6-10 assert#202:4-10 assert#471:6-12 assert_none#204:4-15 assert_none#473:6-17 assert_none_with_error#217:4-26 assert_none_with_error#477:6-28 assert_some#203:4-15 assert_some#472:6-17 assert_some_with_error#216:4-26 assert_some_with_error#476:6-28 assert_with_error#215:4-21 assert_with_error#475:6-23 bake_until_n_cycle_end#268:6-28 baker_account#350:6-19 blake2b#192:6-13 bool#4:5-9 bootstrap_contract#345:6-24 call_view#55:25-34 cardinal#126:6-14 cast_address#270:6-18 check#199:6-11 chr#294:6-9 compile_contract#289:6-22 compile_contract_from_file#398:6-32 compile_contract_with_views#381:8-35 compile_value#246:6-19 concat#166:6-12 concat#187:6-12 concats#164:6-13 concats#182:6-13 cons#151:6-10 constant#31:25-33 constant_to_michelson_program#274:6-35 create_chest#352:6-18 create_chest_key#353:6-22 create_contract#59:25-40 create_contract_uncurried#62:25-50 create_ticket#47:6-19 curry#212:4-9 decompile#267:6-15 drop_context#278:6-18 e#6:9-10 ediv#218:4-8 emit#70:25-29 empty#100:6-11 empty#124:6-11 empty#86:16-21 eprint#254:6-12 eval#244:6-10 failwith#248:6-14 failwith#2:4-12 false#208:4-9 filter_map#136:6-16 filter_map#154:6-16 find#109:6-10 find#95:6-10 find_opt#110:6-14 find_opt#152:6-14 find_opt#94:6-14 fold#113:6-10 fold#134:6-10 fold#148:6-10 fold_desc#135:6-15 fold_left#149:6-15 fold_right#150:6-16 gen#312:8-11 gen_small#313:8-17 get_amount#10:6-16 get_and_update#108:6-20 get_and_update#93:6-20 get_balance#252:6-17 get_balance#9:6-17 get_bootstrap_account#260:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#67:25-39 get_entrypoint_opt#64:25-43 get_last_events_from#329:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#281:6-17 get_storage_of_address#251:6-28 get_time#269:6-14 get_total_voting_power#17:6-28 get_total_voting_power#247:6-28 get_voting_power#255:6-22 hash_key#197:6-14 head_opt#143:6-14 ignore#211:4-10 implicit_account#21:6-22 int#210:4-7 is_nat#206:4-10 is_none#177:6-13 is_some#178:6-13 iter#111:6-10 iter#133:6-10 iter#147:6-10 join_tickets#22:6-18 keccak#196:6-12 last_originations#262:6-23 length#141:6-12 length#163:6-12 length#185:6-12 literal#102:25-32 literal#127:25-32 literal#87:25-32 log#339:6-9 make_test#314:8-17 map#112:6-9 map#146:6-9 map#174:15-18 map_add#119:6-13 map_find_opt#118:6-18 map_remove#120:6-16 mem#104:6-9 mem#129:6-9 mem#89:6-9 michelson_equal#364:6-21 mutate_value#346:6-18 mutation_test#406:6-19 mutation_test_all#418:6-23 never#25:6-11 new_account#266:6-17 nl#304:6-8 nth_bootstrap_account#257:6-27 nth_bootstrap_contract#256:6-28 nth_bootstrap_typed_address#261:6-33 option#5:8-14 or#80:6-9 originate#374:6-15 originate_contract#373:6-24 originate_from_file#401:6-25 originate_from_file_and_mutate#430:6-36 originate_from_file_and_mutate_all#450:6-40 originate_module#391:6-22 originate_uncurried#384:6-25 pack#183:6-10 pairing_check#26:6-19 parse_michelson#275:6-21 pbt_result#237:8-18 pbt_test#236:8-16 print#253:6-11 println#305:6-13 random#263:6-12 read_contract_from_file#293:6-29 read_ticket#23:6-17 register_constant#272:6-23 register_delegate#271:6-23 remove#106:6-12 remove#131:6-12 remove#91:6-12 reset_state#343:6-17 reset_state_at#344:6-20 restore_context#276:6-21 run#243:6-9 run#315:8-11 sapling_empty_state#32:25-44 sapling_verify_update#73:25-46 save_context#277:6-18 save_mutation#347:6-19 self#28:25-29 set_baker#287:6-15 set_baker_policy#286:6-22 set_big_map#351:6-17 set_delegate#27:6-18 set_print_values#308:6-22 set_source#250:6-16 sha256#193:6-12 sha3#195:6-10 sha512#194:6-12 shift_left#81:6-16 shift_right#82:6-17 sign#348:6-10 size#101:6-10 size#125:6-10 size#142:6-10 size#288:6-10 split_ticket#57:6-18 sub#167:6-9 sub#188:6-9 tail_opt#144:6-14 test_baker_policy#231:5-22 test_exec_error#224:5-20 test_exec_error_balance_too_low#221:5-36 test_exec_result#229:5-21 to_contract#249:6-17 to_entrypoint#365:6-19 to_json#280:6-13 to_string#279:6-15 to_typed_address#273:6-22 transaction#49:6-17 transfer#337:6-14 transfer_exn#338:6-18 transfer_to_contract#354:6-26 transfer_to_contract_exn#359:6-30 true#207:4-8 uncurry#213:4-11 unforged_ticket#239:8-23 unit#209:4-8 unopt#171:6-11 unopt_with_error#173:6-22 unpack#184:6-12 unset_print_values#309:6-24 update#107:6-12 update#132:6-12 update#156:6-12 update#92:6-12 update_with#158:6-17 value#175:6-11 value_exn#176:6-15 voting_power#19:6-18 xor#79:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32
    [ Big_map#85:7-14 Bitwise#77:7-14 Bytes#181:7-12 Crypto#191:7-13 List#140:7-11 Map#99:7-10 Option#170:7-13 PBT#311:9-12 Set#123:7-10 String#162:7-13 Test#241:7-11 Tezos#7:7-12 Transpiled#117:7-17 a#1:4-5 abs#205:4-7 add#105:6-9 add#130:6-9 add#90:6-9 add_account#349:6-17 address#20:6-13 and#78:6-10 assert#202:4-10 assert#471:6-12 assert_none#204:4-15 assert_none#473:6-17 assert_none_with_error#217:4-26 assert_none_with_error#477:6-28 assert_some#203:4-15 assert_some#472:6-17 assert_some_with_error#216:4-26 assert_some_with_error#476:6-28 assert_with_error#215:4-21 assert_with_error#475:6-23 bake_until_n_cycle_end#268:6-28 baker_account#350:6-19 blake2b#192:6-13 bool#4:5-9 bootstrap_contract#345:6-24 call_view#55:25-34 cardinal#126:6-14 cast_address#270:6-18 check#199:6-11 chr#294:6-9 compile_contract#289:6-22 compile_contract_from_file#398:6-32 compile_contract_with_views#381:8-35 compile_value#246:6-19 concat#166:6-12 concat#187:6-12 concats#164:6-13 concats#182:6-13 cons#151:6-10 constant#31:25-33 constant_to_michelson_program#274:6-35 create_chest#352:6-18 create_chest_key#353:6-22 create_contract#59:25-40 create_contract_uncurried#62:25-50 create_ticket#47:6-19 curry#212:4-9 decompile#267:6-15 drop_context#278:6-18 ediv#218:4-8 emit#70:25-29 empty#100:6-11 empty#124:6-11 empty#86:16-21 eprint#254:6-12 eval#244:6-10 failwith#248:6-14 failwith#2:4-12 false#208:4-9 filter_map#136:6-16 filter_map#154:6-16 find#109:6-10 find#95:6-10 find_opt#110:6-14 find_opt#152:6-14 find_opt#94:6-14 fold#113:6-10 fold#134:6-10 fold#148:6-10 fold_desc#135:6-15 fold_left#149:6-15 fold_right#150:6-16 gen#312:8-11 gen_small#313:8-17 get_amount#10:6-16 get_and_update#108:6-20 get_and_update#93:6-20 get_balance#252:6-17 get_balance#9:6-17 get_bootstrap_account#260:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#67:25-39 get_entrypoint_opt#64:25-43 get_last_events_from#329:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#281:6-17 get_storage_of_address#251:6-28 get_time#269:6-14 get_total_voting_power#17:6-28 get_total_voting_power#247:6-28 get_voting_power#255:6-22 hash_key#197:6-14 head_opt#143:6-14 ignore#211:4-10 implicit_account#21:6-22 int#210:4-7 is_nat#206:4-10 is_none#177:6-13 is_some#178:6-13 iter#111:6-10 iter#133:6-10 iter#147:6-10 join_tickets#22:6-18 keccak#196:6-12 last_originations#262:6-23 length#141:6-12 length#163:6-12 length#185:6-12 literal#102:25-32 literal#127:25-32 literal#87:25-32 log#339:6-9 make_test#314:8-17 map#112:6-9 map#146:6-9 map#174:15-18 map_add#119:6-13 map_find_opt#118:6-18 map_remove#120:6-16 mem#104:6-9 mem#129:6-9 mem#89:6-9 michelson_equal#364:6-21 mutate_value#346:6-18 mutation_test#406:6-19 mutation_test_all#418:6-23 never#25:6-11 new_account#266:6-17 nl#304:6-8 nth_bootstrap_account#257:6-27 nth_bootstrap_contract#256:6-28 nth_bootstrap_typed_address#261:6-33 option#5:8-14 or#80:6-9 originate#374:6-15 originate_contract#373:6-24 originate_from_file#401:6-25 originate_from_file_and_mutate#430:6-36 originate_from_file_and_mutate_all#450:6-40 originate_module#391:6-22 originate_uncurried#384:6-25 pack#183:6-10 pairing_check#26:6-19 parse_michelson#275:6-21 pbt_result#237:8-18 pbt_test#236:8-16 print#253:6-11 println#305:6-13 random#263:6-12 read_contract_from_file#293:6-29 read_ticket#23:6-17 register_constant#272:6-23 register_delegate#271:6-23 remove#106:6-12 remove#131:6-12 remove#91:6-12 reset_state#343:6-17 reset_state_at#344:6-20 restore_context#276:6-21 run#243:6-9 run#315:8-11 sapling_empty_state#32:25-44 sapling_verify_update#73:25-46 save_context#277:6-18 save_mutation#347:6-19 self#28:25-29 set_baker#287:6-15 set_baker_policy#286:6-22 set_big_map#351:6-17 set_delegate#27:6-18 set_print_values#308:6-22 set_source#250:6-16 sha256#193:6-12 sha3#195:6-10 sha512#194:6-12 shift_left#81:6-16 shift_right#82:6-17 sign#348:6-10 size#101:6-10 size#125:6-10 size#142:6-10 size#288:6-10 split_ticket#57:6-18 sub#167:6-9 sub#188:6-9 tail_opt#144:6-14 test_baker_policy#231:5-22 test_exec_error#224:5-20 test_exec_error_balance_too_low#221:5-36 test_exec_result#229:5-21 to_contract#249:6-17 to_entrypoint#365:6-19 to_json#280:6-13 to_string#279:6-15 to_typed_address#273:6-22 transaction#49:6-17 transfer#337:6-14 transfer_exn#338:6-18 transfer_to_contract#354:6-26 transfer_to_contract_exn#359:6-30 true#207:4-8 uncurry#213:4-11 unforged_ticket#239:8-23 unit#209:4-8 unopt#171:6-11 unopt_with_error#173:6-22 unpack#184:6-12 unset_print_values#309:6-24 update#107:6-12 update#132:6-12 update#156:6-12 update#92:6-12 update_with#158:6-17 value#175:6-11 value_exn#176:6-15 voting_power#19:6-18 xor#79:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#205:4-7 -> abs)
    Range: File "", line 205, characters 4-7
    Body Range: File "", line 205, characters 9-10
    Content: |core: int -> nat|
    references: File "", line 369, characters 31-34
    (assert#202:4-10 -> assert)
    Range: File "", line 202, characters 4-10
    Body Range: File "", line 202, characters 12-13
    Content: |core: bool -> unit|
    references: []
    (assert_none#204:4-15 -> assert_none)
    Range: File "", line 204, characters 4-15
    Body Range: File "", line 204, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#217:4-26 -> assert_none_with_error)
    Range: File "", line 217, characters 4-26
    Body Range: File "", line 217, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#203:4-15 -> assert_some)
    Range: File "", line 203, characters 4-15
    Body Range: File "", line 203, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#216:4-26 -> assert_some_with_error)
    Range: File "", line 216, characters 4-26
    Body Range: File "", line 216, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#215:4-21 -> assert_with_error)
    Range: File "", line 215, characters 4-21
    Body Range: File "", line 215, characters 23-24
    Content: |unresolved|
    references: []
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#5:10-11 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (curry#212:4-9 -> curry)
    Range: File "", line 212, characters 4-9
    Body Range: File "", line 212, characters 10-22
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    (d#5:26-27 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#6:9-10 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    (ediv#218:4-8 -> ediv)
    Range: File "", line 218, characters 4-8
    Body Range: File "", line 218, characters 9-19
    Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_ediv (a ,
    b)|
    references: []
    (failwith#2:4-12 -> failwith)
    Range: File "", line 2, characters 4-12
    Body Range: File "", line 2, characters 13-23
    Content: |unresolved|
    references:
      File "", line 38, characters 27-35 ,
      File "", line 42, characters 27-35 ,
      File "", line 69, characters 27-35 ,
      File "", line 171, characters 79-87 ,
      File "", line 173, characters 103-111 ,
      File "", line 176, characters 83-91 ,
      File "", line 202, characters 49-57 ,
      File "", line 203, characters 72-80 ,
      File "", line 204, characters 87-95 ,
      File "", line 215, characters 66-74 ,
      File "", line 216, characters 96-104 ,
      File "", line 217, characters 111-119
    (false#208:4-9 -> false)
    Range: File "", line 208, characters 4-9
    Body Range: File "", line 208, characters 19-24
    Content: |core: bool|
    references:
      File "", line 264, characters 51-56 ,
      File "", line 309, characters 90-95 ,
      File "", line 312, characters 62-67
    (ignore#211:4-10 -> ignore)
    Range: File "", line 211, characters 4-10
    Body Range: File "", line 211, characters 11-19
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#210:4-7 -> int)
    Range: File "", line 210, characters 4-7
    Body Range: File "", line 210, characters 8-16
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 260, characters 97-100 ,
      File "", line 297, characters 79-82 ,
      File "", line 299, characters 78-81 ,
      File "", line 301, characters 72-75
    (is_nat#206:4-10 -> is_nat)
    Range: File "", line 206, characters 4-10
    Body Range: File "", line 206, characters 12-13
    Content: |core: int -> option (nat)|
    references: []
    (true#207:4-8 -> true)
    Range: File "", line 207, characters 4-8
    Body Range: File "", line 207, characters 18-22
    Content: |core: bool|
    references:
      File "", line 308, characters 88-92 ,
      File "", line 313, characters 68-72
    (uncurry#213:4-11 -> uncurry)
    Range: File "", line 213, characters 4-11
    Body Range: File "", line 213, characters 12-24
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 375, characters 30-37
    (unit#209:4-8 -> unit)
    Range: File "", line 209, characters 4-8
    Body Range: File "", line 209, characters 18-38
    Content: |core: unit|
    references: []
    Type definitions:
    (bool#4:5-9 -> bool)
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
      File "", line 152, characters 34-38 ,
      File "", line 158, characters 37-41 ,
      File "", line 177, characters 40-44 ,
      File "", line 178, characters 40-44 ,
      File "", line 199, characters 52-56 ,
      File "", line 199, characters 145-149 ,
      File "", line 202, characters 16-20 ,
      File "", line 207, characters 11-15 ,
      File "", line 208, characters 12-16 ,
      File "", line 215, characters 27-31 ,
      File "", line 236, characters 41-45 ,
      File "", line 314, characters 53-57 ,
      File "", line 364, characters 74-78 ,
      File "", line 471, characters 18-22 ,
      File "", line 475, characters 29-33
    (option#5:8-14 -> option)
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
      File "", line 136, characters 38-46 ,
      File "", line 143, characters 40-48 ,
      File "", line 144, characters 40-55 ,
      File "", line 152, characters 56-64 ,
      File "", line 153, characters 29-37 ,
      File "", line 154, characters 38-46 ,
      File "", line 156, characters 32-40 ,
      File "", line 171, characters 26-34 ,
      File "", line 173, characters 37-45 ,
      File "", line 174, characters 48-56 ,
      File "", line 174, characters 60-68 ,
      File "", line 175, characters 40-48 ,
      File "", line 176, characters 46-54 ,
      File "", line 177, characters 28-36 ,
      File "", line 178, characters 28-36 ,
      File "", line 184, characters 36-44 ,
      File "", line 184, characters 99-107 ,
      File "", line 203, characters 30-38 ,
      File "", line 204, characters 30-38 ,
      File "", line 206, characters 23-33 ,
      File "", line 206, characters 74-84 ,
      File "", line 216, characters 41-49 ,
      File "", line 217, characters 41-49 ,
      File "", line 294, characters 22-35 ,
      File "", line 337, characters 140-153 ,
      File "", line 338, characters 135-148 ,
      File "", line 343, characters 92-108 ,
      File "", line 346, characters 48-69 ,
      File "", line 347, characters 50-63 ,
      File "", line 350, characters 44-54 ,
      File "", line 356, characters 12-25 ,
      File "", line 361, characters 14-27 ,
      File "", line 399, characters 96-106 ,
      File "", line 406, characters 59-80 ,
      File "", line 409, characters 37-58 ,
      File "", line 431, characters 90-111 ,
      File "", line 437, characters 96-106 ,
      File "", line 440, characters 37-58 ,
      File "", line 457, characters 96-106 ,
      File "", line 472, characters 32-40 ,
      File "", line 473, characters 32-40 ,
      File "", line 476, characters 43-51 ,
      File "", line 477, characters 43-51
    (pbt_result#237:8-18 -> pbt_result)
    Range: File "", line 237, characters 8-18
    Body Range: File "", line 237, characters 0-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 315, characters 55-67 ,
      File "", line 316, characters 37-49 ,
      File "", line 318, characters 82-94 ,
      File "", line 322, characters 94-106 ,
      File "", line 325, characters 66-78
    (pbt_test#236:8-16 -> pbt_test)
    Range: File "", line 236, characters 8-16
    Body Range: File "", line 236, characters 0-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 314, characters 61-71 ,
      File "", line 315, characters 31-41
    (test_baker_policy#231:5-22 -> test_baker_policy)
    Range: File "", line 231, characters 5-22
    Body Range: File "", line 232, character 4 to line 234, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 286, characters 29-46
    (test_exec_error#224:5-20 -> test_exec_error)
    Range: File "", line 224, characters 5-20
    Body Range: File "", line 225, character 4 to line 227, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 229, characters 49-64
    (test_exec_error_balance_too_low#221:5-36 -> test_exec_error_balance_too_low)
    Range: File "", line 221, characters 5-36
    Body Range: File "", line 222, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 226, characters 23-54
    (test_exec_result#229:5-21 -> test_exec_result)
    Range: File "", line 229, characters 5-21
    Body Range: File "", line 229, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 337, characters 65-81 ,
      File "", line 354, characters 73-89
    (unforged_ticket#239:8-23 -> unforged_ticket)
    Range: File "", line 239, characters 8-23
    Body Range: File "", line 239, characters 0-91
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    Module definitions:
    (Big_map#85:7-14 -> Big_map)
    Range: File "", line 85, characters 7-14
    Body Range: File "", line 85, character 0 to line 97, character 3
    Content: Members: Variable definitions:
                      (add#90:6-9 -> add)
                      Range: File "", line 90, characters 6-9
                      Body Range: File "", line 90, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (empty#86:16-21 -> empty)
                      Range: File "", line 86, characters 16-21
                      Body Range: File "", line 86, characters 22-32
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#95:6-10 -> find)
                      Range: File "", line 95, characters 6-10
                      Body Range: File "", line 95, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      (find_opt#94:6-14 -> find_opt)
                      Range: File "", line 94, characters 6-14
                      Body Range: File "", line 94, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (get_and_update#93:6-20 -> get_and_update)
                      Range: File "", line 93, characters 6-20
                      Body Range: File "", line 93, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#87:25-32 -> literal)
                      Range: File "", line 87, characters 25-32
                      Body Range: File "", line 87, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#89:6-9 -> mem)
                      Range: File "", line 89, characters 6-9
                      Body Range: File "", line 89, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (remove#91:6-12 -> remove)
                      Range: File "", line 91, characters 6-12
                      Body Range: File "", line 91, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#92:6-12 -> update)
                      Range: File "", line 92, characters 6-12
                      Body Range: File "", line 92, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#77:7-14 -> Bitwise)
    Range: File "", line 77, characters 7-14
    Body Range: File "", line 77, character 0 to line 83, character 3
    Content: Members: Variable definitions:
                      (and#78:6-10 -> and)
                      Range: File "", line 78, characters 6-10
                      Body Range: File "", line 78, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (or#80:6-9 -> or)
                      Range: File "", line 80, characters 6-9
                      Body Range: File "", line 80, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_left#81:6-16 -> shift_left)
                      Range: File "", line 81, characters 6-16
                      Body Range: File "", line 81, characters 18-19
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_right#82:6-17 -> shift_right)
                      Range: File "", line 82, characters 6-17
                      Body Range: File "", line 82, characters 19-20
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (xor#79:6-9 -> xor)
                      Range: File "", line 79, characters 6-9
                      Body Range: File "", line 79, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#181:7-12 -> Bytes)
    Range: File "", line 181, characters 7-12
    Body Range: File "", line 181, character 0 to line 189, character 3
    Content: Members: Variable definitions:
                      (concat#187:6-12 -> concat)
                      Range: File "", line 187, characters 6-12
                      Body Range: File "", line 187, characters 14-16
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#182:6-13 -> concats)
                      Range: File "", line 182, characters 6-13
                      Body Range: File "", line 182, characters 15-17
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#185:6-12 -> length)
                      Range: File "", line 185, characters 6-12
                      Body Range: File "", line 185, characters 14-15
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#183:6-10 -> pack)
                      Range: File "", line 183, characters 6-10
                      Body Range: File "", line 183, characters 11-19
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#188:6-9 -> sub)
                      Range: File "", line 188, characters 6-9
                      Body Range: File "", line 188, characters 11-12
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#184:6-12 -> unpack)
                      Range: File "", line 184, characters 6-12
                      Body Range: File "", line 184, characters 13-21
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#191:7-13 -> Crypto)
    Range: File "", line 191, characters 7-13
    Body Range: File "", line 191, character 0 to line 200, character 3
    Content: Members: Variable definitions:
                      (blake2b#192:6-13 -> blake2b)
                      Range: File "", line 192, characters 6-13
                      Body Range: File "", line 192, characters 15-16
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#199:6-11 -> check)
                      Range: File "", line 199, characters 6-11
                      Body Range: File "", line 199, characters 13-14
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#197:6-14 -> hash_key)
                      Range: File "", line 197, characters 6-14
                      Body Range: File "", line 197, characters 16-17
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#196:6-12 -> keccak)
                      Range: File "", line 196, characters 6-12
                      Body Range: File "", line 196, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#193:6-12 -> sha256)
                      Range: File "", line 193, characters 6-12
                      Body Range: File "", line 193, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#195:6-10 -> sha3)
                      Range: File "", line 195, characters 6-10
                      Body Range: File "", line 195, characters 12-13
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#194:6-12 -> sha512)
                      Range: File "", line 194, characters 6-12
                      Body Range: File "", line 194, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#140:7-11 -> List)
    Range: File "", line 140, characters 7-11
    Body Range: File "", line 140, character 0 to line 160, character 3
    Content: Members: Variable definitions:
                      (cons#151:6-10 -> cons)
                      Range: File "", line 151, characters 6-10
                      Body Range: File "", line 151, characters 11-19
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (filter_map#154:6-16 -> filter_map)
                      Range: File "", line 154, characters 6-16
                      Body Range: File "", line 154, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      (find_opt#152:6-14 -> find_opt)
                      Range: File "", line 152, characters 6-14
                      Body Range: File "", line 152, characters 15-23
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#148:6-10 -> fold)
                      Range: File "", line 148, characters 6-10
                      Body Range: File "", line 148, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 336, characters 9-13
                      (fold_left#149:6-15 -> fold_left)
                      Range: File "", line 149, characters 6-15
                      Body Range: File "", line 149, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#150:6-16 -> fold_right)
                      Range: File "", line 150, characters 6-16
                      Body Range: File "", line 150, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 153, characters 4-14 ,
                        File "", line 155, characters 4-14
                      (head_opt#143:6-14 -> head_opt)
                      Range: File "", line 143, characters 6-14
                      Body Range: File "", line 143, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#147:6-10 -> iter)
                      Range: File "", line 147, characters 6-10
                      Body Range: File "", line 147, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#141:6-12 -> length)
                      Range: File "", line 141, characters 6-12
                      Body Range: File "", line 141, characters 13-21
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#146:6-9 -> map)
                      Range: File "", line 146, characters 6-9
                      Body Range: File "", line 146, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 157, characters 4-7 ,
                        File "", line 159, characters 4-7
                      (size#142:6-10 -> size)
                      Range: File "", line 142, characters 6-10
                      Body Range: File "", line 142, characters 11-19
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#144:6-14 -> tail_opt)
                      Range: File "", line 144, characters 6-14
                      Body Range: File "", line 144, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      (update#156:6-12 -> update)
                      Range: File "", line 156, characters 6-12
                      Body Range: File "", line 156, characters 13-21
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      (update_with#158:6-17 -> update_with)
                      Range: File "", line 158, characters 6-17
                      Body Range: File "", line 158, characters 18-26
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 336, characters 4-8

    (Map#99:7-10 -> Map)
    Range: File "", line 99, characters 7-10
    Body Range: File "", line 99, character 0 to line 115, character 3
    Content: Members: Variable definitions:
                      (add#105:6-9 -> add)
                      Range: File "", line 105, characters 6-9
                      Body Range: File "", line 105, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (empty#100:6-11 -> empty)
                      Range: File "", line 100, characters 6-11
                      Body Range: File "", line 100, characters 12-22
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#109:6-10 -> find)
                      Range: File "", line 109, characters 6-10
                      Body Range: File "", line 109, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#110:6-14 -> find_opt)
                      Range: File "", line 110, characters 6-14
                      Body Range: File "", line 110, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (fold#113:6-10 -> fold)
                      Range: File "", line 113, characters 6-10
                      Body Range: File "", line 113, characters 11-23
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      (get_and_update#108:6-20 -> get_and_update)
                      Range: File "", line 108, characters 6-20
                      Body Range: File "", line 108, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (iter#111:6-10 -> iter)
                      Range: File "", line 111, characters 6-10
                      Body Range: File "", line 111, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (literal#102:25-32 -> literal)
                      Range: File "", line 102, characters 25-32
                      Body Range: File "", line 102, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#112:6-9 -> map)
                      Range: File "", line 112, characters 6-9
                      Body Range: File "", line 112, characters 10-22
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (mem#104:6-9 -> mem)
                      Range: File "", line 104, characters 6-9
                      Body Range: File "", line 104, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (remove#106:6-12 -> remove)
                      Range: File "", line 106, characters 6-12
                      Body Range: File "", line 106, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (size#101:6-10 -> size)
                      Range: File "", line 101, characters 6-10
                      Body Range: File "", line 101, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#107:6-12 -> update)
                      Range: File "", line 107, characters 6-12
                      Body Range: File "", line 107, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#170:7-13 -> Option)
    Range: File "", line 170, characters 7-13
    Body Range: File "", line 170, character 0 to line 179, character 3
    Content: Members: Variable definitions:
                      (is_none#177:6-13 -> is_none)
                      Range: File "", line 177, characters 6-13
                      Body Range: File "", line 177, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#178:6-13 -> is_some)
                      Range: File "", line 178, characters 6-13
                      Body Range: File "", line 178, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#174:15-18 -> map)
                      Range: File "", line 174, characters 15-18
                      Body Range: File "", line 174, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#171:6-11 -> unopt)
                      Range: File "", line 171, characters 6-11
                      Body Range: File "", line 171, characters 12-20
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#173:6-22 -> unopt_with_error)
                      Range: File "", line 173, characters 6-22
                      Body Range: File "", line 173, characters 23-31
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#175:6-11 -> value)
                      Range: File "", line 175, characters 6-11
                      Body Range: File "", line 175, characters 12-20
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#176:6-15 -> value_exn)
                      Range: File "", line 176, characters 6-15
                      Body Range: File "", line 176, characters 16-28
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#123:7-10 -> Set)
    Range: File "", line 123, characters 7-10
    Body Range: File "", line 123, character 0 to line 138, character 3
    Content: Members: Variable definitions:
                      (add#130:6-9 -> add)
                      Range: File "", line 130, characters 6-9
                      Body Range: File "", line 130, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: File "", line 137, characters 81-84
                      (cardinal#126:6-14 -> cardinal)
                      Range: File "", line 126, characters 6-14
                      Body Range: File "", line 126, characters 15-23
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#124:6-11 -> empty)
                      Range: File "", line 124, characters 6-11
                      Body Range: File "", line 124, characters 12-20
                      Content: |core: ∀ a : * . set (a)|
                      references: File "", line 137, characters 96-101
                      (filter_map#136:6-16 -> filter_map)
                      Range: File "", line 136, characters 6-16
                      Body Range: File "", line 136, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> set (a) -> set (b)|
                      references: []
                      (fold#134:6-10 -> fold)
                      Range: File "", line 134, characters 6-10
                      Body Range: File "", line 134, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#135:6-15 -> fold_desc)
                      Range: File "", line 135, characters 6-15
                      Body Range: File "", line 135, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: File "", line 137, characters 4-13
                      (iter#133:6-10 -> iter)
                      Range: File "", line 133, characters 6-10
                      Body Range: File "", line 133, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#127:25-32 -> literal)
                      Range: File "", line 127, characters 25-32
                      Body Range: File "", line 127, characters 33-41
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#129:6-9 -> mem)
                      Range: File "", line 129, characters 6-9
                      Body Range: File "", line 129, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#131:6-12 -> remove)
                      Range: File "", line 131, characters 6-12
                      Body Range: File "", line 131, characters 13-21
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#125:6-10 -> size)
                      Range: File "", line 125, characters 6-10
                      Body Range: File "", line 125, characters 11-19
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#132:6-12 -> update)
                      Range: File "", line 132, characters 6-12
                      Body Range: File "", line 132, characters 13-21
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#162:7-13 -> String)
    Range: File "", line 162, characters 7-13
    Body Range: File "", line 162, character 0 to line 168, character 3
    Content: Members: Variable definitions:
                      (concat#166:6-12 -> concat)
                      Range: File "", line 166, characters 6-12
                      Body Range: File "", line 166, characters 14-16
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#164:6-13 -> concats)
                      Range: File "", line 164, characters 6-13
                      Body Range: File "", line 164, characters 15-17
                      Content: |core: list (string) -> string|
                      references: []
                      (length#163:6-12 -> length)
                      Range: File "", line 163, characters 6-12
                      Body Range: File "", line 163, characters 14-15
                      Content: |core: string -> nat|
                      references:
                        File "", line 366, characters 22-28 ,
                        File "", line 369, characters 43-49
                      (sub#167:6-9 -> sub)
                      Range: File "", line 167, characters 6-9
                      Body Range: File "", line 167, characters 11-12
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 367, characters 24-27 ,
                        File "", line 369, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 366, characters 15-21 ,
      File "", line 367, characters 17-23 ,
      File "", line 369, characters 16-22 ,
      File "", line 369, characters 36-42

    (Test#241:7-11 -> Test)
    Range: File "", line 241, characters 7-11
    Body Range: File "", line 241, character 0 to line 479, character 3
    Content: Members: Variable definitions:
                      (add_account#349:6-17 -> add_account)
                      Range: File "", line 349, characters 6-17
                      Body Range: File "", line 349, characters 19-20
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#471:6-12 -> assert)
                      Range: File "", line 471, characters 6-12
                      Body Range: File "", line 471, characters 14-15
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#473:6-17 -> assert_none)
                      Range: File "", line 473, characters 6-17
                      Body Range: File "", line 473, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#477:6-28 -> assert_none_with_error)
                      Range: File "", line 477, characters 6-28
                      Body Range: File "", line 477, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#472:6-17 -> assert_some)
                      Range: File "", line 472, characters 6-17
                      Body Range: File "", line 472, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#476:6-28 -> assert_some_with_error)
                      Range: File "", line 476, characters 6-28
                      Body Range: File "", line 476, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#475:6-23 -> assert_with_error)
                      Range: File "", line 475, characters 6-23
                      Body Range: File "", line 475, characters 25-26
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#268:6-28 -> bake_until_n_cycle_end)
                      Range: File "", line 268, characters 6-28
                      Body Range: File "", line 268, characters 30-31
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#350:6-19 -> baker_account)
                      Range: File "", line 350, characters 6-19
                      Body Range: File "", line 350, characters 21-22
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#345:6-24 -> bootstrap_contract)
                      Range: File "", line 345, characters 6-24
                      Body Range: File "", line 345, characters 25-35
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#270:6-18 -> cast_address)
                      Range: File "", line 270, characters 6-18
                      Body Range: File "", line 270, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 379, characters 35-47 ,
                        File "", line 389, characters 35-47 ,
                        File "", line 396, characters 35-47
                      (chr#294:6-9 -> chr)
                      Range: File "", line 294, characters 6-9
                      Body Range: File "", line 294, characters 11-12
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#289:6-22 -> compile_contract)
                      Range: File "", line 289, characters 6-22
                      Body Range: File "", line 289, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 375, characters 12-28 ,
                        File "", line 385, characters 12-28
                      (compile_contract_from_file#398:6-32 -> compile_contract_from_file)
                      Range: File "", line 398, characters 6-32
                      Body Range: File "", line 398, characters 34-36
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 402, characters 12-38
                      (compile_contract_with_views#381:8-35 -> compile_contract_with_views)
                      Range: File "", line 381, characters 8-35
                      Body Range: File "", line 381, characters 36-46
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 392, characters 12-39
                      (compile_value#246:6-19 -> compile_value)
                      Range: File "", line 246, characters 6-19
                      Body Range: File "", line 246, characters 20-28
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#274:6-35 -> constant_to_michelson_program)
                      Range: File "", line 274, characters 6-35
                      Body Range: File "", line 274, characters 37-38
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#352:6-18 -> create_chest)
                      Range: File "", line 352, characters 6-18
                      Body Range: File "", line 352, characters 20-21
                      Content: |core: bytes -> nat -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#353:6-22 -> create_chest_key)
                      Range: File "", line 353, characters 6-22
                      Body Range: File "", line 353, characters 24-25
                      Content: |core: chest -> nat -> chest_key|
                      references: []
                      (decompile#267:6-15 -> decompile)
                      Range: File "", line 267, characters 6-15
                      Body Range: File "", line 267, characters 16-24
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 285, characters 5-14
                      (drop_context#278:6-18 -> drop_context)
                      Range: File "", line 278, characters 6-18
                      Body Range: File "", line 278, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#254:6-12 -> eprint)
                      Range: File "", line 254, characters 6-12
                      Body Range: File "", line 254, characters 14-15
                      Content: |core: string -> unit|
                      references: []
                      (eval#244:6-10 -> eval)
                      Range: File "", line 244, characters 6-10
                      Body Range: File "", line 244, characters 11-19
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 246, characters 59-63 ,
                        File "", line 357, characters 32-36 ,
                        File "", line 362, characters 34-38 ,
                        File "", line 376, characters 12-16 ,
                        File "", line 386, characters 12-16 ,
                        File "", line 393, characters 12-16
                      (failwith#248:6-14 -> failwith)
                      Range: File "", line 248, characters 6-14
                      Body Range: File "", line 248, characters 15-25
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 471, characters 51-59 ,
                        File "", line 472, characters 74-82 ,
                        File "", line 473, characters 89-97 ,
                        File "", line 475, characters 68-76 ,
                        File "", line 476, characters 98-106 ,
                        File "", line 477, characters 113-121
                      (get_balance#252:6-17 -> get_balance)
                      Range: File "", line 252, characters 6-17
                      Body Range: File "", line 252, characters 19-20
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#260:6-27 -> get_bootstrap_account)
                      Range: File "", line 260, characters 6-27
                      Body Range: File "", line 260, characters 29-30
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#329:6-26 -> get_last_events_from)
                      Range: File "", line 329, characters 6-26
                      Body Range: File "", line 329, characters 27-39
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#281:6-17 -> get_storage)
                      Range: File "", line 281, characters 6-17
                      Body Range: File "", line 281, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#251:6-28 -> get_storage_of_address)
                      Range: File "", line 251, characters 6-28
                      Body Range: File "", line 251, characters 30-31
                      Content: |core: address -> michelson_program|
                      references: File "", line 284, characters 32-54
                      (get_time#269:6-14 -> get_time)
                      Range: File "", line 269, characters 6-14
                      Body Range: File "", line 269, characters 16-18
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#247:6-28 -> get_total_voting_power)
                      Range: File "", line 247, characters 6-28
                      Body Range: File "", line 247, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#255:6-22 -> get_voting_power)
                      Range: File "", line 255, characters 6-22
                      Body Range: File "", line 255, characters 24-26
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#262:6-23 -> last_originations)
                      Range: File "", line 262, characters 6-23
                      Body Range: File "", line 262, characters 25-26
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#339:6-9 -> log)
                      Range: File "", line 339, characters 6-9
                      Body Range: File "", line 339, characters 10-18
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 368, characters 25-28
                      (michelson_equal#364:6-21 -> michelson_equal)
                      Range: File "", line 364, characters 6-21
                      Body Range: File "", line 364, characters 23-25
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#346:6-18 -> mutate_value)
                      Range: File "", line 346, characters 6-18
                      Body Range: File "", line 346, characters 19-27
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 410, characters 23-35 ,
                        File "", line 422, characters 23-35
                      (mutation_test#406:6-19 -> mutation_test)
                      Range: File "", line 406, characters 6-19
                      Body Range: File "", line 406, characters 20-30
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#418:6-23 -> mutation_test_all)
                      Range: File "", line 418, characters 6-23
                      Body Range: File "", line 418, characters 24-34
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (new_account#266:6-17 -> new_account)
                      Range: File "", line 266, characters 6-17
                      Body Range: File "", line 266, characters 19-20
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#304:6-8 -> nl)
                      Range: File "", line 304, characters 6-8
                      Body Range: File "", line 304, characters 11-53
                      Content: |unresolved|
                      references: File "", line 306, characters 15-17
                      (nth_bootstrap_account#257:6-27 -> nth_bootstrap_account)
                      Range: File "", line 257, characters 6-27
                      Body Range: File "", line 257, characters 29-30
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#256:6-28 -> nth_bootstrap_contract)
                      Range: File "", line 256, characters 6-28
                      Body Range: File "", line 256, characters 30-31
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#261:6-33 -> nth_bootstrap_typed_address)
                      Range: File "", line 261, characters 6-33
                      Body Range: File "", line 261, characters 34-44
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#374:6-15 -> originate)
                      Range: File "", line 374, characters 6-15
                      Body Range: File "", line 374, characters 16-26
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_contract#373:6-24 -> originate_contract)
                      Range: File "", line 373, characters 6-24
                      Body Range: File "", line 373, characters 26-27
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 377, characters 12-30 ,
                        File "", line 387, characters 12-30 ,
                        File "", line 394, characters 12-30 ,
                        File "", line 403, characters 12-30 ,
                        File "", line 434, characters 14-32 ,
                        File "", line 454, characters 14-32
                      (originate_from_file#401:6-25 -> originate_from_file)
                      Range: File "", line 401, characters 6-25
                      Body Range: File "", line 401, characters 27-29
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_from_file_and_mutate#430:6-36 -> originate_from_file_and_mutate)
                      Range: File "", line 430, characters 6-36
                      Body Range: File "", line 430, characters 37-45
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#450:6-40 -> originate_from_file_and_mutate_all)
                      Range: File "", line 450, characters 6-40
                      Body Range: File "", line 450, characters 41-49
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module#391:6-22 -> originate_module)
                      Range: File "", line 391, characters 6-22
                      Body Range: File "", line 391, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) ->
                                                                ( list (operation) *
                                                                  s ) *
                                                                views (s) ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_uncurried#384:6-25 -> originate_uncurried)
                      Range: File "", line 384, characters 6-25
                      Body Range: File "", line 384, characters 26-36
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (parse_michelson#275:6-21 -> parse_michelson)
                      Range: File "", line 275, characters 6-21
                      Body Range: File "", line 275, characters 23-24
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#253:6-11 -> print)
                      Range: File "", line 253, characters 6-11
                      Body Range: File "", line 253, characters 13-14
                      Content: |core: string -> unit|
                      references:
                        File "", line 306, characters 4-9 ,
                        File "", line 342, characters 4-9
                      (println#305:6-13 -> println)
                      Range: File "", line 305, characters 6-13
                      Body Range: File "", line 305, characters 15-16
                      Content: |core: string -> unit|
                      references: []
                      (random#263:6-12 -> random)
                      Range: File "", line 263, characters 6-12
                      Body Range: File "", line 263, characters 13-21
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#293:6-29 -> read_contract_from_file)
                      Range: File "", line 293, characters 6-29
                      Body Range: File "", line 293, characters 31-33
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#272:6-23 -> register_constant)
                      Range: File "", line 272, characters 6-23
                      Body Range: File "", line 272, characters 25-26
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#271:6-23 -> register_delegate)
                      Range: File "", line 271, characters 6-23
                      Body Range: File "", line 271, characters 25-27
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#343:6-17 -> reset_state)
                      Range: File "", line 343, characters 6-17
                      Body Range: File "", line 343, characters 19-20
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#344:6-20 -> reset_state_at)
                      Range: File "", line 344, characters 6-20
                      Body Range: File "", line 344, characters 22-23
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#276:6-21 -> restore_context)
                      Range: File "", line 276, characters 6-21
                      Body Range: File "", line 276, characters 23-24
                      Content: |core: unit -> unit|
                      references: []
                      (run#243:6-9 -> run)
                      Range: File "", line 243, characters 6-9
                      Body Range: File "", line 243, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 244, characters 50-53
                      (save_context#277:6-18 -> save_context)
                      Range: File "", line 277, characters 6-18
                      Body Range: File "", line 277, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#347:6-19 -> save_mutation)
                      Range: File "", line 347, characters 6-19
                      Body Range: File "", line 347, characters 21-22
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#287:6-15 -> set_baker)
                      Range: File "", line 287, characters 6-15
                      Body Range: File "", line 287, characters 17-18
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#286:6-22 -> set_baker_policy)
                      Range: File "", line 286, characters 6-22
                      Body Range: File "", line 286, characters 24-26
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 287, characters 39-55
                      (set_big_map#351:6-17 -> set_big_map)
                      Range: File "", line 351, characters 6-17
                      Body Range: File "", line 351, characters 18-28
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#308:6-22 -> set_print_values)
                      Range: File "", line 308, characters 6-22
                      Body Range: File "", line 308, characters 24-25
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#250:6-16 -> set_source)
                      Range: File "", line 250, characters 6-16
                      Body Range: File "", line 250, characters 18-19
                      Content: |core: address -> unit|
                      references: []
                      (sign#348:6-10 -> sign)
                      Range: File "", line 348, characters 6-10
                      Body Range: File "", line 348, characters 12-14
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#288:6-10 -> size)
                      Range: File "", line 288, characters 6-10
                      Body Range: File "", line 288, characters 12-13
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 378, characters 12-16 ,
                        File "", line 388, characters 12-16 ,
                        File "", line 395, characters 12-16 ,
                        File "", line 404, characters 12-16 ,
                        File "", line 435, characters 14-18 ,
                        File "", line 455, characters 14-18
                      (to_contract#249:6-17 -> to_contract)
                      Range: File "", line 249, characters 6-17
                      Body Range: File "", line 249, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 282, characters 25-36 ,
                        File "", line 330, characters 30-41
                      (to_entrypoint#365:6-19 -> to_entrypoint)
                      Range: File "", line 365, characters 6-19
                      Body Range: File "", line 365, characters 20-32
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#280:6-13 -> to_json)
                      Range: File "", line 280, characters 6-13
                      Body Range: File "", line 280, characters 14-22
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#279:6-15 -> to_string)
                      Range: File "", line 279, characters 6-15
                      Body Range: File "", line 279, characters 16-24
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 297, characters 68-77 ,
                        File "", line 299, characters 67-76 ,
                        File "", line 301, characters 61-70 ,
                        File "", line 341, characters 12-21
                      (to_typed_address#273:6-22 -> to_typed_address)
                      Range: File "", line 273, characters 6-22
                      Body Range: File "", line 273, characters 23-33
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#337:6-14 -> transfer)
                      Range: File "", line 337, characters 6-14
                      Body Range: File "", line 337, characters 16-17
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#338:6-18 -> transfer_exn)
                      Range: File "", line 338, characters 6-18
                      Body Range: File "", line 338, characters 20-21
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#354:6-26 -> transfer_to_contract)
                      Range: File "", line 354, characters 6-26
                      Body Range: File "", line 354, characters 27-35
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#359:6-30 -> transfer_to_contract_exn)
                      Range: File "", line 359, characters 6-30
                      Body Range: File "", line 359, characters 31-39
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#309:6-24 -> unset_print_values)
                      Range: File "", line 309, characters 6-24
                      Body Range: File "", line 309, characters 26-27
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#311:9-12 -> PBT)
                      Range: File "", line 311, characters 9-12
                      Body Range: File "", line 311, character 2 to line 327, character 5
                      Content: Members: Variable definitions:
                                        (gen#312:8-11 -> gen)
                                        Range: File "", line 312, characters 8-11
                                        Body Range: File "", line 312, characters 12-20
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (gen_small#313:8-17 -> gen_small)
                                        Range: File "", line 313, characters 8-17
                                        Body Range: File "", line 313, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#314:8-17 -> make_test)
                                        Range: File "", line 314, characters 8-17
                                        Body Range: File "", line 314, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#315:8-11 -> run)
                                        Range: File "", line 315, characters 8-11
                                        Body Range: File "", line 315, characters 12-20
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#7:7-12 -> Tezos)
    Range: File "", line 7, characters 7-12
    Body Range: File "", line 7, character 0 to line 75, character 3
    Content: Members: Variable definitions:
                      (address#20:6-13 -> address)
                      Range: File "", line 20, characters 6-13
                      Body Range: File "", line 20, characters 14-22
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 330, characters 21-28
                      (call_view#55:25-34 -> call_view)
                      Range: File "", line 55, characters 25-34
                      Body Range: File "", line 55, characters 35-45
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (constant#31:25-33 -> constant)
                      Range: File "", line 31, characters 25-33
                      Body Range: File "", line 31, characters 34-42
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (create_contract#59:25-40 -> create_contract)
                      Range: File "", line 59, characters 25-40
                      Body Range: File "", line 59, characters 41-51
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: []
                      (create_contract_uncurried#62:25-50 -> create_contract_uncurried)
                      Range: File "", line 62, characters 25-50
                      Body Range: File "", line 62, characters 51-61
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      (create_ticket#47:6-19 -> create_ticket)
                      Range: File "", line 47, characters 6-19
                      Body Range: File "", line 47, characters 20-28
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references: []
                      (emit#70:25-29 -> emit)
                      Range: File "", line 70, characters 25-29
                      Body Range: File "", line 70, characters 30-38
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      (get_amount#10:6-16 -> get_amount)
                      Range: File "", line 10, characters 6-16
                      Body Range: File "", line 10, characters 18-20
                      Content: |core: unit -> tez|
                      references: []
                      (get_balance#9:6-17 -> get_balance)
                      Range: File "", line 9, characters 6-17
                      Body Range: File "", line 9, characters 19-21
                      Content: |core: unit -> tez|
                      references: []
                      (get_chain_id#16:6-18 -> get_chain_id)
                      Range: File "", line 16, characters 6-18
                      Body Range: File "", line 16, characters 20-22
                      Content: |core: unit -> chain_id|
                      references: []
                      (get_contract#36:25-37 -> get_contract)
                      Range: File "", line 36, characters 25-37
                      Body Range: File "", line 36, characters 38-46
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      (get_contract_opt#34:25-41 -> get_contract_opt)
                      Range: File "", line 34, characters 25-41
                      Body Range: File "", line 34, characters 42-50
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 37, characters 12-28 ,
                        File "", line 41, characters 12-28
                      (get_contract_with_error#40:6-29 -> get_contract_with_error)
                      Range: File "", line 40, characters 6-29
                      Body Range: File "", line 40, characters 30-38
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: []
                      (get_entrypoint#67:25-39 -> get_entrypoint)
                      Range: File "", line 67, characters 25-39
                      Body Range: File "", line 67, characters 40-48
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (get_entrypoint_opt#64:25-43 -> get_entrypoint_opt)
                      Range: File "", line 64, characters 25-43
                      Body Range: File "", line 64, characters 44-52
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 68, characters 12-30
                      (get_level#14:6-15 -> get_level)
                      Range: File "", line 14, characters 6-15
                      Body Range: File "", line 14, characters 17-19
                      Content: |core: unit -> nat|
                      references: []
                      (get_min_block_time#18:6-24 -> get_min_block_time)
                      Range: File "", line 18, characters 6-24
                      Body Range: File "", line 18, characters 26-28
                      Content: |core: unit -> nat|
                      references: []
                      (get_now#11:6-13 -> get_now)
                      Range: File "", line 11, characters 6-13
                      Body Range: File "", line 11, characters 15-17
                      Content: |core: unit -> timestamp|
                      references: File "", line 269, characters 47-54
                      (get_self_address#15:6-22 -> get_self_address)
                      Range: File "", line 15, characters 6-22
                      Body Range: File "", line 15, characters 24-26
                      Content: |core: unit -> address|
                      references: []
                      (get_sender#12:6-16 -> get_sender)
                      Range: File "", line 12, characters 6-16
                      Body Range: File "", line 12, characters 18-20
                      Content: |core: unit -> address|
                      references: []
                      (get_source#13:6-16 -> get_source)
                      Range: File "", line 13, characters 6-16
                      Body Range: File "", line 13, characters 18-20
                      Content: |core: unit -> address|
                      references: []
                      (get_total_voting_power#17:6-28 -> get_total_voting_power)
                      Range: File "", line 17, characters 6-28
                      Body Range: File "", line 17, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (implicit_account#21:6-22 -> implicit_account)
                      Range: File "", line 21, characters 6-22
                      Body Range: File "", line 21, characters 24-26
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      (join_tickets#22:6-18 -> join_tickets)
                      Range: File "", line 22, characters 6-18
                      Body Range: File "", line 22, characters 19-27
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      (never#25:6-11 -> never)
                      Range: File "", line 25, characters 6-11
                      Body Range: File "", line 25, characters 12-20
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      (pairing_check#26:6-19 -> pairing_check)
                      Range: File "", line 26, characters 6-19
                      Body Range: File "", line 26, characters 21-22
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      (read_ticket#23:6-17 -> read_ticket)
                      Range: File "", line 23, characters 6-17
                      Body Range: File "", line 23, characters 18-26
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address *
                                                                    ( a * nat ) ) *
                                                                  ticket (a) )|
                      references: []
                      (sapling_empty_state#32:25-44 -> sapling_empty_state)
                      Range: File "", line 32, characters 25-44
                      Body Range: File "", line 32, characters 45-57
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      (sapling_verify_update#73:25-46 -> sapling_verify_update)
                      Range: File "", line 73, characters 25-46
                      Body Range: File "", line 73, characters 47-59
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes *
                        ( int * sapling_state (sap_a) ) ))|
                      references: []
                      (self#28:25-29 -> self)
                      Range: File "", line 28, characters 25-29
                      Body Range: File "", line 28, characters 30-38
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      (set_delegate#27:6-18 -> set_delegate)
                      Range: File "", line 27, characters 6-18
                      Body Range: File "", line 27, characters 20-21
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      (split_ticket#57:6-18 -> split_ticket)
                      Range: File "", line 57, characters 6-18
                      Body Range: File "", line 57, characters 19-27
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      (transaction#49:6-17 -> transaction)
                      Range: File "", line 49, characters 6-17
                      Body Range: File "", line 49, characters 18-26
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: []
                      (voting_power#19:6-18 -> voting_power)
                      Range: File "", line 19, characters 6-18
                      Body Range: File "", line 19, characters 20-22
                      Content: |core: key_hash -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 269, characters 41-46 ,
      File "", line 330, characters 15-20

    (Transpiled#117:7-17 -> Transpiled)
    Range: File "", line 117, characters 7-17
    Body Range: File "", line 117, character 0 to line 121, character 3
    Content: Members: Variable definitions:
                      (map_add#119:6-13 -> map_add)
                      Range: File "", line 119, characters 6-13
                      Body Range: File "", line 119, characters 14-26
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      (map_find_opt#118:6-18 -> map_find_opt)
                      Range: File "", line 118, characters 6-18
                      Body Range: File "", line 118, characters 19-29
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      (map_remove#120:6-16 -> map_remove)
                      Range: File "", line 120, characters 6-16
                      Body Range: File "", line 120, characters 17-27
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: [] |}]
