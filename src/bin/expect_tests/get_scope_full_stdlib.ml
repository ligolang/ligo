open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Big_map#86:7-14 Bitwise#78:7-14 Bytes#182:7-12 Crypto#192:7-13 List#141:7-11 Map#100:7-10 Option#171:7-13 PBT#312:9-12 Set#124:7-10 String#163:7-13 Test#242:7-11 Tezos#7:7-12 Transpiled#118:7-17 abs#206:4-7 add#106:6-9 add#131:6-9 add#91:6-9 add_account#350:6-17 address#20:6-13 and#79:6-10 assert#203:4-10 assert#472:6-12 assert_none#205:4-15 assert_none#474:6-17 assert_none_with_error#218:4-26 assert_none_with_error#478:6-28 assert_some#204:4-15 assert_some#473:6-17 assert_some_with_error#217:4-26 assert_some_with_error#477:6-28 assert_with_error#216:4-21 assert_with_error#476:6-23 bake_until_n_cycle_end#269:6-28 baker_account#351:6-19 blake2b#193:6-13 bool#4:5-9 bootstrap_contract#346:6-24 call_view#55:25-34 cardinal#127:6-14 cast_address#271:6-18 check#200:6-11 chr#295:6-9 compile_contract#290:6-22 compile_contract_from_file#399:6-32 compile_contract_with_views#382:8-35 compile_value#247:6-19 concat#167:6-12 concat#188:6-12 concats#165:6-13 concats#183:6-13 cons#152:6-10 constant#31:25-33 constant_to_michelson_program#275:6-35 create_chest#353:6-18 create_chest_key#354:6-22 create_contract#60:25-40 create_contract_uncurried#63:25-50 create_ticket#47:6-19 curry#213:4-9 decompile#268:6-15 drop_context#279:6-18 ediv#219:4-8 emit#71:25-29 empty#101:6-11 empty#125:6-11 empty#87:16-21 eprint#255:6-12 eval#245:6-10 failwith#249:6-14 failwith#2:4-12 false#209:14-19 filter_map#137:6-16 filter_map#155:6-16 find#110:6-10 find#96:6-10 find_opt#111:6-14 find_opt#153:6-14 find_opt#95:6-14 fold#114:6-10 fold#135:6-10 fold#149:6-10 fold_desc#136:6-15 fold_left#150:6-15 fold_right#151:6-16 gen#313:8-11 gen_small#314:8-17 get_amount#10:6-16 get_and_update#109:6-20 get_and_update#94:6-20 get_balance#253:6-17 get_balance#9:6-17 get_bootstrap_account#261:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#68:25-39 get_entrypoint_opt#65:25-43 get_last_events_from#330:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#282:6-17 get_storage_of_address#252:6-28 get_time#270:6-14 get_total_voting_power#17:6-28 get_total_voting_power#248:6-28 get_voting_power#256:6-22 hash_key#198:6-14 head_opt#144:6-14 ignore#212:4-10 implicit_account#21:6-22 int#211:4-7 is_nat#207:4-10 is_none#178:6-13 is_some#179:6-13 iter#112:6-10 iter#134:6-10 iter#148:6-10 join_tickets#22:6-18 keccak#197:6-12 last_originations#263:6-23 length#142:6-12 length#164:6-12 length#186:6-12 literal#103:25-32 literal#128:25-32 literal#88:25-32 log#340:6-9 make_test#315:8-17 map#113:6-9 map#147:6-9 map#175:15-18 map_add#120:6-13 map_find_opt#119:6-18 map_remove#121:6-16 mem#105:6-9 mem#130:6-9 mem#90:6-9 michelson_equal#365:6-21 mutate_value#347:6-18 mutation_test#407:6-19 mutation_test_all#419:6-23 never#25:6-11 new_account#267:6-17 nl#305:6-8 nth_bootstrap_account#258:6-27 nth_bootstrap_contract#257:6-28 nth_bootstrap_typed_address#262:6-33 option#5:8-14 or#81:6-9 originate#375:6-15 originate_contract#374:6-24 originate_from_file#402:6-25 originate_from_file_and_mutate#431:6-36 originate_from_file_and_mutate_all#451:6-40 originate_module#392:6-22 originate_uncurried#385:6-25 pack#184:6-10 pairing_check#26:6-19 parse_michelson#276:6-21 pbt_result#238:8-18 pbt_test#237:8-16 print#254:6-11 println#306:6-13 random#264:6-12 read_contract_from_file#294:6-29 read_ticket#23:6-17 register_constant#273:6-23 register_delegate#272:6-23 remove#107:6-12 remove#132:6-12 remove#92:6-12 reset_state#344:6-17 reset_state_at#345:6-20 restore_context#277:6-21 run#244:6-9 run#316:8-11 sapling_empty_state#32:25-44 sapling_verify_update#74:25-46 save_context#278:6-18 save_mutation#348:6-19 self#28:25-29 set_baker#288:6-15 set_baker_policy#287:6-22 set_big_map#352:6-17 set_delegate#27:6-18 set_print_values#309:6-22 set_source#251:6-16 sha256#194:6-12 sha3#196:6-10 sha512#195:6-12 shift_left#82:6-16 shift_right#83:6-17 sign#349:6-10 size#102:6-10 size#126:6-10 size#143:6-10 size#289:6-10 split_ticket#58:6-18 sub#168:6-9 sub#189:6-9 tail_opt#145:6-14 test_baker_policy#232:5-22 test_exec_error#225:5-20 test_exec_error_balance_too_low#222:5-36 test_exec_result#230:5-21 to_contract#250:6-17 to_entrypoint#366:6-19 to_json#281:6-13 to_string#280:6-15 to_typed_address#274:6-22 transaction#49:6-17 transfer#338:6-14 transfer_exn#339:6-18 transfer_to_contract#355:6-26 transfer_to_contract_exn#360:6-30 true#208:14-18 uncurry#214:4-11 unforged_ticket#240:8-23 unit#210:14-18 unopt#172:6-11 unopt_with_error#174:6-22 unpack#185:6-12 unset_print_values#310:6-24 update#108:6-12 update#133:6-12 update#157:6-12 update#93:6-12 update_with#159:6-17 value#176:6-11 value_exn#177:6-15 voting_power#19:6-18 xor#80:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ Big_map#86:7-14 Bitwise#78:7-14 Bytes#182:7-12 Crypto#192:7-13 List#141:7-11 Map#100:7-10 Option#171:7-13 PBT#312:9-12 Set#124:7-10 String#163:7-13 Test#242:7-11 Tezos#7:7-12 Transpiled#118:7-17 a#1:4-5 abs#206:4-7 add#106:6-9 add#131:6-9 add#91:6-9 add_account#350:6-17 address#20:6-13 and#79:6-10 assert#203:4-10 assert#472:6-12 assert_none#205:4-15 assert_none#474:6-17 assert_none_with_error#218:4-26 assert_none_with_error#478:6-28 assert_some#204:4-15 assert_some#473:6-17 assert_some_with_error#217:4-26 assert_some_with_error#477:6-28 assert_with_error#216:4-21 assert_with_error#476:6-23 bake_until_n_cycle_end#269:6-28 baker_account#351:6-19 blake2b#193:6-13 bool#4:5-9 bootstrap_contract#346:6-24 c#5:10-11 call_view#55:25-34 cardinal#127:6-14 cast_address#271:6-18 check#200:6-11 chr#295:6-9 compile_contract#290:6-22 compile_contract_from_file#399:6-32 compile_contract_with_views#382:8-35 compile_value#247:6-19 concat#167:6-12 concat#188:6-12 concats#165:6-13 concats#183:6-13 cons#152:6-10 constant#31:25-33 constant_to_michelson_program#275:6-35 create_chest#353:6-18 create_chest_key#354:6-22 create_contract#60:25-40 create_contract_uncurried#63:25-50 create_ticket#47:6-19 curry#213:4-9 decompile#268:6-15 drop_context#279:6-18 ediv#219:4-8 emit#71:25-29 empty#101:6-11 empty#125:6-11 empty#87:16-21 eprint#255:6-12 eval#245:6-10 failwith#249:6-14 failwith#2:4-12 false#209:14-19 filter_map#137:6-16 filter_map#155:6-16 find#110:6-10 find#96:6-10 find_opt#111:6-14 find_opt#153:6-14 find_opt#95:6-14 fold#114:6-10 fold#135:6-10 fold#149:6-10 fold_desc#136:6-15 fold_left#150:6-15 fold_right#151:6-16 gen#313:8-11 gen_small#314:8-17 get_amount#10:6-16 get_and_update#109:6-20 get_and_update#94:6-20 get_balance#253:6-17 get_balance#9:6-17 get_bootstrap_account#261:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#68:25-39 get_entrypoint_opt#65:25-43 get_last_events_from#330:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#282:6-17 get_storage_of_address#252:6-28 get_time#270:6-14 get_total_voting_power#17:6-28 get_total_voting_power#248:6-28 get_voting_power#256:6-22 hash_key#198:6-14 head_opt#144:6-14 ignore#212:4-10 implicit_account#21:6-22 int#211:4-7 is_nat#207:4-10 is_none#178:6-13 is_some#179:6-13 iter#112:6-10 iter#134:6-10 iter#148:6-10 join_tickets#22:6-18 keccak#197:6-12 last_originations#263:6-23 length#142:6-12 length#164:6-12 length#186:6-12 literal#103:25-32 literal#128:25-32 literal#88:25-32 log#340:6-9 make_test#315:8-17 map#113:6-9 map#147:6-9 map#175:15-18 map_add#120:6-13 map_find_opt#119:6-18 map_remove#121:6-16 mem#105:6-9 mem#130:6-9 mem#90:6-9 michelson_equal#365:6-21 mutate_value#347:6-18 mutation_test#407:6-19 mutation_test_all#419:6-23 never#25:6-11 new_account#267:6-17 nl#305:6-8 nth_bootstrap_account#258:6-27 nth_bootstrap_contract#257:6-28 nth_bootstrap_typed_address#262:6-33 option#5:8-14 or#81:6-9 originate#375:6-15 originate_contract#374:6-24 originate_from_file#402:6-25 originate_from_file_and_mutate#431:6-36 originate_from_file_and_mutate_all#451:6-40 originate_module#392:6-22 originate_uncurried#385:6-25 pack#184:6-10 pairing_check#26:6-19 parse_michelson#276:6-21 pbt_result#238:8-18 pbt_test#237:8-16 print#254:6-11 println#306:6-13 random#264:6-12 read_contract_from_file#294:6-29 read_ticket#23:6-17 register_constant#273:6-23 register_delegate#272:6-23 remove#107:6-12 remove#132:6-12 remove#92:6-12 reset_state#344:6-17 reset_state_at#345:6-20 restore_context#277:6-21 run#244:6-9 run#316:8-11 sapling_empty_state#32:25-44 sapling_verify_update#74:25-46 save_context#278:6-18 save_mutation#348:6-19 self#28:25-29 set_baker#288:6-15 set_baker_policy#287:6-22 set_big_map#352:6-17 set_delegate#27:6-18 set_print_values#309:6-22 set_source#251:6-16 sha256#194:6-12 sha3#196:6-10 sha512#195:6-12 shift_left#82:6-16 shift_right#83:6-17 sign#349:6-10 size#102:6-10 size#126:6-10 size#143:6-10 size#289:6-10 split_ticket#58:6-18 sub#168:6-9 sub#189:6-9 tail_opt#145:6-14 test_baker_policy#232:5-22 test_exec_error#225:5-20 test_exec_error_balance_too_low#222:5-36 test_exec_result#230:5-21 to_contract#250:6-17 to_entrypoint#366:6-19 to_json#281:6-13 to_string#280:6-15 to_typed_address#274:6-22 transaction#49:6-17 transfer#338:6-14 transfer_exn#339:6-18 transfer_to_contract#355:6-26 transfer_to_contract_exn#360:6-30 true#208:14-18 uncurry#214:4-11 unforged_ticket#240:8-23 unit#210:14-18 unopt#172:6-11 unopt_with_error#174:6-22 unpack#185:6-12 unset_print_values#310:6-24 update#108:6-12 update#133:6-12 update#157:6-12 update#93:6-12 update_with#159:6-17 value#176:6-11 value_exn#177:6-15 voting_power#19:6-18 xor#80:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ Big_map#86:7-14 Bitwise#78:7-14 Bytes#182:7-12 Crypto#192:7-13 List#141:7-11 Map#100:7-10 Option#171:7-13 PBT#312:9-12 Set#124:7-10 String#163:7-13 Test#242:7-11 Tezos#7:7-12 Transpiled#118:7-17 a#1:4-5 abs#206:4-7 add#106:6-9 add#131:6-9 add#91:6-9 add_account#350:6-17 address#20:6-13 and#79:6-10 assert#203:4-10 assert#472:6-12 assert_none#205:4-15 assert_none#474:6-17 assert_none_with_error#218:4-26 assert_none_with_error#478:6-28 assert_some#204:4-15 assert_some#473:6-17 assert_some_with_error#217:4-26 assert_some_with_error#477:6-28 assert_with_error#216:4-21 assert_with_error#476:6-23 bake_until_n_cycle_end#269:6-28 baker_account#351:6-19 blake2b#193:6-13 bool#4:5-9 bootstrap_contract#346:6-24 c#5:10-11 call_view#55:25-34 cardinal#127:6-14 cast_address#271:6-18 check#200:6-11 chr#295:6-9 compile_contract#290:6-22 compile_contract_from_file#399:6-32 compile_contract_with_views#382:8-35 compile_value#247:6-19 concat#167:6-12 concat#188:6-12 concats#165:6-13 concats#183:6-13 cons#152:6-10 constant#31:25-33 constant_to_michelson_program#275:6-35 create_chest#353:6-18 create_chest_key#354:6-22 create_contract#60:25-40 create_contract_uncurried#63:25-50 create_ticket#47:6-19 curry#213:4-9 d#5:26-27 decompile#268:6-15 drop_context#279:6-18 ediv#219:4-8 emit#71:25-29 empty#101:6-11 empty#125:6-11 empty#87:16-21 eprint#255:6-12 eval#245:6-10 failwith#249:6-14 failwith#2:4-12 false#209:14-19 filter_map#137:6-16 filter_map#155:6-16 find#110:6-10 find#96:6-10 find_opt#111:6-14 find_opt#153:6-14 find_opt#95:6-14 fold#114:6-10 fold#135:6-10 fold#149:6-10 fold_desc#136:6-15 fold_left#150:6-15 fold_right#151:6-16 gen#313:8-11 gen_small#314:8-17 get_amount#10:6-16 get_and_update#109:6-20 get_and_update#94:6-20 get_balance#253:6-17 get_balance#9:6-17 get_bootstrap_account#261:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#68:25-39 get_entrypoint_opt#65:25-43 get_last_events_from#330:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#282:6-17 get_storage_of_address#252:6-28 get_time#270:6-14 get_total_voting_power#17:6-28 get_total_voting_power#248:6-28 get_voting_power#256:6-22 hash_key#198:6-14 head_opt#144:6-14 ignore#212:4-10 implicit_account#21:6-22 int#211:4-7 is_nat#207:4-10 is_none#178:6-13 is_some#179:6-13 iter#112:6-10 iter#134:6-10 iter#148:6-10 join_tickets#22:6-18 keccak#197:6-12 last_originations#263:6-23 length#142:6-12 length#164:6-12 length#186:6-12 literal#103:25-32 literal#128:25-32 literal#88:25-32 log#340:6-9 make_test#315:8-17 map#113:6-9 map#147:6-9 map#175:15-18 map_add#120:6-13 map_find_opt#119:6-18 map_remove#121:6-16 mem#105:6-9 mem#130:6-9 mem#90:6-9 michelson_equal#365:6-21 mutate_value#347:6-18 mutation_test#407:6-19 mutation_test_all#419:6-23 never#25:6-11 new_account#267:6-17 nl#305:6-8 nth_bootstrap_account#258:6-27 nth_bootstrap_contract#257:6-28 nth_bootstrap_typed_address#262:6-33 option#5:8-14 or#81:6-9 originate#375:6-15 originate_contract#374:6-24 originate_from_file#402:6-25 originate_from_file_and_mutate#431:6-36 originate_from_file_and_mutate_all#451:6-40 originate_module#392:6-22 originate_uncurried#385:6-25 pack#184:6-10 pairing_check#26:6-19 parse_michelson#276:6-21 pbt_result#238:8-18 pbt_test#237:8-16 print#254:6-11 println#306:6-13 random#264:6-12 read_contract_from_file#294:6-29 read_ticket#23:6-17 register_constant#273:6-23 register_delegate#272:6-23 remove#107:6-12 remove#132:6-12 remove#92:6-12 reset_state#344:6-17 reset_state_at#345:6-20 restore_context#277:6-21 run#244:6-9 run#316:8-11 sapling_empty_state#32:25-44 sapling_verify_update#74:25-46 save_context#278:6-18 save_mutation#348:6-19 self#28:25-29 set_baker#288:6-15 set_baker_policy#287:6-22 set_big_map#352:6-17 set_delegate#27:6-18 set_print_values#309:6-22 set_source#251:6-16 sha256#194:6-12 sha3#196:6-10 sha512#195:6-12 shift_left#82:6-16 shift_right#83:6-17 sign#349:6-10 size#102:6-10 size#126:6-10 size#143:6-10 size#289:6-10 split_ticket#58:6-18 sub#168:6-9 sub#189:6-9 tail_opt#145:6-14 test_baker_policy#232:5-22 test_exec_error#225:5-20 test_exec_error_balance_too_low#222:5-36 test_exec_result#230:5-21 to_contract#250:6-17 to_entrypoint#366:6-19 to_json#281:6-13 to_string#280:6-15 to_typed_address#274:6-22 transaction#49:6-17 transfer#338:6-14 transfer_exn#339:6-18 transfer_to_contract#355:6-26 transfer_to_contract_exn#360:6-30 true#208:14-18 uncurry#214:4-11 unforged_ticket#240:8-23 unit#210:14-18 unopt#172:6-11 unopt_with_error#174:6-22 unpack#185:6-12 unset_print_values#310:6-24 update#108:6-12 update#133:6-12 update#157:6-12 update#93:6-12 update_with#159:6-17 value#176:6-11 value_exn#177:6-15 voting_power#19:6-18 xor#80:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Big_map#86:7-14 Bitwise#78:7-14 Bytes#182:7-12 Crypto#192:7-13 List#141:7-11 Map#100:7-10 Option#171:7-13 PBT#312:9-12 Set#124:7-10 String#163:7-13 Test#242:7-11 Tezos#7:7-12 Transpiled#118:7-17 a#1:4-5 abs#206:4-7 add#106:6-9 add#131:6-9 add#91:6-9 add_account#350:6-17 address#20:6-13 and#79:6-10 assert#203:4-10 assert#472:6-12 assert_none#205:4-15 assert_none#474:6-17 assert_none_with_error#218:4-26 assert_none_with_error#478:6-28 assert_some#204:4-15 assert_some#473:6-17 assert_some_with_error#217:4-26 assert_some_with_error#477:6-28 assert_with_error#216:4-21 assert_with_error#476:6-23 bake_until_n_cycle_end#269:6-28 baker_account#351:6-19 blake2b#193:6-13 bool#4:5-9 bootstrap_contract#346:6-24 call_view#55:25-34 cardinal#127:6-14 cast_address#271:6-18 check#200:6-11 chr#295:6-9 compile_contract#290:6-22 compile_contract_from_file#399:6-32 compile_contract_with_views#382:8-35 compile_value#247:6-19 concat#167:6-12 concat#188:6-12 concats#165:6-13 concats#183:6-13 cons#152:6-10 constant#31:25-33 constant_to_michelson_program#275:6-35 create_chest#353:6-18 create_chest_key#354:6-22 create_contract#60:25-40 create_contract_uncurried#63:25-50 create_ticket#47:6-19 curry#213:4-9 decompile#268:6-15 drop_context#279:6-18 e#6:9-10 ediv#219:4-8 emit#71:25-29 empty#101:6-11 empty#125:6-11 empty#87:16-21 eprint#255:6-12 eval#245:6-10 failwith#249:6-14 failwith#2:4-12 false#209:14-19 filter_map#137:6-16 filter_map#155:6-16 find#110:6-10 find#96:6-10 find_opt#111:6-14 find_opt#153:6-14 find_opt#95:6-14 fold#114:6-10 fold#135:6-10 fold#149:6-10 fold_desc#136:6-15 fold_left#150:6-15 fold_right#151:6-16 gen#313:8-11 gen_small#314:8-17 get_amount#10:6-16 get_and_update#109:6-20 get_and_update#94:6-20 get_balance#253:6-17 get_balance#9:6-17 get_bootstrap_account#261:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#68:25-39 get_entrypoint_opt#65:25-43 get_last_events_from#330:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#282:6-17 get_storage_of_address#252:6-28 get_time#270:6-14 get_total_voting_power#17:6-28 get_total_voting_power#248:6-28 get_voting_power#256:6-22 hash_key#198:6-14 head_opt#144:6-14 ignore#212:4-10 implicit_account#21:6-22 int#211:4-7 is_nat#207:4-10 is_none#178:6-13 is_some#179:6-13 iter#112:6-10 iter#134:6-10 iter#148:6-10 join_tickets#22:6-18 keccak#197:6-12 last_originations#263:6-23 length#142:6-12 length#164:6-12 length#186:6-12 literal#103:25-32 literal#128:25-32 literal#88:25-32 log#340:6-9 make_test#315:8-17 map#113:6-9 map#147:6-9 map#175:15-18 map_add#120:6-13 map_find_opt#119:6-18 map_remove#121:6-16 mem#105:6-9 mem#130:6-9 mem#90:6-9 michelson_equal#365:6-21 mutate_value#347:6-18 mutation_test#407:6-19 mutation_test_all#419:6-23 never#25:6-11 new_account#267:6-17 nl#305:6-8 nth_bootstrap_account#258:6-27 nth_bootstrap_contract#257:6-28 nth_bootstrap_typed_address#262:6-33 option#5:8-14 or#81:6-9 originate#375:6-15 originate_contract#374:6-24 originate_from_file#402:6-25 originate_from_file_and_mutate#431:6-36 originate_from_file_and_mutate_all#451:6-40 originate_module#392:6-22 originate_uncurried#385:6-25 pack#184:6-10 pairing_check#26:6-19 parse_michelson#276:6-21 pbt_result#238:8-18 pbt_test#237:8-16 print#254:6-11 println#306:6-13 random#264:6-12 read_contract_from_file#294:6-29 read_ticket#23:6-17 register_constant#273:6-23 register_delegate#272:6-23 remove#107:6-12 remove#132:6-12 remove#92:6-12 reset_state#344:6-17 reset_state_at#345:6-20 restore_context#277:6-21 run#244:6-9 run#316:8-11 sapling_empty_state#32:25-44 sapling_verify_update#74:25-46 save_context#278:6-18 save_mutation#348:6-19 self#28:25-29 set_baker#288:6-15 set_baker_policy#287:6-22 set_big_map#352:6-17 set_delegate#27:6-18 set_print_values#309:6-22 set_source#251:6-16 sha256#194:6-12 sha3#196:6-10 sha512#195:6-12 shift_left#82:6-16 shift_right#83:6-17 sign#349:6-10 size#102:6-10 size#126:6-10 size#143:6-10 size#289:6-10 split_ticket#58:6-18 sub#168:6-9 sub#189:6-9 tail_opt#145:6-14 test_baker_policy#232:5-22 test_exec_error#225:5-20 test_exec_error_balance_too_low#222:5-36 test_exec_result#230:5-21 to_contract#250:6-17 to_entrypoint#366:6-19 to_json#281:6-13 to_string#280:6-15 to_typed_address#274:6-22 transaction#49:6-17 transfer#338:6-14 transfer_exn#339:6-18 transfer_to_contract#355:6-26 transfer_to_contract_exn#360:6-30 true#208:14-18 uncurry#214:4-11 unforged_ticket#240:8-23 unit#210:14-18 unopt#172:6-11 unopt_with_error#174:6-22 unpack#185:6-12 unset_print_values#310:6-24 update#108:6-12 update#133:6-12 update#157:6-12 update#93:6-12 update_with#159:6-17 value#176:6-11 value_exn#177:6-15 voting_power#19:6-18 xor#80:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32
    [ Big_map#86:7-14 Bitwise#78:7-14 Bytes#182:7-12 Crypto#192:7-13 List#141:7-11 Map#100:7-10 Option#171:7-13 PBT#312:9-12 Set#124:7-10 String#163:7-13 Test#242:7-11 Tezos#7:7-12 Transpiled#118:7-17 a#1:4-5 abs#206:4-7 add#106:6-9 add#131:6-9 add#91:6-9 add_account#350:6-17 address#20:6-13 and#79:6-10 assert#203:4-10 assert#472:6-12 assert_none#205:4-15 assert_none#474:6-17 assert_none_with_error#218:4-26 assert_none_with_error#478:6-28 assert_some#204:4-15 assert_some#473:6-17 assert_some_with_error#217:4-26 assert_some_with_error#477:6-28 assert_with_error#216:4-21 assert_with_error#476:6-23 bake_until_n_cycle_end#269:6-28 baker_account#351:6-19 blake2b#193:6-13 bool#4:5-9 bootstrap_contract#346:6-24 call_view#55:25-34 cardinal#127:6-14 cast_address#271:6-18 check#200:6-11 chr#295:6-9 compile_contract#290:6-22 compile_contract_from_file#399:6-32 compile_contract_with_views#382:8-35 compile_value#247:6-19 concat#167:6-12 concat#188:6-12 concats#165:6-13 concats#183:6-13 cons#152:6-10 constant#31:25-33 constant_to_michelson_program#275:6-35 create_chest#353:6-18 create_chest_key#354:6-22 create_contract#60:25-40 create_contract_uncurried#63:25-50 create_ticket#47:6-19 curry#213:4-9 decompile#268:6-15 drop_context#279:6-18 ediv#219:4-8 emit#71:25-29 empty#101:6-11 empty#125:6-11 empty#87:16-21 eprint#255:6-12 eval#245:6-10 failwith#249:6-14 failwith#2:4-12 false#209:14-19 filter_map#137:6-16 filter_map#155:6-16 find#110:6-10 find#96:6-10 find_opt#111:6-14 find_opt#153:6-14 find_opt#95:6-14 fold#114:6-10 fold#135:6-10 fold#149:6-10 fold_desc#136:6-15 fold_left#150:6-15 fold_right#151:6-16 gen#313:8-11 gen_small#314:8-17 get_amount#10:6-16 get_and_update#109:6-20 get_and_update#94:6-20 get_balance#253:6-17 get_balance#9:6-17 get_bootstrap_account#261:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#68:25-39 get_entrypoint_opt#65:25-43 get_last_events_from#330:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#282:6-17 get_storage_of_address#252:6-28 get_time#270:6-14 get_total_voting_power#17:6-28 get_total_voting_power#248:6-28 get_voting_power#256:6-22 hash_key#198:6-14 head_opt#144:6-14 ignore#212:4-10 implicit_account#21:6-22 int#211:4-7 is_nat#207:4-10 is_none#178:6-13 is_some#179:6-13 iter#112:6-10 iter#134:6-10 iter#148:6-10 join_tickets#22:6-18 keccak#197:6-12 last_originations#263:6-23 length#142:6-12 length#164:6-12 length#186:6-12 literal#103:25-32 literal#128:25-32 literal#88:25-32 log#340:6-9 make_test#315:8-17 map#113:6-9 map#147:6-9 map#175:15-18 map_add#120:6-13 map_find_opt#119:6-18 map_remove#121:6-16 mem#105:6-9 mem#130:6-9 mem#90:6-9 michelson_equal#365:6-21 mutate_value#347:6-18 mutation_test#407:6-19 mutation_test_all#419:6-23 never#25:6-11 new_account#267:6-17 nl#305:6-8 nth_bootstrap_account#258:6-27 nth_bootstrap_contract#257:6-28 nth_bootstrap_typed_address#262:6-33 option#5:8-14 or#81:6-9 originate#375:6-15 originate_contract#374:6-24 originate_from_file#402:6-25 originate_from_file_and_mutate#431:6-36 originate_from_file_and_mutate_all#451:6-40 originate_module#392:6-22 originate_uncurried#385:6-25 pack#184:6-10 pairing_check#26:6-19 parse_michelson#276:6-21 pbt_result#238:8-18 pbt_test#237:8-16 print#254:6-11 println#306:6-13 random#264:6-12 read_contract_from_file#294:6-29 read_ticket#23:6-17 register_constant#273:6-23 register_delegate#272:6-23 remove#107:6-12 remove#132:6-12 remove#92:6-12 reset_state#344:6-17 reset_state_at#345:6-20 restore_context#277:6-21 run#244:6-9 run#316:8-11 sapling_empty_state#32:25-44 sapling_verify_update#74:25-46 save_context#278:6-18 save_mutation#348:6-19 self#28:25-29 set_baker#288:6-15 set_baker_policy#287:6-22 set_big_map#352:6-17 set_delegate#27:6-18 set_print_values#309:6-22 set_source#251:6-16 sha256#194:6-12 sha3#196:6-10 sha512#195:6-12 shift_left#82:6-16 shift_right#83:6-17 sign#349:6-10 size#102:6-10 size#126:6-10 size#143:6-10 size#289:6-10 split_ticket#58:6-18 sub#168:6-9 sub#189:6-9 tail_opt#145:6-14 test_baker_policy#232:5-22 test_exec_error#225:5-20 test_exec_error_balance_too_low#222:5-36 test_exec_result#230:5-21 to_contract#250:6-17 to_entrypoint#366:6-19 to_json#281:6-13 to_string#280:6-15 to_typed_address#274:6-22 transaction#49:6-17 transfer#338:6-14 transfer_exn#339:6-18 transfer_to_contract#355:6-26 transfer_to_contract_exn#360:6-30 true#208:14-18 uncurry#214:4-11 unforged_ticket#240:8-23 unit#210:14-18 unopt#172:6-11 unopt_with_error#174:6-22 unpack#185:6-12 unset_print_values#310:6-24 update#108:6-12 update#133:6-12 update#157:6-12 update#93:6-12 update_with#159:6-17 value#176:6-11 value_exn#177:6-15 voting_power#19:6-18 xor#80:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#206:4-7 -> abs)
    Range: File "", line 206, characters 4-7
    Body Range: File "", line 206, characters 0-69
    Content: |core: int -> nat|
    references: File "", line 370, characters 31-34
    (assert#203:4-10 -> assert)
    Range: File "", line 203, characters 4-10
    Body Range: File "", line 203, characters 0-76
    Content: |core: bool -> unit|
    references: []
    (assert_none#205:4-15 -> assert_none)
    Range: File "", line 205, characters 4-15
    Body Range: File "", line 205, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#218:4-26 -> assert_none_with_error)
    Range: File "", line 218, characters 4-26
    Body Range: File "", line 218, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#204:4-15 -> assert_some)
    Range: File "", line 204, characters 4-15
    Body Range: File "", line 204, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#217:4-26 -> assert_some_with_error)
    Range: File "", line 217, characters 4-26
    Body Range: File "", line 217, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#216:4-21 -> assert_with_error)
    Range: File "", line 216, characters 4-21
    Body Range: File "", line 216, characters 0-76
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
    (curry#213:4-9 -> curry)
    Range: File "", line 213, characters 4-9
    Body Range: File "", line 213, characters 62-70
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
    (ediv#219:4-8 -> ediv)
    Range: File "", line 219, characters 4-8
    Body Range: File "", line 219, characters 61-138
    Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_ediv (a ,
    b)|
    references: []
    (failwith#2:4-12 -> failwith)
    Range: File "", line 2, characters 4-12
    Body Range: File "", line 2, characters 26-66
    Content: |unresolved|
    references:
      File "", line 38, characters 27-35 ,
      File "", line 42, characters 27-35 ,
      File "", line 70, characters 27-35 ,
      File "", line 172, characters 79-87 ,
      File "", line 174, characters 103-111 ,
      File "", line 177, characters 83-91 ,
      File "", line 203, characters 49-57 ,
      File "", line 204, characters 72-80 ,
      File "", line 205, characters 87-95 ,
      File "", line 216, characters 66-74 ,
      File "", line 217, characters 96-104 ,
      File "", line 218, characters 111-119
    (false#209:14-19 -> false)
    Range: File "", line 209, characters 14-19
    Body Range: File "", line 209, characters 29-34
    Content: |core: bool|
    references:
      File "", line 265, characters 51-56 ,
      File "", line 310, characters 90-95 ,
      File "", line 313, characters 62-67
    (ignore#212:4-10 -> ignore)
    Range: File "", line 212, characters 4-10
    Body Range: File "", line 212, characters 37-39
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#211:4-7 -> int)
    Range: File "", line 211, characters 4-7
    Body Range: File "", line 211, characters 44-96
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 261, characters 97-100 ,
      File "", line 298, characters 79-82 ,
      File "", line 300, characters 78-81 ,
      File "", line 302, characters 72-75
    (is_nat#207:4-10 -> is_nat)
    Range: File "", line 207, characters 4-10
    Body Range: File "", line 207, characters 0-88
    Content: |core: int -> option (nat)|
    references: []
    (true#208:14-18 -> true)
    Range: File "", line 208, characters 14-18
    Body Range: File "", line 208, characters 28-32
    Content: |core: bool|
    references:
      File "", line 309, characters 88-92 ,
      File "", line 314, characters 68-72
    (uncurry#214:4-11 -> uncurry)
    Range: File "", line 214, characters 4-11
    Body Range: File "", line 214, characters 62-73
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 376, characters 30-37
    (unit#210:14-18 -> unit)
    Range: File "", line 210, characters 14-18
    Body Range: File "", line 210, characters 28-48
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
      File "", line 90, characters 52-56 ,
      File "", line 105, characters 48-52 ,
      File "", line 130, characters 41-45 ,
      File "", line 133, characters 35-39 ,
      File "", line 153, characters 34-38 ,
      File "", line 159, characters 37-41 ,
      File "", line 178, characters 40-44 ,
      File "", line 179, characters 40-44 ,
      File "", line 200, characters 52-56 ,
      File "", line 200, characters 145-149 ,
      File "", line 203, characters 16-20 ,
      File "", line 208, characters 21-25 ,
      File "", line 209, characters 22-26 ,
      File "", line 216, characters 27-31 ,
      File "", line 237, characters 41-45 ,
      File "", line 315, characters 53-57 ,
      File "", line 365, characters 74-78 ,
      File "", line 472, characters 18-22 ,
      File "", line 476, characters 29-33
    (option#5:8-14 -> option)
    Range: File "", line 5, characters 8-14
    Body Range: File "", line 5, characters 17-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 22, characters 67-73 ,
      File "", line 22, characters 146-152 ,
      File "", line 27, characters 33-39 ,
      File "", line 27, characters 102-108 ,
      File "", line 29, characters 14-20 ,
      File "", line 34, characters 80-86 ,
      File "", line 35, characters 73-79 ,
      File "", line 47, characters 60-66 ,
      File "", line 47, characters 132-138 ,
      File "", line 55, characters 86-92 ,
      File "", line 57, characters 83-89 ,
      File "", line 58, characters 83-89 ,
      File "", line 59, characters 99-105 ,
      File "", line 60, characters 102-108 ,
      File "", line 63, characters 111-117 ,
      File "", line 65, characters 93-99 ,
      File "", line 67, characters 84-90 ,
      File "", line 74, characters 158-164 ,
      File "", line 74, characters 316-322 ,
      File "", line 93, characters 39-45 ,
      File "", line 94, characters 47-53 ,
      File "", line 94, characters 80-86 ,
      File "", line 95, characters 59-65 ,
      File "", line 108, characters 39-45 ,
      File "", line 109, characters 47-53 ,
      File "", line 109, characters 76-82 ,
      File "", line 111, characters 55-61 ,
      File "", line 137, characters 40-46 ,
      File "", line 144, characters 42-48 ,
      File "", line 145, characters 49-55 ,
      File "", line 153, characters 58-64 ,
      File "", line 154, characters 31-37 ,
      File "", line 155, characters 40-46 ,
      File "", line 157, characters 34-40 ,
      File "", line 172, characters 28-34 ,
      File "", line 174, characters 39-45 ,
      File "", line 175, characters 50-56 ,
      File "", line 175, characters 62-68 ,
      File "", line 176, characters 42-48 ,
      File "", line 177, characters 48-54 ,
      File "", line 178, characters 30-36 ,
      File "", line 179, characters 30-36 ,
      File "", line 185, characters 38-44 ,
      File "", line 185, characters 101-107 ,
      File "", line 204, characters 32-38 ,
      File "", line 205, characters 32-38 ,
      File "", line 207, characters 27-33 ,
      File "", line 207, characters 78-84 ,
      File "", line 217, characters 43-49 ,
      File "", line 218, characters 43-49 ,
      File "", line 295, characters 29-35 ,
      File "", line 338, characters 147-153 ,
      File "", line 339, characters 142-148 ,
      File "", line 344, characters 102-108 ,
      File "", line 347, characters 63-69 ,
      File "", line 348, characters 57-63 ,
      File "", line 351, characters 48-54 ,
      File "", line 357, characters 19-25 ,
      File "", line 362, characters 21-27 ,
      File "", line 400, characters 100-106 ,
      File "", line 407, characters 74-80 ,
      File "", line 410, characters 52-58 ,
      File "", line 432, characters 105-111 ,
      File "", line 438, characters 100-106 ,
      File "", line 441, characters 52-58 ,
      File "", line 458, characters 100-106 ,
      File "", line 473, characters 34-40 ,
      File "", line 474, characters 34-40 ,
      File "", line 477, characters 45-51 ,
      File "", line 478, characters 45-51
    (pbt_result#238:8-18 -> pbt_result)
    Range: File "", line 238, characters 8-18
    Body Range: File "", line 238, characters 21-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 316, characters 57-67 ,
      File "", line 317, characters 39-49 ,
      File "", line 319, characters 84-94 ,
      File "", line 323, characters 96-106 ,
      File "", line 326, characters 68-78
    (pbt_test#237:8-16 -> pbt_test)
    Range: File "", line 237, characters 8-16
    Body Range: File "", line 237, characters 19-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 315, characters 63-71 ,
      File "", line 316, characters 33-41
    (test_baker_policy#232:5-22 -> test_baker_policy)
    Range: File "", line 232, characters 5-22
    Body Range: File "", line 233, character 4 to line 235, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 287, characters 29-46
    (test_exec_error#225:5-20 -> test_exec_error)
    Range: File "", line 225, characters 5-20
    Body Range: File "", line 226, character 4 to line 228, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 230, characters 49-64
    (test_exec_error_balance_too_low#222:5-36 -> test_exec_error_balance_too_low)
    Range: File "", line 222, characters 5-36
    Body Range: File "", line 223, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 227, characters 23-54
    (test_exec_result#230:5-21 -> test_exec_result)
    Range: File "", line 230, characters 5-21
    Body Range: File "", line 230, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 338, characters 65-81 ,
      File "", line 355, characters 73-89
    (unforged_ticket#240:8-23 -> unforged_ticket)
    Range: File "", line 240, characters 8-23
    Body Range: File "", line 240, characters 26-40
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    Module definitions:
    (Big_map#86:7-14 -> Big_map)
    Range: File "", line 86, characters 7-14
    Body Range: File "", line 87, character 2 to line 96, character 87
    Content: Members: Variable definitions:
                      (add#91:6-9 -> add)
                      Range: File "", line 91, characters 6-9
                      Body Range: File "", line 91, characters 77-109
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (empty#87:16-21 -> empty)
                      Range: File "", line 87, characters 16-21
                      Body Range: File "", line 87, characters 52-81
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#96:6-10 -> find)
                      Range: File "", line 96, characters 6-10
                      Body Range: File "", line 96, characters 57-87
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      (find_opt#95:6-14 -> find_opt)
                      Range: File "", line 95, characters 6-14
                      Body Range: File "", line 95, characters 68-102
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (get_and_update#94:6-20 -> get_and_update)
                      Range: File "", line 94, characters 6-20
                      Body Range: File "", line 94, characters 106-153
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#88:25-32 -> literal)
                      Range: File "", line 88, characters 25-32
                      Body Range: File "", line 88, characters 82-116
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#90:6-9 -> mem)
                      Range: File "", line 90, characters 6-9
                      Body Range: File "", line 90, characters 59-88
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (remove#92:6-12 -> remove)
                      Range: File "", line 92, characters 6-12
                      Body Range: File "", line 92, characters 72-104
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#93:6-12 -> update)
                      Range: File "", line 93, characters 6-12
                      Body Range: File "", line 93, characters 87-122
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#78:7-14 -> Bitwise)
    Range: File "", line 78, characters 7-14
    Body Range: File "", line 79, character 2 to line 83, character 71
    Content: Members: Variable definitions:
                      (and#79:6-10 -> and)
                      Range: File "", line 79, characters 6-10
                      Body Range: File "", line 79, characters 62-137
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (or#81:6-9 -> or)
                      Range: File "", line 81, characters 6-9
                      Body Range: File "", line 81, characters 2-62
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_left#82:6-16 -> shift_left)
                      Range: File "", line 82, characters 6-16
                      Body Range: File "", line 82, characters 2-70
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_right#83:6-17 -> shift_right)
                      Range: File "", line 83, characters 6-17
                      Body Range: File "", line 83, characters 2-71
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (xor#80:6-9 -> xor)
                      Range: File "", line 80, characters 6-9
                      Body Range: File "", line 80, characters 2-63
                      Content: |core: nat -> nat -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#182:7-12 -> Bytes)
    Range: File "", line 182, characters 7-12
    Body Range: File "", line 183, character 2 to line 189, character 82
    Content: Members: Variable definitions:
                      (concat#188:6-12 -> concat)
                      Range: File "", line 188, characters 6-12
                      Body Range: File "", line 188, characters 2-79
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#183:6-13 -> concats)
                      Range: File "", line 183, characters 6-13
                      Body Range: File "", line 183, characters 2-69
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#186:6-12 -> length)
                      Range: File "", line 186, characters 6-12
                      Body Range: File "", line 186, characters 2-56
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#184:6-10 -> pack)
                      Range: File "", line 184, characters 6-10
                      Body Range: File "", line 184, characters 38-82
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#189:6-9 -> sub)
                      Range: File "", line 189, characters 6-9
                      Body Range: File "", line 189, characters 2-82
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#185:6-12 -> unpack)
                      Range: File "", line 185, characters 6-12
                      Body Range: File "", line 185, characters 47-122
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#192:7-13 -> Crypto)
    Range: File "", line 192, characters 7-13
    Body Range: File "", line 193, character 2 to line 200, character 161
    Content: Members: Variable definitions:
                      (blake2b#193:6-13 -> blake2b)
                      Range: File "", line 193, characters 6-13
                      Body Range: File "", line 193, characters 2-87
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#200:6-11 -> check)
                      Range: File "", line 200, characters 6-11
                      Body Range: File "", line 200, characters 2-161
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#198:6-14 -> hash_key)
                      Range: File "", line 198, characters 6-14
                      Body Range: File "", line 198, characters 2-91
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#197:6-12 -> keccak)
                      Range: File "", line 197, characters 6-12
                      Body Range: File "", line 197, characters 2-85
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#194:6-12 -> sha256)
                      Range: File "", line 194, characters 6-12
                      Body Range: File "", line 194, characters 2-85
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#196:6-10 -> sha3)
                      Range: File "", line 196, characters 6-10
                      Body Range: File "", line 196, characters 2-81
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#195:6-12 -> sha512)
                      Range: File "", line 195, characters 6-12
                      Body Range: File "", line 195, characters 2-85
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#141:7-11 -> List)
    Range: File "", line 141, characters 7-11
    Body Range: File "", line 142, character 2 to line 160, character 48
    Content: Members: Variable definitions:
                      (cons#152:6-10 -> cons)
                      Range: File "", line 152, characters 6-10
                      Body Range: File "", line 152, characters 53-80
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (filter_map#155:6-16 -> filter_map)
                      Range: File "", line 155, characters 6-16
                      Body Range: File "", line 156, characters 4-100
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      (find_opt#153:6-14 -> find_opt)
                      Range: File "", line 153, characters 6-14
                      Body Range: File "", line 154, characters 4-82
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#149:6-10 -> fold)
                      Range: File "", line 149, characters 6-10
                      Body Range: File "", line 149, characters 67-102
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 337, characters 9-13
                      (fold_left#150:6-15 -> fold_left)
                      Range: File "", line 150, characters 6-15
                      Body Range: File "", line 150, characters 72-112
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#151:6-16 -> fold_right)
                      Range: File "", line 151, characters 6-16
                      Body Range: File "", line 151, characters 73-114
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 154, characters 4-14 ,
                        File "", line 156, characters 4-14
                      (head_opt#144:6-14 -> head_opt)
                      Range: File "", line 144, characters 6-14
                      Body Range: File "", line 144, characters 51-98
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#148:6-10 -> iter)
                      Range: File "", line 148, characters 6-10
                      Body Range: File "", line 148, characters 58-90
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#142:6-12 -> length)
                      Range: File "", line 142, characters 6-12
                      Body Range: File "", line 142, characters 44-73
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#147:6-9 -> map)
                      Range: File "", line 147, characters 6-9
                      Body Range: File "", line 147, characters 59-90
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 158, characters 4-7 ,
                        File "", line 160, characters 4-7
                      (size#143:6-10 -> size)
                      Range: File "", line 143, characters 6-10
                      Body Range: File "", line 143, characters 42-71
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#145:6-14 -> tail_opt)
                      Range: File "", line 145, characters 6-14
                      Body Range: File "", line 145, characters 58-107
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      (update#157:6-12 -> update)
                      Range: File "", line 157, characters 6-12
                      Body Range: File "", line 158, characters 4-62
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      (update_with#159:6-17 -> update_with)
                      Range: File "", line 159, characters 6-17
                      Body Range: File "", line 160, characters 4-48
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 337, characters 4-8

    (Map#100:7-10 -> Map)
    Range: File "", line 100, characters 7-10
    Body Range: File "", line 101, character 2 to line 114, character 111
    Content: Members: Variable definitions:
                      (add#106:6-9 -> add)
                      Range: File "", line 106, characters 6-9
                      Body Range: File "", line 106, characters 69-101
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (empty#101:6-11 -> empty)
                      Range: File "", line 101, characters 6-11
                      Body Range: File "", line 101, characters 38-63
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#110:6-10 -> find)
                      Range: File "", line 110, characters 6-10
                      Body Range: File "", line 110, characters 53-83
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#111:6-14 -> find_opt)
                      Range: File "", line 111, characters 6-14
                      Body Range: File "", line 111, characters 64-98
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (fold#114:6-10 -> fold)
                      Range: File "", line 114, characters 6-10
                      Body Range: File "", line 114, characters 78-111
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      (get_and_update#109:6-20 -> get_and_update)
                      Range: File "", line 109, characters 6-20
                      Body Range: File "", line 109, characters 98-141
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (iter#112:6-10 -> iter)
                      Range: File "", line 112, characters 6-10
                      Body Range: File "", line 112, characters 68-98
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (literal#103:25-32 -> literal)
                      Range: File "", line 103, characters 25-32
                      Body Range: File "", line 103, characters 78-108
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#113:6-9 -> map)
                      Range: File "", line 113, characters 6-9
                      Body Range: File "", line 113, characters 72-101
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (mem#105:6-9 -> mem)
                      Range: File "", line 105, characters 6-9
                      Body Range: File "", line 105, characters 55-84
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (remove#107:6-12 -> remove)
                      Range: File "", line 107, characters 6-12
                      Body Range: File "", line 107, characters 64-96
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (size#102:6-10 -> size)
                      Range: File "", line 102, characters 6-10
                      Body Range: File "", line 102, characters 47-74
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#108:6-12 -> update)
                      Range: File "", line 108, characters 6-12
                      Body Range: File "", line 108, characters 79-114
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#171:7-13 -> Option)
    Range: File "", line 171, characters 7-13
    Body Range: File "", line 172, character 2 to line 179, character 77
    Content: Members: Variable definitions:
                      (is_none#178:6-13 -> is_none)
                      Range: File "", line 178, characters 6-13
                      Body Range: File "", line 178, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#179:6-13 -> is_some)
                      Range: File "", line 179, characters 6-13
                      Body Range: File "", line 179, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#175:15-18 -> map)
                      Range: File "", line 175, characters 15-18
                      Body Range: File "", line 175, characters 71-103
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#172:6-11 -> unopt)
                      Range: File "", line 172, characters 6-11
                      Body Range: File "", line 172, characters 42-104
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#174:6-22 -> unopt_with_error)
                      Range: File "", line 174, characters 6-22
                      Body Range: File "", line 174, characters 66-113
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#176:6-11 -> value)
                      Range: File "", line 176, characters 6-11
                      Body Range: File "", line 176, characters 56-100
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#177:6-15 -> value_exn)
                      Range: File "", line 177, characters 6-15
                      Body Range: File "", line 177, characters 62-109
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#124:7-10 -> Set)
    Range: File "", line 124, characters 7-10
    Body Range: File "", line 125, character 2 to line 138, character 110
    Content: Members: Variable definitions:
                      (add#131:6-9 -> add)
                      Range: File "", line 131, characters 6-9
                      Body Range: File "", line 131, characters 49-78
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: File "", line 138, characters 81-84
                      (cardinal#127:6-14 -> cardinal)
                      Range: File "", line 127, characters 6-14
                      Body Range: File "", line 127, characters 44-71
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#125:6-11 -> empty)
                      Range: File "", line 125, characters 6-11
                      Body Range: File "", line 125, characters 31-56
                      Content: |core: ∀ a : * . set (a)|
                      references: File "", line 138, characters 96-101
                      (filter_map#137:6-16 -> filter_map)
                      Range: File "", line 137, characters 6-16
                      Body Range: File "", line 138, characters 4-110
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> set (a) -> set (b)|
                      references: []
                      (fold#135:6-10 -> fold)
                      Range: File "", line 135, characters 6-10
                      Body Range: File "", line 135, characters 65-98
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#136:6-15 -> fold_desc)
                      Range: File "", line 136, characters 6-15
                      Body Range: File "", line 136, characters 70-108
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: File "", line 138, characters 4-13
                      (iter#134:6-10 -> iter)
                      Range: File "", line 134, characters 6-10
                      Body Range: File "", line 134, characters 57-87
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#128:25-32 -> literal)
                      Range: File "", line 128, characters 25-32
                      Body Range: File "", line 128, characters 65-95
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#130:6-9 -> mem)
                      Range: File "", line 130, characters 6-9
                      Body Range: File "", line 130, characters 48-77
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#132:6-12 -> remove)
                      Range: File "", line 132, characters 6-12
                      Body Range: File "", line 132, characters 52-84
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#126:6-10 -> size)
                      Range: File "", line 126, characters 6-10
                      Body Range: File "", line 126, characters 40-67
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#133:6-12 -> update)
                      Range: File "", line 133, characters 6-12
                      Body Range: File "", line 133, characters 55-90
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#163:7-13 -> String)
    Range: File "", line 163, characters 7-13
    Body Range: File "", line 164, character 2 to line 168, character 84
    Content: Members: Variable definitions:
                      (concat#167:6-12 -> concat)
                      Range: File "", line 167, characters 6-12
                      Body Range: File "", line 167, characters 2-82
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#165:6-13 -> concats)
                      Range: File "", line 165, characters 6-13
                      Body Range: File "", line 165, characters 2-71
                      Content: |core: list (string) -> string|
                      references: []
                      (length#164:6-12 -> length)
                      Range: File "", line 164, characters 6-12
                      Body Range: File "", line 164, characters 2-57
                      Content: |core: string -> nat|
                      references:
                        File "", line 367, characters 22-28 ,
                        File "", line 370, characters 43-49
                      (sub#168:6-9 -> sub)
                      Range: File "", line 168, characters 6-9
                      Body Range: File "", line 168, characters 2-84
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 368, characters 24-27 ,
                        File "", line 370, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 367, characters 15-21 ,
      File "", line 368, characters 17-23 ,
      File "", line 370, characters 16-22 ,
      File "", line 370, characters 36-42

    (Test#242:7-11 -> Test)
    Range: File "", line 242, characters 7-11
    Body Range: File "", line 244, character 2 to line 478, character 102
    Content: Members: Variable definitions:
                      (add_account#350:6-17 -> add_account)
                      Range: File "", line 350, characters 6-17
                      Body Range: File "", line 350, characters 2-88
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#472:6-12 -> assert)
                      Range: File "", line 472, characters 6-12
                      Body Range: File "", line 472, characters 2-78
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#474:6-17 -> assert_none)
                      Range: File "", line 474, characters 6-17
                      Body Range: File "", line 474, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#478:6-28 -> assert_none_with_error)
                      Range: File "", line 478, characters 6-28
                      Body Range: File "", line 478, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#473:6-17 -> assert_some)
                      Range: File "", line 473, characters 6-17
                      Body Range: File "", line 473, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#477:6-28 -> assert_some_with_error)
                      Range: File "", line 477, characters 6-28
                      Body Range: File "", line 477, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#476:6-23 -> assert_with_error)
                      Range: File "", line 476, characters 6-23
                      Body Range: File "", line 476, characters 2-78
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#269:6-28 -> bake_until_n_cycle_end)
                      Range: File "", line 269, characters 6-28
                      Body Range: File "", line 269, characters 2-94
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#351:6-19 -> baker_account)
                      Range: File "", line 351, characters 6-19
                      Body Range: File "", line 351, characters 2-105
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#346:6-24 -> bootstrap_contract)
                      Range: File "", line 346, characters 6-24
                      Body Range: File "", line 346, characters 97-145
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#271:6-18 -> cast_address)
                      Range: File "", line 271, characters 6-18
                      Body Range: File "", line 271, characters 69-105
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 380, characters 35-47 ,
                        File "", line 390, characters 35-47 ,
                        File "", line 397, characters 35-47
                      (chr#295:6-9 -> chr)
                      Range: File "", line 295, characters 6-9
                      Body Range: File "", line 295, character 2 to line 304, character 10
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#290:6-22 -> compile_contract)
                      Range: File "", line 290, characters 6-22
                      Body Range: File "", line 291, character 4 to line 293, character 52
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 376, characters 12-28 ,
                        File "", line 386, characters 12-28
                      (compile_contract_from_file#399:6-32 -> compile_contract_from_file)
                      Range: File "", line 399, characters 6-32
                      Body Range: File "", line 399, character 2 to line 401, character 52
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 403, characters 12-38
                      (compile_contract_with_views#382:8-35 -> compile_contract_with_views)
                      Range: File "", line 382, characters 8-35
                      Body Range: File "", line 383, character 6 to line 384, character 54
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 393, characters 12-39
                      (compile_value#247:6-19 -> compile_value)
                      Range: File "", line 247, characters 6-19
                      Body Range: File "", line 247, characters 59-65
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#275:6-35 -> constant_to_michelson_program)
                      Range: File "", line 275, characters 6-35
                      Body Range: File "", line 275, characters 2-116
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#353:6-18 -> create_chest)
                      Range: File "", line 353, characters 6-18
                      Body Range: File "", line 353, characters 2-102
                      Content: |core: bytes -> nat -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#354:6-22 -> create_chest_key)
                      Range: File "", line 354, characters 6-22
                      Body Range: File "", line 354, characters 2-102
                      Content: |core: chest -> nat -> chest_key|
                      references: []
                      (decompile#268:6-15 -> decompile)
                      Range: File "", line 268, characters 6-15
                      Body Range: File "", line 268, characters 55-88
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 286, characters 5-14
                      (drop_context#279:6-18 -> drop_context)
                      Range: File "", line 279, characters 6-18
                      Body Range: File "", line 279, characters 2-75
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#255:6-12 -> eprint)
                      Range: File "", line 255, characters 6-12
                      Body Range: File "", line 255, characters 2-67
                      Content: |core: string -> unit|
                      references: []
                      (eval#245:6-10 -> eval)
                      Range: File "", line 245, characters 6-10
                      Body Range: File "", line 245, characters 50-74
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 247, characters 59-63 ,
                        File "", line 358, characters 32-36 ,
                        File "", line 363, characters 34-38 ,
                        File "", line 377, characters 12-16 ,
                        File "", line 387, characters 12-16 ,
                        File "", line 394, characters 12-16
                      (failwith#249:6-14 -> failwith)
                      Range: File "", line 249, characters 6-14
                      Body Range: File "", line 249, characters 40-72
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 472, characters 51-59 ,
                        File "", line 473, characters 74-82 ,
                        File "", line 474, characters 89-97 ,
                        File "", line 476, characters 68-76 ,
                        File "", line 477, characters 98-106 ,
                        File "", line 478, characters 113-121
                      (get_balance#253:6-17 -> get_balance)
                      Range: File "", line 253, characters 6-17
                      Body Range: File "", line 253, characters 2-75
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#261:6-27 -> get_bootstrap_account)
                      Range: File "", line 261, characters 6-27
                      Body Range: File "", line 261, characters 2-105
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#330:6-26 -> get_last_events_from)
                      Range: File "", line 330, characters 6-26
                      Body Range: File "", line 331, character 4 to line 337, character 38
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#282:6-17 -> get_storage)
                      Range: File "", line 282, characters 6-17
                      Body Range: File "", line 283, character 4 to line 286, character 21
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#252:6-28 -> get_storage_of_address)
                      Range: File "", line 252, characters 6-28
                      Body Range: File "", line 252, characters 2-111
                      Content: |core: address -> michelson_program|
                      references: File "", line 285, characters 32-54
                      (get_time#270:6-14 -> get_time)
                      Range: File "", line 270, characters 6-14
                      Body Range: File "", line 270, characters 2-57
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#248:6-28 -> get_total_voting_power)
                      Range: File "", line 248, characters 6-28
                      Body Range: File "", line 248, characters 2-96
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#256:6-22 -> get_voting_power)
                      Range: File "", line 256, characters 6-22
                      Body Range: File "", line 256, characters 2-88
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#263:6-23 -> last_originations)
                      Range: File "", line 263, characters 6-23
                      Body Range: File "", line 263, characters 2-108
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#340:6-9 -> log)
                      Range: File "", line 340, characters 6-9
                      Body Range: File "", line 341, character 4 to line 343, character 11
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 369, characters 25-28
                      (michelson_equal#365:6-21 -> michelson_equal)
                      Range: File "", line 365, characters 6-21
                      Body Range: File "", line 365, characters 2-88
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#347:6-18 -> mutate_value)
                      Range: File "", line 347, characters 6-18
                      Body Range: File "", line 347, characters 72-111
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 411, characters 23-35 ,
                        File "", line 423, characters 23-35
                      (mutation_test#407:6-19 -> mutation_test)
                      Range: File "", line 407, characters 6-19
                      Body Range: File "", line 408, character 4 to line 418, character 19
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#419:6-23 -> mutation_test_all)
                      Range: File "", line 419, characters 6-23
                      Body Range: File "", line 420, character 4 to line 430, character 46
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (new_account#267:6-17 -> new_account)
                      Range: File "", line 267, characters 6-17
                      Body Range: File "", line 267, characters 2-81
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#305:6-8 -> nl)
                      Range: File "", line 305, characters 6-8
                      Body Range: File "", line 305, characters 11-53
                      Content: |unresolved|
                      references: File "", line 307, characters 15-17
                      (nth_bootstrap_account#258:6-27 -> nth_bootstrap_account)
                      Range: File "", line 258, characters 6-27
                      Body Range: File "", line 258, character 2 to line 260, character 5
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#257:6-28 -> nth_bootstrap_contract)
                      Range: File "", line 257, characters 6-28
                      Body Range: File "", line 257, characters 2-97
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#262:6-33 -> nth_bootstrap_typed_address)
                      Range: File "", line 262, characters 6-33
                      Body Range: File "", line 262, characters 80-131
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#375:6-15 -> originate)
                      Range: File "", line 375, characters 6-15
                      Body Range: File "", line 376, character 4 to line 381, character 13
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_contract#374:6-24 -> originate_contract)
                      Range: File "", line 374, characters 6-24
                      Body Range: File "", line 374, characters 2-135
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 378, characters 12-30 ,
                        File "", line 388, characters 12-30 ,
                        File "", line 395, characters 12-30 ,
                        File "", line 404, characters 12-30 ,
                        File "", line 435, characters 14-32 ,
                        File "", line 455, characters 14-32
                      (originate_from_file#402:6-25 -> originate_from_file)
                      Range: File "", line 402, characters 6-25
                      Body Range: File "", line 402, character 2 to line 406, character 13
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_from_file_and_mutate#431:6-36 -> originate_from_file_and_mutate)
                      Range: File "", line 431, characters 6-36
                      Body Range: File "", line 433, character 4 to line 450, character 19
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#451:6-40 -> originate_from_file_and_mutate_all)
                      Range: File "", line 451, characters 6-40
                      Body Range: File "", line 453, character 4 to line 470, character 46
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module#392:6-22 -> originate_module)
                      Range: File "", line 392, characters 6-22
                      Body Range: File "", line 393, character 4 to line 398, character 13
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) ->
                                                                ( list (operation) *
                                                                  s ) *
                                                                views (s) ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_uncurried#385:6-25 -> originate_uncurried)
                      Range: File "", line 385, characters 6-25
                      Body Range: File "", line 386, character 4 to line 391, character 13
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (parse_michelson#276:6-21 -> parse_michelson)
                      Range: File "", line 276, characters 6-21
                      Body Range: File "", line 276, characters 2-102
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#254:6-11 -> print)
                      Range: File "", line 254, characters 6-11
                      Body Range: File "", line 254, characters 2-66
                      Content: |core: string -> unit|
                      references:
                        File "", line 307, characters 4-9 ,
                        File "", line 343, characters 4-9
                      (println#306:6-13 -> println)
                      Range: File "", line 306, characters 6-13
                      Body Range: File "", line 306, character 2 to line 307, character 18
                      Content: |core: string -> unit|
                      references: []
                      (random#264:6-12 -> random)
                      Range: File "", line 264, characters 6-12
                      Body Range: File "", line 265, character 4 to line 266, character 42
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#294:6-29 -> read_contract_from_file)
                      Range: File "", line 294, characters 6-29
                      Body Range: File "", line 294, characters 2-115
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#273:6-23 -> register_constant)
                      Range: File "", line 273, characters 6-23
                      Body Range: File "", line 273, characters 2-100
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#272:6-23 -> register_delegate)
                      Range: File "", line 272, characters 6-23
                      Body Range: File "", line 272, characters 2-91
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#344:6-17 -> reset_state)
                      Range: File "", line 344, characters 6-17
                      Body Range: File "", line 344, characters 2-117
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#345:6-20 -> reset_state_at)
                      Range: File "", line 345, characters 6-20
                      Body Range: File "", line 345, characters 2-117
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#277:6-21 -> restore_context)
                      Range: File "", line 277, characters 6-21
                      Body Range: File "", line 277, characters 2-77
                      Content: |core: unit -> unit|
                      references: []
                      (run#244:6-9 -> run)
                      Range: File "", line 244, characters 6-9
                      Body Range: File "", line 244, characters 64-94
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 245, characters 50-53
                      (save_context#278:6-18 -> save_context)
                      Range: File "", line 278, characters 6-18
                      Body Range: File "", line 278, characters 2-75
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#348:6-19 -> save_mutation)
                      Range: File "", line 348, characters 6-19
                      Body Range: File "", line 348, characters 2-106
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#288:6-15 -> set_baker)
                      Range: File "", line 288, characters 6-15
                      Body Range: File "", line 288, characters 2-70
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#287:6-22 -> set_baker_policy)
                      Range: File "", line 287, characters 6-22
                      Body Range: File "", line 287, characters 2-91
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 288, characters 39-55
                      (set_big_map#352:6-17 -> set_big_map)
                      Range: File "", line 352, characters 6-17
                      Body Range: File "", line 352, characters 69-107
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#309:6-22 -> set_print_values)
                      Range: File "", line 309, characters 6-22
                      Body Range: File "", line 309, characters 2-100
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#251:6-16 -> set_source)
                      Range: File "", line 251, characters 6-16
                      Body Range: File "", line 251, characters 2-74
                      Content: |core: address -> unit|
                      references: []
                      (sign#349:6-10 -> sign)
                      Range: File "", line 349, characters 6-10
                      Body Range: File "", line 349, characters 2-83
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#289:6-10 -> size)
                      Range: File "", line 289, characters 6-10
                      Body Range: File "", line 289, characters 2-72
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 379, characters 12-16 ,
                        File "", line 389, characters 12-16 ,
                        File "", line 396, characters 12-16 ,
                        File "", line 405, characters 12-16 ,
                        File "", line 436, characters 14-18 ,
                        File "", line 456, characters 14-18
                      (to_contract#250:6-17 -> to_contract)
                      Range: File "", line 250, characters 6-17
                      Body Range: File "", line 250, characters 71-106
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 283, characters 25-36 ,
                        File "", line 331, characters 30-41
                      (to_entrypoint#366:6-19 -> to_entrypoint)
                      Range: File "", line 366, characters 6-19
                      Body Range: File "", line 367, character 4 to line 373, character 44
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#281:6-13 -> to_json)
                      Range: File "", line 281, characters 6-13
                      Body Range: File "", line 281, characters 42-78
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#280:6-15 -> to_string)
                      Range: File "", line 280, characters 6-15
                      Body Range: File "", line 280, characters 44-80
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 298, characters 68-77 ,
                        File "", line 300, characters 67-76 ,
                        File "", line 302, characters 61-70 ,
                        File "", line 342, characters 12-21
                      (to_typed_address#274:6-22 -> to_typed_address)
                      Range: File "", line 274, characters 6-22
                      Body Range: File "", line 274, characters 76-116
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#338:6-14 -> transfer)
                      Range: File "", line 338, characters 6-14
                      Body Range: File "", line 338, characters 2-162
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#339:6-18 -> transfer_exn)
                      Range: File "", line 339, characters 6-18
                      Body Range: File "", line 339, characters 2-157
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#355:6-26 -> transfer_to_contract)
                      Range: File "", line 355, characters 6-26
                      Body Range: File "", line 356, character 4 to line 359, character 61
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#360:6-30 -> transfer_to_contract_exn)
                      Range: File "", line 360, characters 6-30
                      Body Range: File "", line 361, character 6 to line 364, character 67
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#310:6-24 -> unset_print_values)
                      Range: File "", line 310, characters 6-24
                      Body Range: File "", line 310, characters 2-103
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#312:9-12 -> PBT)
                      Range: File "", line 312, characters 9-12
                      Body Range: File "", line 313, character 4 to line 327, character 7
                      Content: Members: Variable definitions:
                                        (gen#313:8-11 -> gen)
                                        Range: File "", line 313, characters 8-11
                                        Body Range: File "", line 313, characters 35-69
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (gen_small#314:8-17 -> gen_small)
                                        Range: File "", line 314, characters 8-17
                                        Body Range: File "", line 314, characters 41-74
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#315:8-17 -> make_test)
                                        Range: File "", line 315, characters 8-17
                                        Body Range: File "", line 315, characters 75-79
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#316:8-11 -> run)
                                        Range: File "", line 316, characters 8-11
                                        Body Range: File "", line 317, character 6 to line 327, character 7
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#7:7-12 -> Tezos)
    Range: File "", line 7, characters 7-12
    Body Range: File "", line 9, character 2 to line 74, character 20
    Content: Members: Variable definitions:
                      (address#20:6-13 -> address)
                      Range: File "", line 20, characters 6-13
                      Body Range: File "", line 20, characters 52-110
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 331, characters 21-28
                      (call_view#55:25-34 -> call_view)
                      Range: File "", line 55, characters 25-34
                      Body Range: File "", line 56, character 4 to line 57, character 123
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (constant#31:25-33 -> constant)
                      Range: File "", line 31, characters 25-33
                      Body Range: File "", line 31, characters 62-96
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (create_contract#60:25-40 -> create_contract)
                      Range: File "", line 60, characters 25-40
                      Body Range: File "", line 61, character 6 to line 62, character 58
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: []
                      (create_contract_uncurried#63:25-50 -> create_contract_uncurried)
                      Range: File "", line 63, characters 25-50
                      Body Range: File "", line 64, characters 6-50
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      (create_ticket#47:6-19 -> create_ticket)
                      Range: File "", line 47, characters 6-19
                      Body Range: File "", line 47, characters 69-147
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references: []
                      (emit#71:25-29 -> emit)
                      Range: File "", line 71, characters 25-29
                      Body Range: File "", line 72, character 4 to line 73, character 99
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      (get_amount#10:6-16 -> get_amount)
                      Range: File "", line 10, characters 6-16
                      Body Range: File "", line 10, characters 2-92
                      Content: |core: unit -> tez|
                      references: []
                      (get_balance#9:6-17 -> get_balance)
                      Range: File "", line 9, characters 6-17
                      Body Range: File "", line 9, characters 2-94
                      Content: |core: unit -> tez|
                      references: []
                      (get_chain_id#16:6-18 -> get_chain_id)
                      Range: File "", line 16, characters 6-18
                      Body Range: File "", line 16, characters 2-106
                      Content: |core: unit -> chain_id|
                      references: []
                      (get_contract#36:25-37 -> get_contract)
                      Range: File "", line 36, characters 25-37
                      Body Range: File "", line 37, character 4 to line 38, character 68
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      (get_contract_opt#34:25-41 -> get_contract_opt)
                      Range: File "", line 34, characters 25-41
                      Body Range: File "", line 35, characters 4-94
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 37, characters 12-28 ,
                        File "", line 41, characters 12-28
                      (get_contract_with_error#40:6-29 -> get_contract_with_error)
                      Range: File "", line 40, characters 6-29
                      Body Range: File "", line 41, character 4 to line 42, character 39
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: []
                      (get_entrypoint#68:25-39 -> get_entrypoint)
                      Range: File "", line 68, characters 25-39
                      Body Range: File "", line 69, character 4 to line 70, character 70
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (get_entrypoint_opt#65:25-43 -> get_entrypoint_opt)
                      Range: File "", line 65, characters 25-43
                      Body Range: File "", line 66, character 4 to line 67, character 119
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 69, characters 12-30
                      (get_level#14:6-15 -> get_level)
                      Range: File "", line 14, characters 6-15
                      Body Range: File "", line 14, characters 2-90
                      Content: |core: unit -> nat|
                      references: []
                      (get_min_block_time#18:6-24 -> get_min_block_time)
                      Range: File "", line 18, characters 6-24
                      Body Range: File "", line 18, characters 2-108
                      Content: |core: unit -> nat|
                      references: []
                      (get_now#11:6-13 -> get_now)
                      Range: File "", line 11, characters 6-13
                      Body Range: File "", line 11, characters 2-98
                      Content: |core: unit -> timestamp|
                      references: File "", line 270, characters 47-54
                      (get_self_address#15:6-22 -> get_self_address)
                      Range: File "", line 15, characters 6-22
                      Body Range: File "", line 15, characters 2-112
                      Content: |core: unit -> address|
                      references: []
                      (get_sender#12:6-16 -> get_sender)
                      Range: File "", line 12, characters 6-16
                      Body Range: File "", line 12, characters 2-100
                      Content: |core: unit -> address|
                      references: []
                      (get_source#13:6-16 -> get_source)
                      Range: File "", line 13, characters 6-16
                      Body Range: File "", line 13, characters 2-100
                      Content: |core: unit -> address|
                      references: []
                      (get_total_voting_power#17:6-28 -> get_total_voting_power)
                      Range: File "", line 17, characters 6-28
                      Body Range: File "", line 17, characters 2-116
                      Content: |core: unit -> nat|
                      references: []
                      (implicit_account#21:6-22 -> implicit_account)
                      Range: File "", line 21, characters 6-22
                      Body Range: File "", line 21, characters 2-129
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      (join_tickets#22:6-18 -> join_tickets)
                      Range: File "", line 22, characters 6-18
                      Body Range: File "", line 22, characters 76-156
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      (never#25:6-11 -> never)
                      Range: File "", line 25, characters 6-11
                      Body Range: File "", line 25, characters 39-84
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      (pairing_check#26:6-19 -> pairing_check)
                      Range: File "", line 26, characters 6-19
                      Body Range: File "", line 26, characters 2-155
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      (read_ticket#23:6-17 -> read_ticket)
                      Range: File "", line 23, characters 6-17
                      Body Range: File "", line 24, characters 4-96
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address *
                                                                    ( a * nat ) ) *
                                                                  ticket (a) )|
                      references: []
                      (sapling_empty_state#32:25-44 -> sapling_empty_state)
                      Range: File "", line 32, characters 25-44
                      Body Range: File "", line 33, characters 4-114
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      (sapling_verify_update#74:25-46 -> sapling_verify_update)
                      Range: File "", line 74, characters 25-46
                      Body Range: File "", line 74, characters 167-331
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes *
                        ( int * sapling_state (sap_a) ) ))|
                      references: []
                      (self#28:25-29 -> self)
                      Range: File "", line 28, characters 25-29
                      Body Range: File "", line 29, character 4 to line 30, character 91
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      (set_delegate#27:6-18 -> set_delegate)
                      Range: File "", line 27, characters 6-18
                      Body Range: File "", line 27, characters 2-125
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      (split_ticket#58:6-18 -> split_ticket)
                      Range: File "", line 58, characters 6-18
                      Body Range: File "", line 59, characters 4-114
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      (transaction#49:6-17 -> transaction)
                      Range: File "", line 49, characters 6-17
                      Body Range: File "", line 50, characters 4-109
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: []
                      (voting_power#19:6-18 -> voting_power)
                      Range: File "", line 19, characters 6-18
                      Body Range: File "", line 19, characters 2-101
                      Content: |core: key_hash -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 270, characters 41-46 ,
      File "", line 331, characters 15-20

    (Transpiled#118:7-17 -> Transpiled)
    Range: File "", line 118, characters 7-17
    Body Range: File "", line 119, character 2 to line 121, character 228
    Content: Members: Variable definitions:
                      (map_add#120:6-13 -> map_add)
                      Range: File "", line 120, characters 6-13
                      Body Range: File "", line 120, characters 82-198
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      (map_find_opt#119:6-18 -> map_find_opt)
                      Range: File "", line 119, characters 6-18
                      Body Range: File "", line 119, characters 79-163
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      (map_remove#121:6-16 -> map_remove)
                      Range: File "", line 121, characters 6-16
                      Body Range: File "", line 121, characters 75-228
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: [] |}]
