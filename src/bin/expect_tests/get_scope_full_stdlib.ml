open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#310:9-12 Set#114:7-10 String#153:7-13 Test#240:7-11 Tezos#7:7-12 Transpiled#108:7-17 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#348:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#468:6-12 assert_none#195:4-15 assert_none#470:6-17 assert_none_with_error#216:4-26 assert_none_with_error#474:6-28 assert_some#194:4-15 assert_some#469:6-17 assert_some_with_error#215:4-26 assert_some_with_error#473:6-28 assert_with_error#214:4-21 assert_with_error#472:6-23 bake_until_n_cycle_end#267:6-28 baker_account#349:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#344:6-24 bytes#208:4-9 call_view#46:25-34 cardinal#117:6-14 cast_address#269:6-18 check#190:6-11 chr#293:6-9 compile_contract#288:6-22 compile_contract_from_file#395:6-32 compile_contract_with_views#378:8-35 compile_value#245:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#273:6-35 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#211:4-9 decompile#266:6-15 drop_context#277:6-18 ediv#217:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#253:6-12 eval#243:6-10 failwith#247:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#311:8-11 gen_small#312:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#251:6-17 get_balance#9:6-17 get_bootstrap_account#259:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#328:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#280:6-17 get_storage_of_address#250:6-28 get_time#268:6-14 get_total_voting_power#17:6-28 get_total_voting_power#246:6-28 get_voting_power#254:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#210:4-10 implicit_account#21:6-22 int#202:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#261:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#338:6-9 make_test#313:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#361:6-21 mutate_value#345:6-18 mutation_test#403:6-19 mutation_test_all#415:6-23 nat#207:4-7 never#25:6-11 new_account#265:6-17 nl#303:6-8 nth_bootstrap_account#256:6-27 nth_bootstrap_contract#255:6-28 nth_bootstrap_typed_address#260:6-33 option#5:8-14 or#71:6-9 originate#371:6-15 originate_contract#370:6-24 originate_from_file#398:6-25 originate_from_file_and_mutate#427:6-36 originate_from_file_and_mutate_all#447:6-40 originate_module#388:6-22 originate_uncurried#381:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#274:6-21 pbt_result#236:8-18 pbt_test#235:8-16 print#252:6-11 println#304:6-13 random#262:6-12 read_contract_from_file#292:6-29 read_ticket#23:6-17 register_constant#271:6-23 register_delegate#270:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#342:6-17 reset_state_at#343:6-20 restore_context#275:6-21 run#242:6-9 run#314:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#276:6-18 save_mutation#346:6-19 self#28:25-29 set_baker#286:6-15 set_baker_policy#285:6-22 set_big_map#350:6-17 set_delegate#27:6-18 set_print_values#307:6-22 set_source#249:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#347:6-10 size#116:6-10 size#133:6-10 size#287:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#230:5-22 test_exec_error#223:5-20 test_exec_error_balance_too_low#220:5-36 test_exec_result#228:5-21 to_contract#248:6-17 to_entrypoint#362:6-19 to_json#279:6-13 to_string#278:6-15 to_typed_address#272:6-22 transaction#44:6-17 transfer#336:6-14 transfer_exn#337:6-18 transfer_to_contract#351:6-26 transfer_to_contract_exn#356:6-30 true#198:4-8 uncurry#212:4-11 unforged_ticket#238:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#308:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#310:9-12 Set#114:7-10 String#153:7-13 Test#240:7-11 Tezos#7:7-12 Transpiled#108:7-17 a#1:4-5 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#348:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#468:6-12 assert_none#195:4-15 assert_none#470:6-17 assert_none_with_error#216:4-26 assert_none_with_error#474:6-28 assert_some#194:4-15 assert_some#469:6-17 assert_some_with_error#215:4-26 assert_some_with_error#473:6-28 assert_with_error#214:4-21 assert_with_error#472:6-23 bake_until_n_cycle_end#267:6-28 baker_account#349:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#344:6-24 bytes#208:4-9 c#5:10-11 call_view#46:25-34 cardinal#117:6-14 cast_address#269:6-18 check#190:6-11 chr#293:6-9 compile_contract#288:6-22 compile_contract_from_file#395:6-32 compile_contract_with_views#378:8-35 compile_value#245:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#273:6-35 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#211:4-9 decompile#266:6-15 drop_context#277:6-18 ediv#217:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#253:6-12 eval#243:6-10 failwith#247:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#311:8-11 gen_small#312:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#251:6-17 get_balance#9:6-17 get_bootstrap_account#259:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#328:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#280:6-17 get_storage_of_address#250:6-28 get_time#268:6-14 get_total_voting_power#17:6-28 get_total_voting_power#246:6-28 get_voting_power#254:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#210:4-10 implicit_account#21:6-22 int#202:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#261:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#338:6-9 make_test#313:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#361:6-21 mutate_value#345:6-18 mutation_test#403:6-19 mutation_test_all#415:6-23 nat#207:4-7 never#25:6-11 new_account#265:6-17 nl#303:6-8 nth_bootstrap_account#256:6-27 nth_bootstrap_contract#255:6-28 nth_bootstrap_typed_address#260:6-33 option#5:8-14 or#71:6-9 originate#371:6-15 originate_contract#370:6-24 originate_from_file#398:6-25 originate_from_file_and_mutate#427:6-36 originate_from_file_and_mutate_all#447:6-40 originate_module#388:6-22 originate_uncurried#381:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#274:6-21 pbt_result#236:8-18 pbt_test#235:8-16 print#252:6-11 println#304:6-13 random#262:6-12 read_contract_from_file#292:6-29 read_ticket#23:6-17 register_constant#271:6-23 register_delegate#270:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#342:6-17 reset_state_at#343:6-20 restore_context#275:6-21 run#242:6-9 run#314:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#276:6-18 save_mutation#346:6-19 self#28:25-29 set_baker#286:6-15 set_baker_policy#285:6-22 set_big_map#350:6-17 set_delegate#27:6-18 set_print_values#307:6-22 set_source#249:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#347:6-10 size#116:6-10 size#133:6-10 size#287:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#230:5-22 test_exec_error#223:5-20 test_exec_error_balance_too_low#220:5-36 test_exec_result#228:5-21 to_contract#248:6-17 to_entrypoint#362:6-19 to_json#279:6-13 to_string#278:6-15 to_typed_address#272:6-22 transaction#44:6-17 transfer#336:6-14 transfer_exn#337:6-18 transfer_to_contract#351:6-26 transfer_to_contract_exn#356:6-30 true#198:4-8 uncurry#212:4-11 unforged_ticket#238:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#308:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#310:9-12 Set#114:7-10 String#153:7-13 Test#240:7-11 Tezos#7:7-12 Transpiled#108:7-17 a#1:4-5 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#348:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#468:6-12 assert_none#195:4-15 assert_none#470:6-17 assert_none_with_error#216:4-26 assert_none_with_error#474:6-28 assert_some#194:4-15 assert_some#469:6-17 assert_some_with_error#215:4-26 assert_some_with_error#473:6-28 assert_with_error#214:4-21 assert_with_error#472:6-23 bake_until_n_cycle_end#267:6-28 baker_account#349:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#344:6-24 bytes#208:4-9 c#5:10-11 call_view#46:25-34 cardinal#117:6-14 cast_address#269:6-18 check#190:6-11 chr#293:6-9 compile_contract#288:6-22 compile_contract_from_file#395:6-32 compile_contract_with_views#378:8-35 compile_value#245:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#273:6-35 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#211:4-9 d#5:26-27 decompile#266:6-15 drop_context#277:6-18 ediv#217:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#253:6-12 eval#243:6-10 failwith#247:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#311:8-11 gen_small#312:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#251:6-17 get_balance#9:6-17 get_bootstrap_account#259:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#328:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#280:6-17 get_storage_of_address#250:6-28 get_time#268:6-14 get_total_voting_power#17:6-28 get_total_voting_power#246:6-28 get_voting_power#254:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#210:4-10 implicit_account#21:6-22 int#202:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#261:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#338:6-9 make_test#313:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#361:6-21 mutate_value#345:6-18 mutation_test#403:6-19 mutation_test_all#415:6-23 nat#207:4-7 never#25:6-11 new_account#265:6-17 nl#303:6-8 nth_bootstrap_account#256:6-27 nth_bootstrap_contract#255:6-28 nth_bootstrap_typed_address#260:6-33 option#5:8-14 or#71:6-9 originate#371:6-15 originate_contract#370:6-24 originate_from_file#398:6-25 originate_from_file_and_mutate#427:6-36 originate_from_file_and_mutate_all#447:6-40 originate_module#388:6-22 originate_uncurried#381:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#274:6-21 pbt_result#236:8-18 pbt_test#235:8-16 print#252:6-11 println#304:6-13 random#262:6-12 read_contract_from_file#292:6-29 read_ticket#23:6-17 register_constant#271:6-23 register_delegate#270:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#342:6-17 reset_state_at#343:6-20 restore_context#275:6-21 run#242:6-9 run#314:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#276:6-18 save_mutation#346:6-19 self#28:25-29 set_baker#286:6-15 set_baker_policy#285:6-22 set_big_map#350:6-17 set_delegate#27:6-18 set_print_values#307:6-22 set_source#249:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#347:6-10 size#116:6-10 size#133:6-10 size#287:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#230:5-22 test_exec_error#223:5-20 test_exec_error_balance_too_low#220:5-36 test_exec_result#228:5-21 to_contract#248:6-17 to_entrypoint#362:6-19 to_json#279:6-13 to_string#278:6-15 to_typed_address#272:6-22 transaction#44:6-17 transfer#336:6-14 transfer_exn#337:6-18 transfer_to_contract#351:6-26 transfer_to_contract_exn#356:6-30 true#198:4-8 uncurry#212:4-11 unforged_ticket#238:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#308:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#310:9-12 Set#114:7-10 String#153:7-13 Test#240:7-11 Tezos#7:7-12 Transpiled#108:7-17 a#1:4-5 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#348:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#468:6-12 assert_none#195:4-15 assert_none#470:6-17 assert_none_with_error#216:4-26 assert_none_with_error#474:6-28 assert_some#194:4-15 assert_some#469:6-17 assert_some_with_error#215:4-26 assert_some_with_error#473:6-28 assert_with_error#214:4-21 assert_with_error#472:6-23 bake_until_n_cycle_end#267:6-28 baker_account#349:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#344:6-24 bytes#208:4-9 call_view#46:25-34 cardinal#117:6-14 cast_address#269:6-18 check#190:6-11 chr#293:6-9 compile_contract#288:6-22 compile_contract_from_file#395:6-32 compile_contract_with_views#378:8-35 compile_value#245:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#273:6-35 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#211:4-9 decompile#266:6-15 drop_context#277:6-18 e#6:9-10 ediv#217:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#253:6-12 eval#243:6-10 failwith#247:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#311:8-11 gen_small#312:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#251:6-17 get_balance#9:6-17 get_bootstrap_account#259:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#328:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#280:6-17 get_storage_of_address#250:6-28 get_time#268:6-14 get_total_voting_power#17:6-28 get_total_voting_power#246:6-28 get_voting_power#254:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#210:4-10 implicit_account#21:6-22 int#202:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#261:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#338:6-9 make_test#313:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#361:6-21 mutate_value#345:6-18 mutation_test#403:6-19 mutation_test_all#415:6-23 nat#207:4-7 never#25:6-11 new_account#265:6-17 nl#303:6-8 nth_bootstrap_account#256:6-27 nth_bootstrap_contract#255:6-28 nth_bootstrap_typed_address#260:6-33 option#5:8-14 or#71:6-9 originate#371:6-15 originate_contract#370:6-24 originate_from_file#398:6-25 originate_from_file_and_mutate#427:6-36 originate_from_file_and_mutate_all#447:6-40 originate_module#388:6-22 originate_uncurried#381:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#274:6-21 pbt_result#236:8-18 pbt_test#235:8-16 print#252:6-11 println#304:6-13 random#262:6-12 read_contract_from_file#292:6-29 read_ticket#23:6-17 register_constant#271:6-23 register_delegate#270:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#342:6-17 reset_state_at#343:6-20 restore_context#275:6-21 run#242:6-9 run#314:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#276:6-18 save_mutation#346:6-19 self#28:25-29 set_baker#286:6-15 set_baker_policy#285:6-22 set_big_map#350:6-17 set_delegate#27:6-18 set_print_values#307:6-22 set_source#249:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#347:6-10 size#116:6-10 size#133:6-10 size#287:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#230:5-22 test_exec_error#223:5-20 test_exec_error_balance_too_low#220:5-36 test_exec_result#228:5-21 to_contract#248:6-17 to_entrypoint#362:6-19 to_json#279:6-13 to_string#278:6-15 to_typed_address#272:6-22 transaction#44:6-17 transfer#336:6-14 transfer_exn#337:6-18 transfer_to_contract#351:6-26 transfer_to_contract_exn#356:6-30 true#198:4-8 uncurry#212:4-11 unforged_ticket#238:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#308:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#310:9-12 Set#114:7-10 String#153:7-13 Test#240:7-11 Tezos#7:7-12 Transpiled#108:7-17 a#1:4-5 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#348:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#468:6-12 assert_none#195:4-15 assert_none#470:6-17 assert_none_with_error#216:4-26 assert_none_with_error#474:6-28 assert_some#194:4-15 assert_some#469:6-17 assert_some_with_error#215:4-26 assert_some_with_error#473:6-28 assert_with_error#214:4-21 assert_with_error#472:6-23 bake_until_n_cycle_end#267:6-28 baker_account#349:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#344:6-24 bytes#208:4-9 call_view#46:25-34 cardinal#117:6-14 cast_address#269:6-18 check#190:6-11 chr#293:6-9 compile_contract#288:6-22 compile_contract_from_file#395:6-32 compile_contract_with_views#378:8-35 compile_value#245:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#273:6-35 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#211:4-9 decompile#266:6-15 drop_context#277:6-18 ediv#217:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#253:6-12 eval#243:6-10 failwith#247:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#311:8-11 gen_small#312:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#251:6-17 get_balance#9:6-17 get_bootstrap_account#259:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#328:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#280:6-17 get_storage_of_address#250:6-28 get_time#268:6-14 get_total_voting_power#17:6-28 get_total_voting_power#246:6-28 get_voting_power#254:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#210:4-10 implicit_account#21:6-22 int#202:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#261:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#338:6-9 make_test#313:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#361:6-21 mutate_value#345:6-18 mutation_test#403:6-19 mutation_test_all#415:6-23 nat#207:4-7 never#25:6-11 new_account#265:6-17 nl#303:6-8 nth_bootstrap_account#256:6-27 nth_bootstrap_contract#255:6-28 nth_bootstrap_typed_address#260:6-33 option#5:8-14 or#71:6-9 originate#371:6-15 originate_contract#370:6-24 originate_from_file#398:6-25 originate_from_file_and_mutate#427:6-36 originate_from_file_and_mutate_all#447:6-40 originate_module#388:6-22 originate_uncurried#381:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#274:6-21 pbt_result#236:8-18 pbt_test#235:8-16 print#252:6-11 println#304:6-13 random#262:6-12 read_contract_from_file#292:6-29 read_ticket#23:6-17 register_constant#271:6-23 register_delegate#270:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#342:6-17 reset_state_at#343:6-20 restore_context#275:6-21 run#242:6-9 run#314:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#276:6-18 save_mutation#346:6-19 self#28:25-29 set_baker#286:6-15 set_baker_policy#285:6-22 set_big_map#350:6-17 set_delegate#27:6-18 set_print_values#307:6-22 set_source#249:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#347:6-10 size#116:6-10 size#133:6-10 size#287:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#230:5-22 test_exec_error#223:5-20 test_exec_error_balance_too_low#220:5-36 test_exec_result#228:5-21 to_contract#248:6-17 to_entrypoint#362:6-19 to_json#279:6-13 to_string#278:6-15 to_typed_address#272:6-22 transaction#44:6-17 transfer#336:6-14 transfer_exn#337:6-18 transfer_to_contract#351:6-26 transfer_to_contract_exn#356:6-30 true#198:4-8 uncurry#212:4-11 unforged_ticket#238:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#308:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33

    Variable definitions:
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#196:4-7 -> abs)
    Range: File "", line 196, characters 4-7
    Body Range: File "", line 196, characters 0-69
    Content: |core: int -> nat|
    references: File "", line 366, characters 31-34
    (assert#193:4-10 -> assert)
    Range: File "", line 193, characters 4-10
    Body Range: File "", line 193, characters 0-76
    Content: |core: bool -> unit|
    references: []
    (assert_none#195:4-15 -> assert_none)
    Range: File "", line 195, characters 4-15
    Body Range: File "", line 195, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#216:4-26 -> assert_none_with_error)
    Range: File "", line 216, characters 4-26
    Body Range: File "", line 216, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#194:4-15 -> assert_some)
    Range: File "", line 194, characters 4-15
    Body Range: File "", line 194, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#215:4-26 -> assert_some_with_error)
    Range: File "", line 215, characters 4-26
    Body Range: File "", line 215, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#214:4-21 -> assert_with_error)
    Range: File "", line 214, characters 4-21
    Body Range: File "", line 214, characters 0-76
    Content: |unresolved|
    references: []
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (bytes#208:4-9 -> bytes)
    Range: File "", line 208, characters 4-9
    Body Range: File "", line 208, characters 48-104
    Content: |core: ∀ a : * . a -> external_bytes (a)|
    references: []
    (c#5:10-11 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (curry#211:4-9 -> curry)
    Range: File "", line 211, characters 4-9
    Body Range: File "", line 211, characters 62-70
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
    (ediv#217:4-8 -> ediv)
    Range: File "", line 217, characters 4-8
    Body Range: File "", line 217, characters 61-138
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
      File "", line 60, characters 27-35 ,
      File "", line 162, characters 79-87 ,
      File "", line 164, characters 103-111 ,
      File "", line 167, characters 83-91 ,
      File "", line 193, characters 49-57 ,
      File "", line 194, characters 72-80 ,
      File "", line 195, characters 87-95 ,
      File "", line 214, characters 66-74 ,
      File "", line 215, characters 96-104 ,
      File "", line 216, characters 111-119
    (false#199:4-9 -> false)
    Range: File "", line 199, characters 4-9
    Body Range: File "", line 199, characters 19-24
    Content: |core: bool|
    references:
      File "", line 263, characters 51-56 ,
      File "", line 308, characters 90-95 ,
      File "", line 311, characters 62-67
    (ignore#210:4-10 -> ignore)
    Range: File "", line 210, characters 4-10
    Body Range: File "", line 210, characters 37-39
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#202:4-7 -> int)
    Range: File "", line 202, characters 4-7
    Body Range: File "", line 202, characters 44-96
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 259, characters 97-100 ,
      File "", line 296, characters 79-82 ,
      File "", line 298, characters 78-81 ,
      File "", line 300, characters 72-75
    (is_nat#197:4-10 -> is_nat)
    Range: File "", line 197, characters 4-10
    Body Range: File "", line 197, characters 0-88
    Content: |core: int -> option (nat)|
    references: []
    (nat#207:4-7 -> nat)
    Range: File "", line 207, characters 4-7
    Body Range: File "", line 207, characters 0-73
    Content: |core: bytes -> nat|
    references: []
    (true#198:4-8 -> true)
    Range: File "", line 198, characters 4-8
    Body Range: File "", line 198, characters 18-22
    Content: |core: bool|
    references:
      File "", line 307, characters 88-92 ,
      File "", line 312, characters 68-72
    (uncurry#212:4-11 -> uncurry)
    Range: File "", line 212, characters 4-11
    Body Range: File "", line 212, characters 62-73
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 372, characters 30-37
    (unit#200:4-8 -> unit)
    Range: File "", line 200, characters 4-8
    Body Range: File "", line 200, characters 18-38
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
      File "", line 80, characters 52-56 ,
      File "", line 95, characters 48-52 ,
      File "", line 120, characters 41-45 ,
      File "", line 123, characters 35-39 ,
      File "", line 143, characters 34-38 ,
      File "", line 149, characters 37-41 ,
      File "", line 168, characters 40-44 ,
      File "", line 169, characters 40-44 ,
      File "", line 190, characters 52-56 ,
      File "", line 190, characters 145-149 ,
      File "", line 193, characters 16-20 ,
      File "", line 198, characters 11-15 ,
      File "", line 199, characters 12-16 ,
      File "", line 214, characters 27-31 ,
      File "", line 235, characters 41-45 ,
      File "", line 313, characters 53-57 ,
      File "", line 361, characters 74-78 ,
      File "", line 468, characters 18-22 ,
      File "", line 472, characters 29-33
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
      File "", line 43, characters 60-66 ,
      File "", line 43, characters 132-138 ,
      File "", line 46, characters 86-92 ,
      File "", line 47, characters 83-89 ,
      File "", line 48, characters 83-89 ,
      File "", line 49, characters 99-105 ,
      File "", line 50, characters 102-108 ,
      File "", line 53, characters 111-117 ,
      File "", line 55, characters 93-99 ,
      File "", line 57, characters 84-90 ,
      File "", line 64, characters 158-164 ,
      File "", line 64, characters 316-322 ,
      File "", line 83, characters 39-45 ,
      File "", line 84, characters 47-53 ,
      File "", line 84, characters 80-86 ,
      File "", line 85, characters 59-65 ,
      File "", line 98, characters 39-45 ,
      File "", line 99, characters 47-53 ,
      File "", line 99, characters 76-82 ,
      File "", line 101, characters 55-61 ,
      File "", line 127, characters 40-46 ,
      File "", line 134, characters 42-48 ,
      File "", line 135, characters 49-55 ,
      File "", line 143, characters 58-64 ,
      File "", line 144, characters 31-37 ,
      File "", line 145, characters 40-46 ,
      File "", line 147, characters 34-40 ,
      File "", line 162, characters 28-34 ,
      File "", line 164, characters 39-45 ,
      File "", line 165, characters 50-56 ,
      File "", line 165, characters 62-68 ,
      File "", line 166, characters 42-48 ,
      File "", line 167, characters 48-54 ,
      File "", line 168, characters 30-36 ,
      File "", line 169, characters 30-36 ,
      File "", line 175, characters 38-44 ,
      File "", line 175, characters 101-107 ,
      File "", line 194, characters 32-38 ,
      File "", line 195, characters 32-38 ,
      File "", line 197, characters 27-33 ,
      File "", line 197, characters 78-84 ,
      File "", line 215, characters 43-49 ,
      File "", line 216, characters 43-49 ,
      File "", line 293, characters 29-35 ,
      File "", line 336, characters 147-153 ,
      File "", line 337, characters 142-148 ,
      File "", line 342, characters 102-108 ,
      File "", line 345, characters 63-69 ,
      File "", line 346, characters 57-63 ,
      File "", line 349, characters 48-54 ,
      File "", line 353, characters 19-25 ,
      File "", line 358, characters 21-27 ,
      File "", line 396, characters 100-106 ,
      File "", line 403, characters 74-80 ,
      File "", line 406, characters 52-58 ,
      File "", line 428, characters 105-111 ,
      File "", line 434, characters 100-106 ,
      File "", line 437, characters 52-58 ,
      File "", line 454, characters 100-106 ,
      File "", line 469, characters 34-40 ,
      File "", line 470, characters 34-40 ,
      File "", line 473, characters 45-51 ,
      File "", line 474, characters 45-51
    (pbt_result#236:8-18 -> pbt_result)
    Range: File "", line 236, characters 8-18
    Body Range: File "", line 236, characters 21-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 314, characters 57-67 ,
      File "", line 315, characters 39-49 ,
      File "", line 317, characters 84-94 ,
      File "", line 321, characters 96-106 ,
      File "", line 324, characters 68-78
    (pbt_test#235:8-16 -> pbt_test)
    Range: File "", line 235, characters 8-16
    Body Range: File "", line 235, characters 19-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 313, characters 63-71 ,
      File "", line 314, characters 33-41
    (test_baker_policy#230:5-22 -> test_baker_policy)
    Range: File "", line 230, characters 5-22
    Body Range: File "", line 231, character 4 to line 233, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 285, characters 29-46
    (test_exec_error#223:5-20 -> test_exec_error)
    Range: File "", line 223, characters 5-20
    Body Range: File "", line 224, character 4 to line 226, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 228, characters 49-64
    (test_exec_error_balance_too_low#220:5-36 -> test_exec_error_balance_too_low)
    Range: File "", line 220, characters 5-36
    Body Range: File "", line 221, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 225, characters 23-54
    (test_exec_result#228:5-21 -> test_exec_result)
    Range: File "", line 228, characters 5-21
    Body Range: File "", line 228, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 336, characters 65-81 ,
      File "", line 351, characters 73-89
    (unforged_ticket#238:8-23 -> unforged_ticket)
    Range: File "", line 238, characters 8-23
    Body Range: File "", line 238, characters 26-40
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    Module definitions:
    (Big_map#76:7-14 -> Big_map)
    Range: File "", line 76, characters 7-14
    Body Range: File "", line 77, character 2 to line 86, character 87
    Content: Members: Variable definitions:
                      (add#81:6-9 -> add)
                      Range: File "", line 81, characters 6-9
                      Body Range: File "", line 81, characters 77-109
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (empty#77:16-21 -> empty)
                      Range: File "", line 77, characters 16-21
                      Body Range: File "", line 77, characters 52-81
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#86:6-10 -> find)
                      Range: File "", line 86, characters 6-10
                      Body Range: File "", line 86, characters 57-87
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      (find_opt#85:6-14 -> find_opt)
                      Range: File "", line 85, characters 6-14
                      Body Range: File "", line 85, characters 68-102
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (get_and_update#84:6-20 -> get_and_update)
                      Range: File "", line 84, characters 6-20
                      Body Range: File "", line 84, characters 106-153
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#78:25-32 -> literal)
                      Range: File "", line 78, characters 25-32
                      Body Range: File "", line 78, characters 82-116
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#80:6-9 -> mem)
                      Range: File "", line 80, characters 6-9
                      Body Range: File "", line 80, characters 59-88
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (remove#82:6-12 -> remove)
                      Range: File "", line 82, characters 6-12
                      Body Range: File "", line 82, characters 72-104
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#83:6-12 -> update)
                      Range: File "", line 83, characters 6-12
                      Body Range: File "", line 83, characters 87-122
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#68:7-14 -> Bitwise)
    Range: File "", line 68, characters 7-14
    Body Range: File "", line 69, character 2 to line 73, character 144
    Content: Members: Variable definitions:
                      (and#69:6-10 -> and)
                      Range: File "", line 69, characters 6-10
                      Body Range: File "", line 69, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (or#71:6-9 -> or)
                      Range: File "", line 71, characters 6-9
                      Body Range: File "", line 71, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_xor (a ,
                      b)|
                      references: []
                      (shift_left#72:6-16 -> shift_left)
                      Range: File "", line 72, characters 6-16
                      Body Range: File "", line 72, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsl (a ,
                      b)|
                      references: []
                      (shift_right#73:6-17 -> shift_right)
                      Range: File "", line 73, characters 6-17
                      Body Range: File "", line 73, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsr (a ,
                      b)|
                      references: []
                      (xor#70:6-9 -> xor)
                      Range: File "", line 70, characters 6-9
                      Body Range: File "", line 70, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_or (a ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#172:7-12 -> Bytes)
    Range: File "", line 172, characters 7-12
    Body Range: File "", line 173, character 2 to line 179, character 82
    Content: Members: Variable definitions:
                      (concat#178:6-12 -> concat)
                      Range: File "", line 178, characters 6-12
                      Body Range: File "", line 178, characters 2-79
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#173:6-13 -> concats)
                      Range: File "", line 173, characters 6-13
                      Body Range: File "", line 173, characters 2-69
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#176:6-12 -> length)
                      Range: File "", line 176, characters 6-12
                      Body Range: File "", line 176, characters 2-56
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#174:6-10 -> pack)
                      Range: File "", line 174, characters 6-10
                      Body Range: File "", line 174, characters 38-82
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#179:6-9 -> sub)
                      Range: File "", line 179, characters 6-9
                      Body Range: File "", line 179, characters 2-82
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#175:6-12 -> unpack)
                      Range: File "", line 175, characters 6-12
                      Body Range: File "", line 175, characters 47-122
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#182:7-13 -> Crypto)
    Range: File "", line 182, characters 7-13
    Body Range: File "", line 183, character 2 to line 190, character 161
    Content: Members: Variable definitions:
                      (blake2b#183:6-13 -> blake2b)
                      Range: File "", line 183, characters 6-13
                      Body Range: File "", line 183, characters 2-87
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#190:6-11 -> check)
                      Range: File "", line 190, characters 6-11
                      Body Range: File "", line 190, characters 2-161
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#188:6-14 -> hash_key)
                      Range: File "", line 188, characters 6-14
                      Body Range: File "", line 188, characters 2-91
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#187:6-12 -> keccak)
                      Range: File "", line 187, characters 6-12
                      Body Range: File "", line 187, characters 2-85
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#184:6-12 -> sha256)
                      Range: File "", line 184, characters 6-12
                      Body Range: File "", line 184, characters 2-85
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#186:6-10 -> sha3)
                      Range: File "", line 186, characters 6-10
                      Body Range: File "", line 186, characters 2-81
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#185:6-12 -> sha512)
                      Range: File "", line 185, characters 6-12
                      Body Range: File "", line 185, characters 2-85
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#131:7-11 -> List)
    Range: File "", line 131, characters 7-11
    Body Range: File "", line 132, character 2 to line 150, character 48
    Content: Members: Variable definitions:
                      (cons#142:6-10 -> cons)
                      Range: File "", line 142, characters 6-10
                      Body Range: File "", line 142, characters 53-80
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (filter_map#145:6-16 -> filter_map)
                      Range: File "", line 145, characters 6-16
                      Body Range: File "", line 146, characters 4-100
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      (find_opt#143:6-14 -> find_opt)
                      Range: File "", line 143, characters 6-14
                      Body Range: File "", line 144, characters 4-82
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#139:6-10 -> fold)
                      Range: File "", line 139, characters 6-10
                      Body Range: File "", line 139, characters 67-102
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 335, characters 9-13
                      (fold_left#140:6-15 -> fold_left)
                      Range: File "", line 140, characters 6-15
                      Body Range: File "", line 140, characters 72-112
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#141:6-16 -> fold_right)
                      Range: File "", line 141, characters 6-16
                      Body Range: File "", line 141, characters 73-114
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 144, characters 4-14 ,
                        File "", line 146, characters 4-14
                      (head_opt#134:6-14 -> head_opt)
                      Range: File "", line 134, characters 6-14
                      Body Range: File "", line 134, characters 51-98
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#138:6-10 -> iter)
                      Range: File "", line 138, characters 6-10
                      Body Range: File "", line 138, characters 58-90
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#132:6-12 -> length)
                      Range: File "", line 132, characters 6-12
                      Body Range: File "", line 132, characters 44-73
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#137:6-9 -> map)
                      Range: File "", line 137, characters 6-9
                      Body Range: File "", line 137, characters 59-90
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 148, characters 4-7 ,
                        File "", line 150, characters 4-7
                      (size#133:6-10 -> size)
                      Range: File "", line 133, characters 6-10
                      Body Range: File "", line 133, characters 42-71
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#135:6-14 -> tail_opt)
                      Range: File "", line 135, characters 6-14
                      Body Range: File "", line 135, characters 58-107
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      (update#147:6-12 -> update)
                      Range: File "", line 147, characters 6-12
                      Body Range: File "", line 148, characters 4-62
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      (update_with#149:6-17 -> update_with)
                      Range: File "", line 149, characters 6-17
                      Body Range: File "", line 150, characters 4-48
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 335, characters 4-8

    (Map#90:7-10 -> Map)
    Range: File "", line 90, characters 7-10
    Body Range: File "", line 91, character 2 to line 104, character 111
    Content: Members: Variable definitions:
                      (add#96:6-9 -> add)
                      Range: File "", line 96, characters 6-9
                      Body Range: File "", line 96, characters 69-101
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (empty#91:6-11 -> empty)
                      Range: File "", line 91, characters 6-11
                      Body Range: File "", line 91, characters 38-63
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#100:6-10 -> find)
                      Range: File "", line 100, characters 6-10
                      Body Range: File "", line 100, characters 53-83
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#101:6-14 -> find_opt)
                      Range: File "", line 101, characters 6-14
                      Body Range: File "", line 101, characters 64-98
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (fold#104:6-10 -> fold)
                      Range: File "", line 104, characters 6-10
                      Body Range: File "", line 104, characters 78-111
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      (get_and_update#99:6-20 -> get_and_update)
                      Range: File "", line 99, characters 6-20
                      Body Range: File "", line 99, characters 98-141
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (iter#102:6-10 -> iter)
                      Range: File "", line 102, characters 6-10
                      Body Range: File "", line 102, characters 68-98
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (literal#93:25-32 -> literal)
                      Range: File "", line 93, characters 25-32
                      Body Range: File "", line 93, characters 78-108
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#103:6-9 -> map)
                      Range: File "", line 103, characters 6-9
                      Body Range: File "", line 103, characters 72-101
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (mem#95:6-9 -> mem)
                      Range: File "", line 95, characters 6-9
                      Body Range: File "", line 95, characters 55-84
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (remove#97:6-12 -> remove)
                      Range: File "", line 97, characters 6-12
                      Body Range: File "", line 97, characters 64-96
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (size#92:6-10 -> size)
                      Range: File "", line 92, characters 6-10
                      Body Range: File "", line 92, characters 47-74
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#98:6-12 -> update)
                      Range: File "", line 98, characters 6-12
                      Body Range: File "", line 98, characters 79-114
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#161:7-13 -> Option)
    Range: File "", line 161, characters 7-13
    Body Range: File "", line 162, character 2 to line 169, character 77
    Content: Members: Variable definitions:
                      (is_none#168:6-13 -> is_none)
                      Range: File "", line 168, characters 6-13
                      Body Range: File "", line 168, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#169:6-13 -> is_some)
                      Range: File "", line 169, characters 6-13
                      Body Range: File "", line 169, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#165:15-18 -> map)
                      Range: File "", line 165, characters 15-18
                      Body Range: File "", line 165, characters 71-103
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#162:6-11 -> unopt)
                      Range: File "", line 162, characters 6-11
                      Body Range: File "", line 162, characters 42-104
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#164:6-22 -> unopt_with_error)
                      Range: File "", line 164, characters 6-22
                      Body Range: File "", line 164, characters 66-113
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#166:6-11 -> value)
                      Range: File "", line 166, characters 6-11
                      Body Range: File "", line 166, characters 56-100
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#167:6-15 -> value_exn)
                      Range: File "", line 167, characters 6-15
                      Body Range: File "", line 167, characters 62-109
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#114:7-10 -> Set)
    Range: File "", line 114, characters 7-10
    Body Range: File "", line 115, character 2 to line 128, character 110
    Content: Members: Variable definitions:
                      (add#121:6-9 -> add)
                      Range: File "", line 121, characters 6-9
                      Body Range: File "", line 121, characters 49-78
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: File "", line 128, characters 81-84
                      (cardinal#117:6-14 -> cardinal)
                      Range: File "", line 117, characters 6-14
                      Body Range: File "", line 117, characters 44-71
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#115:6-11 -> empty)
                      Range: File "", line 115, characters 6-11
                      Body Range: File "", line 115, characters 31-56
                      Content: |core: ∀ a : * . set (a)|
                      references: File "", line 128, characters 96-101
                      (filter_map#127:6-16 -> filter_map)
                      Range: File "", line 127, characters 6-16
                      Body Range: File "", line 128, characters 4-110
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> set (a) -> set (b)|
                      references: []
                      (fold#125:6-10 -> fold)
                      Range: File "", line 125, characters 6-10
                      Body Range: File "", line 125, characters 65-98
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#126:6-15 -> fold_desc)
                      Range: File "", line 126, characters 6-15
                      Body Range: File "", line 126, characters 70-108
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: File "", line 128, characters 4-13
                      (iter#124:6-10 -> iter)
                      Range: File "", line 124, characters 6-10
                      Body Range: File "", line 124, characters 57-87
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#118:25-32 -> literal)
                      Range: File "", line 118, characters 25-32
                      Body Range: File "", line 118, characters 65-95
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#120:6-9 -> mem)
                      Range: File "", line 120, characters 6-9
                      Body Range: File "", line 120, characters 48-77
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#122:6-12 -> remove)
                      Range: File "", line 122, characters 6-12
                      Body Range: File "", line 122, characters 52-84
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#116:6-10 -> size)
                      Range: File "", line 116, characters 6-10
                      Body Range: File "", line 116, characters 40-67
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#123:6-12 -> update)
                      Range: File "", line 123, characters 6-12
                      Body Range: File "", line 123, characters 55-90
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#153:7-13 -> String)
    Range: File "", line 153, characters 7-13
    Body Range: File "", line 154, character 2 to line 158, character 84
    Content: Members: Variable definitions:
                      (concat#157:6-12 -> concat)
                      Range: File "", line 157, characters 6-12
                      Body Range: File "", line 157, characters 2-82
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#155:6-13 -> concats)
                      Range: File "", line 155, characters 6-13
                      Body Range: File "", line 155, characters 2-71
                      Content: |core: list (string) -> string|
                      references: []
                      (length#154:6-12 -> length)
                      Range: File "", line 154, characters 6-12
                      Body Range: File "", line 154, characters 2-57
                      Content: |core: string -> nat|
                      references:
                        File "", line 363, characters 22-28 ,
                        File "", line 366, characters 43-49
                      (sub#158:6-9 -> sub)
                      Range: File "", line 158, characters 6-9
                      Body Range: File "", line 158, characters 2-84
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 364, characters 24-27 ,
                        File "", line 366, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 363, characters 15-21 ,
      File "", line 364, characters 17-23 ,
      File "", line 366, characters 16-22 ,
      File "", line 366, characters 36-42

    (Test#240:7-11 -> Test)
    Range: File "", line 240, characters 7-11
    Body Range: File "", line 242, character 2 to line 474, character 102
    Content: Members: Variable definitions:
                      (add_account#348:6-17 -> add_account)
                      Range: File "", line 348, characters 6-17
                      Body Range: File "", line 348, characters 2-88
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#468:6-12 -> assert)
                      Range: File "", line 468, characters 6-12
                      Body Range: File "", line 468, characters 2-78
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#470:6-17 -> assert_none)
                      Range: File "", line 470, characters 6-17
                      Body Range: File "", line 470, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#474:6-28 -> assert_none_with_error)
                      Range: File "", line 474, characters 6-28
                      Body Range: File "", line 474, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#469:6-17 -> assert_some)
                      Range: File "", line 469, characters 6-17
                      Body Range: File "", line 469, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#473:6-28 -> assert_some_with_error)
                      Range: File "", line 473, characters 6-28
                      Body Range: File "", line 473, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#472:6-23 -> assert_with_error)
                      Range: File "", line 472, characters 6-23
                      Body Range: File "", line 472, characters 2-78
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#267:6-28 -> bake_until_n_cycle_end)
                      Range: File "", line 267, characters 6-28
                      Body Range: File "", line 267, characters 2-94
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#349:6-19 -> baker_account)
                      Range: File "", line 349, characters 6-19
                      Body Range: File "", line 349, characters 2-105
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#344:6-24 -> bootstrap_contract)
                      Range: File "", line 344, characters 6-24
                      Body Range: File "", line 344, characters 97-145
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#269:6-18 -> cast_address)
                      Range: File "", line 269, characters 6-18
                      Body Range: File "", line 269, characters 69-105
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 376, characters 35-47 ,
                        File "", line 386, characters 35-47 ,
                        File "", line 393, characters 35-47
                      (chr#293:6-9 -> chr)
                      Range: File "", line 293, characters 6-9
                      Body Range: File "", line 293, character 2 to line 302, character 10
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#288:6-22 -> compile_contract)
                      Range: File "", line 288, characters 6-22
                      Body Range: File "", line 289, character 4 to line 291, character 52
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 372, characters 12-28 ,
                        File "", line 382, characters 12-28
                      (compile_contract_from_file#395:6-32 -> compile_contract_from_file)
                      Range: File "", line 395, characters 6-32
                      Body Range: File "", line 395, character 2 to line 397, character 52
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 399, characters 12-38
                      (compile_contract_with_views#378:8-35 -> compile_contract_with_views)
                      Range: File "", line 378, characters 8-35
                      Body Range: File "", line 379, character 6 to line 380, character 54
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 389, characters 12-39
                      (compile_value#245:6-19 -> compile_value)
                      Range: File "", line 245, characters 6-19
                      Body Range: File "", line 245, characters 59-65
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#273:6-35 -> constant_to_michelson_program)
                      Range: File "", line 273, characters 6-35
                      Body Range: File "", line 273, characters 2-116
                      Content: |core: string -> michelson_program|
                      references: []
                      (decompile#266:6-15 -> decompile)
                      Range: File "", line 266, characters 6-15
                      Body Range: File "", line 266, characters 55-88
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 284, characters 5-14
                      (drop_context#277:6-18 -> drop_context)
                      Range: File "", line 277, characters 6-18
                      Body Range: File "", line 277, characters 2-75
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#253:6-12 -> eprint)
                      Range: File "", line 253, characters 6-12
                      Body Range: File "", line 253, characters 2-67
                      Content: |core: string -> unit|
                      references: []
                      (eval#243:6-10 -> eval)
                      Range: File "", line 243, characters 6-10
                      Body Range: File "", line 243, characters 50-74
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 245, characters 59-63 ,
                        File "", line 354, characters 32-36 ,
                        File "", line 359, characters 34-38 ,
                        File "", line 373, characters 12-16 ,
                        File "", line 383, characters 12-16 ,
                        File "", line 390, characters 12-16
                      (failwith#247:6-14 -> failwith)
                      Range: File "", line 247, characters 6-14
                      Body Range: File "", line 247, characters 40-72
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 468, characters 51-59 ,
                        File "", line 469, characters 74-82 ,
                        File "", line 470, characters 89-97 ,
                        File "", line 472, characters 68-76 ,
                        File "", line 473, characters 98-106 ,
                        File "", line 474, characters 113-121
                      (get_balance#251:6-17 -> get_balance)
                      Range: File "", line 251, characters 6-17
                      Body Range: File "", line 251, characters 2-75
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#259:6-27 -> get_bootstrap_account)
                      Range: File "", line 259, characters 6-27
                      Body Range: File "", line 259, characters 2-105
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#328:6-26 -> get_last_events_from)
                      Range: File "", line 328, characters 6-26
                      Body Range: File "", line 329, character 4 to line 335, character 38
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#280:6-17 -> get_storage)
                      Range: File "", line 280, characters 6-17
                      Body Range: File "", line 281, character 4 to line 284, character 21
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#250:6-28 -> get_storage_of_address)
                      Range: File "", line 250, characters 6-28
                      Body Range: File "", line 250, characters 2-111
                      Content: |core: address -> michelson_program|
                      references: File "", line 283, characters 32-54
                      (get_time#268:6-14 -> get_time)
                      Range: File "", line 268, characters 6-14
                      Body Range: File "", line 268, characters 2-57
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#246:6-28 -> get_total_voting_power)
                      Range: File "", line 246, characters 6-28
                      Body Range: File "", line 246, characters 2-96
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#254:6-22 -> get_voting_power)
                      Range: File "", line 254, characters 6-22
                      Body Range: File "", line 254, characters 2-88
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#261:6-23 -> last_originations)
                      Range: File "", line 261, characters 6-23
                      Body Range: File "", line 261, characters 2-108
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#338:6-9 -> log)
                      Range: File "", line 338, characters 6-9
                      Body Range: File "", line 339, character 4 to line 341, character 11
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 365, characters 25-28
                      (michelson_equal#361:6-21 -> michelson_equal)
                      Range: File "", line 361, characters 6-21
                      Body Range: File "", line 361, characters 2-88
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#345:6-18 -> mutate_value)
                      Range: File "", line 345, characters 6-18
                      Body Range: File "", line 345, characters 72-111
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 407, characters 23-35 ,
                        File "", line 419, characters 23-35
                      (mutation_test#403:6-19 -> mutation_test)
                      Range: File "", line 403, characters 6-19
                      Body Range: File "", line 404, character 4 to line 414, character 19
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#415:6-23 -> mutation_test_all)
                      Range: File "", line 415, characters 6-23
                      Body Range: File "", line 416, character 4 to line 426, character 46
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (new_account#265:6-17 -> new_account)
                      Range: File "", line 265, characters 6-17
                      Body Range: File "", line 265, characters 2-81
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#303:6-8 -> nl)
                      Range: File "", line 303, characters 6-8
                      Body Range: File "", line 303, characters 11-53
                      Content: |unresolved|
                      references: File "", line 305, characters 15-17
                      (nth_bootstrap_account#256:6-27 -> nth_bootstrap_account)
                      Range: File "", line 256, characters 6-27
                      Body Range: File "", line 256, character 2 to line 258, character 5
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#255:6-28 -> nth_bootstrap_contract)
                      Range: File "", line 255, characters 6-28
                      Body Range: File "", line 255, characters 2-97
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#260:6-33 -> nth_bootstrap_typed_address)
                      Range: File "", line 260, characters 6-33
                      Body Range: File "", line 260, characters 80-131
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#371:6-15 -> originate)
                      Range: File "", line 371, characters 6-15
                      Body Range: File "", line 372, character 4 to line 377, character 13
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_contract#370:6-24 -> originate_contract)
                      Range: File "", line 370, characters 6-24
                      Body Range: File "", line 370, characters 2-135
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 374, characters 12-30 ,
                        File "", line 384, characters 12-30 ,
                        File "", line 391, characters 12-30 ,
                        File "", line 400, characters 12-30 ,
                        File "", line 431, characters 14-32 ,
                        File "", line 451, characters 14-32
                      (originate_from_file#398:6-25 -> originate_from_file)
                      Range: File "", line 398, characters 6-25
                      Body Range: File "", line 398, character 2 to line 402, character 13
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_from_file_and_mutate#427:6-36 -> originate_from_file_and_mutate)
                      Range: File "", line 427, characters 6-36
                      Body Range: File "", line 429, character 4 to line 446, character 19
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#447:6-40 -> originate_from_file_and_mutate_all)
                      Range: File "", line 447, characters 6-40
                      Body Range: File "", line 449, character 4 to line 466, character 46
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module#388:6-22 -> originate_module)
                      Range: File "", line 388, characters 6-22
                      Body Range: File "", line 389, character 4 to line 394, character 13
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) ->
                                                                ( list (operation) *
                                                                  s ) *
                                                                views (s) ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_uncurried#381:6-25 -> originate_uncurried)
                      Range: File "", line 381, characters 6-25
                      Body Range: File "", line 382, character 4 to line 387, character 13
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (parse_michelson#274:6-21 -> parse_michelson)
                      Range: File "", line 274, characters 6-21
                      Body Range: File "", line 274, characters 2-102
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#252:6-11 -> print)
                      Range: File "", line 252, characters 6-11
                      Body Range: File "", line 252, characters 2-66
                      Content: |core: string -> unit|
                      references:
                        File "", line 305, characters 4-9 ,
                        File "", line 341, characters 4-9
                      (println#304:6-13 -> println)
                      Range: File "", line 304, characters 6-13
                      Body Range: File "", line 304, character 2 to line 305, character 18
                      Content: |core: string -> unit|
                      references: []
                      (random#262:6-12 -> random)
                      Range: File "", line 262, characters 6-12
                      Body Range: File "", line 263, character 4 to line 264, character 42
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#292:6-29 -> read_contract_from_file)
                      Range: File "", line 292, characters 6-29
                      Body Range: File "", line 292, characters 2-115
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#271:6-23 -> register_constant)
                      Range: File "", line 271, characters 6-23
                      Body Range: File "", line 271, characters 2-100
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#270:6-23 -> register_delegate)
                      Range: File "", line 270, characters 6-23
                      Body Range: File "", line 270, characters 2-91
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#342:6-17 -> reset_state)
                      Range: File "", line 342, characters 6-17
                      Body Range: File "", line 342, characters 2-117
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#343:6-20 -> reset_state_at)
                      Range: File "", line 343, characters 6-20
                      Body Range: File "", line 343, characters 2-117
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#275:6-21 -> restore_context)
                      Range: File "", line 275, characters 6-21
                      Body Range: File "", line 275, characters 2-77
                      Content: |core: unit -> unit|
                      references: []
                      (run#242:6-9 -> run)
                      Range: File "", line 242, characters 6-9
                      Body Range: File "", line 242, characters 64-94
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 243, characters 50-53
                      (save_context#276:6-18 -> save_context)
                      Range: File "", line 276, characters 6-18
                      Body Range: File "", line 276, characters 2-75
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#346:6-19 -> save_mutation)
                      Range: File "", line 346, characters 6-19
                      Body Range: File "", line 346, characters 2-106
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#286:6-15 -> set_baker)
                      Range: File "", line 286, characters 6-15
                      Body Range: File "", line 286, characters 2-70
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#285:6-22 -> set_baker_policy)
                      Range: File "", line 285, characters 6-22
                      Body Range: File "", line 285, characters 2-91
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 286, characters 39-55
                      (set_big_map#350:6-17 -> set_big_map)
                      Range: File "", line 350, characters 6-17
                      Body Range: File "", line 350, characters 69-107
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#307:6-22 -> set_print_values)
                      Range: File "", line 307, characters 6-22
                      Body Range: File "", line 307, characters 2-100
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#249:6-16 -> set_source)
                      Range: File "", line 249, characters 6-16
                      Body Range: File "", line 249, characters 2-74
                      Content: |core: address -> unit|
                      references: []
                      (sign#347:6-10 -> sign)
                      Range: File "", line 347, characters 6-10
                      Body Range: File "", line 347, characters 2-83
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#287:6-10 -> size)
                      Range: File "", line 287, characters 6-10
                      Body Range: File "", line 287, characters 2-72
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 375, characters 12-16 ,
                        File "", line 385, characters 12-16 ,
                        File "", line 392, characters 12-16 ,
                        File "", line 401, characters 12-16 ,
                        File "", line 432, characters 14-18 ,
                        File "", line 452, characters 14-18
                      (to_contract#248:6-17 -> to_contract)
                      Range: File "", line 248, characters 6-17
                      Body Range: File "", line 248, characters 71-106
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 281, characters 25-36 ,
                        File "", line 329, characters 30-41
                      (to_entrypoint#362:6-19 -> to_entrypoint)
                      Range: File "", line 362, characters 6-19
                      Body Range: File "", line 363, character 4 to line 369, character 44
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#279:6-13 -> to_json)
                      Range: File "", line 279, characters 6-13
                      Body Range: File "", line 279, characters 42-78
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#278:6-15 -> to_string)
                      Range: File "", line 278, characters 6-15
                      Body Range: File "", line 278, characters 44-80
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 296, characters 68-77 ,
                        File "", line 298, characters 67-76 ,
                        File "", line 300, characters 61-70 ,
                        File "", line 340, characters 12-21
                      (to_typed_address#272:6-22 -> to_typed_address)
                      Range: File "", line 272, characters 6-22
                      Body Range: File "", line 272, characters 76-116
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#336:6-14 -> transfer)
                      Range: File "", line 336, characters 6-14
                      Body Range: File "", line 336, characters 2-162
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#337:6-18 -> transfer_exn)
                      Range: File "", line 337, characters 6-18
                      Body Range: File "", line 337, characters 2-157
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#351:6-26 -> transfer_to_contract)
                      Range: File "", line 351, characters 6-26
                      Body Range: File "", line 352, character 4 to line 355, character 61
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#356:6-30 -> transfer_to_contract_exn)
                      Range: File "", line 356, characters 6-30
                      Body Range: File "", line 357, character 6 to line 360, character 67
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#308:6-24 -> unset_print_values)
                      Range: File "", line 308, characters 6-24
                      Body Range: File "", line 308, characters 2-103
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#310:9-12 -> PBT)
                      Range: File "", line 310, characters 9-12
                      Body Range: File "", line 311, character 4 to line 325, character 7
                      Content: Members: Variable definitions:
                                        (gen#311:8-11 -> gen)
                                        Range: File "", line 311, characters 8-11
                                        Body Range: File "", line 311, characters 35-69
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (gen_small#312:8-17 -> gen_small)
                                        Range: File "", line 312, characters 8-17
                                        Body Range: File "", line 312, characters 41-74
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#313:8-17 -> make_test)
                                        Range: File "", line 313, characters 8-17
                                        Body Range: File "", line 313, characters 75-79
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#314:8-11 -> run)
                                        Range: File "", line 314, characters 8-11
                                        Body Range: File "", line 315, character 6 to line 325, character 7
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#7:7-12 -> Tezos)
    Range: File "", line 7, characters 7-12
    Body Range: File "", line 9, character 2 to line 64, character 20
    Content: Members: Variable definitions:
                      (address#20:6-13 -> address)
                      Range: File "", line 20, characters 6-13
                      Body Range: File "", line 20, characters 52-110
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 329, characters 21-28
                      (call_view#46:25-34 -> call_view)
                      Range: File "", line 46, characters 25-34
                      Body Range: File "", line 47, characters 4-123
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (constant#31:25-33 -> constant)
                      Range: File "", line 31, characters 25-33
                      Body Range: File "", line 31, characters 62-96
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (create_contract#50:25-40 -> create_contract)
                      Range: File "", line 50, characters 25-40
                      Body Range: File "", line 51, character 6 to line 52, character 58
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: []
                      (create_contract_uncurried#53:25-50 -> create_contract_uncurried)
                      Range: File "", line 53, characters 25-50
                      Body Range: File "", line 54, characters 6-50
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      (create_ticket#43:6-19 -> create_ticket)
                      Range: File "", line 43, characters 6-19
                      Body Range: File "", line 43, characters 69-147
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references: []
                      (emit#61:25-29 -> emit)
                      Range: File "", line 61, characters 25-29
                      Body Range: File "", line 62, character 4 to line 63, character 99
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
                      (get_entrypoint#58:25-39 -> get_entrypoint)
                      Range: File "", line 58, characters 25-39
                      Body Range: File "", line 59, character 4 to line 60, character 70
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (get_entrypoint_opt#55:25-43 -> get_entrypoint_opt)
                      Range: File "", line 55, characters 25-43
                      Body Range: File "", line 56, character 4 to line 57, character 119
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 59, characters 12-30
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
                      references: File "", line 268, characters 47-54
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
                      (sapling_verify_update#64:25-46 -> sapling_verify_update)
                      Range: File "", line 64, characters 25-46
                      Body Range: File "", line 64, characters 167-331
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
                      (split_ticket#48:6-18 -> split_ticket)
                      Range: File "", line 48, characters 6-18
                      Body Range: File "", line 49, characters 4-114
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      (transaction#44:6-17 -> transaction)
                      Range: File "", line 44, characters 6-17
                      Body Range: File "", line 45, characters 4-109
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
      File "", line 268, characters 41-46 ,
      File "", line 329, characters 15-20

    (Transpiled#108:7-17 -> Transpiled)
    Range: File "", line 108, characters 7-17
    Body Range: File "", line 109, character 2 to line 111, character 228
    Content: Members: Variable definitions:
                      (map_add#110:6-13 -> map_add)
                      Range: File "", line 110, characters 6-13
                      Body Range: File "", line 110, characters 82-198
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      (map_find_opt#109:6-18 -> map_find_opt)
                      Range: File "", line 109, characters 6-18
                      Body Range: File "", line 109, characters 79-163
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      (map_remove#111:6-16 -> map_remove)
                      Range: File "", line 111, characters 6-16
                      Body Range: File "", line 111, characters 75-228
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: [] |}]
