open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#302:9-12 Set#114:7-10 String#153:7-13 Test#232:7-11 Tezos#7:7-12 Transpiled#108:7-17 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#340:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#462:6-12 assert_none#195:4-15 assert_none#464:6-17 assert_none_with_error#208:4-26 assert_none_with_error#468:6-28 assert_some#194:4-15 assert_some#463:6-17 assert_some_with_error#207:4-26 assert_some_with_error#467:6-28 assert_with_error#206:4-21 assert_with_error#466:6-23 bake_until_n_cycle_end#259:6-28 baker_account#341:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#336:6-24 call_view#46:25-34 cardinal#117:6-14 cast_address#261:6-18 check#190:6-11 chr#285:6-9 compile_contract#280:6-22 compile_contract_from_file#389:6-32 compile_contract_with_views#372:8-35 compile_value#237:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#265:6-35 create_chest#343:6-18 create_chest_key#344:6-22 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#203:4-9 decompile#258:6-15 drop_context#269:6-18 ediv#209:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#245:6-12 eval#235:6-10 failwith#239:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#303:8-11 gen_small#304:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#243:6-17 get_balance#9:6-17 get_bootstrap_account#251:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#320:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#272:6-17 get_storage_of_address#242:6-28 get_time#260:6-14 get_total_voting_power#17:6-28 get_total_voting_power#238:6-28 get_voting_power#246:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#202:4-10 implicit_account#21:6-22 int#201:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#253:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#330:6-9 make_test#305:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#355:6-21 mutate_value#337:6-18 mutation_test#397:6-19 mutation_test_all#409:6-23 never#25:6-11 new_account#257:6-17 nl#295:6-8 nth_bootstrap_account#248:6-27 nth_bootstrap_contract#247:6-28 nth_bootstrap_typed_address#252:6-33 option#5:8-14 or#71:6-9 originate#365:6-15 originate_contract#364:6-24 originate_from_file#392:6-25 originate_from_file_and_mutate#421:6-36 originate_from_file_and_mutate_all#441:6-40 originate_module#382:6-22 originate_uncurried#375:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#266:6-21 pbt_result#228:8-18 pbt_test#227:8-16 print#244:6-11 println#296:6-13 random#254:6-12 read_contract_from_file#284:6-29 read_ticket#23:6-17 register_constant#263:6-23 register_delegate#262:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#334:6-17 reset_state_at#335:6-20 restore_context#267:6-21 run#234:6-9 run#306:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#268:6-18 save_mutation#338:6-19 self#28:25-29 set_baker#278:6-15 set_baker_policy#277:6-22 set_big_map#342:6-17 set_delegate#27:6-18 set_print_values#299:6-22 set_source#241:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#339:6-10 size#116:6-10 size#133:6-10 size#279:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#222:5-22 test_exec_error#215:5-20 test_exec_error_balance_too_low#212:5-36 test_exec_result#220:5-21 to_contract#240:6-17 to_entrypoint#356:6-19 to_json#271:6-13 to_string#270:6-15 to_typed_address#264:6-22 transaction#44:6-17 transfer#328:6-14 transfer_exn#329:6-18 transfer_to_contract#345:6-26 transfer_to_contract_exn#350:6-30 true#198:4-8 uncurry#204:4-11 unforged_ticket#230:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#300:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#302:9-12 Set#114:7-10 String#153:7-13 Test#232:7-11 Tezos#7:7-12 Transpiled#108:7-17 a#1:4-5 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#340:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#462:6-12 assert_none#195:4-15 assert_none#464:6-17 assert_none_with_error#208:4-26 assert_none_with_error#468:6-28 assert_some#194:4-15 assert_some#463:6-17 assert_some_with_error#207:4-26 assert_some_with_error#467:6-28 assert_with_error#206:4-21 assert_with_error#466:6-23 bake_until_n_cycle_end#259:6-28 baker_account#341:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#336:6-24 c#5:10-11 call_view#46:25-34 cardinal#117:6-14 cast_address#261:6-18 check#190:6-11 chr#285:6-9 compile_contract#280:6-22 compile_contract_from_file#389:6-32 compile_contract_with_views#372:8-35 compile_value#237:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#265:6-35 create_chest#343:6-18 create_chest_key#344:6-22 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#203:4-9 decompile#258:6-15 drop_context#269:6-18 ediv#209:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#245:6-12 eval#235:6-10 failwith#239:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#303:8-11 gen_small#304:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#243:6-17 get_balance#9:6-17 get_bootstrap_account#251:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#320:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#272:6-17 get_storage_of_address#242:6-28 get_time#260:6-14 get_total_voting_power#17:6-28 get_total_voting_power#238:6-28 get_voting_power#246:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#202:4-10 implicit_account#21:6-22 int#201:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#253:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#330:6-9 make_test#305:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#355:6-21 mutate_value#337:6-18 mutation_test#397:6-19 mutation_test_all#409:6-23 never#25:6-11 new_account#257:6-17 nl#295:6-8 nth_bootstrap_account#248:6-27 nth_bootstrap_contract#247:6-28 nth_bootstrap_typed_address#252:6-33 option#5:8-14 or#71:6-9 originate#365:6-15 originate_contract#364:6-24 originate_from_file#392:6-25 originate_from_file_and_mutate#421:6-36 originate_from_file_and_mutate_all#441:6-40 originate_module#382:6-22 originate_uncurried#375:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#266:6-21 pbt_result#228:8-18 pbt_test#227:8-16 print#244:6-11 println#296:6-13 random#254:6-12 read_contract_from_file#284:6-29 read_ticket#23:6-17 register_constant#263:6-23 register_delegate#262:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#334:6-17 reset_state_at#335:6-20 restore_context#267:6-21 run#234:6-9 run#306:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#268:6-18 save_mutation#338:6-19 self#28:25-29 set_baker#278:6-15 set_baker_policy#277:6-22 set_big_map#342:6-17 set_delegate#27:6-18 set_print_values#299:6-22 set_source#241:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#339:6-10 size#116:6-10 size#133:6-10 size#279:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#222:5-22 test_exec_error#215:5-20 test_exec_error_balance_too_low#212:5-36 test_exec_result#220:5-21 to_contract#240:6-17 to_entrypoint#356:6-19 to_json#271:6-13 to_string#270:6-15 to_typed_address#264:6-22 transaction#44:6-17 transfer#328:6-14 transfer_exn#329:6-18 transfer_to_contract#345:6-26 transfer_to_contract_exn#350:6-30 true#198:4-8 uncurry#204:4-11 unforged_ticket#230:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#300:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#302:9-12 Set#114:7-10 String#153:7-13 Test#232:7-11 Tezos#7:7-12 Transpiled#108:7-17 a#1:4-5 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#340:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#462:6-12 assert_none#195:4-15 assert_none#464:6-17 assert_none_with_error#208:4-26 assert_none_with_error#468:6-28 assert_some#194:4-15 assert_some#463:6-17 assert_some_with_error#207:4-26 assert_some_with_error#467:6-28 assert_with_error#206:4-21 assert_with_error#466:6-23 bake_until_n_cycle_end#259:6-28 baker_account#341:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#336:6-24 c#5:10-11 call_view#46:25-34 cardinal#117:6-14 cast_address#261:6-18 check#190:6-11 chr#285:6-9 compile_contract#280:6-22 compile_contract_from_file#389:6-32 compile_contract_with_views#372:8-35 compile_value#237:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#265:6-35 create_chest#343:6-18 create_chest_key#344:6-22 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#203:4-9 d#5:26-27 decompile#258:6-15 drop_context#269:6-18 ediv#209:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#245:6-12 eval#235:6-10 failwith#239:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#303:8-11 gen_small#304:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#243:6-17 get_balance#9:6-17 get_bootstrap_account#251:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#320:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#272:6-17 get_storage_of_address#242:6-28 get_time#260:6-14 get_total_voting_power#17:6-28 get_total_voting_power#238:6-28 get_voting_power#246:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#202:4-10 implicit_account#21:6-22 int#201:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#253:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#330:6-9 make_test#305:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#355:6-21 mutate_value#337:6-18 mutation_test#397:6-19 mutation_test_all#409:6-23 never#25:6-11 new_account#257:6-17 nl#295:6-8 nth_bootstrap_account#248:6-27 nth_bootstrap_contract#247:6-28 nth_bootstrap_typed_address#252:6-33 option#5:8-14 or#71:6-9 originate#365:6-15 originate_contract#364:6-24 originate_from_file#392:6-25 originate_from_file_and_mutate#421:6-36 originate_from_file_and_mutate_all#441:6-40 originate_module#382:6-22 originate_uncurried#375:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#266:6-21 pbt_result#228:8-18 pbt_test#227:8-16 print#244:6-11 println#296:6-13 random#254:6-12 read_contract_from_file#284:6-29 read_ticket#23:6-17 register_constant#263:6-23 register_delegate#262:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#334:6-17 reset_state_at#335:6-20 restore_context#267:6-21 run#234:6-9 run#306:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#268:6-18 save_mutation#338:6-19 self#28:25-29 set_baker#278:6-15 set_baker_policy#277:6-22 set_big_map#342:6-17 set_delegate#27:6-18 set_print_values#299:6-22 set_source#241:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#339:6-10 size#116:6-10 size#133:6-10 size#279:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#222:5-22 test_exec_error#215:5-20 test_exec_error_balance_too_low#212:5-36 test_exec_result#220:5-21 to_contract#240:6-17 to_entrypoint#356:6-19 to_json#271:6-13 to_string#270:6-15 to_typed_address#264:6-22 transaction#44:6-17 transfer#328:6-14 transfer_exn#329:6-18 transfer_to_contract#345:6-26 transfer_to_contract_exn#350:6-30 true#198:4-8 uncurry#204:4-11 unforged_ticket#230:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#300:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#302:9-12 Set#114:7-10 String#153:7-13 Test#232:7-11 Tezos#7:7-12 Transpiled#108:7-17 a#1:4-5 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#340:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#462:6-12 assert_none#195:4-15 assert_none#464:6-17 assert_none_with_error#208:4-26 assert_none_with_error#468:6-28 assert_some#194:4-15 assert_some#463:6-17 assert_some_with_error#207:4-26 assert_some_with_error#467:6-28 assert_with_error#206:4-21 assert_with_error#466:6-23 bake_until_n_cycle_end#259:6-28 baker_account#341:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#336:6-24 call_view#46:25-34 cardinal#117:6-14 cast_address#261:6-18 check#190:6-11 chr#285:6-9 compile_contract#280:6-22 compile_contract_from_file#389:6-32 compile_contract_with_views#372:8-35 compile_value#237:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#265:6-35 create_chest#343:6-18 create_chest_key#344:6-22 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#203:4-9 decompile#258:6-15 drop_context#269:6-18 e#6:9-10 ediv#209:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#245:6-12 eval#235:6-10 failwith#239:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#303:8-11 gen_small#304:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#243:6-17 get_balance#9:6-17 get_bootstrap_account#251:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#320:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#272:6-17 get_storage_of_address#242:6-28 get_time#260:6-14 get_total_voting_power#17:6-28 get_total_voting_power#238:6-28 get_voting_power#246:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#202:4-10 implicit_account#21:6-22 int#201:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#253:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#330:6-9 make_test#305:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#355:6-21 mutate_value#337:6-18 mutation_test#397:6-19 mutation_test_all#409:6-23 never#25:6-11 new_account#257:6-17 nl#295:6-8 nth_bootstrap_account#248:6-27 nth_bootstrap_contract#247:6-28 nth_bootstrap_typed_address#252:6-33 option#5:8-14 or#71:6-9 originate#365:6-15 originate_contract#364:6-24 originate_from_file#392:6-25 originate_from_file_and_mutate#421:6-36 originate_from_file_and_mutate_all#441:6-40 originate_module#382:6-22 originate_uncurried#375:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#266:6-21 pbt_result#228:8-18 pbt_test#227:8-16 print#244:6-11 println#296:6-13 random#254:6-12 read_contract_from_file#284:6-29 read_ticket#23:6-17 register_constant#263:6-23 register_delegate#262:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#334:6-17 reset_state_at#335:6-20 restore_context#267:6-21 run#234:6-9 run#306:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#268:6-18 save_mutation#338:6-19 self#28:25-29 set_baker#278:6-15 set_baker_policy#277:6-22 set_big_map#342:6-17 set_delegate#27:6-18 set_print_values#299:6-22 set_source#241:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#339:6-10 size#116:6-10 size#133:6-10 size#279:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#222:5-22 test_exec_error#215:5-20 test_exec_error_balance_too_low#212:5-36 test_exec_result#220:5-21 to_contract#240:6-17 to_entrypoint#356:6-19 to_json#271:6-13 to_string#270:6-15 to_typed_address#264:6-22 transaction#44:6-17 transfer#328:6-14 transfer_exn#329:6-18 transfer_to_contract#345:6-26 transfer_to_contract_exn#350:6-30 true#198:4-8 uncurry#204:4-11 unforged_ticket#230:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#300:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32
    [ Big_map#76:7-14 Bitwise#68:7-14 Bytes#172:7-12 Crypto#182:7-13 List#131:7-11 Map#90:7-10 Option#161:7-13 PBT#302:9-12 Set#114:7-10 String#153:7-13 Test#232:7-11 Tezos#7:7-12 Transpiled#108:7-17 a#1:4-5 abs#196:4-7 add#121:6-9 add#81:6-9 add#96:6-9 add_account#340:6-17 address#20:6-13 and#69:6-10 assert#193:4-10 assert#462:6-12 assert_none#195:4-15 assert_none#464:6-17 assert_none_with_error#208:4-26 assert_none_with_error#468:6-28 assert_some#194:4-15 assert_some#463:6-17 assert_some_with_error#207:4-26 assert_some_with_error#467:6-28 assert_with_error#206:4-21 assert_with_error#466:6-23 bake_until_n_cycle_end#259:6-28 baker_account#341:6-19 blake2b#183:6-13 bool#4:5-9 bootstrap_contract#336:6-24 call_view#46:25-34 cardinal#117:6-14 cast_address#261:6-18 check#190:6-11 chr#285:6-9 compile_contract#280:6-22 compile_contract_from_file#389:6-32 compile_contract_with_views#372:8-35 compile_value#237:6-19 concat#157:6-12 concat#178:6-12 concats#155:6-13 concats#173:6-13 cons#142:6-10 constant#31:25-33 constant_to_michelson_program#265:6-35 create_chest#343:6-18 create_chest_key#344:6-22 create_contract#50:25-40 create_contract_uncurried#53:25-50 create_ticket#43:6-19 curry#203:4-9 decompile#258:6-15 drop_context#269:6-18 ediv#209:4-8 emit#61:25-29 empty#115:6-11 empty#77:16-21 empty#91:6-11 eprint#245:6-12 eval#235:6-10 failwith#239:6-14 failwith#2:4-12 false#199:4-9 filter_map#127:6-16 filter_map#145:6-16 find#100:6-10 find#86:6-10 find_opt#101:6-14 find_opt#143:6-14 find_opt#85:6-14 fold#104:6-10 fold#125:6-10 fold#139:6-10 fold_desc#126:6-15 fold_left#140:6-15 fold_right#141:6-16 gen#303:8-11 gen_small#304:8-17 get_amount#10:6-16 get_and_update#84:6-20 get_and_update#99:6-20 get_balance#243:6-17 get_balance#9:6-17 get_bootstrap_account#251:6-27 get_chain_id#16:6-18 get_contract#36:25-37 get_contract_opt#34:25-41 get_contract_with_error#40:6-29 get_entrypoint#58:25-39 get_entrypoint_opt#55:25-43 get_last_events_from#320:6-26 get_level#14:6-15 get_min_block_time#18:6-24 get_now#11:6-13 get_self_address#15:6-22 get_sender#12:6-16 get_source#13:6-16 get_storage#272:6-17 get_storage_of_address#242:6-28 get_time#260:6-14 get_total_voting_power#17:6-28 get_total_voting_power#238:6-28 get_voting_power#246:6-22 hash_key#188:6-14 head_opt#134:6-14 ignore#202:4-10 implicit_account#21:6-22 int#201:4-7 is_nat#197:4-10 is_none#168:6-13 is_some#169:6-13 iter#102:6-10 iter#124:6-10 iter#138:6-10 join_tickets#22:6-18 keccak#187:6-12 last_originations#253:6-23 length#132:6-12 length#154:6-12 length#176:6-12 literal#118:25-32 literal#78:25-32 literal#93:25-32 log#330:6-9 make_test#305:8-17 map#103:6-9 map#137:6-9 map#165:15-18 map_add#110:6-13 map_find_opt#109:6-18 map_remove#111:6-16 mem#120:6-9 mem#80:6-9 mem#95:6-9 michelson_equal#355:6-21 mutate_value#337:6-18 mutation_test#397:6-19 mutation_test_all#409:6-23 never#25:6-11 new_account#257:6-17 nl#295:6-8 nth_bootstrap_account#248:6-27 nth_bootstrap_contract#247:6-28 nth_bootstrap_typed_address#252:6-33 option#5:8-14 or#71:6-9 originate#365:6-15 originate_contract#364:6-24 originate_from_file#392:6-25 originate_from_file_and_mutate#421:6-36 originate_from_file_and_mutate_all#441:6-40 originate_module#382:6-22 originate_uncurried#375:6-25 pack#174:6-10 pairing_check#26:6-19 parse_michelson#266:6-21 pbt_result#228:8-18 pbt_test#227:8-16 print#244:6-11 println#296:6-13 random#254:6-12 read_contract_from_file#284:6-29 read_ticket#23:6-17 register_constant#263:6-23 register_delegate#262:6-23 remove#122:6-12 remove#82:6-12 remove#97:6-12 reset_state#334:6-17 reset_state_at#335:6-20 restore_context#267:6-21 run#234:6-9 run#306:8-11 sapling_empty_state#32:25-44 sapling_verify_update#64:25-46 save_context#268:6-18 save_mutation#338:6-19 self#28:25-29 set_baker#278:6-15 set_baker_policy#277:6-22 set_big_map#342:6-17 set_delegate#27:6-18 set_print_values#299:6-22 set_source#241:6-16 sha256#184:6-12 sha3#186:6-10 sha512#185:6-12 shift_left#72:6-16 shift_right#73:6-17 sign#339:6-10 size#116:6-10 size#133:6-10 size#279:6-10 size#92:6-10 split_ticket#48:6-18 sub#158:6-9 sub#179:6-9 tail_opt#135:6-14 test_baker_policy#222:5-22 test_exec_error#215:5-20 test_exec_error_balance_too_low#212:5-36 test_exec_result#220:5-21 to_contract#240:6-17 to_entrypoint#356:6-19 to_json#271:6-13 to_string#270:6-15 to_typed_address#264:6-22 transaction#44:6-17 transfer#328:6-14 transfer_exn#329:6-18 transfer_to_contract#345:6-26 transfer_to_contract_exn#350:6-30 true#198:4-8 uncurry#204:4-11 unforged_ticket#230:8-23 unit#200:4-8 unopt#162:6-11 unopt_with_error#164:6-22 unpack#175:6-12 unset_print_values#300:6-24 update#123:6-12 update#147:6-12 update#83:6-12 update#98:6-12 update_with#149:6-17 value#166:6-11 value_exn#167:6-15 voting_power#19:6-18 xor#70:6-9  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33

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
    Body Range: File "", line 196, characters 9-10
    Content: |core: int -> nat|
    references: File "", line 360, characters 31-34
    (assert#193:4-10 -> assert)
    Range: File "", line 193, characters 4-10
    Body Range: File "", line 193, characters 12-13
    Content: |core: bool -> unit|
    references: []
    (assert_none#195:4-15 -> assert_none)
    Range: File "", line 195, characters 4-15
    Body Range: File "", line 195, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#208:4-26 -> assert_none_with_error)
    Range: File "", line 208, characters 4-26
    Body Range: File "", line 208, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#194:4-15 -> assert_some)
    Range: File "", line 194, characters 4-15
    Body Range: File "", line 194, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#207:4-26 -> assert_some_with_error)
    Range: File "", line 207, characters 4-26
    Body Range: File "", line 207, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#206:4-21 -> assert_with_error)
    Range: File "", line 206, characters 4-21
    Body Range: File "", line 206, characters 23-24
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
    (curry#203:4-9 -> curry)
    Range: File "", line 203, characters 4-9
    Body Range: File "", line 203, characters 10-22
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
    (ediv#209:4-8 -> ediv)
    Range: File "", line 209, characters 4-8
    Body Range: File "", line 209, characters 9-19
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
      File "", line 206, characters 66-74 ,
      File "", line 207, characters 96-104 ,
      File "", line 208, characters 111-119
    (false#199:4-9 -> false)
    Range: File "", line 199, characters 4-9
    Body Range: File "", line 199, characters 19-24
    Content: |core: bool|
    references:
      File "", line 255, characters 51-56 ,
      File "", line 300, characters 90-95 ,
      File "", line 303, characters 62-67
    (ignore#202:4-10 -> ignore)
    Range: File "", line 202, characters 4-10
    Body Range: File "", line 202, characters 11-19
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#201:4-7 -> int)
    Range: File "", line 201, characters 4-7
    Body Range: File "", line 201, characters 8-16
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 251, characters 97-100 ,
      File "", line 288, characters 79-82 ,
      File "", line 290, characters 78-81 ,
      File "", line 292, characters 72-75
    (is_nat#197:4-10 -> is_nat)
    Range: File "", line 197, characters 4-10
    Body Range: File "", line 197, characters 12-13
    Content: |core: int -> option (nat)|
    references: []
    (true#198:4-8 -> true)
    Range: File "", line 198, characters 4-8
    Body Range: File "", line 198, characters 18-22
    Content: |core: bool|
    references:
      File "", line 299, characters 88-92 ,
      File "", line 304, characters 68-72
    (uncurry#204:4-11 -> uncurry)
    Range: File "", line 204, characters 4-11
    Body Range: File "", line 204, characters 12-24
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 366, characters 30-37
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
      File "", line 206, characters 27-31 ,
      File "", line 227, characters 41-45 ,
      File "", line 305, characters 53-57 ,
      File "", line 355, characters 74-78 ,
      File "", line 462, characters 18-22 ,
      File "", line 466, characters 29-33
    (option#5:8-14 -> option)
    Range: File "", line 5, characters 8-14
    Body Range: File "", line 5, characters 17-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 22, characters 56-73 ,
      File "", line 22, characters 137-152 ,
      File "", line 27, characters 24-39 ,
      File "", line 27, characters 93-108 ,
      File "", line 29, characters 12-20 ,
      File "", line 34, characters 67-86 ,
      File "", line 35, characters 60-79 ,
      File "", line 43, characters 49-66 ,
      File "", line 43, characters 121-138 ,
      File "", line 46, characters 84-92 ,
      File "", line 47, characters 81-89 ,
      File "", line 48, characters 61-89 ,
      File "", line 49, characters 77-105 ,
      File "", line 50, characters 93-108 ,
      File "", line 53, characters 102-117 ,
      File "", line 55, characters 82-99 ,
      File "", line 57, characters 71-90 ,
      File "", line 64, characters 120-164 ,
      File "", line 64, characters 278-322 ,
      File "", line 83, characters 37-45 ,
      File "", line 84, characters 45-53 ,
      File "", line 84, characters 78-86 ,
      File "", line 85, characters 57-65 ,
      File "", line 98, characters 37-45 ,
      File "", line 99, characters 45-53 ,
      File "", line 99, characters 74-82 ,
      File "", line 101, characters 53-61 ,
      File "", line 127, characters 38-46 ,
      File "", line 134, characters 40-48 ,
      File "", line 135, characters 40-55 ,
      File "", line 143, characters 56-64 ,
      File "", line 144, characters 29-37 ,
      File "", line 145, characters 38-46 ,
      File "", line 147, characters 32-40 ,
      File "", line 162, characters 26-34 ,
      File "", line 164, characters 37-45 ,
      File "", line 165, characters 48-56 ,
      File "", line 165, characters 60-68 ,
      File "", line 166, characters 40-48 ,
      File "", line 167, characters 46-54 ,
      File "", line 168, characters 28-36 ,
      File "", line 169, characters 28-36 ,
      File "", line 175, characters 36-44 ,
      File "", line 175, characters 99-107 ,
      File "", line 194, characters 30-38 ,
      File "", line 195, characters 30-38 ,
      File "", line 197, characters 23-33 ,
      File "", line 197, characters 74-84 ,
      File "", line 207, characters 41-49 ,
      File "", line 208, characters 41-49 ,
      File "", line 285, characters 22-35 ,
      File "", line 328, characters 140-153 ,
      File "", line 329, characters 135-148 ,
      File "", line 334, characters 92-108 ,
      File "", line 337, characters 48-69 ,
      File "", line 338, characters 50-63 ,
      File "", line 341, characters 44-54 ,
      File "", line 347, characters 12-25 ,
      File "", line 352, characters 14-27 ,
      File "", line 390, characters 96-106 ,
      File "", line 397, characters 59-80 ,
      File "", line 400, characters 37-58 ,
      File "", line 422, characters 90-111 ,
      File "", line 428, characters 96-106 ,
      File "", line 431, characters 37-58 ,
      File "", line 448, characters 96-106 ,
      File "", line 463, characters 32-40 ,
      File "", line 464, characters 32-40 ,
      File "", line 467, characters 43-51 ,
      File "", line 468, characters 43-51
    (pbt_result#228:8-18 -> pbt_result)
    Range: File "", line 228, characters 8-18
    Body Range: File "", line 228, characters 0-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 306, characters 55-67 ,
      File "", line 307, characters 37-49 ,
      File "", line 309, characters 82-94 ,
      File "", line 313, characters 94-106 ,
      File "", line 316, characters 66-78
    (pbt_test#227:8-16 -> pbt_test)
    Range: File "", line 227, characters 8-16
    Body Range: File "", line 227, characters 0-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 305, characters 61-71 ,
      File "", line 306, characters 31-41
    (test_baker_policy#222:5-22 -> test_baker_policy)
    Range: File "", line 222, characters 5-22
    Body Range: File "", line 223, character 4 to line 225, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 277, characters 29-46
    (test_exec_error#215:5-20 -> test_exec_error)
    Range: File "", line 215, characters 5-20
    Body Range: File "", line 216, character 4 to line 218, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 220, characters 49-64
    (test_exec_error_balance_too_low#212:5-36 -> test_exec_error_balance_too_low)
    Range: File "", line 212, characters 5-36
    Body Range: File "", line 213, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 217, characters 23-54
    (test_exec_result#220:5-21 -> test_exec_result)
    Range: File "", line 220, characters 5-21
    Body Range: File "", line 220, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 328, characters 65-81 ,
      File "", line 345, characters 73-89
    (unforged_ticket#230:8-23 -> unforged_ticket)
    Range: File "", line 230, characters 8-23
    Body Range: File "", line 230, characters 0-91
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    Module definitions:
    (Big_map#76:7-14 -> Big_map)
    Range: File "", line 76, characters 7-14
    Body Range: File "", line 76, character 0 to line 88, character 3
    Content: Members: Variable definitions:
                      (add#81:6-9 -> add)
                      Range: File "", line 81, characters 6-9
                      Body Range: File "", line 81, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (empty#77:16-21 -> empty)
                      Range: File "", line 77, characters 16-21
                      Body Range: File "", line 77, characters 22-32
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#86:6-10 -> find)
                      Range: File "", line 86, characters 6-10
                      Body Range: File "", line 86, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      (find_opt#85:6-14 -> find_opt)
                      Range: File "", line 85, characters 6-14
                      Body Range: File "", line 85, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (get_and_update#84:6-20 -> get_and_update)
                      Range: File "", line 84, characters 6-20
                      Body Range: File "", line 84, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#78:25-32 -> literal)
                      Range: File "", line 78, characters 25-32
                      Body Range: File "", line 78, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#80:6-9 -> mem)
                      Range: File "", line 80, characters 6-9
                      Body Range: File "", line 80, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (remove#82:6-12 -> remove)
                      Range: File "", line 82, characters 6-12
                      Body Range: File "", line 82, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#83:6-12 -> update)
                      Range: File "", line 83, characters 6-12
                      Body Range: File "", line 83, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#68:7-14 -> Bitwise)
    Range: File "", line 68, characters 7-14
    Body Range: File "", line 68, character 0 to line 74, character 3
    Content: Members: Variable definitions:
                      (and#69:6-10 -> and)
                      Range: File "", line 69, characters 6-10
                      Body Range: File "", line 69, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (or#71:6-9 -> or)
                      Range: File "", line 71, characters 6-9
                      Body Range: File "", line 71, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_left#72:6-16 -> shift_left)
                      Range: File "", line 72, characters 6-16
                      Body Range: File "", line 72, characters 18-19
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_right#73:6-17 -> shift_right)
                      Range: File "", line 73, characters 6-17
                      Body Range: File "", line 73, characters 19-20
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (xor#70:6-9 -> xor)
                      Range: File "", line 70, characters 6-9
                      Body Range: File "", line 70, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#172:7-12 -> Bytes)
    Range: File "", line 172, characters 7-12
    Body Range: File "", line 172, character 0 to line 180, character 3
    Content: Members: Variable definitions:
                      (concat#178:6-12 -> concat)
                      Range: File "", line 178, characters 6-12
                      Body Range: File "", line 178, characters 14-16
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#173:6-13 -> concats)
                      Range: File "", line 173, characters 6-13
                      Body Range: File "", line 173, characters 15-17
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#176:6-12 -> length)
                      Range: File "", line 176, characters 6-12
                      Body Range: File "", line 176, characters 14-15
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#174:6-10 -> pack)
                      Range: File "", line 174, characters 6-10
                      Body Range: File "", line 174, characters 11-19
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#179:6-9 -> sub)
                      Range: File "", line 179, characters 6-9
                      Body Range: File "", line 179, characters 11-12
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#175:6-12 -> unpack)
                      Range: File "", line 175, characters 6-12
                      Body Range: File "", line 175, characters 13-21
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#182:7-13 -> Crypto)
    Range: File "", line 182, characters 7-13
    Body Range: File "", line 182, character 0 to line 191, character 3
    Content: Members: Variable definitions:
                      (blake2b#183:6-13 -> blake2b)
                      Range: File "", line 183, characters 6-13
                      Body Range: File "", line 183, characters 15-16
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#190:6-11 -> check)
                      Range: File "", line 190, characters 6-11
                      Body Range: File "", line 190, characters 13-14
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#188:6-14 -> hash_key)
                      Range: File "", line 188, characters 6-14
                      Body Range: File "", line 188, characters 16-17
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#187:6-12 -> keccak)
                      Range: File "", line 187, characters 6-12
                      Body Range: File "", line 187, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#184:6-12 -> sha256)
                      Range: File "", line 184, characters 6-12
                      Body Range: File "", line 184, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#186:6-10 -> sha3)
                      Range: File "", line 186, characters 6-10
                      Body Range: File "", line 186, characters 12-13
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#185:6-12 -> sha512)
                      Range: File "", line 185, characters 6-12
                      Body Range: File "", line 185, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#131:7-11 -> List)
    Range: File "", line 131, characters 7-11
    Body Range: File "", line 131, character 0 to line 151, character 3
    Content: Members: Variable definitions:
                      (cons#142:6-10 -> cons)
                      Range: File "", line 142, characters 6-10
                      Body Range: File "", line 142, characters 11-19
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (filter_map#145:6-16 -> filter_map)
                      Range: File "", line 145, characters 6-16
                      Body Range: File "", line 145, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      (find_opt#143:6-14 -> find_opt)
                      Range: File "", line 143, characters 6-14
                      Body Range: File "", line 143, characters 15-23
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#139:6-10 -> fold)
                      Range: File "", line 139, characters 6-10
                      Body Range: File "", line 139, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 327, characters 9-13
                      (fold_left#140:6-15 -> fold_left)
                      Range: File "", line 140, characters 6-15
                      Body Range: File "", line 140, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#141:6-16 -> fold_right)
                      Range: File "", line 141, characters 6-16
                      Body Range: File "", line 141, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 144, characters 4-14 ,
                        File "", line 146, characters 4-14
                      (head_opt#134:6-14 -> head_opt)
                      Range: File "", line 134, characters 6-14
                      Body Range: File "", line 134, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#138:6-10 -> iter)
                      Range: File "", line 138, characters 6-10
                      Body Range: File "", line 138, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#132:6-12 -> length)
                      Range: File "", line 132, characters 6-12
                      Body Range: File "", line 132, characters 13-21
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#137:6-9 -> map)
                      Range: File "", line 137, characters 6-9
                      Body Range: File "", line 137, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 148, characters 4-7 ,
                        File "", line 150, characters 4-7
                      (size#133:6-10 -> size)
                      Range: File "", line 133, characters 6-10
                      Body Range: File "", line 133, characters 11-19
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#135:6-14 -> tail_opt)
                      Range: File "", line 135, characters 6-14
                      Body Range: File "", line 135, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      (update#147:6-12 -> update)
                      Range: File "", line 147, characters 6-12
                      Body Range: File "", line 147, characters 13-21
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      (update_with#149:6-17 -> update_with)
                      Range: File "", line 149, characters 6-17
                      Body Range: File "", line 149, characters 18-26
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 327, characters 4-8

    (Map#90:7-10 -> Map)
    Range: File "", line 90, characters 7-10
    Body Range: File "", line 90, character 0 to line 106, character 3
    Content: Members: Variable definitions:
                      (add#96:6-9 -> add)
                      Range: File "", line 96, characters 6-9
                      Body Range: File "", line 96, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (empty#91:6-11 -> empty)
                      Range: File "", line 91, characters 6-11
                      Body Range: File "", line 91, characters 12-22
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#100:6-10 -> find)
                      Range: File "", line 100, characters 6-10
                      Body Range: File "", line 100, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#101:6-14 -> find_opt)
                      Range: File "", line 101, characters 6-14
                      Body Range: File "", line 101, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (fold#104:6-10 -> fold)
                      Range: File "", line 104, characters 6-10
                      Body Range: File "", line 104, characters 11-23
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      (get_and_update#99:6-20 -> get_and_update)
                      Range: File "", line 99, characters 6-20
                      Body Range: File "", line 99, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (iter#102:6-10 -> iter)
                      Range: File "", line 102, characters 6-10
                      Body Range: File "", line 102, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (literal#93:25-32 -> literal)
                      Range: File "", line 93, characters 25-32
                      Body Range: File "", line 93, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#103:6-9 -> map)
                      Range: File "", line 103, characters 6-9
                      Body Range: File "", line 103, characters 10-22
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (mem#95:6-9 -> mem)
                      Range: File "", line 95, characters 6-9
                      Body Range: File "", line 95, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (remove#97:6-12 -> remove)
                      Range: File "", line 97, characters 6-12
                      Body Range: File "", line 97, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (size#92:6-10 -> size)
                      Range: File "", line 92, characters 6-10
                      Body Range: File "", line 92, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#98:6-12 -> update)
                      Range: File "", line 98, characters 6-12
                      Body Range: File "", line 98, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#161:7-13 -> Option)
    Range: File "", line 161, characters 7-13
    Body Range: File "", line 161, character 0 to line 170, character 3
    Content: Members: Variable definitions:
                      (is_none#168:6-13 -> is_none)
                      Range: File "", line 168, characters 6-13
                      Body Range: File "", line 168, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#169:6-13 -> is_some)
                      Range: File "", line 169, characters 6-13
                      Body Range: File "", line 169, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#165:15-18 -> map)
                      Range: File "", line 165, characters 15-18
                      Body Range: File "", line 165, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#162:6-11 -> unopt)
                      Range: File "", line 162, characters 6-11
                      Body Range: File "", line 162, characters 12-20
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#164:6-22 -> unopt_with_error)
                      Range: File "", line 164, characters 6-22
                      Body Range: File "", line 164, characters 23-31
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#166:6-11 -> value)
                      Range: File "", line 166, characters 6-11
                      Body Range: File "", line 166, characters 12-20
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#167:6-15 -> value_exn)
                      Range: File "", line 167, characters 6-15
                      Body Range: File "", line 167, characters 16-28
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#114:7-10 -> Set)
    Range: File "", line 114, characters 7-10
    Body Range: File "", line 114, character 0 to line 129, character 3
    Content: Members: Variable definitions:
                      (add#121:6-9 -> add)
                      Range: File "", line 121, characters 6-9
                      Body Range: File "", line 121, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: File "", line 128, characters 81-84
                      (cardinal#117:6-14 -> cardinal)
                      Range: File "", line 117, characters 6-14
                      Body Range: File "", line 117, characters 15-23
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#115:6-11 -> empty)
                      Range: File "", line 115, characters 6-11
                      Body Range: File "", line 115, characters 12-20
                      Content: |core: ∀ a : * . set (a)|
                      references: File "", line 128, characters 96-101
                      (filter_map#127:6-16 -> filter_map)
                      Range: File "", line 127, characters 6-16
                      Body Range: File "", line 127, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> set (a) -> set (b)|
                      references: []
                      (fold#125:6-10 -> fold)
                      Range: File "", line 125, characters 6-10
                      Body Range: File "", line 125, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#126:6-15 -> fold_desc)
                      Range: File "", line 126, characters 6-15
                      Body Range: File "", line 126, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: File "", line 128, characters 4-13
                      (iter#124:6-10 -> iter)
                      Range: File "", line 124, characters 6-10
                      Body Range: File "", line 124, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#118:25-32 -> literal)
                      Range: File "", line 118, characters 25-32
                      Body Range: File "", line 118, characters 33-41
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#120:6-9 -> mem)
                      Range: File "", line 120, characters 6-9
                      Body Range: File "", line 120, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#122:6-12 -> remove)
                      Range: File "", line 122, characters 6-12
                      Body Range: File "", line 122, characters 13-21
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#116:6-10 -> size)
                      Range: File "", line 116, characters 6-10
                      Body Range: File "", line 116, characters 11-19
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#123:6-12 -> update)
                      Range: File "", line 123, characters 6-12
                      Body Range: File "", line 123, characters 13-21
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#153:7-13 -> String)
    Range: File "", line 153, characters 7-13
    Body Range: File "", line 153, character 0 to line 159, character 3
    Content: Members: Variable definitions:
                      (concat#157:6-12 -> concat)
                      Range: File "", line 157, characters 6-12
                      Body Range: File "", line 157, characters 14-16
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#155:6-13 -> concats)
                      Range: File "", line 155, characters 6-13
                      Body Range: File "", line 155, characters 15-17
                      Content: |core: list (string) -> string|
                      references: []
                      (length#154:6-12 -> length)
                      Range: File "", line 154, characters 6-12
                      Body Range: File "", line 154, characters 14-15
                      Content: |core: string -> nat|
                      references:
                        File "", line 357, characters 22-28 ,
                        File "", line 360, characters 43-49
                      (sub#158:6-9 -> sub)
                      Range: File "", line 158, characters 6-9
                      Body Range: File "", line 158, characters 11-12
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 358, characters 24-27 ,
                        File "", line 360, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 357, characters 15-21 ,
      File "", line 358, characters 17-23 ,
      File "", line 360, characters 16-22 ,
      File "", line 360, characters 36-42

    (Test#232:7-11 -> Test)
    Range: File "", line 232, characters 7-11
    Body Range: File "", line 232, character 0 to line 470, character 3
    Content: Members: Variable definitions:
                      (add_account#340:6-17 -> add_account)
                      Range: File "", line 340, characters 6-17
                      Body Range: File "", line 340, characters 19-20
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#462:6-12 -> assert)
                      Range: File "", line 462, characters 6-12
                      Body Range: File "", line 462, characters 14-15
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#464:6-17 -> assert_none)
                      Range: File "", line 464, characters 6-17
                      Body Range: File "", line 464, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#468:6-28 -> assert_none_with_error)
                      Range: File "", line 468, characters 6-28
                      Body Range: File "", line 468, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#463:6-17 -> assert_some)
                      Range: File "", line 463, characters 6-17
                      Body Range: File "", line 463, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#467:6-28 -> assert_some_with_error)
                      Range: File "", line 467, characters 6-28
                      Body Range: File "", line 467, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#466:6-23 -> assert_with_error)
                      Range: File "", line 466, characters 6-23
                      Body Range: File "", line 466, characters 25-26
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#259:6-28 -> bake_until_n_cycle_end)
                      Range: File "", line 259, characters 6-28
                      Body Range: File "", line 259, characters 30-31
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#341:6-19 -> baker_account)
                      Range: File "", line 341, characters 6-19
                      Body Range: File "", line 341, characters 21-22
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#336:6-24 -> bootstrap_contract)
                      Range: File "", line 336, characters 6-24
                      Body Range: File "", line 336, characters 25-35
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#261:6-18 -> cast_address)
                      Range: File "", line 261, characters 6-18
                      Body Range: File "", line 261, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 370, characters 35-47 ,
                        File "", line 380, characters 35-47 ,
                        File "", line 387, characters 35-47
                      (chr#285:6-9 -> chr)
                      Range: File "", line 285, characters 6-9
                      Body Range: File "", line 285, characters 11-12
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#280:6-22 -> compile_contract)
                      Range: File "", line 280, characters 6-22
                      Body Range: File "", line 280, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 366, characters 12-28 ,
                        File "", line 376, characters 12-28
                      (compile_contract_from_file#389:6-32 -> compile_contract_from_file)
                      Range: File "", line 389, characters 6-32
                      Body Range: File "", line 389, characters 34-36
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 393, characters 12-38
                      (compile_contract_with_views#372:8-35 -> compile_contract_with_views)
                      Range: File "", line 372, characters 8-35
                      Body Range: File "", line 372, characters 36-46
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 383, characters 12-39
                      (compile_value#237:6-19 -> compile_value)
                      Range: File "", line 237, characters 6-19
                      Body Range: File "", line 237, characters 20-28
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#265:6-35 -> constant_to_michelson_program)
                      Range: File "", line 265, characters 6-35
                      Body Range: File "", line 265, characters 37-38
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#343:6-18 -> create_chest)
                      Range: File "", line 343, characters 6-18
                      Body Range: File "", line 343, characters 20-21
                      Content: |core: bytes -> nat -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#344:6-22 -> create_chest_key)
                      Range: File "", line 344, characters 6-22
                      Body Range: File "", line 344, characters 24-25
                      Content: |core: chest -> nat -> chest_key|
                      references: []
                      (decompile#258:6-15 -> decompile)
                      Range: File "", line 258, characters 6-15
                      Body Range: File "", line 258, characters 16-24
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 276, characters 5-14
                      (drop_context#269:6-18 -> drop_context)
                      Range: File "", line 269, characters 6-18
                      Body Range: File "", line 269, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#245:6-12 -> eprint)
                      Range: File "", line 245, characters 6-12
                      Body Range: File "", line 245, characters 14-15
                      Content: |core: string -> unit|
                      references: []
                      (eval#235:6-10 -> eval)
                      Range: File "", line 235, characters 6-10
                      Body Range: File "", line 235, characters 11-19
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 237, characters 59-63 ,
                        File "", line 348, characters 32-36 ,
                        File "", line 353, characters 34-38 ,
                        File "", line 367, characters 12-16 ,
                        File "", line 377, characters 12-16 ,
                        File "", line 384, characters 12-16
                      (failwith#239:6-14 -> failwith)
                      Range: File "", line 239, characters 6-14
                      Body Range: File "", line 239, characters 15-25
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 462, characters 51-59 ,
                        File "", line 463, characters 74-82 ,
                        File "", line 464, characters 89-97 ,
                        File "", line 466, characters 68-76 ,
                        File "", line 467, characters 98-106 ,
                        File "", line 468, characters 113-121
                      (get_balance#243:6-17 -> get_balance)
                      Range: File "", line 243, characters 6-17
                      Body Range: File "", line 243, characters 19-20
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#251:6-27 -> get_bootstrap_account)
                      Range: File "", line 251, characters 6-27
                      Body Range: File "", line 251, characters 29-30
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#320:6-26 -> get_last_events_from)
                      Range: File "", line 320, characters 6-26
                      Body Range: File "", line 320, characters 27-39
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#272:6-17 -> get_storage)
                      Range: File "", line 272, characters 6-17
                      Body Range: File "", line 272, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#242:6-28 -> get_storage_of_address)
                      Range: File "", line 242, characters 6-28
                      Body Range: File "", line 242, characters 30-31
                      Content: |core: address -> michelson_program|
                      references: File "", line 275, characters 32-54
                      (get_time#260:6-14 -> get_time)
                      Range: File "", line 260, characters 6-14
                      Body Range: File "", line 260, characters 16-18
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#238:6-28 -> get_total_voting_power)
                      Range: File "", line 238, characters 6-28
                      Body Range: File "", line 238, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#246:6-22 -> get_voting_power)
                      Range: File "", line 246, characters 6-22
                      Body Range: File "", line 246, characters 24-26
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#253:6-23 -> last_originations)
                      Range: File "", line 253, characters 6-23
                      Body Range: File "", line 253, characters 25-26
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#330:6-9 -> log)
                      Range: File "", line 330, characters 6-9
                      Body Range: File "", line 330, characters 10-18
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 359, characters 25-28
                      (michelson_equal#355:6-21 -> michelson_equal)
                      Range: File "", line 355, characters 6-21
                      Body Range: File "", line 355, characters 23-25
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#337:6-18 -> mutate_value)
                      Range: File "", line 337, characters 6-18
                      Body Range: File "", line 337, characters 19-27
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 401, characters 23-35 ,
                        File "", line 413, characters 23-35
                      (mutation_test#397:6-19 -> mutation_test)
                      Range: File "", line 397, characters 6-19
                      Body Range: File "", line 397, characters 20-30
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#409:6-23 -> mutation_test_all)
                      Range: File "", line 409, characters 6-23
                      Body Range: File "", line 409, characters 24-34
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (new_account#257:6-17 -> new_account)
                      Range: File "", line 257, characters 6-17
                      Body Range: File "", line 257, characters 19-20
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#295:6-8 -> nl)
                      Range: File "", line 295, characters 6-8
                      Body Range: File "", line 295, characters 11-53
                      Content: |unresolved|
                      references: File "", line 297, characters 15-17
                      (nth_bootstrap_account#248:6-27 -> nth_bootstrap_account)
                      Range: File "", line 248, characters 6-27
                      Body Range: File "", line 248, characters 29-30
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#247:6-28 -> nth_bootstrap_contract)
                      Range: File "", line 247, characters 6-28
                      Body Range: File "", line 247, characters 30-31
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#252:6-33 -> nth_bootstrap_typed_address)
                      Range: File "", line 252, characters 6-33
                      Body Range: File "", line 252, characters 34-44
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#365:6-15 -> originate)
                      Range: File "", line 365, characters 6-15
                      Body Range: File "", line 365, characters 16-26
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_contract#364:6-24 -> originate_contract)
                      Range: File "", line 364, characters 6-24
                      Body Range: File "", line 364, characters 26-27
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 368, characters 12-30 ,
                        File "", line 378, characters 12-30 ,
                        File "", line 385, characters 12-30 ,
                        File "", line 394, characters 12-30 ,
                        File "", line 425, characters 14-32 ,
                        File "", line 445, characters 14-32
                      (originate_from_file#392:6-25 -> originate_from_file)
                      Range: File "", line 392, characters 6-25
                      Body Range: File "", line 392, characters 27-29
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_from_file_and_mutate#421:6-36 -> originate_from_file_and_mutate)
                      Range: File "", line 421, characters 6-36
                      Body Range: File "", line 421, characters 37-45
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#441:6-40 -> originate_from_file_and_mutate_all)
                      Range: File "", line 441, characters 6-40
                      Body Range: File "", line 441, characters 41-49
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module#382:6-22 -> originate_module)
                      Range: File "", line 382, characters 6-22
                      Body Range: File "", line 382, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) ->
                                                                ( list (operation) *
                                                                  s ) *
                                                                views (s) ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_uncurried#375:6-25 -> originate_uncurried)
                      Range: File "", line 375, characters 6-25
                      Body Range: File "", line 375, characters 26-36
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (parse_michelson#266:6-21 -> parse_michelson)
                      Range: File "", line 266, characters 6-21
                      Body Range: File "", line 266, characters 23-24
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#244:6-11 -> print)
                      Range: File "", line 244, characters 6-11
                      Body Range: File "", line 244, characters 13-14
                      Content: |core: string -> unit|
                      references:
                        File "", line 297, characters 4-9 ,
                        File "", line 333, characters 4-9
                      (println#296:6-13 -> println)
                      Range: File "", line 296, characters 6-13
                      Body Range: File "", line 296, characters 15-16
                      Content: |core: string -> unit|
                      references: []
                      (random#254:6-12 -> random)
                      Range: File "", line 254, characters 6-12
                      Body Range: File "", line 254, characters 13-21
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#284:6-29 -> read_contract_from_file)
                      Range: File "", line 284, characters 6-29
                      Body Range: File "", line 284, characters 31-33
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#263:6-23 -> register_constant)
                      Range: File "", line 263, characters 6-23
                      Body Range: File "", line 263, characters 25-26
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#262:6-23 -> register_delegate)
                      Range: File "", line 262, characters 6-23
                      Body Range: File "", line 262, characters 25-27
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#334:6-17 -> reset_state)
                      Range: File "", line 334, characters 6-17
                      Body Range: File "", line 334, characters 19-20
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#335:6-20 -> reset_state_at)
                      Range: File "", line 335, characters 6-20
                      Body Range: File "", line 335, characters 22-23
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#267:6-21 -> restore_context)
                      Range: File "", line 267, characters 6-21
                      Body Range: File "", line 267, characters 23-24
                      Content: |core: unit -> unit|
                      references: []
                      (run#234:6-9 -> run)
                      Range: File "", line 234, characters 6-9
                      Body Range: File "", line 234, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 235, characters 50-53
                      (save_context#268:6-18 -> save_context)
                      Range: File "", line 268, characters 6-18
                      Body Range: File "", line 268, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#338:6-19 -> save_mutation)
                      Range: File "", line 338, characters 6-19
                      Body Range: File "", line 338, characters 21-22
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#278:6-15 -> set_baker)
                      Range: File "", line 278, characters 6-15
                      Body Range: File "", line 278, characters 17-18
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#277:6-22 -> set_baker_policy)
                      Range: File "", line 277, characters 6-22
                      Body Range: File "", line 277, characters 24-26
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 278, characters 39-55
                      (set_big_map#342:6-17 -> set_big_map)
                      Range: File "", line 342, characters 6-17
                      Body Range: File "", line 342, characters 18-28
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#299:6-22 -> set_print_values)
                      Range: File "", line 299, characters 6-22
                      Body Range: File "", line 299, characters 24-25
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#241:6-16 -> set_source)
                      Range: File "", line 241, characters 6-16
                      Body Range: File "", line 241, characters 18-19
                      Content: |core: address -> unit|
                      references: []
                      (sign#339:6-10 -> sign)
                      Range: File "", line 339, characters 6-10
                      Body Range: File "", line 339, characters 12-14
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#279:6-10 -> size)
                      Range: File "", line 279, characters 6-10
                      Body Range: File "", line 279, characters 12-13
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 369, characters 12-16 ,
                        File "", line 379, characters 12-16 ,
                        File "", line 386, characters 12-16 ,
                        File "", line 395, characters 12-16 ,
                        File "", line 426, characters 14-18 ,
                        File "", line 446, characters 14-18
                      (to_contract#240:6-17 -> to_contract)
                      Range: File "", line 240, characters 6-17
                      Body Range: File "", line 240, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 273, characters 25-36 ,
                        File "", line 321, characters 30-41
                      (to_entrypoint#356:6-19 -> to_entrypoint)
                      Range: File "", line 356, characters 6-19
                      Body Range: File "", line 356, characters 20-32
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#271:6-13 -> to_json)
                      Range: File "", line 271, characters 6-13
                      Body Range: File "", line 271, characters 14-22
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#270:6-15 -> to_string)
                      Range: File "", line 270, characters 6-15
                      Body Range: File "", line 270, characters 16-24
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 288, characters 68-77 ,
                        File "", line 290, characters 67-76 ,
                        File "", line 292, characters 61-70 ,
                        File "", line 332, characters 12-21
                      (to_typed_address#264:6-22 -> to_typed_address)
                      Range: File "", line 264, characters 6-22
                      Body Range: File "", line 264, characters 23-33
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#328:6-14 -> transfer)
                      Range: File "", line 328, characters 6-14
                      Body Range: File "", line 328, characters 16-17
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#329:6-18 -> transfer_exn)
                      Range: File "", line 329, characters 6-18
                      Body Range: File "", line 329, characters 20-21
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#345:6-26 -> transfer_to_contract)
                      Range: File "", line 345, characters 6-26
                      Body Range: File "", line 345, characters 27-35
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#350:6-30 -> transfer_to_contract_exn)
                      Range: File "", line 350, characters 6-30
                      Body Range: File "", line 350, characters 31-39
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#300:6-24 -> unset_print_values)
                      Range: File "", line 300, characters 6-24
                      Body Range: File "", line 300, characters 26-27
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#302:9-12 -> PBT)
                      Range: File "", line 302, characters 9-12
                      Body Range: File "", line 302, character 2 to line 318, character 5
                      Content: Members: Variable definitions:
                                        (gen#303:8-11 -> gen)
                                        Range: File "", line 303, characters 8-11
                                        Body Range: File "", line 303, characters 12-20
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (gen_small#304:8-17 -> gen_small)
                                        Range: File "", line 304, characters 8-17
                                        Body Range: File "", line 304, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#305:8-17 -> make_test)
                                        Range: File "", line 305, characters 8-17
                                        Body Range: File "", line 305, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#306:8-11 -> run)
                                        Range: File "", line 306, characters 8-11
                                        Body Range: File "", line 306, characters 12-20
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#7:7-12 -> Tezos)
    Range: File "", line 7, characters 7-12
    Body Range: File "", line 7, character 0 to line 66, character 3
    Content: Members: Variable definitions:
                      (address#20:6-13 -> address)
                      Range: File "", line 20, characters 6-13
                      Body Range: File "", line 20, characters 52-110
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 321, characters 21-28
                      (call_view#46:25-34 -> call_view)
                      Range: File "", line 46, characters 25-34
                      Body Range: File "", line 46, characters 35-45
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (constant#31:25-33 -> constant)
                      Range: File "", line 31, characters 25-33
                      Body Range: File "", line 31, characters 62-96
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (create_contract#50:25-40 -> create_contract)
                      Range: File "", line 50, characters 25-40
                      Body Range: File "", line 50, characters 41-51
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: []
                      (create_contract_uncurried#53:25-50 -> create_contract_uncurried)
                      Range: File "", line 53, characters 25-50
                      Body Range: File "", line 53, characters 51-61
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      (create_ticket#43:6-19 -> create_ticket)
                      Range: File "", line 43, characters 6-19
                      Body Range: File "", line 43, characters 20-28
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references: []
                      (emit#61:25-29 -> emit)
                      Range: File "", line 61, characters 25-29
                      Body Range: File "", line 61, characters 30-38
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
                      Body Range: File "", line 58, characters 40-48
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (get_entrypoint_opt#55:25-43 -> get_entrypoint_opt)
                      Range: File "", line 55, characters 25-43
                      Body Range: File "", line 55, characters 44-52
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
                      references: File "", line 260, characters 47-54
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
                      Body Range: File "", line 64, characters 47-59
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
                      Body Range: File "", line 48, characters 19-27
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      (transaction#44:6-17 -> transaction)
                      Range: File "", line 44, characters 6-17
                      Body Range: File "", line 44, characters 18-26
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
      File "", line 260, characters 41-46 ,
      File "", line 321, characters 15-20

    (Transpiled#108:7-17 -> Transpiled)
    Range: File "", line 108, characters 7-17
    Body Range: File "", line 108, character 0 to line 112, character 3
    Content: Members: Variable definitions:
                      (map_add#110:6-13 -> map_add)
                      Range: File "", line 110, characters 6-13
                      Body Range: File "", line 110, characters 14-26
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      (map_find_opt#109:6-18 -> map_find_opt)
                      Range: File "", line 109, characters 6-18
                      Body Range: File "", line 109, characters 19-29
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      (map_remove#111:6-16 -> map_remove)
                      Range: File "", line 111, characters 6-16
                      Body Range: File "", line 111, characters 17-27
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: [] |}]
