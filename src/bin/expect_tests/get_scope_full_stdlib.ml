open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ failwith#2:4-12 bool#4:5-9 option#5:8-14 Tezos#7:7-12 get_balance#9:6-17 get_amount#10:6-16 get_now#11:6-13 get_sender#12:6-16 get_source#13:6-16 get_level#14:6-15 get_self_address#15:6-22 get_chain_id#16:6-18 get_total_voting_power#17:6-28 get_min_block_time#18:6-24 voting_power#19:6-18 address#20:6-13 implicit_account#21:6-22 join_tickets#22:6-18 read_ticket#23:6-17 never#25:6-11 pairing_check#26:6-19 set_delegate#27:6-18 self#28:25-29 constant#31:25-33 sapling_empty_state#32:25-44 get_contract_opt#34:25-41 get_contract#36:25-37 get_contract_with_error#40:6-29 create_ticket#43:6-19 transaction#44:6-17 call_view#46:25-34 split_ticket#49:6-18 create_contract#51:25-40 create_contract_uncurried#54:25-50 get_entrypoint_opt#56:25-43 get_entrypoint#59:25-39 emit#62:25-29 sapling_verify_update#65:25-46 Bitwise#69:7-14 and#70:6-10 xor#71:6-9 or#72:6-9 shift_left#73:6-16 shift_right#74:6-17 Big_map#77:7-14 empty#78:16-21 literal#79:25-32 mem#81:6-9 add#82:6-9 remove#83:6-12 update#84:6-12 get_and_update#85:6-20 find_opt#86:6-14 find#87:6-10 Map#91:7-10 empty#92:6-11 size#93:6-10 literal#94:25-32 mem#96:6-9 add#97:6-9 remove#98:6-12 update#99:6-12 get_and_update#100:6-20 find#101:6-10 find_opt#102:6-14 iter#103:6-10 map#104:6-9 fold#105:6-10 Transpiled#109:7-17 map_find_opt#110:6-18 map_add#111:6-13 map_remove#112:6-16 Set#115:7-10 empty#116:6-11 size#117:6-10 cardinal#118:6-14 literal#119:25-32 mem#121:6-9 add#122:6-9 remove#123:6-12 update#124:6-12 iter#125:6-10 fold#126:6-10 fold_desc#127:6-15 filter_map#128:6-16 List#132:7-11 length#133:6-12 size#134:6-10 head_opt#135:6-14 tail_opt#136:6-14 map#138:6-9 iter#139:6-10 fold#140:6-10 fold_left#141:6-15 fold_right#142:6-16 cons#143:6-10 find_opt#144:6-14 filter_map#146:6-16 update#148:6-12 update_with#150:6-17 String#154:7-13 length#155:6-12 concats#156:6-13 concat#158:6-12 sub#159:6-9 Option#162:7-13 unopt#163:6-11 unopt_with_error#165:6-22 map#166:15-18 value#167:6-11 value_exn#168:6-15 is_none#169:6-13 is_some#170:6-13 Bytes#173:7-12 concats#174:6-13 pack#175:6-10 unpack#176:6-12 length#177:6-12 concat#179:6-12 sub#180:6-9 Crypto#183:7-13 blake2b#184:6-13 sha256#185:6-12 sha512#186:6-12 sha3#187:6-10 keccak#188:6-12 hash_key#189:6-14 check#190:6-11 assert#193:4-10 assert_some#194:4-15 assert_none#195:4-15 abs#196:4-7 is_nat#197:4-10 true#198:14-18 false#199:14-19 unit#200:14-18 int#202:4-7 nat#207:4-7 bytes#208:4-9 ignore#210:4-10 curry#211:4-9 uncurry#212:4-11 assert_with_error#214:4-21 assert_some_with_error#215:4-26 assert_none_with_error#216:4-26 ediv#217:4-8 test_exec_error_balance_too_low#220:5-36 test_exec_error#223:5-20 test_exec_result#228:5-21 test_baker_policy#230:5-22 pbt_test#235:8-16 pbt_result#236:8-18 unforged_ticket#238:8-23 module_contract#240:14-29 Test#242:7-11 run#244:6-9 eval#245:6-10 compile_value#247:6-19 get_total_voting_power#248:6-28 failwith#249:6-14 to_contract#250:6-17 set_source#251:6-16 get_storage_of_address#252:6-28 get_balance#253:6-17 print#254:6-11 eprint#255:6-12 get_voting_power#256:6-22 nth_bootstrap_contract#257:6-28 nth_bootstrap_account#258:6-27 get_bootstrap_account#261:6-27 nth_bootstrap_typed_address#262:6-33 last_originations#263:6-23 random#264:6-12 new_account#267:6-17 decompile#268:6-15 bake_until_n_cycle_end#269:6-28 get_time#270:6-14 cast_address#271:6-18 register_delegate#272:6-23 register_constant#273:6-23 to_typed_address#274:6-22 constant_to_michelson_program#275:6-35 parse_michelson#276:6-21 restore_context#277:6-21 save_context#278:6-18 drop_context#279:6-18 to_string#280:6-15 to_json#281:6-13 get_storage#282:6-17 set_baker_policy#287:6-22 set_baker#288:6-15 size#289:6-10 compile_contract#290:6-22 read_contract_from_file#294:6-29 chr#295:6-9 nl#305:6-8 println#306:6-13 set_print_values#309:6-22 unset_print_values#310:6-24 PBT#312:9-12 gen#313:8-11 gen_small#314:8-17 make_test#315:8-17 run#316:8-11 get_last_events_from#330:6-26 transfer#338:6-14 transfer_exn#339:6-18 log#340:6-9 reset_state#344:6-17 reset_state_at#345:6-20 bootstrap_contract#346:6-24 mutate_value#347:6-18 save_mutation#348:6-19 sign#349:6-10 add_account#350:6-17 baker_account#351:6-19 set_big_map#352:6-17 transfer_to_contract#353:6-26 transfer_to_contract_exn#358:6-30 michelson_equal#363:6-21 to_entrypoint#364:6-19 originate_contract#372:6-24 originate#373:6-15 compile_contract_with_views#380:8-35 originate_uncurried#383:6-25 originate_module#390:6-22 compile_contract_from_file#397:6-32 originate_from_file#400:6-25 mutation_test#405:6-19 mutation_test_all#417:6-23 originate_from_file_and_mutate#429:6-36 originate_from_file_and_mutate_all#449:6-40 originate_module_and_mutate#469:6-33 originate_module_and_mutate_all#491:6-37 assert#514:6-12 assert_some#515:6-17 assert_none#516:6-17 assert_with_error#518:6-23 assert_some_with_error#519:6-28 assert_none_with_error#520:6-28 Proxy_ticket#522:9-21 proxy_transfer_contract#523:19-42 proxy_originate_contract#535:19-43 proxy_address#547:12-25 init_transfer#549:8-21 transfer#556:8-16 originate_uncurried#562:8-27 originate#577:8-17  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ failwith#2:4-12 bool#4:5-9 option#5:8-14 Tezos#7:7-12 get_balance#9:6-17 get_amount#10:6-16 get_now#11:6-13 get_sender#12:6-16 get_source#13:6-16 get_level#14:6-15 get_self_address#15:6-22 get_chain_id#16:6-18 get_total_voting_power#17:6-28 get_min_block_time#18:6-24 voting_power#19:6-18 address#20:6-13 implicit_account#21:6-22 join_tickets#22:6-18 read_ticket#23:6-17 never#25:6-11 pairing_check#26:6-19 set_delegate#27:6-18 self#28:25-29 constant#31:25-33 sapling_empty_state#32:25-44 get_contract_opt#34:25-41 get_contract#36:25-37 get_contract_with_error#40:6-29 create_ticket#43:6-19 transaction#44:6-17 call_view#46:25-34 split_ticket#49:6-18 create_contract#51:25-40 create_contract_uncurried#54:25-50 get_entrypoint_opt#56:25-43 get_entrypoint#59:25-39 emit#62:25-29 sapling_verify_update#65:25-46 Bitwise#69:7-14 and#70:6-10 xor#71:6-9 or#72:6-9 shift_left#73:6-16 shift_right#74:6-17 Big_map#77:7-14 empty#78:16-21 literal#79:25-32 mem#81:6-9 add#82:6-9 remove#83:6-12 update#84:6-12 get_and_update#85:6-20 find_opt#86:6-14 find#87:6-10 Map#91:7-10 empty#92:6-11 size#93:6-10 literal#94:25-32 mem#96:6-9 add#97:6-9 remove#98:6-12 update#99:6-12 get_and_update#100:6-20 find#101:6-10 find_opt#102:6-14 iter#103:6-10 map#104:6-9 fold#105:6-10 Transpiled#109:7-17 map_find_opt#110:6-18 map_add#111:6-13 map_remove#112:6-16 Set#115:7-10 empty#116:6-11 size#117:6-10 cardinal#118:6-14 literal#119:25-32 mem#121:6-9 add#122:6-9 remove#123:6-12 update#124:6-12 iter#125:6-10 fold#126:6-10 fold_desc#127:6-15 filter_map#128:6-16 List#132:7-11 length#133:6-12 size#134:6-10 head_opt#135:6-14 tail_opt#136:6-14 map#138:6-9 iter#139:6-10 fold#140:6-10 fold_left#141:6-15 fold_right#142:6-16 cons#143:6-10 find_opt#144:6-14 filter_map#146:6-16 update#148:6-12 update_with#150:6-17 String#154:7-13 length#155:6-12 concats#156:6-13 concat#158:6-12 sub#159:6-9 Option#162:7-13 unopt#163:6-11 unopt_with_error#165:6-22 map#166:15-18 value#167:6-11 value_exn#168:6-15 is_none#169:6-13 is_some#170:6-13 Bytes#173:7-12 concats#174:6-13 pack#175:6-10 unpack#176:6-12 length#177:6-12 concat#179:6-12 sub#180:6-9 Crypto#183:7-13 blake2b#184:6-13 sha256#185:6-12 sha512#186:6-12 sha3#187:6-10 keccak#188:6-12 hash_key#189:6-14 check#190:6-11 assert#193:4-10 assert_some#194:4-15 assert_none#195:4-15 abs#196:4-7 is_nat#197:4-10 true#198:14-18 false#199:14-19 unit#200:14-18 int#202:4-7 nat#207:4-7 bytes#208:4-9 ignore#210:4-10 curry#211:4-9 uncurry#212:4-11 assert_with_error#214:4-21 assert_some_with_error#215:4-26 assert_none_with_error#216:4-26 ediv#217:4-8 test_exec_error_balance_too_low#220:5-36 test_exec_error#223:5-20 test_exec_result#228:5-21 test_baker_policy#230:5-22 pbt_test#235:8-16 pbt_result#236:8-18 unforged_ticket#238:8-23 module_contract#240:14-29 Test#242:7-11 run#244:6-9 eval#245:6-10 compile_value#247:6-19 get_total_voting_power#248:6-28 failwith#249:6-14 to_contract#250:6-17 set_source#251:6-16 get_storage_of_address#252:6-28 get_balance#253:6-17 print#254:6-11 eprint#255:6-12 get_voting_power#256:6-22 nth_bootstrap_contract#257:6-28 nth_bootstrap_account#258:6-27 get_bootstrap_account#261:6-27 nth_bootstrap_typed_address#262:6-33 last_originations#263:6-23 random#264:6-12 new_account#267:6-17 decompile#268:6-15 bake_until_n_cycle_end#269:6-28 get_time#270:6-14 cast_address#271:6-18 register_delegate#272:6-23 register_constant#273:6-23 to_typed_address#274:6-22 constant_to_michelson_program#275:6-35 parse_michelson#276:6-21 restore_context#277:6-21 save_context#278:6-18 drop_context#279:6-18 to_string#280:6-15 to_json#281:6-13 get_storage#282:6-17 set_baker_policy#287:6-22 set_baker#288:6-15 size#289:6-10 compile_contract#290:6-22 read_contract_from_file#294:6-29 chr#295:6-9 nl#305:6-8 println#306:6-13 set_print_values#309:6-22 unset_print_values#310:6-24 PBT#312:9-12 gen#313:8-11 gen_small#314:8-17 make_test#315:8-17 run#316:8-11 get_last_events_from#330:6-26 transfer#338:6-14 transfer_exn#339:6-18 log#340:6-9 reset_state#344:6-17 reset_state_at#345:6-20 bootstrap_contract#346:6-24 mutate_value#347:6-18 save_mutation#348:6-19 sign#349:6-10 add_account#350:6-17 baker_account#351:6-19 set_big_map#352:6-17 transfer_to_contract#353:6-26 transfer_to_contract_exn#358:6-30 michelson_equal#363:6-21 to_entrypoint#364:6-19 originate_contract#372:6-24 originate#373:6-15 compile_contract_with_views#380:8-35 originate_uncurried#383:6-25 originate_module#390:6-22 compile_contract_from_file#397:6-32 originate_from_file#400:6-25 mutation_test#405:6-19 mutation_test_all#417:6-23 originate_from_file_and_mutate#429:6-36 originate_from_file_and_mutate_all#449:6-40 originate_module_and_mutate#469:6-33 originate_module_and_mutate_all#491:6-37 assert#514:6-12 assert_some#515:6-17 assert_none#516:6-17 assert_with_error#518:6-23 assert_some_with_error#519:6-28 assert_none_with_error#520:6-28 Proxy_ticket#522:9-21 proxy_transfer_contract#523:19-42 proxy_originate_contract#535:19-43 proxy_address#547:12-25 init_transfer#549:8-21 transfer#556:8-16 originate_uncurried#562:8-27 originate#577:8-17 a#1:4-5  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    [ failwith#2:4-12 bool#4:5-9 option#5:8-14 Tezos#7:7-12 get_balance#9:6-17 get_amount#10:6-16 get_now#11:6-13 get_sender#12:6-16 get_source#13:6-16 get_level#14:6-15 get_self_address#15:6-22 get_chain_id#16:6-18 get_total_voting_power#17:6-28 get_min_block_time#18:6-24 voting_power#19:6-18 address#20:6-13 implicit_account#21:6-22 join_tickets#22:6-18 read_ticket#23:6-17 never#25:6-11 pairing_check#26:6-19 set_delegate#27:6-18 self#28:25-29 constant#31:25-33 sapling_empty_state#32:25-44 get_contract_opt#34:25-41 get_contract#36:25-37 get_contract_with_error#40:6-29 create_ticket#43:6-19 transaction#44:6-17 call_view#46:25-34 split_ticket#49:6-18 create_contract#51:25-40 create_contract_uncurried#54:25-50 get_entrypoint_opt#56:25-43 get_entrypoint#59:25-39 emit#62:25-29 sapling_verify_update#65:25-46 Bitwise#69:7-14 and#70:6-10 xor#71:6-9 or#72:6-9 shift_left#73:6-16 shift_right#74:6-17 Big_map#77:7-14 empty#78:16-21 literal#79:25-32 mem#81:6-9 add#82:6-9 remove#83:6-12 update#84:6-12 get_and_update#85:6-20 find_opt#86:6-14 find#87:6-10 Map#91:7-10 empty#92:6-11 size#93:6-10 literal#94:25-32 mem#96:6-9 add#97:6-9 remove#98:6-12 update#99:6-12 get_and_update#100:6-20 find#101:6-10 find_opt#102:6-14 iter#103:6-10 map#104:6-9 fold#105:6-10 Transpiled#109:7-17 map_find_opt#110:6-18 map_add#111:6-13 map_remove#112:6-16 Set#115:7-10 empty#116:6-11 size#117:6-10 cardinal#118:6-14 literal#119:25-32 mem#121:6-9 add#122:6-9 remove#123:6-12 update#124:6-12 iter#125:6-10 fold#126:6-10 fold_desc#127:6-15 filter_map#128:6-16 List#132:7-11 length#133:6-12 size#134:6-10 head_opt#135:6-14 tail_opt#136:6-14 map#138:6-9 iter#139:6-10 fold#140:6-10 fold_left#141:6-15 fold_right#142:6-16 cons#143:6-10 find_opt#144:6-14 filter_map#146:6-16 update#148:6-12 update_with#150:6-17 String#154:7-13 length#155:6-12 concats#156:6-13 concat#158:6-12 sub#159:6-9 Option#162:7-13 unopt#163:6-11 unopt_with_error#165:6-22 map#166:15-18 value#167:6-11 value_exn#168:6-15 is_none#169:6-13 is_some#170:6-13 Bytes#173:7-12 concats#174:6-13 pack#175:6-10 unpack#176:6-12 length#177:6-12 concat#179:6-12 sub#180:6-9 Crypto#183:7-13 blake2b#184:6-13 sha256#185:6-12 sha512#186:6-12 sha3#187:6-10 keccak#188:6-12 hash_key#189:6-14 check#190:6-11 assert#193:4-10 assert_some#194:4-15 assert_none#195:4-15 abs#196:4-7 is_nat#197:4-10 true#198:14-18 false#199:14-19 unit#200:14-18 int#202:4-7 nat#207:4-7 bytes#208:4-9 ignore#210:4-10 curry#211:4-9 uncurry#212:4-11 assert_with_error#214:4-21 assert_some_with_error#215:4-26 assert_none_with_error#216:4-26 ediv#217:4-8 test_exec_error_balance_too_low#220:5-36 test_exec_error#223:5-20 test_exec_result#228:5-21 test_baker_policy#230:5-22 pbt_test#235:8-16 pbt_result#236:8-18 unforged_ticket#238:8-23 module_contract#240:14-29 Test#242:7-11 run#244:6-9 eval#245:6-10 compile_value#247:6-19 get_total_voting_power#248:6-28 failwith#249:6-14 to_contract#250:6-17 set_source#251:6-16 get_storage_of_address#252:6-28 get_balance#253:6-17 print#254:6-11 eprint#255:6-12 get_voting_power#256:6-22 nth_bootstrap_contract#257:6-28 nth_bootstrap_account#258:6-27 get_bootstrap_account#261:6-27 nth_bootstrap_typed_address#262:6-33 last_originations#263:6-23 random#264:6-12 new_account#267:6-17 decompile#268:6-15 bake_until_n_cycle_end#269:6-28 get_time#270:6-14 cast_address#271:6-18 register_delegate#272:6-23 register_constant#273:6-23 to_typed_address#274:6-22 constant_to_michelson_program#275:6-35 parse_michelson#276:6-21 restore_context#277:6-21 save_context#278:6-18 drop_context#279:6-18 to_string#280:6-15 to_json#281:6-13 get_storage#282:6-17 set_baker_policy#287:6-22 set_baker#288:6-15 size#289:6-10 compile_contract#290:6-22 read_contract_from_file#294:6-29 chr#295:6-9 nl#305:6-8 println#306:6-13 set_print_values#309:6-22 unset_print_values#310:6-24 PBT#312:9-12 gen#313:8-11 gen_small#314:8-17 make_test#315:8-17 run#316:8-11 get_last_events_from#330:6-26 transfer#338:6-14 transfer_exn#339:6-18 log#340:6-9 reset_state#344:6-17 reset_state_at#345:6-20 bootstrap_contract#346:6-24 mutate_value#347:6-18 save_mutation#348:6-19 sign#349:6-10 add_account#350:6-17 baker_account#351:6-19 set_big_map#352:6-17 transfer_to_contract#353:6-26 transfer_to_contract_exn#358:6-30 michelson_equal#363:6-21 to_entrypoint#364:6-19 originate_contract#372:6-24 originate#373:6-15 compile_contract_with_views#380:8-35 originate_uncurried#383:6-25 originate_module#390:6-22 compile_contract_from_file#397:6-32 originate_from_file#400:6-25 mutation_test#405:6-19 mutation_test_all#417:6-23 originate_from_file_and_mutate#429:6-36 originate_from_file_and_mutate_all#449:6-40 originate_module_and_mutate#469:6-33 originate_module_and_mutate_all#491:6-37 assert#514:6-12 assert_some#515:6-17 assert_none#516:6-17 assert_with_error#518:6-23 assert_some_with_error#519:6-28 assert_none_with_error#520:6-28 Proxy_ticket#522:9-21 proxy_transfer_contract#523:19-42 proxy_originate_contract#535:19-43 proxy_address#547:12-25 init_transfer#549:8-21 transfer#556:8-16 originate_uncurried#562:8-27 originate#577:8-17 a#1:4-5 c#5:10-11  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ failwith#2:4-12 bool#4:5-9 option#5:8-14 Tezos#7:7-12 get_balance#9:6-17 get_amount#10:6-16 get_now#11:6-13 get_sender#12:6-16 get_source#13:6-16 get_level#14:6-15 get_self_address#15:6-22 get_chain_id#16:6-18 get_total_voting_power#17:6-28 get_min_block_time#18:6-24 voting_power#19:6-18 address#20:6-13 implicit_account#21:6-22 join_tickets#22:6-18 read_ticket#23:6-17 never#25:6-11 pairing_check#26:6-19 set_delegate#27:6-18 self#28:25-29 constant#31:25-33 sapling_empty_state#32:25-44 get_contract_opt#34:25-41 get_contract#36:25-37 get_contract_with_error#40:6-29 create_ticket#43:6-19 transaction#44:6-17 call_view#46:25-34 split_ticket#49:6-18 create_contract#51:25-40 create_contract_uncurried#54:25-50 get_entrypoint_opt#56:25-43 get_entrypoint#59:25-39 emit#62:25-29 sapling_verify_update#65:25-46 Bitwise#69:7-14 and#70:6-10 xor#71:6-9 or#72:6-9 shift_left#73:6-16 shift_right#74:6-17 Big_map#77:7-14 empty#78:16-21 literal#79:25-32 mem#81:6-9 add#82:6-9 remove#83:6-12 update#84:6-12 get_and_update#85:6-20 find_opt#86:6-14 find#87:6-10 Map#91:7-10 empty#92:6-11 size#93:6-10 literal#94:25-32 mem#96:6-9 add#97:6-9 remove#98:6-12 update#99:6-12 get_and_update#100:6-20 find#101:6-10 find_opt#102:6-14 iter#103:6-10 map#104:6-9 fold#105:6-10 Transpiled#109:7-17 map_find_opt#110:6-18 map_add#111:6-13 map_remove#112:6-16 Set#115:7-10 empty#116:6-11 size#117:6-10 cardinal#118:6-14 literal#119:25-32 mem#121:6-9 add#122:6-9 remove#123:6-12 update#124:6-12 iter#125:6-10 fold#126:6-10 fold_desc#127:6-15 filter_map#128:6-16 List#132:7-11 length#133:6-12 size#134:6-10 head_opt#135:6-14 tail_opt#136:6-14 map#138:6-9 iter#139:6-10 fold#140:6-10 fold_left#141:6-15 fold_right#142:6-16 cons#143:6-10 find_opt#144:6-14 filter_map#146:6-16 update#148:6-12 update_with#150:6-17 String#154:7-13 length#155:6-12 concats#156:6-13 concat#158:6-12 sub#159:6-9 Option#162:7-13 unopt#163:6-11 unopt_with_error#165:6-22 map#166:15-18 value#167:6-11 value_exn#168:6-15 is_none#169:6-13 is_some#170:6-13 Bytes#173:7-12 concats#174:6-13 pack#175:6-10 unpack#176:6-12 length#177:6-12 concat#179:6-12 sub#180:6-9 Crypto#183:7-13 blake2b#184:6-13 sha256#185:6-12 sha512#186:6-12 sha3#187:6-10 keccak#188:6-12 hash_key#189:6-14 check#190:6-11 assert#193:4-10 assert_some#194:4-15 assert_none#195:4-15 abs#196:4-7 is_nat#197:4-10 true#198:14-18 false#199:14-19 unit#200:14-18 int#202:4-7 nat#207:4-7 bytes#208:4-9 ignore#210:4-10 curry#211:4-9 uncurry#212:4-11 assert_with_error#214:4-21 assert_some_with_error#215:4-26 assert_none_with_error#216:4-26 ediv#217:4-8 test_exec_error_balance_too_low#220:5-36 test_exec_error#223:5-20 test_exec_result#228:5-21 test_baker_policy#230:5-22 pbt_test#235:8-16 pbt_result#236:8-18 unforged_ticket#238:8-23 module_contract#240:14-29 Test#242:7-11 run#244:6-9 eval#245:6-10 compile_value#247:6-19 get_total_voting_power#248:6-28 failwith#249:6-14 to_contract#250:6-17 set_source#251:6-16 get_storage_of_address#252:6-28 get_balance#253:6-17 print#254:6-11 eprint#255:6-12 get_voting_power#256:6-22 nth_bootstrap_contract#257:6-28 nth_bootstrap_account#258:6-27 get_bootstrap_account#261:6-27 nth_bootstrap_typed_address#262:6-33 last_originations#263:6-23 random#264:6-12 new_account#267:6-17 decompile#268:6-15 bake_until_n_cycle_end#269:6-28 get_time#270:6-14 cast_address#271:6-18 register_delegate#272:6-23 register_constant#273:6-23 to_typed_address#274:6-22 constant_to_michelson_program#275:6-35 parse_michelson#276:6-21 restore_context#277:6-21 save_context#278:6-18 drop_context#279:6-18 to_string#280:6-15 to_json#281:6-13 get_storage#282:6-17 set_baker_policy#287:6-22 set_baker#288:6-15 size#289:6-10 compile_contract#290:6-22 read_contract_from_file#294:6-29 chr#295:6-9 nl#305:6-8 println#306:6-13 set_print_values#309:6-22 unset_print_values#310:6-24 PBT#312:9-12 gen#313:8-11 gen_small#314:8-17 make_test#315:8-17 run#316:8-11 get_last_events_from#330:6-26 transfer#338:6-14 transfer_exn#339:6-18 log#340:6-9 reset_state#344:6-17 reset_state_at#345:6-20 bootstrap_contract#346:6-24 mutate_value#347:6-18 save_mutation#348:6-19 sign#349:6-10 add_account#350:6-17 baker_account#351:6-19 set_big_map#352:6-17 transfer_to_contract#353:6-26 transfer_to_contract_exn#358:6-30 michelson_equal#363:6-21 to_entrypoint#364:6-19 originate_contract#372:6-24 originate#373:6-15 compile_contract_with_views#380:8-35 originate_uncurried#383:6-25 originate_module#390:6-22 compile_contract_from_file#397:6-32 originate_from_file#400:6-25 mutation_test#405:6-19 mutation_test_all#417:6-23 originate_from_file_and_mutate#429:6-36 originate_from_file_and_mutate_all#449:6-40 originate_module_and_mutate#469:6-33 originate_module_and_mutate_all#491:6-37 assert#514:6-12 assert_some#515:6-17 assert_none#516:6-17 assert_with_error#518:6-23 assert_some_with_error#519:6-28 assert_none_with_error#520:6-28 Proxy_ticket#522:9-21 proxy_transfer_contract#523:19-42 proxy_originate_contract#535:19-43 proxy_address#547:12-25 init_transfer#549:8-21 transfer#556:8-16 originate_uncurried#562:8-27 originate#577:8-17 a#1:4-5 c#5:10-11 d#5:26-27  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ failwith#2:4-12 bool#4:5-9 option#5:8-14 Tezos#7:7-12 get_balance#9:6-17 get_amount#10:6-16 get_now#11:6-13 get_sender#12:6-16 get_source#13:6-16 get_level#14:6-15 get_self_address#15:6-22 get_chain_id#16:6-18 get_total_voting_power#17:6-28 get_min_block_time#18:6-24 voting_power#19:6-18 address#20:6-13 implicit_account#21:6-22 join_tickets#22:6-18 read_ticket#23:6-17 never#25:6-11 pairing_check#26:6-19 set_delegate#27:6-18 self#28:25-29 constant#31:25-33 sapling_empty_state#32:25-44 get_contract_opt#34:25-41 get_contract#36:25-37 get_contract_with_error#40:6-29 create_ticket#43:6-19 transaction#44:6-17 call_view#46:25-34 split_ticket#49:6-18 create_contract#51:25-40 create_contract_uncurried#54:25-50 get_entrypoint_opt#56:25-43 get_entrypoint#59:25-39 emit#62:25-29 sapling_verify_update#65:25-46 Bitwise#69:7-14 and#70:6-10 xor#71:6-9 or#72:6-9 shift_left#73:6-16 shift_right#74:6-17 Big_map#77:7-14 empty#78:16-21 literal#79:25-32 mem#81:6-9 add#82:6-9 remove#83:6-12 update#84:6-12 get_and_update#85:6-20 find_opt#86:6-14 find#87:6-10 Map#91:7-10 empty#92:6-11 size#93:6-10 literal#94:25-32 mem#96:6-9 add#97:6-9 remove#98:6-12 update#99:6-12 get_and_update#100:6-20 find#101:6-10 find_opt#102:6-14 iter#103:6-10 map#104:6-9 fold#105:6-10 Transpiled#109:7-17 map_find_opt#110:6-18 map_add#111:6-13 map_remove#112:6-16 Set#115:7-10 empty#116:6-11 size#117:6-10 cardinal#118:6-14 literal#119:25-32 mem#121:6-9 add#122:6-9 remove#123:6-12 update#124:6-12 iter#125:6-10 fold#126:6-10 fold_desc#127:6-15 filter_map#128:6-16 List#132:7-11 length#133:6-12 size#134:6-10 head_opt#135:6-14 tail_opt#136:6-14 map#138:6-9 iter#139:6-10 fold#140:6-10 fold_left#141:6-15 fold_right#142:6-16 cons#143:6-10 find_opt#144:6-14 filter_map#146:6-16 update#148:6-12 update_with#150:6-17 String#154:7-13 length#155:6-12 concats#156:6-13 concat#158:6-12 sub#159:6-9 Option#162:7-13 unopt#163:6-11 unopt_with_error#165:6-22 map#166:15-18 value#167:6-11 value_exn#168:6-15 is_none#169:6-13 is_some#170:6-13 Bytes#173:7-12 concats#174:6-13 pack#175:6-10 unpack#176:6-12 length#177:6-12 concat#179:6-12 sub#180:6-9 Crypto#183:7-13 blake2b#184:6-13 sha256#185:6-12 sha512#186:6-12 sha3#187:6-10 keccak#188:6-12 hash_key#189:6-14 check#190:6-11 assert#193:4-10 assert_some#194:4-15 assert_none#195:4-15 abs#196:4-7 is_nat#197:4-10 true#198:14-18 false#199:14-19 unit#200:14-18 int#202:4-7 nat#207:4-7 bytes#208:4-9 ignore#210:4-10 curry#211:4-9 uncurry#212:4-11 assert_with_error#214:4-21 assert_some_with_error#215:4-26 assert_none_with_error#216:4-26 ediv#217:4-8 test_exec_error_balance_too_low#220:5-36 test_exec_error#223:5-20 test_exec_result#228:5-21 test_baker_policy#230:5-22 pbt_test#235:8-16 pbt_result#236:8-18 unforged_ticket#238:8-23 module_contract#240:14-29 Test#242:7-11 run#244:6-9 eval#245:6-10 compile_value#247:6-19 get_total_voting_power#248:6-28 failwith#249:6-14 to_contract#250:6-17 set_source#251:6-16 get_storage_of_address#252:6-28 get_balance#253:6-17 print#254:6-11 eprint#255:6-12 get_voting_power#256:6-22 nth_bootstrap_contract#257:6-28 nth_bootstrap_account#258:6-27 get_bootstrap_account#261:6-27 nth_bootstrap_typed_address#262:6-33 last_originations#263:6-23 random#264:6-12 new_account#267:6-17 decompile#268:6-15 bake_until_n_cycle_end#269:6-28 get_time#270:6-14 cast_address#271:6-18 register_delegate#272:6-23 register_constant#273:6-23 to_typed_address#274:6-22 constant_to_michelson_program#275:6-35 parse_michelson#276:6-21 restore_context#277:6-21 save_context#278:6-18 drop_context#279:6-18 to_string#280:6-15 to_json#281:6-13 get_storage#282:6-17 set_baker_policy#287:6-22 set_baker#288:6-15 size#289:6-10 compile_contract#290:6-22 read_contract_from_file#294:6-29 chr#295:6-9 nl#305:6-8 println#306:6-13 set_print_values#309:6-22 unset_print_values#310:6-24 PBT#312:9-12 gen#313:8-11 gen_small#314:8-17 make_test#315:8-17 run#316:8-11 get_last_events_from#330:6-26 transfer#338:6-14 transfer_exn#339:6-18 log#340:6-9 reset_state#344:6-17 reset_state_at#345:6-20 bootstrap_contract#346:6-24 mutate_value#347:6-18 save_mutation#348:6-19 sign#349:6-10 add_account#350:6-17 baker_account#351:6-19 set_big_map#352:6-17 transfer_to_contract#353:6-26 transfer_to_contract_exn#358:6-30 michelson_equal#363:6-21 to_entrypoint#364:6-19 originate_contract#372:6-24 originate#373:6-15 compile_contract_with_views#380:8-35 originate_uncurried#383:6-25 originate_module#390:6-22 compile_contract_from_file#397:6-32 originate_from_file#400:6-25 mutation_test#405:6-19 mutation_test_all#417:6-23 originate_from_file_and_mutate#429:6-36 originate_from_file_and_mutate_all#449:6-40 originate_module_and_mutate#469:6-33 originate_module_and_mutate_all#491:6-37 assert#514:6-12 assert_some#515:6-17 assert_none#516:6-17 assert_with_error#518:6-23 assert_some_with_error#519:6-28 assert_none_with_error#520:6-28 Proxy_ticket#522:9-21 proxy_transfer_contract#523:19-42 proxy_originate_contract#535:19-43 proxy_address#547:12-25 init_transfer#549:8-21 transfer#556:8-16 originate_uncurried#562:8-27 originate#577:8-17 a#1:4-5 e#6:9-10  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32

    Variable definitions:
    (failwith#2:4-12 -> failwith)
    Range: File "", line 2, characters 4-12
    Body Range: File "", line 2, characters 34-73
    Content: |unresolved|
    references:
      File "", line 38, characters 27-35 ,
      File "", line 42, characters 27-35 ,
      File "", line 61, characters 27-35 ,
      File "", line 163, characters 79-87 ,
      File "", line 165, characters 103-111 ,
      File "", line 168, characters 83-91 ,
      File "", line 193, characters 49-57 ,
      File "", line 194, characters 72-80 ,
      File "", line 195, characters 87-95 ,
      File "", line 214, characters 66-74 ,
      File "", line 215, characters 96-104 ,
      File "", line 216, characters 111-119
    (assert#193:4-10 -> assert)
    Range: File "", line 193, characters 4-10
    Body Range: File "", line 193, characters 0-76
    Content: |core: bool -> unit|
    references: []
    (assert_some#194:4-15 -> assert_some)
    Range: File "", line 194, characters 4-15
    Body Range: File "", line 194, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none#195:4-15 -> assert_none)
    Range: File "", line 195, characters 4-15
    Body Range: File "", line 195, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (abs#196:4-7 -> abs)
    Range: File "", line 196, characters 4-7
    Body Range: File "", line 196, characters 0-62
    Content: |core: int -> nat|
    references: File "", line 368, characters 31-34
    (is_nat#197:4-10 -> is_nat)
    Range: File "", line 197, characters 4-10
    Body Range: File "", line 197, characters 0-81
    Content: |core: int -> option (nat)|
    references: []
    (true#198:14-18 -> true)
    Range: File "", line 198, characters 14-18
    Body Range: File "", line 198, characters 28-32
    Content: |core: bool|
    references:
      File "", line 309, characters 88-92 ,
      File "", line 314, characters 68-72
    (false#199:14-19 -> false)
    Range: File "", line 199, characters 14-19
    Body Range: File "", line 199, characters 29-34
    Content: |core: bool|
    references:
      File "", line 265, characters 51-56 ,
      File "", line 310, characters 90-95 ,
      File "", line 313, characters 62-67
    (unit#200:14-18 -> unit)
    Range: File "", line 200, characters 14-18
    Body Range: File "", line 200, characters 28-48
    Content: |core: unit|
    references: []
    (int#202:4-7 -> int)
    Range: File "", line 202, characters 4-7
    Body Range: File "", line 202, characters 44-96
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 261, characters 97-100 ,
      File "", line 298, characters 79-82 ,
      File "", line 300, characters 78-81 ,
      File "", line 302, characters 72-75
    (nat#207:4-7 -> nat)
    Range: File "", line 207, characters 4-7
    Body Range: File "", line 207, characters 0-73
    Content: |core: bytes -> nat|
    references: []
    (bytes#208:4-9 -> bytes)
    Range: File "", line 208, characters 4-9
    Body Range: File "", line 208, characters 48-104
    Content: |core: ∀ a : * . a -> external_bytes (a)|
    references: []
    (ignore#210:4-10 -> ignore)
    Range: File "", line 210, characters 4-10
    Body Range: File "", line 210, characters 37-39
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (curry#211:4-9 -> curry)
    Range: File "", line 211, characters 4-9
    Body Range: File "", line 211, characters 62-70
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    (uncurry#212:4-11 -> uncurry)
    Range: File "", line 212, characters 4-11
    Body Range: File "", line 212, characters 62-73
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 374, characters 30-37
    (assert_with_error#214:4-21 -> assert_with_error)
    Range: File "", line 214, characters 4-21
    Body Range: File "", line 214, characters 0-76
    Content: |unresolved|
    references: []
    (assert_some_with_error#215:4-26 -> assert_some_with_error)
    Range: File "", line 215, characters 4-26
    Body Range: File "", line 215, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_none_with_error#216:4-26 -> assert_none_with_error)
    Range: File "", line 216, characters 4-26
    Body Range: File "", line 216, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (ediv#217:4-8 -> ediv)
    Range: File "", line 217, characters 4-8
    Body Range: File "", line 217, characters 61-117
    Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_ediv (a ,
    b)|
    references: []
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
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
    Type definitions:
    (bool#4:5-9 -> bool)
    Range: File "", line 4, characters 5-9
    Body Range: File "", line 4, characters 12-24
    Content: : |sum[False -> unit , True -> unit]|
    references:
      File "", line 26, characters 63-67 ,
      File "", line 26, characters 111-115 ,
      File "", line 81, characters 52-56 ,
      File "", line 96, characters 48-52 ,
      File "", line 121, characters 41-45 ,
      File "", line 124, characters 35-39 ,
      File "", line 144, characters 34-38 ,
      File "", line 150, characters 37-41 ,
      File "", line 169, characters 40-44 ,
      File "", line 170, characters 40-44 ,
      File "", line 190, characters 52-56 ,
      File "", line 190, characters 106-110 ,
      File "", line 193, characters 16-20 ,
      File "", line 198, characters 21-25 ,
      File "", line 199, characters 22-26 ,
      File "", line 214, characters 27-31 ,
      File "", line 235, characters 41-45 ,
      File "", line 315, characters 53-57 ,
      File "", line 363, characters 74-78 ,
      File "", line 514, characters 18-22 ,
      File "", line 518, characters 29-33
    (option#5:8-14 -> option)
    Range: File "", line 5, characters 8-14
    Body Range: File "", line 5, characters 17-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 22, characters 67-73 ,
      File "", line 22, characters 125-131 ,
      File "", line 27, characters 33-39 ,
      File "", line 29, characters 14-20 ,
      File "", line 33, characters 74-80 ,
      File "", line 34, characters 80-86 ,
      File "", line 35, characters 59-65 ,
      File "", line 35, characters 84-90 ,
      File "", line 43, characters 60-66 ,
      File "", line 43, characters 132-138 ,
      File "", line 46, characters 86-92 ,
      File "", line 48, characters 80-86 ,
      File "", line 48, characters 96-102 ,
      File "", line 49, characters 83-89 ,
      File "", line 50, characters 68-74 ,
      File "", line 51, characters 102-108 ,
      File "", line 54, characters 111-117 ,
      File "", line 56, characters 93-99 ,
      File "", line 58, characters 83-89 ,
      File "", line 58, characters 108-114 ,
      File "", line 64, characters 79-85 ,
      File "", line 65, characters 158-164 ,
      File "", line 65, characters 256-262 ,
      File "", line 84, characters 39-45 ,
      File "", line 85, characters 47-53 ,
      File "", line 85, characters 80-86 ,
      File "", line 86, characters 59-65 ,
      File "", line 99, characters 39-45 ,
      File "", line 100, characters 47-53 ,
      File "", line 100, characters 76-82 ,
      File "", line 102, characters 55-61 ,
      File "", line 112, characters 176-182 ,
      File "", line 128, characters 40-46 ,
      File "", line 135, characters 42-48 ,
      File "", line 136, characters 49-55 ,
      File "", line 144, characters 58-64 ,
      File "", line 145, characters 31-37 ,
      File "", line 146, characters 40-46 ,
      File "", line 148, characters 34-40 ,
      File "", line 163, characters 28-34 ,
      File "", line 165, characters 39-45 ,
      File "", line 166, characters 50-56 ,
      File "", line 166, characters 62-68 ,
      File "", line 167, characters 42-48 ,
      File "", line 168, characters 48-54 ,
      File "", line 169, characters 30-36 ,
      File "", line 170, characters 30-36 ,
      File "", line 176, characters 38-44 ,
      File "", line 176, characters 100-106 ,
      File "", line 176, characters 114-120 ,
      File "", line 194, characters 32-38 ,
      File "", line 195, characters 32-38 ,
      File "", line 197, characters 27-33 ,
      File "", line 197, characters 73-79 ,
      File "", line 215, characters 43-49 ,
      File "", line 216, characters 43-49 ,
      File "", line 295, characters 29-35 ,
      File "", line 338, characters 147-153 ,
      File "", line 339, characters 142-148 ,
      File "", line 344, characters 102-108 ,
      File "", line 347, characters 63-69 ,
      File "", line 348, characters 57-63 ,
      File "", line 351, characters 48-54 ,
      File "", line 355, characters 19-25 ,
      File "", line 360, characters 21-27 ,
      File "", line 398, characters 100-106 ,
      File "", line 405, characters 74-80 ,
      File "", line 408, characters 52-58 ,
      File "", line 430, characters 105-111 ,
      File "", line 436, characters 100-106 ,
      File "", line 439, characters 52-58 ,
      File "", line 456, characters 100-106 ,
      File "", line 470, characters 117-123 ,
      File "", line 481, characters 52-58 ,
      File "", line 515, characters 34-40 ,
      File "", line 516, characters 34-40 ,
      File "", line 519, characters 45-51 ,
      File "", line 520, characters 45-51 ,
      File "", line 539, characters 30-36 ,
      File "", line 540, characters 35-41 ,
      File "", line 544, characters 63-69 ,
      File "", line 566, characters 50-56 ,
      File "", line 566, characters 85-91 ,
      File "", line 569, characters 76-82 ,
      File "", line 581, characters 50-56 ,
      File "", line 581, characters 85-91 ,
      File "", line 584, characters 76-82
    (test_exec_error_balance_too_low#220:5-36 -> test_exec_error_balance_too_low)
    Range: File "", line 220, characters 5-36
    Body Range: File "", line 221, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 225, characters 23-54
    (test_exec_error#223:5-20 -> test_exec_error)
    Range: File "", line 223, characters 5-20
    Body Range: File "", line 224, character 4 to line 226, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 228, characters 49-64
    (test_exec_result#228:5-21 -> test_exec_result)
    Range: File "", line 228, characters 5-21
    Body Range: File "", line 228, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 338, characters 65-81 ,
      File "", line 353, characters 73-89 ,
      File "", line 558, characters 47-63
    (test_baker_policy#230:5-22 -> test_baker_policy)
    Range: File "", line 230, characters 5-22
    Body Range: File "", line 231, character 4 to line 233, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 287, characters 29-46
    (pbt_test#235:8-16 -> pbt_test)
    Range: File "", line 235, characters 8-16
    Body Range: File "", line 235, characters 19-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 315, characters 63-71 ,
      File "", line 316, characters 33-41
    (pbt_result#236:8-18 -> pbt_result)
    Range: File "", line 236, characters 8-18
    Body Range: File "", line 236, characters 21-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 316, characters 57-67 ,
      File "", line 317, characters 39-49 ,
      File "", line 319, characters 84-94 ,
      File "", line 323, characters 96-106 ,
      File "", line 326, characters 68-78
    (unforged_ticket#238:8-23 -> unforged_ticket)
    Range: File "", line 238, characters 8-23
    Body Range: File "", line 238, characters 26-40
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    (module_contract#240:14-29 -> module_contract)
    Range: File "", line 240, characters 14-29
    Body Range: File "", line 240, characters 32-75
    Content: : |funtype 'p : * . funtype 's : * . ( ( 'p * 's ) -> ( list (operation) *
                                                                     's ) *
                                                    views ('s) )|
    references:
      File "", line 390, characters 52-67 ,
      File "", line 469, characters 65-80 ,
      File "", line 491, characters 69-84
    Module definitions:
    (Tezos#7:7-12 -> Tezos)
    Range: File "", line 7, characters 7-12
    Body Range: File "", line 9, character 2 to line 65, character 20
    Content: Members: Variable definitions:
                      (get_balance#9:6-17 -> get_balance)
                      Range: File "", line 9, characters 6-17
                      Body Range: File "", line 9, characters 2-76
                      Content: |core: unit -> tez|
                      references: []
                      (get_amount#10:6-16 -> get_amount)
                      Range: File "", line 10, characters 6-16
                      Body Range: File "", line 10, characters 2-74
                      Content: |core: unit -> tez|
                      references: []
                      (get_now#11:6-13 -> get_now)
                      Range: File "", line 11, characters 6-13
                      Body Range: File "", line 11, characters 2-80
                      Content: |core: unit -> timestamp|
                      references: File "", line 270, characters 47-54
                      (get_sender#12:6-16 -> get_sender)
                      Range: File "", line 12, characters 6-16
                      Body Range: File "", line 12, characters 2-82
                      Content: |core: unit -> address|
                      references: []
                      (get_source#13:6-16 -> get_source)
                      Range: File "", line 13, characters 6-16
                      Body Range: File "", line 13, characters 2-82
                      Content: |core: unit -> address|
                      references: []
                      (get_level#14:6-15 -> get_level)
                      Range: File "", line 14, characters 6-15
                      Body Range: File "", line 14, characters 2-72
                      Content: |core: unit -> nat|
                      references: []
                      (get_self_address#15:6-22 -> get_self_address)
                      Range: File "", line 15, characters 6-22
                      Body Range: File "", line 15, characters 2-94
                      Content: |core: unit -> address|
                      references: []
                      (get_chain_id#16:6-18 -> get_chain_id)
                      Range: File "", line 16, characters 6-18
                      Body Range: File "", line 16, characters 2-88
                      Content: |core: unit -> chain_id|
                      references: []
                      (get_total_voting_power#17:6-28 -> get_total_voting_power)
                      Range: File "", line 17, characters 6-28
                      Body Range: File "", line 17, characters 2-98
                      Content: |core: unit -> nat|
                      references: []
                      (get_min_block_time#18:6-24 -> get_min_block_time)
                      Range: File "", line 18, characters 6-24
                      Body Range: File "", line 18, characters 2-90
                      Content: |core: unit -> nat|
                      references: []
                      (voting_power#19:6-18 -> voting_power)
                      Range: File "", line 19, characters 6-18
                      Body Range: File "", line 19, characters 2-89
                      Content: |core: key_hash -> nat|
                      references: []
                      (address#20:6-13 -> address)
                      Range: File "", line 20, characters 6-13
                      Body Range: File "", line 20, characters 52-96
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 331, characters 21-28
                      (implicit_account#21:6-22 -> implicit_account)
                      Range: File "", line 21, characters 6-22
                      Body Range: File "", line 21, characters 2-117
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      (join_tickets#22:6-18 -> join_tickets)
                      Range: File "", line 22, characters 6-18
                      Body Range: File "", line 22, characters 76-133
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      (read_ticket#23:6-17 -> read_ticket)
                      Range: File "", line 23, characters 6-17
                      Body Range: File "", line 24, characters 4-84
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address *
                                                                    ( a * nat ) ) *
                                                                  ticket (a) )|
                      references: []
                      (never#25:6-11 -> never)
                      Range: File "", line 25, characters 6-11
                      Body Range: File "", line 25, characters 39-75
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      (pairing_check#26:6-19 -> pairing_check)
                      Range: File "", line 26, characters 6-19
                      Body Range: File "", line 26, characters 2-117
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      (set_delegate#27:6-18 -> set_delegate)
                      Range: File "", line 27, characters 6-18
                      Body Range: File "", line 27, characters 2-106
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      (self#28:25-29 -> self)
                      Range: File "", line 28, characters 25-29
                      Body Range: File "", line 29, character 4 to line 30, character 70
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      (constant#31:25-33 -> constant)
                      Range: File "", line 31, characters 25-33
                      Body Range: File "", line 31, characters 62-96
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (sapling_empty_state#32:25-44 -> sapling_empty_state)
                      Range: File "", line 32, characters 25-44
                      Body Range: File "", line 33, characters 4-105
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      (get_contract_opt#34:25-41 -> get_contract_opt)
                      Range: File "", line 34, characters 25-41
                      Body Range: File "", line 35, characters 4-92
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 37, characters 12-28 ,
                        File "", line 41, characters 12-28
                      (get_contract#36:25-37 -> get_contract)
                      Range: File "", line 36, characters 25-37
                      Body Range: File "", line 37, character 4 to line 38, character 68
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      (get_contract_with_error#40:6-29 -> get_contract_with_error)
                      Range: File "", line 40, characters 6-29
                      Body Range: File "", line 41, character 4 to line 42, character 39
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: File "", line 531, characters 39-62
                      (create_ticket#43:6-19 -> create_ticket)
                      Range: File "", line 43, characters 6-19
                      Body Range: File "", line 43, characters 69-147
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references:
                        File "", line 529, characters 39-52 ,
                        File "", line 542, characters 39-52
                      (transaction#44:6-17 -> transaction)
                      Range: File "", line 44, characters 6-17
                      Body Range: File "", line 45, characters 4-109
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: File "", line 532, characters 21-32
                      (call_view#46:25-34 -> call_view)
                      Range: File "", line 46, characters 25-34
                      Body Range: File "", line 47, character 4 to line 48, character 104
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (split_ticket#49:6-18 -> split_ticket)
                      Range: File "", line 49, characters 6-18
                      Body Range: File "", line 50, characters 4-76
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      (create_contract#51:25-40 -> create_contract)
                      Range: File "", line 51, characters 25-40
                      Body Range: File "", line 52, character 6 to line 53, character 58
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: File "", line 544, characters 26-41
                      (create_contract_uncurried#54:25-50 -> create_contract_uncurried)
                      Range: File "", line 54, characters 25-50
                      Body Range: File "", line 55, characters 6-50
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      (get_entrypoint_opt#56:25-43 -> get_entrypoint_opt)
                      Range: File "", line 56, characters 25-43
                      Body Range: File "", line 57, character 4 to line 58, character 116
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 60, characters 12-30
                      (get_entrypoint#59:25-39 -> get_entrypoint)
                      Range: File "", line 59, characters 25-39
                      Body Range: File "", line 60, character 4 to line 61, character 70
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (emit#62:25-29 -> emit)
                      Range: File "", line 62, characters 25-29
                      Body Range: File "", line 63, character 4 to line 64, character 102
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      (sapling_verify_update#65:25-46 -> sapling_verify_update)
                      Range: File "", line 65, characters 25-46
                      Body Range: File "", line 65, characters 167-264
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes *
                        ( int * sapling_state (sap_a) ) ))|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 270, characters 41-46 ,
      File "", line 331, characters 15-20 ,
      File "", line 529, characters 33-38 ,
      File "", line 531, characters 33-38 ,
      File "", line 532, characters 15-20 ,
      File "", line 542, characters 33-38 ,
      File "", line 544, characters 20-25

    (Bitwise#69:7-14 -> Bitwise)
    Range: File "", line 69, characters 7-14
    Body Range: File "", line 70, character 2 to line 74, character 144
    Content: Members: Variable definitions:
                      (and#70:6-10 -> and)
                      Range: File "", line 70, characters 6-10
                      Body Range: File "", line 70, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (xor#71:6-9 -> xor)
                      Range: File "", line 71, characters 6-9
                      Body Range: File "", line 71, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_or (a ,
                      b)|
                      references: []
                      (or#72:6-9 -> or)
                      Range: File "", line 72, characters 6-9
                      Body Range: File "", line 72, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_xor (a ,
                      b)|
                      references: []
                      (shift_left#73:6-16 -> shift_left)
                      Range: File "", line 73, characters 6-16
                      Body Range: File "", line 73, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsl (a ,
                      b)|
                      references: []
                      (shift_right#74:6-17 -> shift_right)
                      Range: File "", line 74, characters 6-17
                      Body Range: File "", line 74, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsr (a ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Big_map#77:7-14 -> Big_map)
    Range: File "", line 77, characters 7-14
    Body Range: File "", line 78, character 2 to line 87, character 87
    Content: Members: Variable definitions:
                      (empty#78:16-21 -> empty)
                      Range: File "", line 78, characters 16-21
                      Body Range: File "", line 78, characters 52-81
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (literal#79:25-32 -> literal)
                      Range: File "", line 79, characters 25-32
                      Body Range: File "", line 79, characters 82-116
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#81:6-9 -> mem)
                      Range: File "", line 81, characters 6-9
                      Body Range: File "", line 81, characters 59-88
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (add#82:6-9 -> add)
                      Range: File "", line 82, characters 6-9
                      Body Range: File "", line 82, characters 77-109
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (remove#83:6-12 -> remove)
                      Range: File "", line 83, characters 6-12
                      Body Range: File "", line 83, characters 72-104
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#84:6-12 -> update)
                      Range: File "", line 84, characters 6-12
                      Body Range: File "", line 84, characters 87-122
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (get_and_update#85:6-20 -> get_and_update)
                      Range: File "", line 85, characters 6-20
                      Body Range: File "", line 85, characters 106-153
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (find_opt#86:6-14 -> find_opt)
                      Range: File "", line 86, characters 6-14
                      Body Range: File "", line 86, characters 68-102
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (find#87:6-10 -> find)
                      Range: File "", line 87, characters 6-10
                      Body Range: File "", line 87, characters 57-87
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Map#91:7-10 -> Map)
    Range: File "", line 91, characters 7-10
    Body Range: File "", line 92, character 2 to line 105, character 111
    Content: Members: Variable definitions:
                      (empty#92:6-11 -> empty)
                      Range: File "", line 92, characters 6-11
                      Body Range: File "", line 92, characters 38-63
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (size#93:6-10 -> size)
                      Range: File "", line 93, characters 6-10
                      Body Range: File "", line 93, characters 47-74
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (literal#94:25-32 -> literal)
                      Range: File "", line 94, characters 25-32
                      Body Range: File "", line 94, characters 78-108
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (mem#96:6-9 -> mem)
                      Range: File "", line 96, characters 6-9
                      Body Range: File "", line 96, characters 55-84
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (add#97:6-9 -> add)
                      Range: File "", line 97, characters 6-9
                      Body Range: File "", line 97, characters 69-101
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (remove#98:6-12 -> remove)
                      Range: File "", line 98, characters 6-12
                      Body Range: File "", line 98, characters 64-96
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (update#99:6-12 -> update)
                      Range: File "", line 99, characters 6-12
                      Body Range: File "", line 99, characters 79-114
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (get_and_update#100:6-20 -> get_and_update)
                      Range: File "", line 100, characters 6-20
                      Body Range: File "", line 100, characters 98-141
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (find#101:6-10 -> find)
                      Range: File "", line 101, characters 6-10
                      Body Range: File "", line 101, characters 53-83
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#102:6-14 -> find_opt)
                      Range: File "", line 102, characters 6-14
                      Body Range: File "", line 102, characters 64-98
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (iter#103:6-10 -> iter)
                      Range: File "", line 103, characters 6-10
                      Body Range: File "", line 103, characters 68-98
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (map#104:6-9 -> map)
                      Range: File "", line 104, characters 6-9
                      Body Range: File "", line 104, characters 72-101
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (fold#105:6-10 -> fold)
                      Range: File "", line 105, characters 6-10
                      Body Range: File "", line 105, characters 78-111
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Transpiled#109:7-17 -> Transpiled)
    Range: File "", line 109, characters 7-17
    Body Range: File "", line 110, character 2 to line 112, character 218
    Content: Members: Variable definitions:
                      (map_find_opt#110:6-18 -> map_find_opt)
                      Range: File "", line 110, characters 6-18
                      Body Range: File "", line 110, characters 79-142
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      (map_add#111:6-13 -> map_add)
                      Range: File "", line 111, characters 6-13
                      Body Range: File "", line 111, characters 82-163
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      (map_remove#112:6-16 -> map_remove)
                      Range: File "", line 112, characters 6-16
                      Body Range: File "", line 112, characters 75-218
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#115:7-10 -> Set)
    Range: File "", line 115, characters 7-10
    Body Range: File "", line 116, character 2 to line 129, character 110
    Content: Members: Variable definitions:
                      (empty#116:6-11 -> empty)
                      Range: File "", line 116, characters 6-11
                      Body Range: File "", line 116, characters 31-56
                      Content: |core: ∀ a : * . set (a)|
                      references: File "", line 129, characters 96-101
                      (size#117:6-10 -> size)
                      Range: File "", line 117, characters 6-10
                      Body Range: File "", line 117, characters 40-67
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (cardinal#118:6-14 -> cardinal)
                      Range: File "", line 118, characters 6-14
                      Body Range: File "", line 118, characters 44-71
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (literal#119:25-32 -> literal)
                      Range: File "", line 119, characters 25-32
                      Body Range: File "", line 119, characters 65-95
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#121:6-9 -> mem)
                      Range: File "", line 121, characters 6-9
                      Body Range: File "", line 121, characters 48-77
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (add#122:6-9 -> add)
                      Range: File "", line 122, characters 6-9
                      Body Range: File "", line 122, characters 49-78
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: File "", line 129, characters 81-84
                      (remove#123:6-12 -> remove)
                      Range: File "", line 123, characters 6-12
                      Body Range: File "", line 123, characters 52-84
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (update#124:6-12 -> update)
                      Range: File "", line 124, characters 6-12
                      Body Range: File "", line 124, characters 55-90
                      Content: |unresolved|
                      references: []
                      (iter#125:6-10 -> iter)
                      Range: File "", line 125, characters 6-10
                      Body Range: File "", line 125, characters 57-87
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (fold#126:6-10 -> fold)
                      Range: File "", line 126, characters 6-10
                      Body Range: File "", line 126, characters 65-98
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#127:6-15 -> fold_desc)
                      Range: File "", line 127, characters 6-15
                      Body Range: File "", line 127, characters 70-108
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: File "", line 129, characters 4-13
                      (filter_map#128:6-16 -> filter_map)
                      Range: File "", line 128, characters 6-16
                      Body Range: File "", line 129, characters 4-110
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> set (a) -> set (b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#132:7-11 -> List)
    Range: File "", line 132, characters 7-11
    Body Range: File "", line 133, character 2 to line 151, character 48
    Content: Members: Variable definitions:
                      (length#133:6-12 -> length)
                      Range: File "", line 133, characters 6-12
                      Body Range: File "", line 133, characters 44-73
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (size#134:6-10 -> size)
                      Range: File "", line 134, characters 6-10
                      Body Range: File "", line 134, characters 42-71
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (head_opt#135:6-14 -> head_opt)
                      Range: File "", line 135, characters 6-14
                      Body Range: File "", line 135, characters 51-98
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (tail_opt#136:6-14 -> tail_opt)
                      Range: File "", line 136, characters 6-14
                      Body Range: File "", line 136, characters 58-107
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      (map#138:6-9 -> map)
                      Range: File "", line 138, characters 6-9
                      Body Range: File "", line 138, characters 59-90
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 149, characters 4-7 ,
                        File "", line 151, characters 4-7
                      (iter#139:6-10 -> iter)
                      Range: File "", line 139, characters 6-10
                      Body Range: File "", line 139, characters 58-90
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (fold#140:6-10 -> fold)
                      Range: File "", line 140, characters 6-10
                      Body Range: File "", line 140, characters 67-102
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 337, characters 9-13
                      (fold_left#141:6-15 -> fold_left)
                      Range: File "", line 141, characters 6-15
                      Body Range: File "", line 141, characters 72-112
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#142:6-16 -> fold_right)
                      Range: File "", line 142, characters 6-16
                      Body Range: File "", line 142, characters 73-114
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 145, characters 4-14 ,
                        File "", line 147, characters 4-14
                      (cons#143:6-10 -> cons)
                      Range: File "", line 143, characters 6-10
                      Body Range: File "", line 143, characters 53-80
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (find_opt#144:6-14 -> find_opt)
                      Range: File "", line 144, characters 6-14
                      Body Range: File "", line 145, characters 4-82
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (filter_map#146:6-16 -> filter_map)
                      Range: File "", line 146, characters 6-16
                      Body Range: File "", line 147, characters 4-100
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      (update#148:6-12 -> update)
                      Range: File "", line 148, characters 6-12
                      Body Range: File "", line 149, characters 4-62
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      (update_with#150:6-17 -> update_with)
                      Range: File "", line 150, characters 6-17
                      Body Range: File "", line 151, characters 4-48
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 337, characters 4-8

    (String#154:7-13 -> String)
    Range: File "", line 154, characters 7-13
    Body Range: File "", line 155, character 2 to line 159, character 84
    Content: Members: Variable definitions:
                      (length#155:6-12 -> length)
                      Range: File "", line 155, characters 6-12
                      Body Range: File "", line 155, characters 2-57
                      Content: |core: string -> nat|
                      references:
                        File "", line 365, characters 22-28 ,
                        File "", line 368, characters 43-49
                      (concats#156:6-13 -> concats)
                      Range: File "", line 156, characters 6-13
                      Body Range: File "", line 156, characters 2-71
                      Content: |core: list (string) -> string|
                      references: []
                      (concat#158:6-12 -> concat)
                      Range: File "", line 158, characters 6-12
                      Body Range: File "", line 158, characters 2-82
                      Content: |core: string -> string -> string|
                      references: []
                      (sub#159:6-9 -> sub)
                      Range: File "", line 159, characters 6-9
                      Body Range: File "", line 159, characters 2-84
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 366, characters 24-27 ,
                        File "", line 368, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 365, characters 15-21 ,
      File "", line 366, characters 17-23 ,
      File "", line 368, characters 16-22 ,
      File "", line 368, characters 36-42

    (Option#162:7-13 -> Option)
    Range: File "", line 162, characters 7-13
    Body Range: File "", line 163, character 2 to line 170, character 77
    Content: Members: Variable definitions:
                      (unopt#163:6-11 -> unopt)
                      Range: File "", line 163, characters 6-11
                      Body Range: File "", line 163, characters 42-104
                      Content: |core: ∀ a : * . option (a) -> a|
                      references:
                        File "", line 529, characters 26-31 ,
                        File "", line 542, characters 26-31
                      (unopt_with_error#165:6-22 -> unopt_with_error)
                      Range: File "", line 165, characters 6-22
                      Body Range: File "", line 165, characters 66-113
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (map#166:15-18 -> map)
                      Range: File "", line 166, characters 15-18
                      Body Range: File "", line 166, characters 71-103
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (value#167:6-11 -> value)
                      Range: File "", line 167, characters 6-11
                      Body Range: File "", line 167, characters 56-100
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#168:6-15 -> value_exn)
                      Range: File "", line 168, characters 6-15
                      Body Range: File "", line 168, characters 62-109
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      (is_none#169:6-13 -> is_none)
                      Range: File "", line 169, characters 6-13
                      Body Range: File "", line 169, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#170:6-13 -> is_some)
                      Range: File "", line 170, characters 6-13
                      Body Range: File "", line 170, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 529, characters 19-25 ,
      File "", line 542, characters 19-25

    (Bytes#173:7-12 -> Bytes)
    Range: File "", line 173, characters 7-12
    Body Range: File "", line 174, character 2 to line 180, character 82
    Content: Members: Variable definitions:
                      (concats#174:6-13 -> concats)
                      Range: File "", line 174, characters 6-13
                      Body Range: File "", line 174, characters 2-69
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (pack#175:6-10 -> pack)
                      Range: File "", line 175, characters 6-10
                      Body Range: File "", line 175, characters 38-77
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (unpack#176:6-12 -> unpack)
                      Range: File "", line 176, characters 6-12
                      Body Range: File "", line 176, characters 47-122
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      (length#177:6-12 -> length)
                      Range: File "", line 177, characters 6-12
                      Body Range: File "", line 177, characters 2-56
                      Content: |core: bytes -> nat|
                      references: []
                      (concat#179:6-12 -> concat)
                      Range: File "", line 179, characters 6-12
                      Body Range: File "", line 179, characters 2-79
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (sub#180:6-9 -> sub)
                      Range: File "", line 180, characters 6-9
                      Body Range: File "", line 180, characters 2-82
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#183:7-13 -> Crypto)
    Range: File "", line 183, characters 7-13
    Body Range: File "", line 184, character 2 to line 190, character 112
    Content: Members: Variable definitions:
                      (blake2b#184:6-13 -> blake2b)
                      Range: File "", line 184, characters 6-13
                      Body Range: File "", line 184, characters 2-78
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#185:6-12 -> sha256)
                      Range: File "", line 185, characters 6-12
                      Body Range: File "", line 185, characters 2-76
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#186:6-12 -> sha512)
                      Range: File "", line 186, characters 6-12
                      Body Range: File "", line 186, characters 2-76
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#187:6-10 -> sha3)
                      Range: File "", line 187, characters 6-10
                      Body Range: File "", line 187, characters 2-72
                      Content: |core: bytes -> bytes|
                      references: []
                      (keccak#188:6-12 -> keccak)
                      Range: File "", line 188, characters 6-12
                      Body Range: File "", line 188, characters 2-76
                      Content: |core: bytes -> bytes|
                      references: []
                      (hash_key#189:6-14 -> hash_key)
                      Range: File "", line 189, characters 6-14
                      Body Range: File "", line 189, characters 2-84
                      Content: |core: key -> key_hash|
                      references: []
                      (check#190:6-11 -> check)
                      Range: File "", line 190, characters 6-11
                      Body Range: File "", line 190, characters 2-112
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Test#242:7-11 -> Test)
    Range: File "", line 242, characters 7-11
    Body Range: File "", line 244, character 2 to line 591, character 5
    Content: Members: Variable definitions:
                      (run#244:6-9 -> run)
                      Range: File "", line 244, characters 6-9
                      Body Range: File "", line 244, characters 64-94
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 245, characters 50-53
                      (eval#245:6-10 -> eval)
                      Range: File "", line 245, characters 6-10
                      Body Range: File "", line 245, characters 50-74
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 247, characters 59-63 ,
                        File "", line 356, characters 32-36 ,
                        File "", line 361, characters 34-38 ,
                        File "", line 375, characters 12-16 ,
                        File "", line 385, characters 12-16 ,
                        File "", line 392, characters 12-16 ,
                        File "", line 471, characters 12-16 ,
                        File "", line 493, characters 12-16
                      (compile_value#247:6-19 -> compile_value)
                      Range: File "", line 247, characters 6-19
                      Body Range: File "", line 247, characters 59-65
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (get_total_voting_power#248:6-28 -> get_total_voting_power)
                      Range: File "", line 248, characters 6-28
                      Body Range: File "", line 248, characters 2-96
                      Content: |core: unit -> nat|
                      references: []
                      (failwith#249:6-14 -> failwith)
                      Range: File "", line 249, characters 6-14
                      Body Range: File "", line 249, characters 40-72
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 514, characters 51-59 ,
                        File "", line 515, characters 74-82 ,
                        File "", line 516, characters 89-97 ,
                        File "", line 518, characters 68-76 ,
                        File "", line 519, characters 98-106 ,
                        File "", line 520, characters 113-121 ,
                        File "", line 575, characters 16-24 ,
                        File "", line 590, characters 16-24
                      (to_contract#250:6-17 -> to_contract)
                      Range: File "", line 250, characters 6-17
                      Body Range: File "", line 250, characters 71-106
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 283, characters 25-36 ,
                        File "", line 331, characters 30-41 ,
                        File "", line 560, characters 28-39 ,
                        File "", line 570, characters 40-51 ,
                        File "", line 585, characters 40-51
                      (set_source#251:6-16 -> set_source)
                      Range: File "", line 251, characters 6-16
                      Body Range: File "", line 251, characters 2-74
                      Content: |core: address -> unit|
                      references: []
                      (get_storage_of_address#252:6-28 -> get_storage_of_address)
                      Range: File "", line 252, characters 6-28
                      Body Range: File "", line 252, characters 2-111
                      Content: |core: address -> michelson_program|
                      references: File "", line 285, characters 32-54
                      (get_balance#253:6-17 -> get_balance)
                      Range: File "", line 253, characters 6-17
                      Body Range: File "", line 253, characters 2-75
                      Content: |core: address -> tez|
                      references: []
                      (print#254:6-11 -> print)
                      Range: File "", line 254, characters 6-11
                      Body Range: File "", line 254, characters 2-66
                      Content: |core: string -> unit|
                      references:
                        File "", line 307, characters 4-9 ,
                        File "", line 343, characters 4-9
                      (eprint#255:6-12 -> eprint)
                      Range: File "", line 255, characters 6-12
                      Body Range: File "", line 255, characters 2-67
                      Content: |core: string -> unit|
                      references: []
                      (get_voting_power#256:6-22 -> get_voting_power)
                      Range: File "", line 256, characters 6-22
                      Body Range: File "", line 256, characters 2-88
                      Content: |core: key_hash -> nat|
                      references: []
                      (nth_bootstrap_contract#257:6-28 -> nth_bootstrap_contract)
                      Range: File "", line 257, characters 6-28
                      Body Range: File "", line 257, characters 2-97
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_account#258:6-27 -> nth_bootstrap_account)
                      Range: File "", line 258, characters 6-27
                      Body Range: File "", line 258, character 2 to line 260, character 5
                      Content: |core: int -> address|
                      references: []
                      (get_bootstrap_account#261:6-27 -> get_bootstrap_account)
                      Range: File "", line 261, characters 6-27
                      Body Range: File "", line 261, characters 2-105
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (nth_bootstrap_typed_address#262:6-33 -> nth_bootstrap_typed_address)
                      Range: File "", line 262, characters 6-33
                      Body Range: File "", line 262, characters 80-131
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (last_originations#263:6-23 -> last_originations)
                      Range: File "", line 263, characters 6-23
                      Body Range: File "", line 263, characters 2-108
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (random#264:6-12 -> random)
                      Range: File "", line 264, characters 6-12
                      Body Range: File "", line 265, character 4 to line 266, character 42
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (new_account#267:6-17 -> new_account)
                      Range: File "", line 267, characters 6-17
                      Body Range: File "", line 267, characters 2-81
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (decompile#268:6-15 -> decompile)
                      Range: File "", line 268, characters 6-15
                      Body Range: File "", line 268, characters 55-88
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 286, characters 5-14
                      (bake_until_n_cycle_end#269:6-28 -> bake_until_n_cycle_end)
                      Range: File "", line 269, characters 6-28
                      Body Range: File "", line 269, characters 2-94
                      Content: |core: nat -> unit|
                      references: []
                      (get_time#270:6-14 -> get_time)
                      Range: File "", line 270, characters 6-14
                      Body Range: File "", line 270, characters 2-57
                      Content: |core: unit -> timestamp|
                      references: []
                      (cast_address#271:6-18 -> cast_address)
                      Range: File "", line 271, characters 6-18
                      Body Range: File "", line 271, characters 69-105
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 378, characters 35-47 ,
                        File "", line 388, characters 35-47 ,
                        File "", line 395, characters 35-47 ,
                        File "", line 476, characters 37-49 ,
                        File "", line 498, characters 37-49 ,
                        File "", line 573, characters 22-34 ,
                        File "", line 588, characters 22-34
                      (register_delegate#272:6-23 -> register_delegate)
                      Range: File "", line 272, characters 6-23
                      Body Range: File "", line 272, characters 2-91
                      Content: |core: key_hash -> unit|
                      references: []
                      (register_constant#273:6-23 -> register_constant)
                      Range: File "", line 273, characters 6-23
                      Body Range: File "", line 273, characters 2-100
                      Content: |core: michelson_program -> string|
                      references: []
                      (to_typed_address#274:6-22 -> to_typed_address)
                      Range: File "", line 274, characters 6-22
                      Body Range: File "", line 274, characters 76-116
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (constant_to_michelson_program#275:6-35 -> constant_to_michelson_program)
                      Range: File "", line 275, characters 6-35
                      Body Range: File "", line 275, characters 2-116
                      Content: |core: string -> michelson_program|
                      references: []
                      (parse_michelson#276:6-21 -> parse_michelson)
                      Range: File "", line 276, characters 6-21
                      Body Range: File "", line 276, characters 2-102
                      Content: |core: string -> michelson_program|
                      references: []
                      (restore_context#277:6-21 -> restore_context)
                      Range: File "", line 277, characters 6-21
                      Body Range: File "", line 277, characters 2-77
                      Content: |core: unit -> unit|
                      references: []
                      (save_context#278:6-18 -> save_context)
                      Range: File "", line 278, characters 6-18
                      Body Range: File "", line 278, characters 2-75
                      Content: |core: unit -> unit|
                      references: []
                      (drop_context#279:6-18 -> drop_context)
                      Range: File "", line 279, characters 6-18
                      Body Range: File "", line 279, characters 2-75
                      Content: |core: unit -> unit|
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
                      (to_json#281:6-13 -> to_json)
                      Range: File "", line 281, characters 6-13
                      Body Range: File "", line 281, characters 42-78
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (get_storage#282:6-17 -> get_storage)
                      Range: File "", line 282, characters 6-17
                      Body Range: File "", line 283, character 4 to line 286, character 21
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references:
                        File "", line 571, characters 12-23 ,
                        File "", line 586, characters 12-23
                      (set_baker_policy#287:6-22 -> set_baker_policy)
                      Range: File "", line 287, characters 6-22
                      Body Range: File "", line 287, characters 2-91
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 288, characters 39-55
                      (set_baker#288:6-15 -> set_baker)
                      Range: File "", line 288, characters 6-15
                      Body Range: File "", line 288, characters 2-70
                      Content: |core: address -> unit|
                      references: []
                      (size#289:6-10 -> size)
                      Range: File "", line 289, characters 6-10
                      Body Range: File "", line 289, characters 2-72
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 377, characters 12-16 ,
                        File "", line 387, characters 12-16 ,
                        File "", line 394, characters 12-16 ,
                        File "", line 403, characters 12-16 ,
                        File "", line 434, characters 14-18 ,
                        File "", line 454, characters 14-18 ,
                        File "", line 475, characters 14-18 ,
                        File "", line 497, characters 14-18
                      (compile_contract#290:6-22 -> compile_contract)
                      Range: File "", line 290, characters 6-22
                      Body Range: File "", line 291, character 4 to line 293, character 52
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 374, characters 12-28 ,
                        File "", line 384, characters 12-28
                      (read_contract_from_file#294:6-29 -> read_contract_from_file)
                      Range: File "", line 294, characters 6-29
                      Body Range: File "", line 294, characters 2-115
                      Content: |core: string -> michelson_contract|
                      references: []
                      (chr#295:6-9 -> chr)
                      Range: File "", line 295, characters 6-9
                      Body Range: File "", line 295, character 2 to line 304, character 10
                      Content: |core: nat -> option (string)|
                      references: []
                      (nl#305:6-8 -> nl)
                      Range: File "", line 305, characters 6-8
                      Body Range: File "", line 305, characters 11-53
                      Content: |unresolved|
                      references: File "", line 307, characters 15-17
                      (println#306:6-13 -> println)
                      Range: File "", line 306, characters 6-13
                      Body Range: File "", line 306, character 2 to line 307, character 18
                      Content: |core: string -> unit|
                      references: []
                      (set_print_values#309:6-22 -> set_print_values)
                      Range: File "", line 309, characters 6-22
                      Body Range: File "", line 309, characters 2-100
                      Content: |core: unit -> unit|
                      references: []
                      (unset_print_values#310:6-24 -> unset_print_values)
                      Range: File "", line 310, characters 6-24
                      Body Range: File "", line 310, characters 2-103
                      Content: |core: unit -> unit|
                      references: []
                      (get_last_events_from#330:6-26 -> get_last_events_from)
                      Range: File "", line 330, characters 6-26
                      Body Range: File "", line 331, character 4 to line 337, character 38
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
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
                      (log#340:6-9 -> log)
                      Range: File "", line 340, characters 6-9
                      Body Range: File "", line 341, character 4 to line 343, character 11
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 367, characters 25-28
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
                      (bootstrap_contract#346:6-24 -> bootstrap_contract)
                      Range: File "", line 346, characters 6-24
                      Body Range: File "", line 346, characters 97-145
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (mutate_value#347:6-18 -> mutate_value)
                      Range: File "", line 347, characters 6-18
                      Body Range: File "", line 347, characters 72-111
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 409, characters 23-35 ,
                        File "", line 421, characters 23-35
                      (save_mutation#348:6-19 -> save_mutation)
                      Range: File "", line 348, characters 6-19
                      Body Range: File "", line 348, characters 2-106
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (sign#349:6-10 -> sign)
                      Range: File "", line 349, characters 6-10
                      Body Range: File "", line 349, characters 2-83
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (add_account#350:6-17 -> add_account)
                      Range: File "", line 350, characters 6-17
                      Body Range: File "", line 350, characters 2-88
                      Content: |core: string -> key -> unit|
                      references: []
                      (baker_account#351:6-19 -> baker_account)
                      Range: File "", line 351, characters 6-19
                      Body Range: File "", line 351, characters 2-105
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (set_big_map#352:6-17 -> set_big_map)
                      Range: File "", line 352, characters 6-17
                      Body Range: File "", line 352, characters 69-107
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (transfer_to_contract#353:6-26 -> transfer_to_contract)
                      Range: File "", line 353, characters 6-26
                      Body Range: File "", line 354, character 4 to line 357, character 61
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: File "", line 560, characters 6-26
                      (transfer_to_contract_exn#358:6-30 -> transfer_to_contract_exn)
                      Range: File "", line 358, characters 6-30
                      Body Range: File "", line 359, character 6 to line 362, character 67
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references:
                        File "", line 570, characters 14-38 ,
                        File "", line 585, characters 14-38
                      (michelson_equal#363:6-21 -> michelson_equal)
                      Range: File "", line 363, characters 6-21
                      Body Range: File "", line 363, characters 2-88
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (to_entrypoint#364:6-19 -> to_entrypoint)
                      Range: File "", line 364, characters 6-19
                      Body Range: File "", line 365, character 4 to line 371, character 44
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (originate_contract#372:6-24 -> originate_contract)
                      Range: File "", line 372, characters 6-24
                      Body Range: File "", line 372, characters 2-135
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 376, characters 12-30 ,
                        File "", line 386, characters 12-30 ,
                        File "", line 393, characters 12-30 ,
                        File "", line 402, characters 12-30 ,
                        File "", line 433, characters 14-32 ,
                        File "", line 453, characters 14-32 ,
                        File "", line 474, characters 14-32 ,
                        File "", line 496, characters 14-32
                      (originate#373:6-15 -> originate)
                      Range: File "", line 373, characters 6-15
                      Body Range: File "", line 374, character 4 to line 379, character 13
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references:
                        File "", line 553, characters 32-41 ,
                        File "", line 569, characters 32-41 ,
                        File "", line 584, characters 32-41
                      (compile_contract_with_views#380:8-35 -> compile_contract_with_views)
                      Range: File "", line 380, characters 8-35
                      Body Range: File "", line 381, character 6 to line 382, character 54
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 391, characters 12-39
                      (originate_uncurried#383:6-25 -> originate_uncurried)
                      Range: File "", line 383, characters 6-25
                      Body Range: File "", line 384, character 4 to line 389, character 13
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (originate_module#390:6-22 -> originate_module)
                      Range: File "", line 390, characters 6-22
                      Body Range: File "", line 391, character 4 to line 396, character 13
                      Content: |core: ∀ p : * . ∀ s : * . module_contract (p ,
                      s) -> s -> tez -> ( typed_address (p ,
                                          s) *
                                          michelson_contract *
                                          int )|
                      references: []
                      (compile_contract_from_file#397:6-32 -> compile_contract_from_file)
                      Range: File "", line 397, characters 6-32
                      Body Range: File "", line 397, character 2 to line 399, character 52
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 401, characters 12-38
                      (originate_from_file#400:6-25 -> originate_from_file)
                      Range: File "", line 400, characters 6-25
                      Body Range: File "", line 400, character 2 to line 404, character 13
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (mutation_test#405:6-19 -> mutation_test)
                      Range: File "", line 405, characters 6-19
                      Body Range: File "", line 406, character 4 to line 416, character 19
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#417:6-23 -> mutation_test_all)
                      Range: File "", line 417, characters 6-23
                      Body Range: File "", line 418, character 4 to line 428, character 46
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (originate_from_file_and_mutate#429:6-36 -> originate_from_file_and_mutate)
                      Range: File "", line 429, characters 6-36
                      Body Range: File "", line 431, character 4 to line 448, character 19
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#449:6-40 -> originate_from_file_and_mutate_all)
                      Range: File "", line 449, characters 6-40
                      Body Range: File "", line 451, character 4 to line 468, character 46
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module_and_mutate#469:6-33 -> originate_module_and_mutate)
                      Range: File "", line 469, characters 6-33
                      Body Range: File "", line 471, character 4 to line 490, character 19
                      Content: |core: ∀ p : * . ∀ s : * . ∀ b : * . module_contract (p ,
                      s) -> s -> tez -> typed_address (p ,
                      s) -> michelson_contract -> int -> b -> option (( b *
                                                                        mutation ))|
                      references: []
                      (originate_module_and_mutate_all#491:6-37 -> originate_module_and_mutate_all)
                      Range: File "", line 491, characters 6-37
                      Body Range: File "", line 493, character 4 to line 512, character 46
                      Content: |core: ∀ p : * . ∀ s : * . ∀ b : * . module_contract (p ,
                      s) -> s -> tez -> typed_address (p ,
                      s) -> michelson_contract -> int -> b -> list (( b *
                                                                      mutation ))|
                      references: []
                      (assert#514:6-12 -> assert)
                      Range: File "", line 514, characters 6-12
                      Body Range: File "", line 514, characters 2-78
                      Content: |core: bool -> unit|
                      references: []
                      (assert_some#515:6-17 -> assert_some)
                      Range: File "", line 515, characters 6-17
                      Body Range: File "", line 515, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none#516:6-17 -> assert_none)
                      Range: File "", line 516, characters 6-17
                      Body Range: File "", line 516, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_with_error#518:6-23 -> assert_with_error)
                      Range: File "", line 518, characters 6-23
                      Body Range: File "", line 518, characters 2-78
                      Content: |unresolved|
                      references: []
                      (assert_some_with_error#519:6-28 -> assert_some_with_error)
                      Range: File "", line 519, characters 6-28
                      Body Range: File "", line 519, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_none_with_error#520:6-28 -> assert_none_with_error)
                      Range: File "", line 520, characters 6-28
                      Body Range: File "", line 520, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
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

                      (Proxy_ticket#522:9-21 -> Proxy_ticket)
                      Range: File "", line 522, characters 9-21
                      Body Range: File "", line 523, character 4 to line 590, character 7
                      Content: Members: Variable definitions:
                                        (proxy_transfer_contract#523:19-42 -> proxy_transfer_contract)
                                        Range: File "", line 523, characters 19-42
                                        Body Range: File "", line 528, character 6 to line 533, character 14
                                        Content: |core: ∀ vt : * . ∀ whole_p : * . ticket (vt) -> whole_p ->
                                        ( ( vt * nat ) *
                                          address ) -> unit -> ( list (operation) *
                                                                 unit )|
                                        references:
                                          File "", line 551, characters 8-31
                                        (proxy_originate_contract#535:19-43 -> proxy_originate_contract)
                                        Range: File "", line 535, characters 19-43
                                        Body Range: File "", line 541, character 6 to line 545, character 21
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * . ticket (vt) -> whole_s -> vp -> whole_s ->
                                        ( list (operation) *
                                          whole_s ) -> ( vt * nat ) -> option (address) ->
                                        ( list (operation) *
                                          option (address) )|
                                        references:
                                          File "", line 567, characters 8-32 ,
                                          File "", line 582, characters 8-32
                                        (init_transfer#549:8-21 -> init_transfer)
                                        Range: File "", line 549, characters 8-21
                                        Body Range: File "", line 550, character 6 to line 554, character 17
                                        Content: |core: ∀ vt : * . ∀ whole_p : * . ticket (vt) -> whole_p -> proxy_address (vt)|
                                        references: []
                                        (transfer#556:8-16 -> transfer)
                                        Range: File "", line 556, characters 8-16
                                        Body Range: File "", line 559, character 6 to line 560, character 84
                                        Content: |core: ∀ vt : * . proxy_address (vt) ->
                                        ( ( vt * nat ) *
                                          address ) -> test_exec_result|
                                        references: []
                                        (originate_uncurried#562:8-27 -> originate_uncurried)
                                        Range: File "", line 562, characters 8-27
                                        Body Range: File "", line 566, character 6 to line 575, character 7
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * .
                                        ( vt *
                                          nat ) -> ticket (vt) -> whole_s ->
                                        ( vp *
                                          whole_s ) -> ( list (operation) *
                                                         whole_s ) -> address|
                                        references: []
                                        (originate#577:8-17 -> originate)
                                        Range: File "", line 577, characters 8-17
                                        Body Range: File "", line 581, character 6 to line 590, character 7
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * .
                                        ( vt *
                                          nat ) -> ticket (vt) -> whole_s -> vp -> whole_s ->
                                        ( list (operation) *
                                          whole_s ) -> address|
                                        references: []
                                        Type definitions:
                                        (proxy_address#547:12-25 -> proxy_address)
                                        Range: File "", line 547, characters 12-25
                                        Body Range: File "", line 547, characters 28-71
                                        Content: : |funtype 'v : * . typed_address (
                                        ( ( 'v * nat ) *
                                          address ) ,
                                        unit)|
                                        references:
                                          File "", line 549, characters 78-91 ,
                                          File "", line 557, characters 26-39
                                        Module definitions:

                      references: []


    references: [] |}]
