open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_int_lima#39:5-22 external_ediv#40:5-18 external_and#41:5-17 external_or#42:5-16 external_xor#43:5-17 external_lsl#44:5-17 external_lsr#45:5-17 external_map_find_opt#46:5-26 external_map_add#47:5-21 external_map_remove#48:5-24 external_map_remove_value#49:5-30 failwith#56:4-12 bool#58:5-9 option#59:8-14 Tezos#61:7-12 get_balance#63:6-17 get_amount#64:6-16 get_now#65:6-13 get_sender#66:6-16 get_source#67:6-16 get_level#68:6-15 get_self_address#69:6-22 get_chain_id#70:6-18 get_total_voting_power#71:6-28 get_min_block_time#72:6-24 voting_power#73:6-18 address#74:6-13 implicit_account#75:6-22 join_tickets#76:6-18 read_ticket#77:6-17 never#79:6-11 pairing_check#80:6-19 set_delegate#81:6-18 self#82:25-29 constant#85:25-33 sapling_empty_state#86:25-44 get_contract_opt#88:25-41 get_contract#90:25-37 get_contract_with_error#94:6-29 create_ticket#97:6-19 transaction#98:6-17 call_view#100:25-34 split_ticket#103:6-18 create_contract#105:25-40 create_contract_uncurried#108:25-50 get_entrypoint_opt#110:25-43 get_entrypoint#113:25-39 emit#116:25-29 sapling_verify_update#119:25-46 Bitwise#123:7-14 and#124:6-10 xor#125:6-9 or#126:6-9 shift_left#127:6-16 shift_right#128:6-17 Big_map#131:7-14 empty#132:16-21 literal#133:25-32 mem#135:6-9 add#136:6-9 remove#137:6-12 update#138:6-12 get_and_update#139:6-20 find_opt#140:6-14 find#141:6-10 Map#145:7-10 empty#146:6-11 size#147:6-10 literal#148:25-32 mem#150:6-9 add#151:6-9 remove#152:6-12 update#153:6-12 get_and_update#154:6-20 find#155:6-10 find_opt#156:6-14 iter#157:6-10 map#158:6-9 fold#159:6-10 Transpiled#163:7-17 map_find_opt#164:6-18 map_add#165:6-13 map_remove#166:6-16 Set#169:7-10 empty#170:6-11 size#171:6-10 cardinal#172:6-14 literal#173:25-32 mem#175:6-9 add#176:6-9 remove#177:6-12 update#178:6-12 iter#179:6-10 fold#180:6-10 fold_desc#181:6-15 filter_map#182:6-16 List#186:7-11 length#187:6-12 size#188:6-10 head_opt#189:6-14 tail_opt#190:6-14 map#192:6-9 iter#193:6-10 fold#194:6-10 fold_left#195:6-15 fold_right#196:6-16 cons#197:6-10 find_opt#198:6-14 filter_map#200:6-16 update#202:6-12 update_with#204:6-17 String#208:7-13 length#209:6-12 concats#210:6-13 concat#212:6-12 sub#213:6-9 Option#216:7-13 unopt#217:6-11 unopt_with_error#219:6-22 map#220:15-18 value#221:6-11 value_exn#222:6-15 is_none#223:6-13 is_some#224:6-13 Bytes#227:7-12 concats#228:6-13 pack#229:6-10 unpack#230:6-12 length#231:6-12 concat#233:6-12 sub#234:6-9 Crypto#237:7-13 blake2b#238:6-13 sha256#239:6-12 sha512#240:6-12 sha3#241:6-10 keccak#242:6-12 hash_key#243:6-14 check#244:6-11 assert#247:4-10 assert_some#248:4-15 assert_none#249:4-15 abs#250:4-7 is_nat#251:4-10 true#252:14-18 false#253:14-19 unit#254:14-18 int#256:4-7 nat#261:4-7 bytes#262:4-9 ignore#264:4-10 curry#265:4-9 uncurry#266:4-11 assert_with_error#268:4-21 assert_some_with_error#269:4-26 assert_none_with_error#270:4-26 ediv#271:4-8 michelson_program#273:5-22 typed_address#274:5-18 mutation#275:5-13 michelson_contract#276:5-23 ast_contract#277:5-17 pbt_gen#278:5-12 int64#279:5-10 views#280:5-10 test_exec_error_balance_too_low#282:5-36 test_exec_error#285:5-20 test_exec_result#290:5-21 test_baker_policy#292:5-22 pbt_test#297:8-16 pbt_result#298:8-18 unforged_ticket#300:8-23 module_contract#302:14-29 Test#304:7-11 run#306:6-9 eval#307:6-10 compile_value#309:6-19 get_total_voting_power#310:6-28 failwith#311:6-14 to_contract#312:6-17 set_source#313:6-16 get_storage_of_address#314:6-28 get_balance#315:6-17 print#316:6-11 eprint#317:6-12 get_voting_power#318:6-22 nth_bootstrap_contract#319:6-28 nth_bootstrap_account#320:6-27 get_bootstrap_account#323:6-27 nth_bootstrap_typed_address#324:6-33 last_originations#325:6-23 random#326:6-12 new_account#329:6-17 decompile#330:6-15 bake_until_n_cycle_end#331:6-28 get_time#332:6-14 cast_address#333:6-18 register_delegate#334:6-23 register_constant#335:6-23 to_typed_address#336:6-22 constant_to_michelson_program#337:6-35 parse_michelson#338:6-21 restore_context#339:6-21 save_context#340:6-18 drop_context#341:6-18 to_string#342:6-15 to_json#343:6-13 get_storage#344:6-17 set_baker_policy#349:6-22 set_baker#350:6-15 size#351:6-10 compile_contract#352:6-22 read_contract_from_file#356:6-29 chr#357:6-9 nl#367:6-8 println#368:6-13 set_print_values#371:6-22 unset_print_values#372:6-24 PBT#374:9-12 gen#375:8-11 gen_small#376:8-17 make_test#377:8-17 run#378:8-11 get_last_events_from#392:6-26 transfer#400:6-14 transfer_exn#401:6-18 log#402:6-9 reset_state#406:6-17 reset_state_at#407:6-20 bootstrap_contract#408:6-24 mutate_value#409:6-18 save_mutation#410:6-19 sign#411:6-10 add_account#412:6-17 baker_account#413:6-19 set_big_map#414:6-17 transfer_to_contract#415:6-26 transfer_to_contract_exn#420:6-30 michelson_equal#425:6-21 to_entrypoint#426:6-19 originate_contract#434:6-24 originate#435:6-15 compile_contract_with_views#442:8-35 originate_uncurried#445:6-25 originate_module#452:6-22 compile_contract_from_file#459:6-32 originate_from_file#462:6-25 mutation_test#467:6-19 mutation_test_all#479:6-23 originate_from_file_and_mutate#491:6-36 originate_from_file_and_mutate_all#511:6-40 originate_module_and_mutate#531:6-33 originate_module_and_mutate_all#553:6-37 assert#576:6-12 assert_some#577:6-17 assert_none#578:6-17 assert_with_error#580:6-23 assert_some_with_error#581:6-28 assert_none_with_error#582:6-28 Proxy_ticket#584:9-21 proxy_transfer_contract#585:19-42 proxy_originate_contract#597:19-43 proxy_address#609:12-25 init_transfer#611:8-21 transfer#618:8-16 originate_uncurried#624:8-27 originate#639:8-17  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_int_lima#39:5-22 external_ediv#40:5-18 external_and#41:5-17 external_or#42:5-16 external_xor#43:5-17 external_lsl#44:5-17 external_lsr#45:5-17 external_map_find_opt#46:5-26 external_map_add#47:5-21 external_map_remove#48:5-24 external_map_remove_value#49:5-30 failwith#56:4-12 bool#58:5-9 option#59:8-14 Tezos#61:7-12 get_balance#63:6-17 get_amount#64:6-16 get_now#65:6-13 get_sender#66:6-16 get_source#67:6-16 get_level#68:6-15 get_self_address#69:6-22 get_chain_id#70:6-18 get_total_voting_power#71:6-28 get_min_block_time#72:6-24 voting_power#73:6-18 address#74:6-13 implicit_account#75:6-22 join_tickets#76:6-18 read_ticket#77:6-17 never#79:6-11 pairing_check#80:6-19 set_delegate#81:6-18 self#82:25-29 constant#85:25-33 sapling_empty_state#86:25-44 get_contract_opt#88:25-41 get_contract#90:25-37 get_contract_with_error#94:6-29 create_ticket#97:6-19 transaction#98:6-17 call_view#100:25-34 split_ticket#103:6-18 create_contract#105:25-40 create_contract_uncurried#108:25-50 get_entrypoint_opt#110:25-43 get_entrypoint#113:25-39 emit#116:25-29 sapling_verify_update#119:25-46 Bitwise#123:7-14 and#124:6-10 xor#125:6-9 or#126:6-9 shift_left#127:6-16 shift_right#128:6-17 Big_map#131:7-14 empty#132:16-21 literal#133:25-32 mem#135:6-9 add#136:6-9 remove#137:6-12 update#138:6-12 get_and_update#139:6-20 find_opt#140:6-14 find#141:6-10 Map#145:7-10 empty#146:6-11 size#147:6-10 literal#148:25-32 mem#150:6-9 add#151:6-9 remove#152:6-12 update#153:6-12 get_and_update#154:6-20 find#155:6-10 find_opt#156:6-14 iter#157:6-10 map#158:6-9 fold#159:6-10 Transpiled#163:7-17 map_find_opt#164:6-18 map_add#165:6-13 map_remove#166:6-16 Set#169:7-10 empty#170:6-11 size#171:6-10 cardinal#172:6-14 literal#173:25-32 mem#175:6-9 add#176:6-9 remove#177:6-12 update#178:6-12 iter#179:6-10 fold#180:6-10 fold_desc#181:6-15 filter_map#182:6-16 List#186:7-11 length#187:6-12 size#188:6-10 head_opt#189:6-14 tail_opt#190:6-14 map#192:6-9 iter#193:6-10 fold#194:6-10 fold_left#195:6-15 fold_right#196:6-16 cons#197:6-10 find_opt#198:6-14 filter_map#200:6-16 update#202:6-12 update_with#204:6-17 String#208:7-13 length#209:6-12 concats#210:6-13 concat#212:6-12 sub#213:6-9 Option#216:7-13 unopt#217:6-11 unopt_with_error#219:6-22 map#220:15-18 value#221:6-11 value_exn#222:6-15 is_none#223:6-13 is_some#224:6-13 Bytes#227:7-12 concats#228:6-13 pack#229:6-10 unpack#230:6-12 length#231:6-12 concat#233:6-12 sub#234:6-9 Crypto#237:7-13 blake2b#238:6-13 sha256#239:6-12 sha512#240:6-12 sha3#241:6-10 keccak#242:6-12 hash_key#243:6-14 check#244:6-11 assert#247:4-10 assert_some#248:4-15 assert_none#249:4-15 abs#250:4-7 is_nat#251:4-10 true#252:14-18 false#253:14-19 unit#254:14-18 int#256:4-7 nat#261:4-7 bytes#262:4-9 ignore#264:4-10 curry#265:4-9 uncurry#266:4-11 assert_with_error#268:4-21 assert_some_with_error#269:4-26 assert_none_with_error#270:4-26 ediv#271:4-8 michelson_program#273:5-22 typed_address#274:5-18 mutation#275:5-13 michelson_contract#276:5-23 ast_contract#277:5-17 pbt_gen#278:5-12 int64#279:5-10 views#280:5-10 test_exec_error_balance_too_low#282:5-36 test_exec_error#285:5-20 test_exec_result#290:5-21 test_baker_policy#292:5-22 pbt_test#297:8-16 pbt_result#298:8-18 unforged_ticket#300:8-23 module_contract#302:14-29 Test#304:7-11 run#306:6-9 eval#307:6-10 compile_value#309:6-19 get_total_voting_power#310:6-28 failwith#311:6-14 to_contract#312:6-17 set_source#313:6-16 get_storage_of_address#314:6-28 get_balance#315:6-17 print#316:6-11 eprint#317:6-12 get_voting_power#318:6-22 nth_bootstrap_contract#319:6-28 nth_bootstrap_account#320:6-27 get_bootstrap_account#323:6-27 nth_bootstrap_typed_address#324:6-33 last_originations#325:6-23 random#326:6-12 new_account#329:6-17 decompile#330:6-15 bake_until_n_cycle_end#331:6-28 get_time#332:6-14 cast_address#333:6-18 register_delegate#334:6-23 register_constant#335:6-23 to_typed_address#336:6-22 constant_to_michelson_program#337:6-35 parse_michelson#338:6-21 restore_context#339:6-21 save_context#340:6-18 drop_context#341:6-18 to_string#342:6-15 to_json#343:6-13 get_storage#344:6-17 set_baker_policy#349:6-22 set_baker#350:6-15 size#351:6-10 compile_contract#352:6-22 read_contract_from_file#356:6-29 chr#357:6-9 nl#367:6-8 println#368:6-13 set_print_values#371:6-22 unset_print_values#372:6-24 PBT#374:9-12 gen#375:8-11 gen_small#376:8-17 make_test#377:8-17 run#378:8-11 get_last_events_from#392:6-26 transfer#400:6-14 transfer_exn#401:6-18 log#402:6-9 reset_state#406:6-17 reset_state_at#407:6-20 bootstrap_contract#408:6-24 mutate_value#409:6-18 save_mutation#410:6-19 sign#411:6-10 add_account#412:6-17 baker_account#413:6-19 set_big_map#414:6-17 transfer_to_contract#415:6-26 transfer_to_contract_exn#420:6-30 michelson_equal#425:6-21 to_entrypoint#426:6-19 originate_contract#434:6-24 originate#435:6-15 compile_contract_with_views#442:8-35 originate_uncurried#445:6-25 originate_module#452:6-22 compile_contract_from_file#459:6-32 originate_from_file#462:6-25 mutation_test#467:6-19 mutation_test_all#479:6-23 originate_from_file_and_mutate#491:6-36 originate_from_file_and_mutate_all#511:6-40 originate_module_and_mutate#531:6-33 originate_module_and_mutate_all#553:6-37 assert#576:6-12 assert_some#577:6-17 assert_none#578:6-17 assert_with_error#580:6-23 assert_some_with_error#581:6-28 assert_none_with_error#582:6-28 Proxy_ticket#584:9-21 proxy_transfer_contract#585:19-42 proxy_originate_contract#597:19-43 proxy_address#609:12-25 init_transfer#611:8-21 transfer#618:8-16 originate_uncurried#624:8-27 originate#639:8-17 a#1:4-5  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_int_lima#39:5-22 external_ediv#40:5-18 external_and#41:5-17 external_or#42:5-16 external_xor#43:5-17 external_lsl#44:5-17 external_lsr#45:5-17 external_map_find_opt#46:5-26 external_map_add#47:5-21 external_map_remove#48:5-24 external_map_remove_value#49:5-30 failwith#56:4-12 bool#58:5-9 option#59:8-14 Tezos#61:7-12 get_balance#63:6-17 get_amount#64:6-16 get_now#65:6-13 get_sender#66:6-16 get_source#67:6-16 get_level#68:6-15 get_self_address#69:6-22 get_chain_id#70:6-18 get_total_voting_power#71:6-28 get_min_block_time#72:6-24 voting_power#73:6-18 address#74:6-13 implicit_account#75:6-22 join_tickets#76:6-18 read_ticket#77:6-17 never#79:6-11 pairing_check#80:6-19 set_delegate#81:6-18 self#82:25-29 constant#85:25-33 sapling_empty_state#86:25-44 get_contract_opt#88:25-41 get_contract#90:25-37 get_contract_with_error#94:6-29 create_ticket#97:6-19 transaction#98:6-17 call_view#100:25-34 split_ticket#103:6-18 create_contract#105:25-40 create_contract_uncurried#108:25-50 get_entrypoint_opt#110:25-43 get_entrypoint#113:25-39 emit#116:25-29 sapling_verify_update#119:25-46 Bitwise#123:7-14 and#124:6-10 xor#125:6-9 or#126:6-9 shift_left#127:6-16 shift_right#128:6-17 Big_map#131:7-14 empty#132:16-21 literal#133:25-32 mem#135:6-9 add#136:6-9 remove#137:6-12 update#138:6-12 get_and_update#139:6-20 find_opt#140:6-14 find#141:6-10 Map#145:7-10 empty#146:6-11 size#147:6-10 literal#148:25-32 mem#150:6-9 add#151:6-9 remove#152:6-12 update#153:6-12 get_and_update#154:6-20 find#155:6-10 find_opt#156:6-14 iter#157:6-10 map#158:6-9 fold#159:6-10 Transpiled#163:7-17 map_find_opt#164:6-18 map_add#165:6-13 map_remove#166:6-16 Set#169:7-10 empty#170:6-11 size#171:6-10 cardinal#172:6-14 literal#173:25-32 mem#175:6-9 add#176:6-9 remove#177:6-12 update#178:6-12 iter#179:6-10 fold#180:6-10 fold_desc#181:6-15 filter_map#182:6-16 List#186:7-11 length#187:6-12 size#188:6-10 head_opt#189:6-14 tail_opt#190:6-14 map#192:6-9 iter#193:6-10 fold#194:6-10 fold_left#195:6-15 fold_right#196:6-16 cons#197:6-10 find_opt#198:6-14 filter_map#200:6-16 update#202:6-12 update_with#204:6-17 String#208:7-13 length#209:6-12 concats#210:6-13 concat#212:6-12 sub#213:6-9 Option#216:7-13 unopt#217:6-11 unopt_with_error#219:6-22 map#220:15-18 value#221:6-11 value_exn#222:6-15 is_none#223:6-13 is_some#224:6-13 Bytes#227:7-12 concats#228:6-13 pack#229:6-10 unpack#230:6-12 length#231:6-12 concat#233:6-12 sub#234:6-9 Crypto#237:7-13 blake2b#238:6-13 sha256#239:6-12 sha512#240:6-12 sha3#241:6-10 keccak#242:6-12 hash_key#243:6-14 check#244:6-11 assert#247:4-10 assert_some#248:4-15 assert_none#249:4-15 abs#250:4-7 is_nat#251:4-10 true#252:14-18 false#253:14-19 unit#254:14-18 int#256:4-7 nat#261:4-7 bytes#262:4-9 ignore#264:4-10 curry#265:4-9 uncurry#266:4-11 assert_with_error#268:4-21 assert_some_with_error#269:4-26 assert_none_with_error#270:4-26 ediv#271:4-8 michelson_program#273:5-22 typed_address#274:5-18 mutation#275:5-13 michelson_contract#276:5-23 ast_contract#277:5-17 pbt_gen#278:5-12 int64#279:5-10 views#280:5-10 test_exec_error_balance_too_low#282:5-36 test_exec_error#285:5-20 test_exec_result#290:5-21 test_baker_policy#292:5-22 pbt_test#297:8-16 pbt_result#298:8-18 unforged_ticket#300:8-23 module_contract#302:14-29 Test#304:7-11 run#306:6-9 eval#307:6-10 compile_value#309:6-19 get_total_voting_power#310:6-28 failwith#311:6-14 to_contract#312:6-17 set_source#313:6-16 get_storage_of_address#314:6-28 get_balance#315:6-17 print#316:6-11 eprint#317:6-12 get_voting_power#318:6-22 nth_bootstrap_contract#319:6-28 nth_bootstrap_account#320:6-27 get_bootstrap_account#323:6-27 nth_bootstrap_typed_address#324:6-33 last_originations#325:6-23 random#326:6-12 new_account#329:6-17 decompile#330:6-15 bake_until_n_cycle_end#331:6-28 get_time#332:6-14 cast_address#333:6-18 register_delegate#334:6-23 register_constant#335:6-23 to_typed_address#336:6-22 constant_to_michelson_program#337:6-35 parse_michelson#338:6-21 restore_context#339:6-21 save_context#340:6-18 drop_context#341:6-18 to_string#342:6-15 to_json#343:6-13 get_storage#344:6-17 set_baker_policy#349:6-22 set_baker#350:6-15 size#351:6-10 compile_contract#352:6-22 read_contract_from_file#356:6-29 chr#357:6-9 nl#367:6-8 println#368:6-13 set_print_values#371:6-22 unset_print_values#372:6-24 PBT#374:9-12 gen#375:8-11 gen_small#376:8-17 make_test#377:8-17 run#378:8-11 get_last_events_from#392:6-26 transfer#400:6-14 transfer_exn#401:6-18 log#402:6-9 reset_state#406:6-17 reset_state_at#407:6-20 bootstrap_contract#408:6-24 mutate_value#409:6-18 save_mutation#410:6-19 sign#411:6-10 add_account#412:6-17 baker_account#413:6-19 set_big_map#414:6-17 transfer_to_contract#415:6-26 transfer_to_contract_exn#420:6-30 michelson_equal#425:6-21 to_entrypoint#426:6-19 originate_contract#434:6-24 originate#435:6-15 compile_contract_with_views#442:8-35 originate_uncurried#445:6-25 originate_module#452:6-22 compile_contract_from_file#459:6-32 originate_from_file#462:6-25 mutation_test#467:6-19 mutation_test_all#479:6-23 originate_from_file_and_mutate#491:6-36 originate_from_file_and_mutate_all#511:6-40 originate_module_and_mutate#531:6-33 originate_module_and_mutate_all#553:6-37 assert#576:6-12 assert_some#577:6-17 assert_none#578:6-17 assert_with_error#580:6-23 assert_some_with_error#581:6-28 assert_none_with_error#582:6-28 Proxy_ticket#584:9-21 proxy_transfer_contract#585:19-42 proxy_originate_contract#597:19-43 proxy_address#609:12-25 init_transfer#611:8-21 transfer#618:8-16 originate_uncurried#624:8-27 originate#639:8-17 a#1:4-5 c#5:10-11  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_int_lima#39:5-22 external_ediv#40:5-18 external_and#41:5-17 external_or#42:5-16 external_xor#43:5-17 external_lsl#44:5-17 external_lsr#45:5-17 external_map_find_opt#46:5-26 external_map_add#47:5-21 external_map_remove#48:5-24 external_map_remove_value#49:5-30 failwith#56:4-12 bool#58:5-9 option#59:8-14 Tezos#61:7-12 get_balance#63:6-17 get_amount#64:6-16 get_now#65:6-13 get_sender#66:6-16 get_source#67:6-16 get_level#68:6-15 get_self_address#69:6-22 get_chain_id#70:6-18 get_total_voting_power#71:6-28 get_min_block_time#72:6-24 voting_power#73:6-18 address#74:6-13 implicit_account#75:6-22 join_tickets#76:6-18 read_ticket#77:6-17 never#79:6-11 pairing_check#80:6-19 set_delegate#81:6-18 self#82:25-29 constant#85:25-33 sapling_empty_state#86:25-44 get_contract_opt#88:25-41 get_contract#90:25-37 get_contract_with_error#94:6-29 create_ticket#97:6-19 transaction#98:6-17 call_view#100:25-34 split_ticket#103:6-18 create_contract#105:25-40 create_contract_uncurried#108:25-50 get_entrypoint_opt#110:25-43 get_entrypoint#113:25-39 emit#116:25-29 sapling_verify_update#119:25-46 Bitwise#123:7-14 and#124:6-10 xor#125:6-9 or#126:6-9 shift_left#127:6-16 shift_right#128:6-17 Big_map#131:7-14 empty#132:16-21 literal#133:25-32 mem#135:6-9 add#136:6-9 remove#137:6-12 update#138:6-12 get_and_update#139:6-20 find_opt#140:6-14 find#141:6-10 Map#145:7-10 empty#146:6-11 size#147:6-10 literal#148:25-32 mem#150:6-9 add#151:6-9 remove#152:6-12 update#153:6-12 get_and_update#154:6-20 find#155:6-10 find_opt#156:6-14 iter#157:6-10 map#158:6-9 fold#159:6-10 Transpiled#163:7-17 map_find_opt#164:6-18 map_add#165:6-13 map_remove#166:6-16 Set#169:7-10 empty#170:6-11 size#171:6-10 cardinal#172:6-14 literal#173:25-32 mem#175:6-9 add#176:6-9 remove#177:6-12 update#178:6-12 iter#179:6-10 fold#180:6-10 fold_desc#181:6-15 filter_map#182:6-16 List#186:7-11 length#187:6-12 size#188:6-10 head_opt#189:6-14 tail_opt#190:6-14 map#192:6-9 iter#193:6-10 fold#194:6-10 fold_left#195:6-15 fold_right#196:6-16 cons#197:6-10 find_opt#198:6-14 filter_map#200:6-16 update#202:6-12 update_with#204:6-17 String#208:7-13 length#209:6-12 concats#210:6-13 concat#212:6-12 sub#213:6-9 Option#216:7-13 unopt#217:6-11 unopt_with_error#219:6-22 map#220:15-18 value#221:6-11 value_exn#222:6-15 is_none#223:6-13 is_some#224:6-13 Bytes#227:7-12 concats#228:6-13 pack#229:6-10 unpack#230:6-12 length#231:6-12 concat#233:6-12 sub#234:6-9 Crypto#237:7-13 blake2b#238:6-13 sha256#239:6-12 sha512#240:6-12 sha3#241:6-10 keccak#242:6-12 hash_key#243:6-14 check#244:6-11 assert#247:4-10 assert_some#248:4-15 assert_none#249:4-15 abs#250:4-7 is_nat#251:4-10 true#252:14-18 false#253:14-19 unit#254:14-18 int#256:4-7 nat#261:4-7 bytes#262:4-9 ignore#264:4-10 curry#265:4-9 uncurry#266:4-11 assert_with_error#268:4-21 assert_some_with_error#269:4-26 assert_none_with_error#270:4-26 ediv#271:4-8 michelson_program#273:5-22 typed_address#274:5-18 mutation#275:5-13 michelson_contract#276:5-23 ast_contract#277:5-17 pbt_gen#278:5-12 int64#279:5-10 views#280:5-10 test_exec_error_balance_too_low#282:5-36 test_exec_error#285:5-20 test_exec_result#290:5-21 test_baker_policy#292:5-22 pbt_test#297:8-16 pbt_result#298:8-18 unforged_ticket#300:8-23 module_contract#302:14-29 Test#304:7-11 run#306:6-9 eval#307:6-10 compile_value#309:6-19 get_total_voting_power#310:6-28 failwith#311:6-14 to_contract#312:6-17 set_source#313:6-16 get_storage_of_address#314:6-28 get_balance#315:6-17 print#316:6-11 eprint#317:6-12 get_voting_power#318:6-22 nth_bootstrap_contract#319:6-28 nth_bootstrap_account#320:6-27 get_bootstrap_account#323:6-27 nth_bootstrap_typed_address#324:6-33 last_originations#325:6-23 random#326:6-12 new_account#329:6-17 decompile#330:6-15 bake_until_n_cycle_end#331:6-28 get_time#332:6-14 cast_address#333:6-18 register_delegate#334:6-23 register_constant#335:6-23 to_typed_address#336:6-22 constant_to_michelson_program#337:6-35 parse_michelson#338:6-21 restore_context#339:6-21 save_context#340:6-18 drop_context#341:6-18 to_string#342:6-15 to_json#343:6-13 get_storage#344:6-17 set_baker_policy#349:6-22 set_baker#350:6-15 size#351:6-10 compile_contract#352:6-22 read_contract_from_file#356:6-29 chr#357:6-9 nl#367:6-8 println#368:6-13 set_print_values#371:6-22 unset_print_values#372:6-24 PBT#374:9-12 gen#375:8-11 gen_small#376:8-17 make_test#377:8-17 run#378:8-11 get_last_events_from#392:6-26 transfer#400:6-14 transfer_exn#401:6-18 log#402:6-9 reset_state#406:6-17 reset_state_at#407:6-20 bootstrap_contract#408:6-24 mutate_value#409:6-18 save_mutation#410:6-19 sign#411:6-10 add_account#412:6-17 baker_account#413:6-19 set_big_map#414:6-17 transfer_to_contract#415:6-26 transfer_to_contract_exn#420:6-30 michelson_equal#425:6-21 to_entrypoint#426:6-19 originate_contract#434:6-24 originate#435:6-15 compile_contract_with_views#442:8-35 originate_uncurried#445:6-25 originate_module#452:6-22 compile_contract_from_file#459:6-32 originate_from_file#462:6-25 mutation_test#467:6-19 mutation_test_all#479:6-23 originate_from_file_and_mutate#491:6-36 originate_from_file_and_mutate_all#511:6-40 originate_module_and_mutate#531:6-33 originate_module_and_mutate_all#553:6-37 assert#576:6-12 assert_some#577:6-17 assert_none#578:6-17 assert_with_error#580:6-23 assert_some_with_error#581:6-28 assert_none_with_error#582:6-28 Proxy_ticket#584:9-21 proxy_transfer_contract#585:19-42 proxy_originate_contract#597:19-43 proxy_address#609:12-25 init_transfer#611:8-21 transfer#618:8-16 originate_uncurried#624:8-27 originate#639:8-17 a#1:4-5 c#5:10-11 d#5:26-27  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_int_lima#39:5-22 external_ediv#40:5-18 external_and#41:5-17 external_or#42:5-16 external_xor#43:5-17 external_lsl#44:5-17 external_lsr#45:5-17 external_map_find_opt#46:5-26 external_map_add#47:5-21 external_map_remove#48:5-24 external_map_remove_value#49:5-30 failwith#56:4-12 bool#58:5-9 option#59:8-14 Tezos#61:7-12 get_balance#63:6-17 get_amount#64:6-16 get_now#65:6-13 get_sender#66:6-16 get_source#67:6-16 get_level#68:6-15 get_self_address#69:6-22 get_chain_id#70:6-18 get_total_voting_power#71:6-28 get_min_block_time#72:6-24 voting_power#73:6-18 address#74:6-13 implicit_account#75:6-22 join_tickets#76:6-18 read_ticket#77:6-17 never#79:6-11 pairing_check#80:6-19 set_delegate#81:6-18 self#82:25-29 constant#85:25-33 sapling_empty_state#86:25-44 get_contract_opt#88:25-41 get_contract#90:25-37 get_contract_with_error#94:6-29 create_ticket#97:6-19 transaction#98:6-17 call_view#100:25-34 split_ticket#103:6-18 create_contract#105:25-40 create_contract_uncurried#108:25-50 get_entrypoint_opt#110:25-43 get_entrypoint#113:25-39 emit#116:25-29 sapling_verify_update#119:25-46 Bitwise#123:7-14 and#124:6-10 xor#125:6-9 or#126:6-9 shift_left#127:6-16 shift_right#128:6-17 Big_map#131:7-14 empty#132:16-21 literal#133:25-32 mem#135:6-9 add#136:6-9 remove#137:6-12 update#138:6-12 get_and_update#139:6-20 find_opt#140:6-14 find#141:6-10 Map#145:7-10 empty#146:6-11 size#147:6-10 literal#148:25-32 mem#150:6-9 add#151:6-9 remove#152:6-12 update#153:6-12 get_and_update#154:6-20 find#155:6-10 find_opt#156:6-14 iter#157:6-10 map#158:6-9 fold#159:6-10 Transpiled#163:7-17 map_find_opt#164:6-18 map_add#165:6-13 map_remove#166:6-16 Set#169:7-10 empty#170:6-11 size#171:6-10 cardinal#172:6-14 literal#173:25-32 mem#175:6-9 add#176:6-9 remove#177:6-12 update#178:6-12 iter#179:6-10 fold#180:6-10 fold_desc#181:6-15 filter_map#182:6-16 List#186:7-11 length#187:6-12 size#188:6-10 head_opt#189:6-14 tail_opt#190:6-14 map#192:6-9 iter#193:6-10 fold#194:6-10 fold_left#195:6-15 fold_right#196:6-16 cons#197:6-10 find_opt#198:6-14 filter_map#200:6-16 update#202:6-12 update_with#204:6-17 String#208:7-13 length#209:6-12 concats#210:6-13 concat#212:6-12 sub#213:6-9 Option#216:7-13 unopt#217:6-11 unopt_with_error#219:6-22 map#220:15-18 value#221:6-11 value_exn#222:6-15 is_none#223:6-13 is_some#224:6-13 Bytes#227:7-12 concats#228:6-13 pack#229:6-10 unpack#230:6-12 length#231:6-12 concat#233:6-12 sub#234:6-9 Crypto#237:7-13 blake2b#238:6-13 sha256#239:6-12 sha512#240:6-12 sha3#241:6-10 keccak#242:6-12 hash_key#243:6-14 check#244:6-11 assert#247:4-10 assert_some#248:4-15 assert_none#249:4-15 abs#250:4-7 is_nat#251:4-10 true#252:14-18 false#253:14-19 unit#254:14-18 int#256:4-7 nat#261:4-7 bytes#262:4-9 ignore#264:4-10 curry#265:4-9 uncurry#266:4-11 assert_with_error#268:4-21 assert_some_with_error#269:4-26 assert_none_with_error#270:4-26 ediv#271:4-8 michelson_program#273:5-22 typed_address#274:5-18 mutation#275:5-13 michelson_contract#276:5-23 ast_contract#277:5-17 pbt_gen#278:5-12 int64#279:5-10 views#280:5-10 test_exec_error_balance_too_low#282:5-36 test_exec_error#285:5-20 test_exec_result#290:5-21 test_baker_policy#292:5-22 pbt_test#297:8-16 pbt_result#298:8-18 unforged_ticket#300:8-23 module_contract#302:14-29 Test#304:7-11 run#306:6-9 eval#307:6-10 compile_value#309:6-19 get_total_voting_power#310:6-28 failwith#311:6-14 to_contract#312:6-17 set_source#313:6-16 get_storage_of_address#314:6-28 get_balance#315:6-17 print#316:6-11 eprint#317:6-12 get_voting_power#318:6-22 nth_bootstrap_contract#319:6-28 nth_bootstrap_account#320:6-27 get_bootstrap_account#323:6-27 nth_bootstrap_typed_address#324:6-33 last_originations#325:6-23 random#326:6-12 new_account#329:6-17 decompile#330:6-15 bake_until_n_cycle_end#331:6-28 get_time#332:6-14 cast_address#333:6-18 register_delegate#334:6-23 register_constant#335:6-23 to_typed_address#336:6-22 constant_to_michelson_program#337:6-35 parse_michelson#338:6-21 restore_context#339:6-21 save_context#340:6-18 drop_context#341:6-18 to_string#342:6-15 to_json#343:6-13 get_storage#344:6-17 set_baker_policy#349:6-22 set_baker#350:6-15 size#351:6-10 compile_contract#352:6-22 read_contract_from_file#356:6-29 chr#357:6-9 nl#367:6-8 println#368:6-13 set_print_values#371:6-22 unset_print_values#372:6-24 PBT#374:9-12 gen#375:8-11 gen_small#376:8-17 make_test#377:8-17 run#378:8-11 get_last_events_from#392:6-26 transfer#400:6-14 transfer_exn#401:6-18 log#402:6-9 reset_state#406:6-17 reset_state_at#407:6-20 bootstrap_contract#408:6-24 mutate_value#409:6-18 save_mutation#410:6-19 sign#411:6-10 add_account#412:6-17 baker_account#413:6-19 set_big_map#414:6-17 transfer_to_contract#415:6-26 transfer_to_contract_exn#420:6-30 michelson_equal#425:6-21 to_entrypoint#426:6-19 originate_contract#434:6-24 originate#435:6-15 compile_contract_with_views#442:8-35 originate_uncurried#445:6-25 originate_module#452:6-22 compile_contract_from_file#459:6-32 originate_from_file#462:6-25 mutation_test#467:6-19 mutation_test_all#479:6-23 originate_from_file_and_mutate#491:6-36 originate_from_file_and_mutate_all#511:6-40 originate_module_and_mutate#531:6-33 originate_module_and_mutate_all#553:6-37 assert#576:6-12 assert_some#577:6-17 assert_none#578:6-17 assert_with_error#580:6-23 assert_some_with_error#581:6-28 assert_none_with_error#582:6-28 Proxy_ticket#584:9-21 proxy_transfer_contract#585:19-42 proxy_originate_contract#597:19-43 proxy_address#609:12-25 init_transfer#611:8-21 transfer#618:8-16 originate_uncurried#624:8-27 originate#639:8-17 a#1:4-5 e#6:9-10  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32

    Variable definitions:
    (failwith#56:4-12 -> failwith)
    Range: File "", line 56, characters 4-12
    Body Range: File "", line 56, characters 34-73
    Content: |unresolved|
    references:
      File "", line 92, characters 27-35 ,
      File "", line 96, characters 27-35 ,
      File "", line 115, characters 27-35 ,
      File "", line 217, characters 79-87 ,
      File "", line 219, characters 103-111 ,
      File "", line 222, characters 83-91 ,
      File "", line 247, characters 49-57 ,
      File "", line 248, characters 72-80 ,
      File "", line 249, characters 87-95 ,
      File "", line 268, characters 66-74 ,
      File "", line 269, characters 96-104 ,
      File "", line 270, characters 111-119
    (assert#247:4-10 -> assert)
    Range: File "", line 247, characters 4-10
    Body Range: File "", line 247, characters 0-76
    Content: |core: bool -> unit|
    references: []
    (assert_some#248:4-15 -> assert_some)
    Range: File "", line 248, characters 4-15
    Body Range: File "", line 248, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none#249:4-15 -> assert_none)
    Range: File "", line 249, characters 4-15
    Body Range: File "", line 249, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (abs#250:4-7 -> abs)
    Range: File "", line 250, characters 4-7
    Body Range: File "", line 250, characters 0-62
    Content: |core: int -> nat|
    references: File "", line 430, characters 31-34
    (is_nat#251:4-10 -> is_nat)
    Range: File "", line 251, characters 4-10
    Body Range: File "", line 251, characters 0-81
    Content: |core: int -> option (nat)|
    references: []
    (true#252:14-18 -> true)
    Range: File "", line 252, characters 14-18
    Body Range: File "", line 252, characters 28-32
    Content: |core: bool|
    references:
      File "", line 371, characters 88-92 ,
      File "", line 376, characters 68-72
    (false#253:14-19 -> false)
    Range: File "", line 253, characters 14-19
    Body Range: File "", line 253, characters 29-34
    Content: |core: bool|
    references:
      File "", line 327, characters 51-56 ,
      File "", line 372, characters 90-95 ,
      File "", line 375, characters 62-67
    (unit#254:14-18 -> unit)
    Range: File "", line 254, characters 14-18
    Body Range: File "", line 254, characters 28-48
    Content: |core: unit|
    references: []
    (int#256:4-7 -> int)
    Range: File "", line 256, characters 4-7
    Body Range: File "", line 256, characters 44-96
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 323, characters 97-100 ,
      File "", line 360, characters 79-82 ,
      File "", line 362, characters 78-81 ,
      File "", line 364, characters 72-75
    (nat#261:4-7 -> nat)
    Range: File "", line 261, characters 4-7
    Body Range: File "", line 261, characters 0-73
    Content: |core: bytes -> nat|
    references: []
    (bytes#262:4-9 -> bytes)
    Range: File "", line 262, characters 4-9
    Body Range: File "", line 262, characters 48-104
    Content: |core: ∀ a : * . a -> external_bytes (a)|
    references: []
    (ignore#264:4-10 -> ignore)
    Range: File "", line 264, characters 4-10
    Body Range: File "", line 264, characters 37-39
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (curry#265:4-9 -> curry)
    Range: File "", line 265, characters 4-9
    Body Range: File "", line 265, characters 62-70
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    (uncurry#266:4-11 -> uncurry)
    Range: File "", line 266, characters 4-11
    Body Range: File "", line 266, characters 62-73
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 436, characters 30-37
    (assert_with_error#268:4-21 -> assert_with_error)
    Range: File "", line 268, characters 4-21
    Body Range: File "", line 268, characters 0-76
    Content: |unresolved|
    references: []
    (assert_some_with_error#269:4-26 -> assert_some_with_error)
    Range: File "", line 269, characters 4-26
    Body Range: File "", line 269, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_none_with_error#270:4-26 -> assert_none_with_error)
    Range: File "", line 270, characters 4-26
    Body Range: File "", line 270, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (ediv#271:4-8 -> ediv)
    Range: File "", line 271, characters 4-8
    Body Range: File "", line 271, characters 61-117
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
    (string#3:5-11 -> string)
    Range: File "", line 3, characters 5-11
    Body Range: File "", line 3, characters 14-32
    Content: : |string|
    references:
      File "", line 82, characters 44-50 ,
      File "", line 84, characters 48-54 ,
      File "", line 85, characters 48-54 ,
      File "", line 94, characters 58-64 ,
      File "", line 100, characters 51-57 ,
      File "", line 102, characters 62-68 ,
      File "", line 110, characters 58-64 ,
      File "", line 112, characters 65-71 ,
      File "", line 113, characters 54-60 ,
      File "", line 116, characters 44-50 ,
      File "", line 118, characters 61-67 ,
      File "", line 209, characters 18-24 ,
      File "", line 210, characters 20-26 ,
      File "", line 210, characters 35-41 ,
      File "", line 212, characters 19-25 ,
      File "", line 212, characters 33-39 ,
      File "", line 212, characters 43-49 ,
      File "", line 213, characters 35-41 ,
      File "", line 213, characters 45-51 ,
      File "", line 219, characters 52-58 ,
      File "", line 268, characters 38-44 ,
      File "", line 269, characters 56-62 ,
      File "", line 270, characters 56-62 ,
      File "", line 288, characters 13-19 ,
      File "", line 316, characters 17-23 ,
      File "", line 317, characters 18-24 ,
      File "", line 323, characters 56-62 ,
      File "", line 329, characters 31-37 ,
      File "", line 335, characters 50-56 ,
      File "", line 337, characters 41-47 ,
      File "", line 338, characters 27-33 ,
      File "", line 342, characters 35-41 ,
      File "", line 343, characters 33-39 ,
      File "", line 356, characters 36-42 ,
      File "", line 357, characters 22-28 ,
      File "", line 368, characters 19-25 ,
      File "", line 392, characters 76-82 ,
      File "", line 400, characters 140-146 ,
      File "", line 401, characters 135-141 ,
      File "", line 410, characters 25-31 ,
      File "", line 410, characters 50-56 ,
      File "", line 411, characters 17-23 ,
      File "", line 412, characters 23-29 ,
      File "", line 413, characters 25-31 ,
      File "", line 417, characters 12-18 ,
      File "", line 422, characters 14-20 ,
      File "", line 426, characters 38-44 ,
      File "", line 459, characters 39-45 ,
      File "", line 459, characters 52-58 ,
      File "", line 459, characters 65-71 ,
      File "", line 462, characters 32-38 ,
      File "", line 462, characters 45-51 ,
      File "", line 462, characters 58-64 ,
      File "", line 491, characters 52-58 ,
      File "", line 491, characters 65-71 ,
      File "", line 491, characters 78-84 ,
      File "", line 511, characters 56-62 ,
      File "", line 511, characters 69-75 ,
      File "", line 511, characters 82-88 ,
      File "", line 580, characters 40-46 ,
      File "", line 581, characters 58-64 ,
      File "", line 582, characters 58-64
    (bytes#4:5-10 -> bytes)
    Range: File "", line 4, characters 5-10
    Body Range: File "", line 4, characters 13-30
    Content: : |bytes|
    references:
      File "", line 119, characters 121-126 ,
      File "", line 119, characters 219-224 ,
      File "", line 228, characters 20-25 ,
      File "", line 228, characters 34-39 ,
      File "", line 229, characters 30-35 ,
      File "", line 229, characters 70-75 ,
      File "", line 230, characters 27-32 ,
      File "", line 231, characters 18-23 ,
      File "", line 233, characters 19-24 ,
      File "", line 233, characters 32-37 ,
      File "", line 233, characters 41-46 ,
      File "", line 234, characters 35-40 ,
      File "", line 234, characters 44-49 ,
      File "", line 238, characters 19-24 ,
      File "", line 238, characters 28-33 ,
      File "", line 238, characters 71-76 ,
      File "", line 239, characters 18-23 ,
      File "", line 239, characters 27-32 ,
      File "", line 239, characters 69-74 ,
      File "", line 240, characters 18-23 ,
      File "", line 240, characters 27-32 ,
      File "", line 240, characters 69-74 ,
      File "", line 241, characters 16-21 ,
      File "", line 241, characters 25-30 ,
      File "", line 241, characters 65-70 ,
      File "", line 242, characters 18-23 ,
      File "", line 242, characters 27-32 ,
      File "", line 242, characters 69-74 ,
      File "", line 244, characters 43-48 ,
      File "", line 261, characters 13-18 ,
      File "", line 261, characters 57-62 ,
      File "", line 411, characters 30-35
    (int#5:5-8 -> int)
    Range: File "", line 5, characters 5-8
    Body Range: File "", line 5, characters 11-26
    Content: : |int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 14-17 ,
      File "", line 119, characters 130-133 ,
      File "", line 119, characters 228-231 ,
      File "", line 250, characters 13-16 ,
      File "", line 251, characters 16-19 ,
      File "", line 293, characters 16-19 ,
      File "", line 320, characters 33-36 ,
      File "", line 351, characters 38-41 ,
      File "", line 414, characters 34-37 ,
      File "", line 435, characters 127-130 ,
      File "", line 445, characters 136-139 ,
      File "", line 452, characters 134-137 ,
      File "", line 462, characters 139-142 ,
      File "", line 492, characters 78-81 ,
      File "", line 512, characters 82-85 ,
      File "", line 532, characters 90-93 ,
      File "", line 554, characters 94-97
    (nat#6:5-8 -> nat)
    Range: File "", line 6, characters 5-8
    Body Range: File "", line 6, characters 11-26
    Content: : |nat|
    references:
      File "", line 68, characters 30-33 ,
      File "", line 68, characters 67-70 ,
      File "", line 71, characters 43-46 ,
      File "", line 71, characters 93-96 ,
      File "", line 72, characters 39-42 ,
      File "", line 72, characters 85-88 ,
      File "", line 73, characters 37-40 ,
      File "", line 73, characters 84-87 ,
      File "", line 77, characters 60-63 ,
      File "", line 78, characters 66-69 ,
      File "", line 97, characters 42-45 ,
      File "", line 97, characters 114-117 ,
      File "", line 103, characters 48-51 ,
      File "", line 103, characters 54-57 ,
      File "", line 147, characters 41-44 ,
      File "", line 171, characters 34-37 ,
      File "", line 172, characters 38-41 ,
      File "", line 187, characters 38-41 ,
      File "", line 188, characters 36-39 ,
      File "", line 209, characters 28-31 ,
      File "", line 213, characters 15-18 ,
      File "", line 213, characters 25-28 ,
      File "", line 231, characters 27-30 ,
      File "", line 234, characters 15-18 ,
      File "", line 234, characters 25-28 ,
      File "", line 250, characters 20-23 ,
      File "", line 250, characters 57-60 ,
      File "", line 251, characters 23-26 ,
      File "", line 251, characters 69-72 ,
      File "", line 261, characters 22-25 ,
      File "", line 261, characters 66-69 ,
      File "", line 290, characters 35-38 ,
      File "", line 300, characters 86-89 ,
      File "", line 310, characters 43-46 ,
      File "", line 318, characters 41-44 ,
      File "", line 319, characters 34-37 ,
      File "", line 323, characters 33-36 ,
      File "", line 324, characters 50-53 ,
      File "", line 331, characters 34-37 ,
      File "", line 357, characters 15-18 ,
      File "", line 378, characters 48-51 ,
      File "", line 379, characters 31-34 ,
      File "", line 401, characters 69-72 ,
      File "", line 406, characters 23-26 ,
      File "", line 407, characters 40-43 ,
      File "", line 409, characters 33-36 ,
      File "", line 420, characters 77-80 ,
      File "", line 460, characters 96-99 ,
      File "", line 470, characters 30-33 ,
      File "", line 482, characters 58-61 ,
      File "", line 498, characters 96-99 ,
      File "", line 501, characters 30-33 ,
      File "", line 518, characters 96-99 ,
      File "", line 521, characters 58-61 ,
      File "", line 543, characters 30-33 ,
      File "", line 565, characters 58-61 ,
      File "", line 587, characters 26-29 ,
      File "", line 600, characters 28-31 ,
      File "", line 609, characters 35-38 ,
      File "", line 612, characters 33-36 ,
      File "", line 620, characters 29-32 ,
      File "", line 625, characters 28-31 ,
      File "", line 628, characters 35-38 ,
      File "", line 640, characters 28-31 ,
      File "", line 643, characters 35-38
    (unit#7:5-9 -> unit)
    Range: File "", line 7, characters 5-9
    Body Range: File "", line 7, characters 12-28
    Content: : |unit|
    references:
      File "", line 58, characters 12-24 ,
      File "", line 59, characters 17-34 ,
      File "", line 63, characters 24-28 ,
      File "", line 64, characters 23-27 ,
      File "", line 65, characters 20-24 ,
      File "", line 66, characters 23-27 ,
      File "", line 67, characters 23-27 ,
      File "", line 68, characters 22-26 ,
      File "", line 69, characters 29-33 ,
      File "", line 70, characters 25-29 ,
      File "", line 71, characters 35-39 ,
      File "", line 72, characters 31-35 ,
      File "", line 75, characters 41-45 ,
      File "", line 75, characters 102-106 ,
      File "", line 101, characters 12-16 ,
      File "", line 111, characters 12-16 ,
      File "", line 117, characters 12-16 ,
      File "", line 157, characters 36-40 ,
      File "", line 157, characters 61-65 ,
      File "", line 179, characters 30-34 ,
      File "", line 179, characters 50-54 ,
      File "", line 193, characters 30-34 ,
      File "", line 193, characters 51-55 ,
      File "", line 247, characters 24-28 ,
      File "", line 248, characters 42-46 ,
      File "", line 249, characters 42-46 ,
      File "", line 254, characters 21-25 ,
      File "", line 264, characters 30-34 ,
      File "", line 269, characters 66-70 ,
      File "", line 270, characters 66-70 ,
      File "", line 298, characters 21-41 ,
      File "", line 310, characters 35-39 ,
      File "", line 313, characters 33-37 ,
      File "", line 316, characters 27-31 ,
      File "", line 317, characters 28-32 ,
      File "", line 325, characters 29-33 ,
      File "", line 326, characters 28-32 ,
      File "", line 329, characters 23-27 ,
      File "", line 331, characters 41-45 ,
      File "", line 332, characters 21-25 ,
      File "", line 334, characters 42-46 ,
      File "", line 339, characters 27-31 ,
      File "", line 339, characters 35-39 ,
      File "", line 340, characters 24-28 ,
      File "", line 340, characters 32-36 ,
      File "", line 341, characters 24-28 ,
      File "", line 341, characters 32-36 ,
      File "", line 349, characters 50-54 ,
      File "", line 350, characters 32-36 ,
      File "", line 368, characters 29-33 ,
      File "", line 371, characters 28-32 ,
      File "", line 371, characters 36-40 ,
      File "", line 372, characters 30-34 ,
      File "", line 372, characters 38-42 ,
      File "", line 402, characters 29-33 ,
      File "", line 406, characters 45-49 ,
      File "", line 407, characters 62-66 ,
      File "", line 408, characters 90-94 ,
      File "", line 412, characters 43-47 ,
      File "", line 413, characters 58-62 ,
      File "", line 414, characters 62-66 ,
      File "", line 429, characters 20-22 ,
      File "", line 468, characters 31-35 ,
      File "", line 468, characters 47-51 ,
      File "", line 469, characters 20-62 ,
      File "", line 472, characters 44-48 ,
      File "", line 472, characters 98-102 ,
      File "", line 480, characters 31-35 ,
      File "", line 480, characters 47-51 ,
      File "", line 481, characters 20-62 ,
      File "", line 484, characters 44-48 ,
      File "", line 484, characters 98-102 ,
      File "", line 499, characters 31-35 ,
      File "", line 499, characters 47-51 ,
      File "", line 500, characters 20-62 ,
      File "", line 504, characters 44-48 ,
      File "", line 504, characters 103-107 ,
      File "", line 519, characters 31-35 ,
      File "", line 519, characters 47-51 ,
      File "", line 520, characters 20-62 ,
      File "", line 524, characters 44-48 ,
      File "", line 524, characters 103-107 ,
      File "", line 541, characters 31-35 ,
      File "", line 541, characters 47-51 ,
      File "", line 542, characters 20-62 ,
      File "", line 546, characters 44-48 ,
      File "", line 546, characters 103-107 ,
      File "", line 563, characters 31-35 ,
      File "", line 563, characters 47-51 ,
      File "", line 564, characters 20-62 ,
      File "", line 568, characters 44-48 ,
      File "", line 568, characters 103-107 ,
      File "", line 576, characters 26-30 ,
      File "", line 577, characters 44-48 ,
      File "", line 578, characters 44-48 ,
      File "", line 581, characters 68-72 ,
      File "", line 582, characters 68-72 ,
      File "", line 588, characters 20-24 ,
      File "", line 589, characters 27-31 ,
      File "", line 609, characters 52-56 ,
      File "", line 612, characters 51-55 ,
      File "", line 612, characters 76-80
    (operation#10:5-14 -> operation)
    Range: File "", line 10, characters 5-14
    Body Range: File "", line 10, characters 17-38
    Content: : |operation|
    references:
      File "", line 81, characters 43-52 ,
      File "", line 81, characters 95-104 ,
      File "", line 98, characters 65-74 ,
      File "", line 99, characters 87-96 ,
      File "", line 105, characters 67-76 ,
      File "", line 105, characters 131-140 ,
      File "", line 108, characters 76-85 ,
      File "", line 108, characters 140-149 ,
      File "", line 116, characters 62-71 ,
      File "", line 118, characters 91-100 ,
      File "", line 302, characters 44-53 ,
      File "", line 352, characters 48-57 ,
      File "", line 408, characters 50-59 ,
      File "", line 435, characters 42-51 ,
      File "", line 442, characters 61-70 ,
      File "", line 445, characters 51-60 ,
      File "", line 589, characters 10-19 ,
      File "", line 599, characters 39-48 ,
      File "", line 602, characters 10-19 ,
      File "", line 612, characters 59-68 ,
      File "", line 627, characters 39-48 ,
      File "", line 628, characters 60-69 ,
      File "", line 642, characters 40-49 ,
      File "", line 643, characters 60-69
    (tez#11:5-8 -> tez)
    Range: File "", line 11, characters 5-8
    Body Range: File "", line 11, characters 11-26
    Content: : |tez|
    references:
      File "", line 63, characters 32-35 ,
      File "", line 63, characters 71-74 ,
      File "", line 64, characters 31-34 ,
      File "", line 64, characters 69-72 ,
      File "", line 98, characters 41-44 ,
      File "", line 99, characters 67-70 ,
      File "", line 105, characters 115-118 ,
      File "", line 108, characters 124-127 ,
      File "", line 283, characters 52-55 ,
      File "", line 283, characters 74-77 ,
      File "", line 315, characters 34-37 ,
      File "", line 400, characters 58-61 ,
      File "", line 401, characters 62-65 ,
      File "", line 406, characters 33-36 ,
      File "", line 407, characters 50-53 ,
      File "", line 408, characters 83-86 ,
      File "", line 413, characters 44-47 ,
      File "", line 415, characters 66-69 ,
      File "", line 420, characters 70-73 ,
      File "", line 434, characters 79-82 ,
      File "", line 435, characters 75-78 ,
      File "", line 445, characters 84-87 ,
      File "", line 452, characters 82-85 ,
      File "", line 462, characters 101-104 ,
      File "", line 491, characters 120-123 ,
      File "", line 511, characters 124-127 ,
      File "", line 531, characters 95-98 ,
      File "", line 553, characters 99-102
    (address#12:5-12 -> address)
    Range: File "", line 12, characters 5-12
    Body Range: File "", line 12, characters 15-34
    Content: : |address|
    references:
      File "", line 66, characters 31-38 ,
      File "", line 66, characters 73-80 ,
      File "", line 67, characters 31-38 ,
      File "", line 67, characters 73-80 ,
      File "", line 69, characters 37-44 ,
      File "", line 69, characters 85-92 ,
      File "", line 74, characters 42-49 ,
      File "", line 74, characters 87-94 ,
      File "", line 77, characters 45-52 ,
      File "", line 78, characters 51-58 ,
      File "", line 88, characters 56-63 ,
      File "", line 90, characters 52-59 ,
      File "", line 94, characters 44-51 ,
      File "", line 100, characters 72-79 ,
      File "", line 105, characters 143-150 ,
      File "", line 108, characters 152-159 ,
      File "", line 110, characters 71-78 ,
      File "", line 113, characters 67-74 ,
      File "", line 283, characters 23-30 ,
      File "", line 286, characters 36-43 ,
      File "", line 294, characters 18-25 ,
      File "", line 295, characters 17-24 ,
      File "", line 300, characters 54-61 ,
      File "", line 313, characters 22-29 ,
      File "", line 314, characters 34-41 ,
      File "", line 315, characters 23-30 ,
      File "", line 319, characters 41-48 ,
      File "", line 320, characters 40-47 ,
      File "", line 323, characters 40-47 ,
      File "", line 325, characters 38-45 ,
      File "", line 325, characters 47-54 ,
      File "", line 333, characters 35-42 ,
      File "", line 346, characters 12-19 ,
      File "", line 350, characters 21-28 ,
      File "", line 394, characters 21-28 ,
      File "", line 395, characters 45-52 ,
      File "", line 400, characters 20-27 ,
      File "", line 401, characters 24-31 ,
      File "", line 416, characters 12-19 ,
      File "", line 421, characters 14-21 ,
      File "", line 434, characters 86-93 ,
      File "", line 462, characters 108-115 ,
      File "", line 492, characters 47-54 ,
      File "", line 512, characters 51-58 ,
      File "", line 587, characters 33-40 ,
      File "", line 601, characters 22-29 ,
      File "", line 602, characters 27-34 ,
      File "", line 609, characters 42-49 ,
      File "", line 612, characters 40-47 ,
      File "", line 620, characters 36-43 ,
      File "", line 627, characters 67-74 ,
      File "", line 628, characters 42-49 ,
      File "", line 628, characters 77-84 ,
      File "", line 631, characters 68-75 ,
      File "", line 642, characters 68-75 ,
      File "", line 643, characters 42-49 ,
      File "", line 643, characters 77-84 ,
      File "", line 646, characters 68-75
    (signature#13:5-14 -> signature)
    Range: File "", line 13, characters 5-14
    Body Range: File "", line 13, characters 17-38
    Content: : |signature|
    references:
      File "", line 244, characters 27-36 ,
      File "", line 411, characters 39-48
    (key#14:5-8 -> key)
    Range: File "", line 14, characters 5-8
    Body Range: File "", line 14, characters 11-26
    Content: : |key|
    references:
      File "", line 243, characters 20-23 ,
      File "", line 244, characters 17-20 ,
      File "", line 323, characters 50-53 ,
      File "", line 329, characters 40-43 ,
      File "", line 412, characters 36-39 ,
      File "", line 413, characters 34-37
    (key_hash#15:5-13 -> key_hash)
    Range: File "", line 15, characters 5-13
    Body Range: File "", line 15, characters 16-36
    Content: : |key_hash|
    references:
      File "", line 73, characters 25-33 ,
      File "", line 75, characters 29-37 ,
      File "", line 81, characters 24-32 ,
      File "", line 105, characters 93-101 ,
      File "", line 108, characters 102-110 ,
      File "", line 243, characters 27-35 ,
      File "", line 243, characters 74-82 ,
      File "", line 318, characters 29-37 ,
      File "", line 334, characters 30-38 ,
      File "", line 606, characters 54-62
    (timestamp#16:5-14 -> timestamp)
    Range: File "", line 16, characters 5-14
    Body Range: File "", line 16, characters 17-38
    Content: : |timestamp|
    references:
      File "", line 65, characters 28-37 ,
      File "", line 65, characters 69-78 ,
      File "", line 332, characters 29-38 ,
      File "", line 406, characters 92-101 ,
      File "", line 407, characters 24-33
    (list#17:5-9 -> list)
    Range: File "", line 17, characters 5-9
    Body Range: File "", line 17, characters 12-28
    Content: : |list|
    references:
      File "", line 80, characters 55-59 ,
      File "", line 105, characters 77-81 ,
      File "", line 108, characters 86-90 ,
      File "", line 133, characters 57-61 ,
      File "", line 148, characters 57-61 ,
      File "", line 173, characters 49-53 ,
      File "", line 187, characters 30-34 ,
      File "", line 188, characters 28-32 ,
      File "", line 189, characters 32-36 ,
      File "", line 190, characters 32-36 ,
      File "", line 190, characters 43-47 ,
      File "", line 192, characters 42-46 ,
      File "", line 192, characters 52-56 ,
      File "", line 193, characters 44-48 ,
      File "", line 194, characters 47-51 ,
      File "", line 195, characters 60-64 ,
      File "", line 196, characters 53-57 ,
      File "", line 197, characters 36-40 ,
      File "", line 197, characters 46-50 ,
      File "", line 198, characters 48-52 ,
      File "", line 200, characters 56-60 ,
      File "", line 200, characters 66-70 ,
      File "", line 201, characters 31-35 ,
      File "", line 202, characters 50-54 ,
      File "", line 202, characters 60-64 ,
      File "", line 204, characters 59-63 ,
      File "", line 204, characters 69-73 ,
      File "", line 210, characters 27-31 ,
      File "", line 228, characters 26-30 ,
      File "", line 295, characters 25-29 ,
      File "", line 302, characters 54-58 ,
      File "", line 325, characters 55-59 ,
      File "", line 352, characters 58-62 ,
      File "", line 392, characters 88-92 ,
      File "", line 394, characters 34-38 ,
      File "", line 395, characters 37-41 ,
      File "", line 395, characters 63-67 ,
      File "", line 399, characters 33-37 ,
      File "", line 406, characters 37-41 ,
      File "", line 407, characters 54-58 ,
      File "", line 408, characters 60-64 ,
      File "", line 435, characters 52-56 ,
      File "", line 442, characters 71-75 ,
      File "", line 445, characters 61-65 ,
      File "", line 459, characters 72-76 ,
      File "", line 462, characters 65-69 ,
      File "", line 479, characters 78-82 ,
      File "", line 482, characters 47-51 ,
      File "", line 482, characters 80-84 ,
      File "", line 490, characters 38-42 ,
      File "", line 491, characters 85-89 ,
      File "", line 511, characters 89-93 ,
      File "", line 512, characters 109-113 ,
      File "", line 521, characters 47-51 ,
      File "", line 521, characters 80-84 ,
      File "", line 530, characters 38-42 ,
      File "", line 554, characters 121-125 ,
      File "", line 565, characters 47-51 ,
      File "", line 565, characters 80-84 ,
      File "", line 574, characters 38-42 ,
      File "", line 589, characters 20-24 ,
      File "", line 599, characters 49-53 ,
      File "", line 602, characters 20-24 ,
      File "", line 612, characters 69-73 ,
      File "", line 627, characters 49-53 ,
      File "", line 628, characters 70-74 ,
      File "", line 642, characters 50-54 ,
      File "", line 643, characters 70-74
    (big_map#18:5-12 -> big_map)
    Range: File "", line 18, characters 5-12
    Body Range: File "", line 18, characters 15-34
    Content: : |big_map|
    references:
      File "", line 132, characters 42-49 ,
      File "", line 133, characters 72-79 ,
      File "", line 135, characters 41-48 ,
      File "", line 136, characters 49-56 ,
      File "", line 136, characters 67-74 ,
      File "", line 137, characters 44-51 ,
      File "", line 137, characters 62-69 ,
      File "", line 138, characters 59-66 ,
      File "", line 138, characters 77-84 ,
      File "", line 139, characters 67-74 ,
      File "", line 139, characters 96-103 ,
      File "", line 140, characters 46-53 ,
      File "", line 141, characters 42-49 ,
      File "", line 414, characters 51-58
    (map#19:5-8 -> map)
    Range: File "", line 19, characters 5-8
    Body Range: File "", line 19, characters 11-26
    Content: : |map|
    references:
      File "", line 146, characters 32-35 ,
      File "", line 147, characters 34-37 ,
      File "", line 148, characters 72-75 ,
      File "", line 150, characters 41-44 ,
      File "", line 151, characters 49-52 ,
      File "", line 151, characters 63-66 ,
      File "", line 152, characters 44-47 ,
      File "", line 152, characters 58-61 ,
      File "", line 153, characters 59-62 ,
      File "", line 153, characters 73-76 ,
      File "", line 154, characters 67-70 ,
      File "", line 154, characters 92-95 ,
      File "", line 155, characters 42-45 ,
      File "", line 156, characters 46-49 ,
      File "", line 157, characters 54-57 ,
      File "", line 158, characters 52-55 ,
      File "", line 158, characters 66-69 ,
      File "", line 159, characters 59-62 ,
      File "", line 325, characters 61-64
    (set#20:5-8 -> set)
    Range: File "", line 20, characters 5-8
    Body Range: File "", line 20, characters 11-26
    Content: : |set|
    references:
      File "", line 170, characters 25-28 ,
      File "", line 171, characters 27-30 ,
      File "", line 172, characters 31-34 ,
      File "", line 173, characters 59-62 ,
      File "", line 175, characters 34-37 ,
      File "", line 176, characters 34-37 ,
      File "", line 176, characters 43-46 ,
      File "", line 177, characters 37-40 ,
      File "", line 177, characters 46-49 ,
      File "", line 178, characters 48-51 ,
      File "", line 179, characters 43-46 ,
      File "", line 180, characters 46-49 ,
      File "", line 181, characters 51-54 ,
      File "", line 182, characters 56-59 ,
      File "", line 182, characters 65-68 ,
      File "", line 183, characters 30-33 ,
      File "", line 183, characters 106-109
    (contract#21:5-13 -> contract)
    Range: File "", line 21, characters 5-13
    Body Range: File "", line 21, characters 16-36
    Content: : |contract|
    references:
      File "", line 74, characters 30-38 ,
      File "", line 75, characters 46-54 ,
      File "", line 75, characters 107-115 ,
      File "", line 82, characters 56-64 ,
      File "", line 84, characters 60-68 ,
      File "", line 88, characters 70-78 ,
      File "", line 89, characters 74-82 ,
      File "", line 90, characters 66-74 ,
      File "", line 94, characters 70-78 ,
      File "", line 98, characters 53-61 ,
      File "", line 99, characters 75-83 ,
      File "", line 110, characters 84-92 ,
      File "", line 112, characters 98-106 ,
      File "", line 113, characters 80-88 ,
      File "", line 312, characters 60-68 ,
      File "", line 336, characters 41-49 ,
      File "", line 345, characters 14-22 ,
      File "", line 415, characters 43-51 ,
      File "", line 420, characters 47-55 ,
      File "", line 426, characters 77-85 ,
      File "", line 593, characters 22-30
    (michelson_or#22:5-17 -> michelson_or)
    Range: File "", line 22, characters 5-17
    Body Range: File "", line 22, characters 20-44
    Content: : |michelson_or|
    references: []
    (michelson_pair#23:5-19 -> michelson_pair)
    Range: File "", line 23, characters 5-19
    Body Range: File "", line 23, characters 22-48
    Content: : |michelson_pair|
    references: []
    (chain_id#24:5-13 -> chain_id)
    Range: File "", line 24, characters 5-13
    Body Range: File "", line 24, characters 16-36
    Content: : |chain_id|
    references:
      File "", line 70, characters 33-41 ,
      File "", line 70, characters 78-86
    (baker_hash#25:5-15 -> baker_hash)
    Range: File "", line 25, characters 5-15
    Body Range: File "", line 25, characters 18-40
    Content: : |baker_hash|
    references: []
    (pvss_key#26:5-13 -> pvss_key)
    Range: File "", line 26, characters 5-13
    Body Range: File "", line 26, characters 16-36
    Content: : |pvss_key|
    references: []
    (sapling_state#27:5-18 -> sapling_state)
    Range: File "", line 27, characters 5-18
    Body Range: File "", line 27, characters 21-46
    Content: : |sapling_state|
    references:
      File "", line 86, characters 66-79 ,
      File "", line 87, characters 90-103 ,
      File "", line 119, characters 103-116 ,
      File "", line 119, characters 142-155 ,
      File "", line 119, characters 240-253
    (sapling_transaction#28:5-24 -> sapling_transaction)
    Range: File "", line 28, characters 5-24
    Body Range: File "", line 28, characters 27-58
    Content: : |sapling_transaction|
    references: File "", line 119, characters 71-90
    (baker_operation#29:5-20 -> baker_operation)
    Range: File "", line 29, characters 5-20
    Body Range: File "", line 29, characters 23-50
    Content: : |baker_operation|
    references: []
    (bls12_381_g1#30:5-17 -> bls12_381_g1)
    Range: File "", line 30, characters 5-17
    Body Range: File "", line 30, characters 20-44
    Content: : |bls12_381_g1|
    references: File "", line 80, characters 26-38
    (bls12_381_g2#31:5-17 -> bls12_381_g2)
    Range: File "", line 31, characters 5-17
    Body Range: File "", line 31, characters 20-44
    Content: : |bls12_381_g2|
    references: File "", line 80, characters 41-53
    (bls12_381_fr#32:5-17 -> bls12_381_fr)
    Range: File "", line 32, characters 5-17
    Body Range: File "", line 32, characters 20-44
    Content: : |bls12_381_fr|
    references: []
    (never#33:5-10 -> never)
    Range: File "", line 33, characters 5-10
    Body Range: File "", line 33, characters 13-30
    Content: : |never|
    references: File "", line 79, characters 26-31
    (ticket#34:5-11 -> ticket)
    Range: File "", line 34, characters 5-11
    Body Range: File "", line 34, characters 14-32
    Content: : |ticket|
    references:
      File "", line 76, characters 35-41 ,
      File "", line 76, characters 46-52 ,
      File "", line 76, characters 59-65 ,
      File "", line 76, characters 118-124 ,
      File "", line 77, characters 34-40 ,
      File "", line 77, characters 70-76 ,
      File "", line 78, characters 76-82 ,
      File "", line 97, characters 52-58 ,
      File "", line 97, characters 124-130 ,
      File "", line 103, characters 35-41 ,
      File "", line 103, characters 64-70 ,
      File "", line 103, characters 75-81 ,
      File "", line 104, characters 49-55 ,
      File "", line 104, characters 60-66 ,
      File "", line 586, characters 23-29 ,
      File "", line 598, characters 25-31 ,
      File "", line 611, characters 54-60 ,
      File "", line 626, characters 26-32 ,
      File "", line 641, characters 26-32
    (external_bytes#37:5-19 -> external_bytes)
    Range: File "", line 37, characters 5-19
    Body Range: File "", line 37, characters 22-48
    Content: : |external_bytes|
    references:
      File "", line 262, characters 31-45 ,
      File "", line 262, characters 86-100
    (external_int#38:5-17 -> external_int)
    Range: File "", line 38, characters 5-17
    Body Range: File "", line 38, characters 20-44
    Content: : |external_int|
    references:
      File "", line 256, characters 29-41 ,
      File "", line 256, characters 80-92
    (external_int_lima#39:5-22 -> external_int_lima)
    Range: File "", line 39, characters 5-22
    Body Range: File "", line 39, characters 25-54
    Content: : |external_int_lima|
    references: []
    (external_ediv#40:5-18 -> external_ediv)
    Range: File "", line 40, characters 5-18
    Body Range: File "", line 40, characters 21-46
    Content: : |external_ediv|
    references:
      File "", line 271, characters 45-58 ,
      File "", line 271, characters 102-115
    (external_and#41:5-17 -> external_and)
    Range: File "", line 41, characters 5-17
    Body Range: File "", line 41, characters 20-44
    Content: : |external_and|
    references:
      File "", line 124, characters 54-66 ,
      File "", line 124, characters 123-135
    (external_or#42:5-16 -> external_or)
    Range: File "", line 42, characters 5-16
    Body Range: File "", line 42, characters 19-42
    Content: : |external_or|
    references:
      File "", line 125, characters 54-65 ,
      File "", line 125, characters 123-134
    (external_xor#43:5-17 -> external_xor)
    Range: File "", line 43, characters 5-17
    Body Range: File "", line 43, characters 20-44
    Content: : |external_xor|
    references:
      File "", line 126, characters 54-66 ,
      File "", line 126, characters 123-135
    (external_lsl#44:5-17 -> external_lsl)
    Range: File "", line 44, characters 5-17
    Body Range: File "", line 44, characters 20-44
    Content: : |external_lsl|
    references:
      File "", line 127, characters 54-66 ,
      File "", line 127, characters 123-135
    (external_lsr#45:5-17 -> external_lsr)
    Range: File "", line 45, characters 5-17
    Body Range: File "", line 45, characters 20-44
    Content: : |external_lsr|
    references:
      File "", line 128, characters 54-66 ,
      File "", line 128, characters 123-135
    (external_map_find_opt#46:5-26 -> external_map_find_opt)
    Range: File "", line 46, characters 5-26
    Body Range: File "", line 46, characters 29-62
    Content: : |external_map_find_opt|
    references:
      File "", line 164, characters 55-76 ,
      File "", line 164, characters 119-140
    (external_map_add#47:5-21 -> external_map_add)
    Range: File "", line 47, characters 5-21
    Body Range: File "", line 47, characters 24-52
    Content: : |external_map_add|
    references:
      File "", line 165, characters 63-79 ,
      File "", line 165, characters 145-161
    (external_map_remove#48:5-24 -> external_map_remove)
    Range: File "", line 48, characters 5-24
    Body Range: File "", line 48, characters 27-58
    Content: : |external_map_remove|
    references:
      File "", line 166, characters 53-72 ,
      File "", line 166, characters 197-216
    (external_map_remove_value#49:5-30 -> external_map_remove_value)
    Range: File "", line 49, characters 5-30
    Body Range: File "", line 49, characters 33-70
    Content: : |external_map_remove_value|
    references: File "", line 166, characters 149-174
    (bool#58:5-9 -> bool)
    Range: File "", line 58, characters 5-9
    Body Range: File "", line 58, characters 12-24
    Content: : |sum[False -> unit , True -> unit]|
    references:
      File "", line 80, characters 63-67 ,
      File "", line 80, characters 111-115 ,
      File "", line 135, characters 52-56 ,
      File "", line 150, characters 48-52 ,
      File "", line 175, characters 41-45 ,
      File "", line 178, characters 35-39 ,
      File "", line 198, characters 34-38 ,
      File "", line 204, characters 37-41 ,
      File "", line 223, characters 40-44 ,
      File "", line 224, characters 40-44 ,
      File "", line 244, characters 52-56 ,
      File "", line 244, characters 106-110 ,
      File "", line 247, characters 16-20 ,
      File "", line 252, characters 21-25 ,
      File "", line 253, characters 22-26 ,
      File "", line 268, characters 27-31 ,
      File "", line 297, characters 41-45 ,
      File "", line 377, characters 53-57 ,
      File "", line 425, characters 74-78 ,
      File "", line 576, characters 18-22 ,
      File "", line 580, characters 29-33
    (option#59:8-14 -> option)
    Range: File "", line 59, characters 8-14
    Body Range: File "", line 59, characters 17-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 76, characters 67-73 ,
      File "", line 76, characters 125-131 ,
      File "", line 81, characters 33-39 ,
      File "", line 83, characters 14-20 ,
      File "", line 87, characters 74-80 ,
      File "", line 88, characters 80-86 ,
      File "", line 89, characters 59-65 ,
      File "", line 89, characters 84-90 ,
      File "", line 97, characters 60-66 ,
      File "", line 97, characters 132-138 ,
      File "", line 100, characters 86-92 ,
      File "", line 102, characters 80-86 ,
      File "", line 102, characters 96-102 ,
      File "", line 103, characters 83-89 ,
      File "", line 104, characters 68-74 ,
      File "", line 105, characters 102-108 ,
      File "", line 108, characters 111-117 ,
      File "", line 110, characters 93-99 ,
      File "", line 112, characters 83-89 ,
      File "", line 112, characters 108-114 ,
      File "", line 118, characters 79-85 ,
      File "", line 119, characters 158-164 ,
      File "", line 119, characters 256-262 ,
      File "", line 138, characters 39-45 ,
      File "", line 139, characters 47-53 ,
      File "", line 139, characters 80-86 ,
      File "", line 140, characters 59-65 ,
      File "", line 153, characters 39-45 ,
      File "", line 154, characters 47-53 ,
      File "", line 154, characters 76-82 ,
      File "", line 156, characters 55-61 ,
      File "", line 166, characters 176-182 ,
      File "", line 182, characters 40-46 ,
      File "", line 189, characters 42-48 ,
      File "", line 190, characters 49-55 ,
      File "", line 198, characters 58-64 ,
      File "", line 199, characters 31-37 ,
      File "", line 200, characters 40-46 ,
      File "", line 202, characters 34-40 ,
      File "", line 217, characters 28-34 ,
      File "", line 219, characters 39-45 ,
      File "", line 220, characters 50-56 ,
      File "", line 220, characters 62-68 ,
      File "", line 221, characters 42-48 ,
      File "", line 222, characters 48-54 ,
      File "", line 223, characters 30-36 ,
      File "", line 224, characters 30-36 ,
      File "", line 230, characters 38-44 ,
      File "", line 230, characters 100-106 ,
      File "", line 230, characters 114-120 ,
      File "", line 248, characters 32-38 ,
      File "", line 249, characters 32-38 ,
      File "", line 251, characters 27-33 ,
      File "", line 251, characters 73-79 ,
      File "", line 269, characters 43-49 ,
      File "", line 270, characters 43-49 ,
      File "", line 357, characters 29-35 ,
      File "", line 400, characters 147-153 ,
      File "", line 401, characters 142-148 ,
      File "", line 406, characters 102-108 ,
      File "", line 409, characters 63-69 ,
      File "", line 410, characters 57-63 ,
      File "", line 413, characters 48-54 ,
      File "", line 417, characters 19-25 ,
      File "", line 422, characters 21-27 ,
      File "", line 460, characters 100-106 ,
      File "", line 467, characters 74-80 ,
      File "", line 470, characters 52-58 ,
      File "", line 492, characters 105-111 ,
      File "", line 498, characters 100-106 ,
      File "", line 501, characters 52-58 ,
      File "", line 518, characters 100-106 ,
      File "", line 532, characters 117-123 ,
      File "", line 543, characters 52-58 ,
      File "", line 577, characters 34-40 ,
      File "", line 578, characters 34-40 ,
      File "", line 581, characters 45-51 ,
      File "", line 582, characters 45-51 ,
      File "", line 601, characters 30-36 ,
      File "", line 602, characters 35-41 ,
      File "", line 606, characters 63-69 ,
      File "", line 628, characters 50-56 ,
      File "", line 628, characters 85-91 ,
      File "", line 631, characters 76-82 ,
      File "", line 643, characters 50-56 ,
      File "", line 643, characters 85-91 ,
      File "", line 646, characters 76-82
    (michelson_program#273:5-22 -> michelson_program)
    Range: File "", line 273, characters 5-22
    Body Range: File "", line 273, characters 25-54
    Content: : |michelson_program|
    references:
      File "", line 286, characters 16-33 ,
      File "", line 306, characters 44-61 ,
      File "", line 307, characters 30-47 ,
      File "", line 309, characters 39-56 ,
      File "", line 314, characters 45-62 ,
      File "", line 330, characters 30-47 ,
      File "", line 335, characters 29-46 ,
      File "", line 337, characters 51-68 ,
      File "", line 338, characters 37-54 ,
      File "", line 347, characters 12-29 ,
      File "", line 400, characters 34-51 ,
      File "", line 401, characters 38-55 ,
      File "", line 418, characters 12-29 ,
      File "", line 423, characters 14-31 ,
      File "", line 425, characters 28-45 ,
      File "", line 425, characters 53-70 ,
      File "", line 434, characters 55-72 ,
      File "", line 462, characters 76-93 ,
      File "", line 491, characters 96-113 ,
      File "", line 511, characters 100-117
    (typed_address#274:5-18 -> typed_address)
    Range: File "", line 274, characters 5-18
    Body Range: File "", line 274, characters 21-46
    Content: : |typed_address|
    references:
      File "", line 312, characters 41-54 ,
      File "", line 324, characters 64-77 ,
      File "", line 333, characters 53-66 ,
      File "", line 336, characters 60-73 ,
      File "", line 344, characters 41-54 ,
      File "", line 392, characters 54-67 ,
      File "", line 426, characters 58-71 ,
      File "", line 435, characters 90-103 ,
      File "", line 440, characters 19-32 ,
      File "", line 445, characters 99-112 ,
      File "", line 450, characters 19-32 ,
      File "", line 452, characters 97-110 ,
      File "", line 457, characters 19-32 ,
      File "", line 532, characters 51-64 ,
      File "", line 538, characters 21-34 ,
      File "", line 554, characters 55-68 ,
      File "", line 560, characters 21-34 ,
      File "", line 609, characters 58-71 ,
      File "", line 635, characters 55-68 ,
      File "", line 650, characters 55-68
    (mutation#275:5-13 -> mutation)
    Range: File "", line 275, characters 5-13
    Body Range: File "", line 275, characters 16-36
    Content: : |mutation|
    references:
      File "", line 409, characters 53-61 ,
      File "", line 410, characters 38-46 ,
      File "", line 467, characters 64-72 ,
      File "", line 469, characters 35-43 ,
      File "", line 470, characters 42-50 ,
      File "", line 479, characters 68-76 ,
      File "", line 481, characters 35-43 ,
      File "", line 482, characters 37-45 ,
      File "", line 482, characters 70-78 ,
      File "", line 490, characters 28-36 ,
      File "", line 492, characters 95-103 ,
      File "", line 500, characters 35-43 ,
      File "", line 501, characters 42-50 ,
      File "", line 512, characters 99-107 ,
      File "", line 520, characters 35-43 ,
      File "", line 521, characters 37-45 ,
      File "", line 521, characters 70-78 ,
      File "", line 530, characters 28-36 ,
      File "", line 532, characters 107-115 ,
      File "", line 542, characters 35-43 ,
      File "", line 543, characters 42-50 ,
      File "", line 554, characters 111-119 ,
      File "", line 564, characters 35-43 ,
      File "", line 565, characters 37-45 ,
      File "", line 565, characters 70-78 ,
      File "", line 574, characters 28-36
    (michelson_contract#276:5-23 -> michelson_contract)
    Range: File "", line 276, characters 5-23
    Body Range: File "", line 276, characters 26-56
    Content: : |michelson_contract|
    references:
      File "", line 351, characters 16-34 ,
      File "", line 352, characters 70-88 ,
      File "", line 356, characters 46-64 ,
      File "", line 434, characters 30-48 ,
      File "", line 435, characters 106-124 ,
      File "", line 442, characters 98-116 ,
      File "", line 445, characters 115-133 ,
      File "", line 452, characters 113-131 ,
      File "", line 459, characters 80-98 ,
      File "", line 462, characters 118-136 ,
      File "", line 492, characters 57-75 ,
      File "", line 512, characters 61-79 ,
      File "", line 532, characters 68-86 ,
      File "", line 554, characters 72-90
    (ast_contract#277:5-17 -> ast_contract)
    Range: File "", line 277, characters 5-17
    Body Range: File "", line 277, characters 20-44
    Content: : |ast_contract|
    references:
      File "", line 354, characters 16-28 ,
      File "", line 443, characters 18-30 ,
      File "", line 460, characters 16-28 ,
      File "", line 493, characters 25-37 ,
      File "", line 498, characters 16-28 ,
      File "", line 513, characters 25-37 ,
      File "", line 518, characters 16-28 ,
      File "", line 534, characters 25-37 ,
      File "", line 540, characters 16-28 ,
      File "", line 556, characters 25-37 ,
      File "", line 562, characters 16-28
    (pbt_gen#278:5-12 -> pbt_gen)
    Range: File "", line 278, characters 5-12
    Body Range: File "", line 278, characters 15-34
    Content: : |pbt_gen|
    references:
      File "", line 297, characters 23-30 ,
      File "", line 327, characters 14-21 ,
      File "", line 375, characters 25-32 ,
      File "", line 376, characters 31-38 ,
      File "", line 377, characters 34-41
    (int64#279:5-10 -> int64)
    Range: File "", line 279, characters 5-10
    Body Range: File "", line 279, characters 13-30
    Content: : |int64|
    references: []
    (views#280:5-10 -> views)
    Range: File "", line 280, characters 5-10
    Body Range: File "", line 280, characters 13-30
    Content: : |views|
    references:
      File "", line 302, characters 70-75 ,
      File "", line 353, characters 18-23 ,
      File "", line 442, characters 89-94
    (test_exec_error_balance_too_low#282:5-36 -> test_exec_error_balance_too_low)
    Range: File "", line 282, characters 5-36
    Body Range: File "", line 283, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 287, characters 23-54
    (test_exec_error#285:5-20 -> test_exec_error)
    Range: File "", line 285, characters 5-20
    Body Range: File "", line 286, character 4 to line 288, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 290, characters 49-64
    (test_exec_result#290:5-21 -> test_exec_result)
    Range: File "", line 290, characters 5-21
    Body Range: File "", line 290, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 400, characters 65-81 ,
      File "", line 415, characters 73-89 ,
      File "", line 620, characters 47-63
    (test_baker_policy#292:5-22 -> test_baker_policy)
    Range: File "", line 292, characters 5-22
    Body Range: File "", line 293, character 4 to line 295, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 349, characters 29-46
    (pbt_test#297:8-16 -> pbt_test)
    Range: File "", line 297, characters 8-16
    Body Range: File "", line 297, characters 19-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 377, characters 63-71 ,
      File "", line 378, characters 33-41
    (pbt_result#298:8-18 -> pbt_result)
    Range: File "", line 298, characters 8-18
    Body Range: File "", line 298, characters 21-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 378, characters 57-67 ,
      File "", line 379, characters 39-49 ,
      File "", line 381, characters 84-94 ,
      File "", line 385, characters 96-106 ,
      File "", line 388, characters 68-78
    (unforged_ticket#300:8-23 -> unforged_ticket)
    Range: File "", line 300, characters 8-23
    Body Range: File "", line 300, characters 41-91
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    (module_contract#302:14-29 -> module_contract)
    Range: File "", line 302, characters 14-29
    Body Range: File "", line 302, characters 32-75
    Content: : |funtype 'p : * . funtype 's : * . ( ( 'p * 's ) -> ( list (operation) *
                                                                     's ) *
                                                    views ('s) )|
    references:
      File "", line 452, characters 52-67 ,
      File "", line 531, characters 65-80 ,
      File "", line 553, characters 69-84
    Module definitions:
    (Tezos#61:7-12 -> Tezos)
    Range: File "", line 61, characters 7-12
    Body Range: File "", line 63, character 2 to line 119, character 264
    Content: Members: Variable definitions:
                      (get_balance#63:6-17 -> get_balance)
                      Range: File "", line 63, characters 6-17
                      Body Range: File "", line 63, characters 2-76
                      Content: |core: unit -> tez|
                      references: []
                      (get_amount#64:6-16 -> get_amount)
                      Range: File "", line 64, characters 6-16
                      Body Range: File "", line 64, characters 2-74
                      Content: |core: unit -> tez|
                      references: []
                      (get_now#65:6-13 -> get_now)
                      Range: File "", line 65, characters 6-13
                      Body Range: File "", line 65, characters 2-80
                      Content: |core: unit -> timestamp|
                      references: File "", line 332, characters 47-54
                      (get_sender#66:6-16 -> get_sender)
                      Range: File "", line 66, characters 6-16
                      Body Range: File "", line 66, characters 2-82
                      Content: |core: unit -> address|
                      references: []
                      (get_source#67:6-16 -> get_source)
                      Range: File "", line 67, characters 6-16
                      Body Range: File "", line 67, characters 2-82
                      Content: |core: unit -> address|
                      references: []
                      (get_level#68:6-15 -> get_level)
                      Range: File "", line 68, characters 6-15
                      Body Range: File "", line 68, characters 2-72
                      Content: |core: unit -> nat|
                      references: []
                      (get_self_address#69:6-22 -> get_self_address)
                      Range: File "", line 69, characters 6-22
                      Body Range: File "", line 69, characters 2-94
                      Content: |core: unit -> address|
                      references: []
                      (get_chain_id#70:6-18 -> get_chain_id)
                      Range: File "", line 70, characters 6-18
                      Body Range: File "", line 70, characters 2-88
                      Content: |core: unit -> chain_id|
                      references: []
                      (get_total_voting_power#71:6-28 -> get_total_voting_power)
                      Range: File "", line 71, characters 6-28
                      Body Range: File "", line 71, characters 2-98
                      Content: |core: unit -> nat|
                      references: []
                      (get_min_block_time#72:6-24 -> get_min_block_time)
                      Range: File "", line 72, characters 6-24
                      Body Range: File "", line 72, characters 2-90
                      Content: |core: unit -> nat|
                      references: []
                      (voting_power#73:6-18 -> voting_power)
                      Range: File "", line 73, characters 6-18
                      Body Range: File "", line 73, characters 2-89
                      Content: |core: key_hash -> nat|
                      references: []
                      (address#74:6-13 -> address)
                      Range: File "", line 74, characters 6-13
                      Body Range: File "", line 74, characters 52-96
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 393, characters 21-28
                      (implicit_account#75:6-22 -> implicit_account)
                      Range: File "", line 75, characters 6-22
                      Body Range: File "", line 75, characters 2-117
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      (join_tickets#76:6-18 -> join_tickets)
                      Range: File "", line 76, characters 6-18
                      Body Range: File "", line 76, characters 76-133
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      (read_ticket#77:6-17 -> read_ticket)
                      Range: File "", line 77, characters 6-17
                      Body Range: File "", line 78, characters 4-84
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address *
                                                                    ( a * nat ) ) *
                                                                  ticket (a) )|
                      references: []
                      (never#79:6-11 -> never)
                      Range: File "", line 79, characters 6-11
                      Body Range: File "", line 79, characters 39-75
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      (pairing_check#80:6-19 -> pairing_check)
                      Range: File "", line 80, characters 6-19
                      Body Range: File "", line 80, characters 2-117
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      (set_delegate#81:6-18 -> set_delegate)
                      Range: File "", line 81, characters 6-18
                      Body Range: File "", line 81, characters 2-106
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      (self#82:25-29 -> self)
                      Range: File "", line 82, characters 25-29
                      Body Range: File "", line 83, character 4 to line 84, character 70
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      (constant#85:25-33 -> constant)
                      Range: File "", line 85, characters 25-33
                      Body Range: File "", line 85, characters 62-96
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (sapling_empty_state#86:25-44 -> sapling_empty_state)
                      Range: File "", line 86, characters 25-44
                      Body Range: File "", line 87, characters 4-105
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      (get_contract_opt#88:25-41 -> get_contract_opt)
                      Range: File "", line 88, characters 25-41
                      Body Range: File "", line 89, characters 4-92
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 91, characters 12-28 ,
                        File "", line 95, characters 12-28
                      (get_contract#90:25-37 -> get_contract)
                      Range: File "", line 90, characters 25-37
                      Body Range: File "", line 91, character 4 to line 92, character 68
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      (get_contract_with_error#94:6-29 -> get_contract_with_error)
                      Range: File "", line 94, characters 6-29
                      Body Range: File "", line 95, character 4 to line 96, character 39
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: File "", line 593, characters 39-62
                      (create_ticket#97:6-19 -> create_ticket)
                      Range: File "", line 97, characters 6-19
                      Body Range: File "", line 97, characters 69-147
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references:
                        File "", line 591, characters 39-52 ,
                        File "", line 604, characters 39-52
                      (transaction#98:6-17 -> transaction)
                      Range: File "", line 98, characters 6-17
                      Body Range: File "", line 99, characters 4-109
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: File "", line 594, characters 21-32
                      (call_view#100:25-34 -> call_view)
                      Range: File "", line 100, characters 25-34
                      Body Range: File "", line 101, character 4 to line 102, character 104
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (split_ticket#103:6-18 -> split_ticket)
                      Range: File "", line 103, characters 6-18
                      Body Range: File "", line 104, characters 4-76
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      (create_contract#105:25-40 -> create_contract)
                      Range: File "", line 105, characters 25-40
                      Body Range: File "", line 106, character 6 to line 107, character 58
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: File "", line 606, characters 26-41
                      (create_contract_uncurried#108:25-50 -> create_contract_uncurried)
                      Range: File "", line 108, characters 25-50
                      Body Range: File "", line 109, characters 6-50
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      (get_entrypoint_opt#110:25-43 -> get_entrypoint_opt)
                      Range: File "", line 110, characters 25-43
                      Body Range: File "", line 111, character 4 to line 112, character 116
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 114, characters 12-30
                      (get_entrypoint#113:25-39 -> get_entrypoint)
                      Range: File "", line 113, characters 25-39
                      Body Range: File "", line 114, character 4 to line 115, character 70
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (emit#116:25-29 -> emit)
                      Range: File "", line 116, characters 25-29
                      Body Range: File "", line 117, character 4 to line 118, character 102
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      (sapling_verify_update#119:25-46 -> sapling_verify_update)
                      Range: File "", line 119, characters 25-46
                      Body Range: File "", line 119, characters 167-264
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes *
                        ( int * sapling_state (sap_a) ) ))|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 332, characters 41-46 ,
      File "", line 393, characters 15-20 ,
      File "", line 591, characters 33-38 ,
      File "", line 593, characters 33-38 ,
      File "", line 594, characters 15-20 ,
      File "", line 604, characters 33-38 ,
      File "", line 606, characters 20-25

    (Bitwise#123:7-14 -> Bitwise)
    Range: File "", line 123, characters 7-14
    Body Range: File "", line 124, character 2 to line 128, character 144
    Content: Members: Variable definitions:
                      (and#124:6-10 -> and)
                      Range: File "", line 124, characters 6-10
                      Body Range: File "", line 124, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (xor#125:6-9 -> xor)
                      Range: File "", line 125, characters 6-9
                      Body Range: File "", line 125, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_or (a ,
                      b)|
                      references: []
                      (or#126:6-9 -> or)
                      Range: File "", line 126, characters 6-9
                      Body Range: File "", line 126, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_xor (a ,
                      b)|
                      references: []
                      (shift_left#127:6-16 -> shift_left)
                      Range: File "", line 127, characters 6-16
                      Body Range: File "", line 127, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsl (a ,
                      b)|
                      references: []
                      (shift_right#128:6-17 -> shift_right)
                      Range: File "", line 128, characters 6-17
                      Body Range: File "", line 128, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsr (a ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Big_map#131:7-14 -> Big_map)
    Range: File "", line 131, characters 7-14
    Body Range: File "", line 132, character 12 to line 141, character 87
    Content: Members: Variable definitions:
                      (empty#132:16-21 -> empty)
                      Range: File "", line 132, characters 16-21
                      Body Range: File "", line 132, characters 52-81
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (literal#133:25-32 -> literal)
                      Range: File "", line 133, characters 25-32
                      Body Range: File "", line 133, characters 82-116
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#135:6-9 -> mem)
                      Range: File "", line 135, characters 6-9
                      Body Range: File "", line 135, characters 59-88
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (add#136:6-9 -> add)
                      Range: File "", line 136, characters 6-9
                      Body Range: File "", line 136, characters 77-109
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (remove#137:6-12 -> remove)
                      Range: File "", line 137, characters 6-12
                      Body Range: File "", line 137, characters 72-104
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#138:6-12 -> update)
                      Range: File "", line 138, characters 6-12
                      Body Range: File "", line 138, characters 87-122
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (get_and_update#139:6-20 -> get_and_update)
                      Range: File "", line 139, characters 6-20
                      Body Range: File "", line 139, characters 106-153
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (find_opt#140:6-14 -> find_opt)
                      Range: File "", line 140, characters 6-14
                      Body Range: File "", line 140, characters 68-102
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (find#141:6-10 -> find)
                      Range: File "", line 141, characters 6-10
                      Body Range: File "", line 141, characters 57-87
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Map#145:7-10 -> Map)
    Range: File "", line 145, characters 7-10
    Body Range: File "", line 146, character 2 to line 159, character 111
    Content: Members: Variable definitions:
                      (empty#146:6-11 -> empty)
                      Range: File "", line 146, characters 6-11
                      Body Range: File "", line 146, characters 38-63
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (size#147:6-10 -> size)
                      Range: File "", line 147, characters 6-10
                      Body Range: File "", line 147, characters 47-74
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (literal#148:25-32 -> literal)
                      Range: File "", line 148, characters 25-32
                      Body Range: File "", line 148, characters 78-108
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (mem#150:6-9 -> mem)
                      Range: File "", line 150, characters 6-9
                      Body Range: File "", line 150, characters 55-84
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (add#151:6-9 -> add)
                      Range: File "", line 151, characters 6-9
                      Body Range: File "", line 151, characters 69-101
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (remove#152:6-12 -> remove)
                      Range: File "", line 152, characters 6-12
                      Body Range: File "", line 152, characters 64-96
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (update#153:6-12 -> update)
                      Range: File "", line 153, characters 6-12
                      Body Range: File "", line 153, characters 79-114
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (get_and_update#154:6-20 -> get_and_update)
                      Range: File "", line 154, characters 6-20
                      Body Range: File "", line 154, characters 98-141
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (find#155:6-10 -> find)
                      Range: File "", line 155, characters 6-10
                      Body Range: File "", line 155, characters 53-83
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#156:6-14 -> find_opt)
                      Range: File "", line 156, characters 6-14
                      Body Range: File "", line 156, characters 64-98
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (iter#157:6-10 -> iter)
                      Range: File "", line 157, characters 6-10
                      Body Range: File "", line 157, characters 68-98
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (map#158:6-9 -> map)
                      Range: File "", line 158, characters 6-9
                      Body Range: File "", line 158, characters 72-101
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (fold#159:6-10 -> fold)
                      Range: File "", line 159, characters 6-10
                      Body Range: File "", line 159, characters 78-111
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Transpiled#163:7-17 -> Transpiled)
    Range: File "", line 163, characters 7-17
    Body Range: File "", line 164, character 2 to line 166, character 218
    Content: Members: Variable definitions:
                      (map_find_opt#164:6-18 -> map_find_opt)
                      Range: File "", line 164, characters 6-18
                      Body Range: File "", line 164, characters 79-142
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      (map_add#165:6-13 -> map_add)
                      Range: File "", line 165, characters 6-13
                      Body Range: File "", line 165, characters 82-163
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      (map_remove#166:6-16 -> map_remove)
                      Range: File "", line 166, characters 6-16
                      Body Range: File "", line 166, characters 75-218
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#169:7-10 -> Set)
    Range: File "", line 169, characters 7-10
    Body Range: File "", line 170, character 2 to line 183, character 110
    Content: Members: Variable definitions:
                      (empty#170:6-11 -> empty)
                      Range: File "", line 170, characters 6-11
                      Body Range: File "", line 170, characters 31-56
                      Content: |core: ∀ a : * . set (a)|
                      references: File "", line 183, characters 96-101
                      (size#171:6-10 -> size)
                      Range: File "", line 171, characters 6-10
                      Body Range: File "", line 171, characters 40-67
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (cardinal#172:6-14 -> cardinal)
                      Range: File "", line 172, characters 6-14
                      Body Range: File "", line 172, characters 44-71
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (literal#173:25-32 -> literal)
                      Range: File "", line 173, characters 25-32
                      Body Range: File "", line 173, characters 65-95
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#175:6-9 -> mem)
                      Range: File "", line 175, characters 6-9
                      Body Range: File "", line 175, characters 48-77
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (add#176:6-9 -> add)
                      Range: File "", line 176, characters 6-9
                      Body Range: File "", line 176, characters 49-78
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: File "", line 183, characters 81-84
                      (remove#177:6-12 -> remove)
                      Range: File "", line 177, characters 6-12
                      Body Range: File "", line 177, characters 52-84
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (update#178:6-12 -> update)
                      Range: File "", line 178, characters 6-12
                      Body Range: File "", line 178, characters 55-90
                      Content: |unresolved|
                      references: []
                      (iter#179:6-10 -> iter)
                      Range: File "", line 179, characters 6-10
                      Body Range: File "", line 179, characters 57-87
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (fold#180:6-10 -> fold)
                      Range: File "", line 180, characters 6-10
                      Body Range: File "", line 180, characters 65-98
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#181:6-15 -> fold_desc)
                      Range: File "", line 181, characters 6-15
                      Body Range: File "", line 181, characters 70-108
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: File "", line 183, characters 4-13
                      (filter_map#182:6-16 -> filter_map)
                      Range: File "", line 182, characters 6-16
                      Body Range: File "", line 183, characters 4-110
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> set (a) -> set (b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#186:7-11 -> List)
    Range: File "", line 186, characters 7-11
    Body Range: File "", line 187, character 2 to line 205, character 48
    Content: Members: Variable definitions:
                      (length#187:6-12 -> length)
                      Range: File "", line 187, characters 6-12
                      Body Range: File "", line 187, characters 44-73
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (size#188:6-10 -> size)
                      Range: File "", line 188, characters 6-10
                      Body Range: File "", line 188, characters 42-71
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (head_opt#189:6-14 -> head_opt)
                      Range: File "", line 189, characters 6-14
                      Body Range: File "", line 189, characters 51-98
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (tail_opt#190:6-14 -> tail_opt)
                      Range: File "", line 190, characters 6-14
                      Body Range: File "", line 190, characters 58-107
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      (map#192:6-9 -> map)
                      Range: File "", line 192, characters 6-9
                      Body Range: File "", line 192, characters 59-90
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 203, characters 4-7 ,
                        File "", line 205, characters 4-7
                      (iter#193:6-10 -> iter)
                      Range: File "", line 193, characters 6-10
                      Body Range: File "", line 193, characters 58-90
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (fold#194:6-10 -> fold)
                      Range: File "", line 194, characters 6-10
                      Body Range: File "", line 194, characters 67-102
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 399, characters 9-13
                      (fold_left#195:6-15 -> fold_left)
                      Range: File "", line 195, characters 6-15
                      Body Range: File "", line 195, characters 72-112
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#196:6-16 -> fold_right)
                      Range: File "", line 196, characters 6-16
                      Body Range: File "", line 196, characters 73-114
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 199, characters 4-14 ,
                        File "", line 201, characters 4-14
                      (cons#197:6-10 -> cons)
                      Range: File "", line 197, characters 6-10
                      Body Range: File "", line 197, characters 53-80
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (find_opt#198:6-14 -> find_opt)
                      Range: File "", line 198, characters 6-14
                      Body Range: File "", line 199, characters 4-82
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (filter_map#200:6-16 -> filter_map)
                      Range: File "", line 200, characters 6-16
                      Body Range: File "", line 201, characters 4-100
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      (update#202:6-12 -> update)
                      Range: File "", line 202, characters 6-12
                      Body Range: File "", line 203, characters 4-62
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      (update_with#204:6-17 -> update_with)
                      Range: File "", line 204, characters 6-17
                      Body Range: File "", line 205, characters 4-48
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 399, characters 4-8

    (String#208:7-13 -> String)
    Range: File "", line 208, characters 7-13
    Body Range: File "", line 209, character 2 to line 213, character 84
    Content: Members: Variable definitions:
                      (length#209:6-12 -> length)
                      Range: File "", line 209, characters 6-12
                      Body Range: File "", line 209, characters 2-57
                      Content: |core: string -> nat|
                      references:
                        File "", line 427, characters 22-28 ,
                        File "", line 430, characters 43-49
                      (concats#210:6-13 -> concats)
                      Range: File "", line 210, characters 6-13
                      Body Range: File "", line 210, characters 2-71
                      Content: |core: list (string) -> string|
                      references: []
                      (concat#212:6-12 -> concat)
                      Range: File "", line 212, characters 6-12
                      Body Range: File "", line 212, characters 2-82
                      Content: |core: string -> string -> string|
                      references: []
                      (sub#213:6-9 -> sub)
                      Range: File "", line 213, characters 6-9
                      Body Range: File "", line 213, characters 2-84
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 428, characters 24-27 ,
                        File "", line 430, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 427, characters 15-21 ,
      File "", line 428, characters 17-23 ,
      File "", line 430, characters 16-22 ,
      File "", line 430, characters 36-42

    (Option#216:7-13 -> Option)
    Range: File "", line 216, characters 7-13
    Body Range: File "", line 217, character 2 to line 224, character 77
    Content: Members: Variable definitions:
                      (unopt#217:6-11 -> unopt)
                      Range: File "", line 217, characters 6-11
                      Body Range: File "", line 217, characters 42-104
                      Content: |core: ∀ a : * . option (a) -> a|
                      references:
                        File "", line 591, characters 26-31 ,
                        File "", line 604, characters 26-31
                      (unopt_with_error#219:6-22 -> unopt_with_error)
                      Range: File "", line 219, characters 6-22
                      Body Range: File "", line 219, characters 66-113
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (map#220:15-18 -> map)
                      Range: File "", line 220, characters 15-18
                      Body Range: File "", line 220, characters 71-103
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (value#221:6-11 -> value)
                      Range: File "", line 221, characters 6-11
                      Body Range: File "", line 221, characters 56-100
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#222:6-15 -> value_exn)
                      Range: File "", line 222, characters 6-15
                      Body Range: File "", line 222, characters 62-109
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      (is_none#223:6-13 -> is_none)
                      Range: File "", line 223, characters 6-13
                      Body Range: File "", line 223, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#224:6-13 -> is_some)
                      Range: File "", line 224, characters 6-13
                      Body Range: File "", line 224, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 591, characters 19-25 ,
      File "", line 604, characters 19-25

    (Bytes#227:7-12 -> Bytes)
    Range: File "", line 227, characters 7-12
    Body Range: File "", line 228, character 2 to line 234, character 82
    Content: Members: Variable definitions:
                      (concats#228:6-13 -> concats)
                      Range: File "", line 228, characters 6-13
                      Body Range: File "", line 228, characters 2-69
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (pack#229:6-10 -> pack)
                      Range: File "", line 229, characters 6-10
                      Body Range: File "", line 229, characters 38-77
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (unpack#230:6-12 -> unpack)
                      Range: File "", line 230, characters 6-12
                      Body Range: File "", line 230, characters 47-122
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      (length#231:6-12 -> length)
                      Range: File "", line 231, characters 6-12
                      Body Range: File "", line 231, characters 2-56
                      Content: |core: bytes -> nat|
                      references: []
                      (concat#233:6-12 -> concat)
                      Range: File "", line 233, characters 6-12
                      Body Range: File "", line 233, characters 2-79
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (sub#234:6-9 -> sub)
                      Range: File "", line 234, characters 6-9
                      Body Range: File "", line 234, characters 2-82
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#237:7-13 -> Crypto)
    Range: File "", line 237, characters 7-13
    Body Range: File "", line 238, character 2 to line 244, character 112
    Content: Members: Variable definitions:
                      (blake2b#238:6-13 -> blake2b)
                      Range: File "", line 238, characters 6-13
                      Body Range: File "", line 238, characters 2-78
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#239:6-12 -> sha256)
                      Range: File "", line 239, characters 6-12
                      Body Range: File "", line 239, characters 2-76
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#240:6-12 -> sha512)
                      Range: File "", line 240, characters 6-12
                      Body Range: File "", line 240, characters 2-76
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#241:6-10 -> sha3)
                      Range: File "", line 241, characters 6-10
                      Body Range: File "", line 241, characters 2-72
                      Content: |core: bytes -> bytes|
                      references: []
                      (keccak#242:6-12 -> keccak)
                      Range: File "", line 242, characters 6-12
                      Body Range: File "", line 242, characters 2-76
                      Content: |core: bytes -> bytes|
                      references: []
                      (hash_key#243:6-14 -> hash_key)
                      Range: File "", line 243, characters 6-14
                      Body Range: File "", line 243, characters 2-84
                      Content: |core: key -> key_hash|
                      references: []
                      (check#244:6-11 -> check)
                      Range: File "", line 244, characters 6-11
                      Body Range: File "", line 244, characters 2-112
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Test#304:7-11 -> Test)
    Range: File "", line 304, characters 7-11
    Body Range: File "", line 306, character 2 to line 653, character 5
    Content: Members: Variable definitions:
                      (run#306:6-9 -> run)
                      Range: File "", line 306, characters 6-9
                      Body Range: File "", line 306, characters 64-94
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 307, characters 50-53
                      (eval#307:6-10 -> eval)
                      Range: File "", line 307, characters 6-10
                      Body Range: File "", line 307, characters 50-74
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 309, characters 59-63 ,
                        File "", line 418, characters 32-36 ,
                        File "", line 423, characters 34-38 ,
                        File "", line 437, characters 12-16 ,
                        File "", line 447, characters 12-16 ,
                        File "", line 454, characters 12-16 ,
                        File "", line 533, characters 12-16 ,
                        File "", line 555, characters 12-16
                      (compile_value#309:6-19 -> compile_value)
                      Range: File "", line 309, characters 6-19
                      Body Range: File "", line 309, characters 59-65
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (get_total_voting_power#310:6-28 -> get_total_voting_power)
                      Range: File "", line 310, characters 6-28
                      Body Range: File "", line 310, characters 2-96
                      Content: |core: unit -> nat|
                      references: []
                      (failwith#311:6-14 -> failwith)
                      Range: File "", line 311, characters 6-14
                      Body Range: File "", line 311, characters 40-72
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 576, characters 51-59 ,
                        File "", line 577, characters 74-82 ,
                        File "", line 578, characters 89-97 ,
                        File "", line 580, characters 68-76 ,
                        File "", line 581, characters 98-106 ,
                        File "", line 582, characters 113-121 ,
                        File "", line 637, characters 16-24 ,
                        File "", line 652, characters 16-24
                      (to_contract#312:6-17 -> to_contract)
                      Range: File "", line 312, characters 6-17
                      Body Range: File "", line 312, characters 71-106
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 345, characters 25-36 ,
                        File "", line 393, characters 30-41 ,
                        File "", line 622, characters 28-39 ,
                        File "", line 632, characters 40-51 ,
                        File "", line 647, characters 40-51
                      (set_source#313:6-16 -> set_source)
                      Range: File "", line 313, characters 6-16
                      Body Range: File "", line 313, characters 2-74
                      Content: |core: address -> unit|
                      references: []
                      (get_storage_of_address#314:6-28 -> get_storage_of_address)
                      Range: File "", line 314, characters 6-28
                      Body Range: File "", line 314, characters 2-111
                      Content: |core: address -> michelson_program|
                      references: File "", line 347, characters 32-54
                      (get_balance#315:6-17 -> get_balance)
                      Range: File "", line 315, characters 6-17
                      Body Range: File "", line 315, characters 2-75
                      Content: |core: address -> tez|
                      references: []
                      (print#316:6-11 -> print)
                      Range: File "", line 316, characters 6-11
                      Body Range: File "", line 316, characters 2-66
                      Content: |core: string -> unit|
                      references:
                        File "", line 369, characters 4-9 ,
                        File "", line 405, characters 4-9
                      (eprint#317:6-12 -> eprint)
                      Range: File "", line 317, characters 6-12
                      Body Range: File "", line 317, characters 2-67
                      Content: |core: string -> unit|
                      references: []
                      (get_voting_power#318:6-22 -> get_voting_power)
                      Range: File "", line 318, characters 6-22
                      Body Range: File "", line 318, characters 2-88
                      Content: |core: key_hash -> nat|
                      references: []
                      (nth_bootstrap_contract#319:6-28 -> nth_bootstrap_contract)
                      Range: File "", line 319, characters 6-28
                      Body Range: File "", line 319, characters 2-97
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_account#320:6-27 -> nth_bootstrap_account)
                      Range: File "", line 320, characters 6-27
                      Body Range: File "", line 320, character 2 to line 322, character 5
                      Content: |core: int -> address|
                      references: []
                      (get_bootstrap_account#323:6-27 -> get_bootstrap_account)
                      Range: File "", line 323, characters 6-27
                      Body Range: File "", line 323, characters 2-105
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (nth_bootstrap_typed_address#324:6-33 -> nth_bootstrap_typed_address)
                      Range: File "", line 324, characters 6-33
                      Body Range: File "", line 324, characters 80-131
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (last_originations#325:6-23 -> last_originations)
                      Range: File "", line 325, characters 6-23
                      Body Range: File "", line 325, characters 2-108
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (random#326:6-12 -> random)
                      Range: File "", line 326, characters 6-12
                      Body Range: File "", line 327, character 4 to line 328, character 42
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (new_account#329:6-17 -> new_account)
                      Range: File "", line 329, characters 6-17
                      Body Range: File "", line 329, characters 2-81
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (decompile#330:6-15 -> decompile)
                      Range: File "", line 330, characters 6-15
                      Body Range: File "", line 330, characters 55-88
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 348, characters 5-14
                      (bake_until_n_cycle_end#331:6-28 -> bake_until_n_cycle_end)
                      Range: File "", line 331, characters 6-28
                      Body Range: File "", line 331, characters 2-94
                      Content: |core: nat -> unit|
                      references: []
                      (get_time#332:6-14 -> get_time)
                      Range: File "", line 332, characters 6-14
                      Body Range: File "", line 332, characters 2-57
                      Content: |core: unit -> timestamp|
                      references: []
                      (cast_address#333:6-18 -> cast_address)
                      Range: File "", line 333, characters 6-18
                      Body Range: File "", line 333, characters 69-105
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 440, characters 35-47 ,
                        File "", line 450, characters 35-47 ,
                        File "", line 457, characters 35-47 ,
                        File "", line 538, characters 37-49 ,
                        File "", line 560, characters 37-49 ,
                        File "", line 635, characters 22-34 ,
                        File "", line 650, characters 22-34
                      (register_delegate#334:6-23 -> register_delegate)
                      Range: File "", line 334, characters 6-23
                      Body Range: File "", line 334, characters 2-91
                      Content: |core: key_hash -> unit|
                      references: []
                      (register_constant#335:6-23 -> register_constant)
                      Range: File "", line 335, characters 6-23
                      Body Range: File "", line 335, characters 2-100
                      Content: |core: michelson_program -> string|
                      references: []
                      (to_typed_address#336:6-22 -> to_typed_address)
                      Range: File "", line 336, characters 6-22
                      Body Range: File "", line 336, characters 76-116
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (constant_to_michelson_program#337:6-35 -> constant_to_michelson_program)
                      Range: File "", line 337, characters 6-35
                      Body Range: File "", line 337, characters 2-116
                      Content: |core: string -> michelson_program|
                      references: []
                      (parse_michelson#338:6-21 -> parse_michelson)
                      Range: File "", line 338, characters 6-21
                      Body Range: File "", line 338, characters 2-102
                      Content: |core: string -> michelson_program|
                      references: []
                      (restore_context#339:6-21 -> restore_context)
                      Range: File "", line 339, characters 6-21
                      Body Range: File "", line 339, characters 2-77
                      Content: |core: unit -> unit|
                      references: []
                      (save_context#340:6-18 -> save_context)
                      Range: File "", line 340, characters 6-18
                      Body Range: File "", line 340, characters 2-75
                      Content: |core: unit -> unit|
                      references: []
                      (drop_context#341:6-18 -> drop_context)
                      Range: File "", line 341, characters 6-18
                      Body Range: File "", line 341, characters 2-75
                      Content: |core: unit -> unit|
                      references: []
                      (to_string#342:6-15 -> to_string)
                      Range: File "", line 342, characters 6-15
                      Body Range: File "", line 342, characters 44-80
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 360, characters 68-77 ,
                        File "", line 362, characters 67-76 ,
                        File "", line 364, characters 61-70 ,
                        File "", line 404, characters 12-21
                      (to_json#343:6-13 -> to_json)
                      Range: File "", line 343, characters 6-13
                      Body Range: File "", line 343, characters 42-78
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (get_storage#344:6-17 -> get_storage)
                      Range: File "", line 344, characters 6-17
                      Body Range: File "", line 345, character 4 to line 348, character 21
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references:
                        File "", line 633, characters 12-23 ,
                        File "", line 648, characters 12-23
                      (set_baker_policy#349:6-22 -> set_baker_policy)
                      Range: File "", line 349, characters 6-22
                      Body Range: File "", line 349, characters 2-91
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 350, characters 39-55
                      (set_baker#350:6-15 -> set_baker)
                      Range: File "", line 350, characters 6-15
                      Body Range: File "", line 350, characters 2-70
                      Content: |core: address -> unit|
                      references: []
                      (size#351:6-10 -> size)
                      Range: File "", line 351, characters 6-10
                      Body Range: File "", line 351, characters 2-72
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 439, characters 12-16 ,
                        File "", line 449, characters 12-16 ,
                        File "", line 456, characters 12-16 ,
                        File "", line 465, characters 12-16 ,
                        File "", line 496, characters 14-18 ,
                        File "", line 516, characters 14-18 ,
                        File "", line 537, characters 14-18 ,
                        File "", line 559, characters 14-18
                      (compile_contract#352:6-22 -> compile_contract)
                      Range: File "", line 352, characters 6-22
                      Body Range: File "", line 353, character 4 to line 355, character 52
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 436, characters 12-28 ,
                        File "", line 446, characters 12-28
                      (read_contract_from_file#356:6-29 -> read_contract_from_file)
                      Range: File "", line 356, characters 6-29
                      Body Range: File "", line 356, characters 2-115
                      Content: |core: string -> michelson_contract|
                      references: []
                      (chr#357:6-9 -> chr)
                      Range: File "", line 357, characters 6-9
                      Body Range: File "", line 357, character 2 to line 366, character 10
                      Content: |core: nat -> option (string)|
                      references: []
                      (nl#367:6-8 -> nl)
                      Range: File "", line 367, characters 6-8
                      Body Range: File "", line 367, characters 11-53
                      Content: |unresolved|
                      references: File "", line 369, characters 15-17
                      (println#368:6-13 -> println)
                      Range: File "", line 368, characters 6-13
                      Body Range: File "", line 368, character 2 to line 369, character 18
                      Content: |core: string -> unit|
                      references: []
                      (set_print_values#371:6-22 -> set_print_values)
                      Range: File "", line 371, characters 6-22
                      Body Range: File "", line 371, characters 2-100
                      Content: |core: unit -> unit|
                      references: []
                      (unset_print_values#372:6-24 -> unset_print_values)
                      Range: File "", line 372, characters 6-24
                      Body Range: File "", line 372, characters 2-103
                      Content: |core: unit -> unit|
                      references: []
                      (get_last_events_from#392:6-26 -> get_last_events_from)
                      Range: File "", line 392, characters 6-26
                      Body Range: File "", line 393, character 4 to line 399, character 38
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (transfer#400:6-14 -> transfer)
                      Range: File "", line 400, characters 6-14
                      Body Range: File "", line 400, characters 2-162
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#401:6-18 -> transfer_exn)
                      Range: File "", line 401, characters 6-18
                      Body Range: File "", line 401, characters 2-157
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (log#402:6-9 -> log)
                      Range: File "", line 402, characters 6-9
                      Body Range: File "", line 403, character 4 to line 405, character 11
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 429, characters 25-28
                      (reset_state#406:6-17 -> reset_state)
                      Range: File "", line 406, characters 6-17
                      Body Range: File "", line 406, characters 2-117
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#407:6-20 -> reset_state_at)
                      Range: File "", line 407, characters 6-20
                      Body Range: File "", line 407, characters 2-117
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (bootstrap_contract#408:6-24 -> bootstrap_contract)
                      Range: File "", line 408, characters 6-24
                      Body Range: File "", line 408, characters 97-145
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (mutate_value#409:6-18 -> mutate_value)
                      Range: File "", line 409, characters 6-18
                      Body Range: File "", line 409, characters 72-111
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 471, characters 23-35 ,
                        File "", line 483, characters 23-35
                      (save_mutation#410:6-19 -> save_mutation)
                      Range: File "", line 410, characters 6-19
                      Body Range: File "", line 410, characters 2-106
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (sign#411:6-10 -> sign)
                      Range: File "", line 411, characters 6-10
                      Body Range: File "", line 411, characters 2-83
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (add_account#412:6-17 -> add_account)
                      Range: File "", line 412, characters 6-17
                      Body Range: File "", line 412, characters 2-88
                      Content: |core: string -> key -> unit|
                      references: []
                      (baker_account#413:6-19 -> baker_account)
                      Range: File "", line 413, characters 6-19
                      Body Range: File "", line 413, characters 2-105
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (set_big_map#414:6-17 -> set_big_map)
                      Range: File "", line 414, characters 6-17
                      Body Range: File "", line 414, characters 69-107
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (transfer_to_contract#415:6-26 -> transfer_to_contract)
                      Range: File "", line 415, characters 6-26
                      Body Range: File "", line 416, character 4 to line 419, character 61
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: File "", line 622, characters 6-26
                      (transfer_to_contract_exn#420:6-30 -> transfer_to_contract_exn)
                      Range: File "", line 420, characters 6-30
                      Body Range: File "", line 421, character 6 to line 424, character 67
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references:
                        File "", line 632, characters 14-38 ,
                        File "", line 647, characters 14-38
                      (michelson_equal#425:6-21 -> michelson_equal)
                      Range: File "", line 425, characters 6-21
                      Body Range: File "", line 425, characters 2-88
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (to_entrypoint#426:6-19 -> to_entrypoint)
                      Range: File "", line 426, characters 6-19
                      Body Range: File "", line 427, character 4 to line 433, character 44
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (originate_contract#434:6-24 -> originate_contract)
                      Range: File "", line 434, characters 6-24
                      Body Range: File "", line 434, characters 2-135
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 438, characters 12-30 ,
                        File "", line 448, characters 12-30 ,
                        File "", line 455, characters 12-30 ,
                        File "", line 464, characters 12-30 ,
                        File "", line 495, characters 14-32 ,
                        File "", line 515, characters 14-32 ,
                        File "", line 536, characters 14-32 ,
                        File "", line 558, characters 14-32
                      (originate#435:6-15 -> originate)
                      Range: File "", line 435, characters 6-15
                      Body Range: File "", line 436, character 4 to line 441, character 13
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references:
                        File "", line 615, characters 32-41 ,
                        File "", line 631, characters 32-41 ,
                        File "", line 646, characters 32-41
                      (compile_contract_with_views#442:8-35 -> compile_contract_with_views)
                      Range: File "", line 442, characters 8-35
                      Body Range: File "", line 443, character 6 to line 444, character 54
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 453, characters 12-39
                      (originate_uncurried#445:6-25 -> originate_uncurried)
                      Range: File "", line 445, characters 6-25
                      Body Range: File "", line 446, character 4 to line 451, character 13
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (originate_module#452:6-22 -> originate_module)
                      Range: File "", line 452, characters 6-22
                      Body Range: File "", line 453, character 4 to line 458, character 13
                      Content: |core: ∀ p : * . ∀ s : * . module_contract (p ,
                      s) -> s -> tez -> ( typed_address (p ,
                                          s) *
                                          michelson_contract *
                                          int )|
                      references: []
                      (compile_contract_from_file#459:6-32 -> compile_contract_from_file)
                      Range: File "", line 459, characters 6-32
                      Body Range: File "", line 459, character 2 to line 461, character 52
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 463, characters 12-38
                      (originate_from_file#462:6-25 -> originate_from_file)
                      Range: File "", line 462, characters 6-25
                      Body Range: File "", line 462, character 2 to line 466, character 13
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (mutation_test#467:6-19 -> mutation_test)
                      Range: File "", line 467, characters 6-19
                      Body Range: File "", line 468, character 4 to line 478, character 19
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#479:6-23 -> mutation_test_all)
                      Range: File "", line 479, characters 6-23
                      Body Range: File "", line 480, character 4 to line 490, character 46
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (originate_from_file_and_mutate#491:6-36 -> originate_from_file_and_mutate)
                      Range: File "", line 491, characters 6-36
                      Body Range: File "", line 493, character 4 to line 510, character 19
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#511:6-40 -> originate_from_file_and_mutate_all)
                      Range: File "", line 511, characters 6-40
                      Body Range: File "", line 513, character 4 to line 530, character 46
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module_and_mutate#531:6-33 -> originate_module_and_mutate)
                      Range: File "", line 531, characters 6-33
                      Body Range: File "", line 533, character 4 to line 552, character 19
                      Content: |core: ∀ p : * . ∀ s : * . ∀ b : * . module_contract (p ,
                      s) -> s -> tez -> typed_address (p ,
                      s) -> michelson_contract -> int -> b -> option (( b *
                                                                        mutation ))|
                      references: []
                      (originate_module_and_mutate_all#553:6-37 -> originate_module_and_mutate_all)
                      Range: File "", line 553, characters 6-37
                      Body Range: File "", line 555, character 4 to line 574, character 46
                      Content: |core: ∀ p : * . ∀ s : * . ∀ b : * . module_contract (p ,
                      s) -> s -> tez -> typed_address (p ,
                      s) -> michelson_contract -> int -> b -> list (( b *
                                                                      mutation ))|
                      references: []
                      (assert#576:6-12 -> assert)
                      Range: File "", line 576, characters 6-12
                      Body Range: File "", line 576, characters 2-78
                      Content: |core: bool -> unit|
                      references: []
                      (assert_some#577:6-17 -> assert_some)
                      Range: File "", line 577, characters 6-17
                      Body Range: File "", line 577, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none#578:6-17 -> assert_none)
                      Range: File "", line 578, characters 6-17
                      Body Range: File "", line 578, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_with_error#580:6-23 -> assert_with_error)
                      Range: File "", line 580, characters 6-23
                      Body Range: File "", line 580, characters 2-78
                      Content: |unresolved|
                      references: []
                      (assert_some_with_error#581:6-28 -> assert_some_with_error)
                      Range: File "", line 581, characters 6-28
                      Body Range: File "", line 581, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_none_with_error#582:6-28 -> assert_none_with_error)
                      Range: File "", line 582, characters 6-28
                      Body Range: File "", line 582, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#374:9-12 -> PBT)
                      Range: File "", line 374, characters 9-12
                      Body Range: File "", line 375, character 4 to line 389, character 7
                      Content: Members: Variable definitions:
                                        (gen#375:8-11 -> gen)
                                        Range: File "", line 375, characters 8-11
                                        Body Range: File "", line 375, characters 35-69
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (gen_small#376:8-17 -> gen_small)
                                        Range: File "", line 376, characters 8-17
                                        Body Range: File "", line 376, characters 41-74
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#377:8-17 -> make_test)
                                        Range: File "", line 377, characters 8-17
                                        Body Range: File "", line 377, characters 75-79
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#378:8-11 -> run)
                                        Range: File "", line 378, characters 8-11
                                        Body Range: File "", line 379, character 6 to line 389, character 7
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []

                      (Proxy_ticket#584:9-21 -> Proxy_ticket)
                      Range: File "", line 584, characters 9-21
                      Body Range: File "", line 585, character 15 to line 652, character 7
                      Content: Members: Variable definitions:
                                        (proxy_transfer_contract#585:19-42 -> proxy_transfer_contract)
                                        Range: File "", line 585, characters 19-42
                                        Body Range: File "", line 590, character 6 to line 595, character 14
                                        Content: |core: ∀ vt : * . ∀ whole_p : * . ticket (vt) -> whole_p ->
                                        ( ( vt * nat ) *
                                          address ) -> unit -> ( list (operation) *
                                                                 unit )|
                                        references:
                                          File "", line 613, characters 8-31
                                        (proxy_originate_contract#597:19-43 -> proxy_originate_contract)
                                        Range: File "", line 597, characters 19-43
                                        Body Range: File "", line 603, character 6 to line 607, character 21
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * . ticket (vt) -> whole_s -> vp -> whole_s ->
                                        ( list (operation) *
                                          whole_s ) -> ( vt * nat ) -> option (address) ->
                                        ( list (operation) *
                                          option (address) )|
                                        references:
                                          File "", line 629, characters 8-32 ,
                                          File "", line 644, characters 8-32
                                        (init_transfer#611:8-21 -> init_transfer)
                                        Range: File "", line 611, characters 8-21
                                        Body Range: File "", line 612, character 6 to line 616, character 17
                                        Content: |core: ∀ vt : * . ∀ whole_p : * . ticket (vt) -> whole_p -> proxy_address (vt)|
                                        references: []
                                        (transfer#618:8-16 -> transfer)
                                        Range: File "", line 618, characters 8-16
                                        Body Range: File "", line 621, character 6 to line 622, character 84
                                        Content: |core: ∀ vt : * . proxy_address (vt) ->
                                        ( ( vt * nat ) *
                                          address ) -> test_exec_result|
                                        references: []
                                        (originate_uncurried#624:8-27 -> originate_uncurried)
                                        Range: File "", line 624, characters 8-27
                                        Body Range: File "", line 628, character 6 to line 637, character 7
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * .
                                        ( vt *
                                          nat ) -> ticket (vt) -> whole_s ->
                                        ( vp *
                                          whole_s ) -> ( list (operation) *
                                                         whole_s ) -> address|
                                        references: []
                                        (originate#639:8-17 -> originate)
                                        Range: File "", line 639, characters 8-17
                                        Body Range: File "", line 643, character 6 to line 652, character 7
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * .
                                        ( vt *
                                          nat ) -> ticket (vt) -> whole_s -> vp -> whole_s ->
                                        ( list (operation) *
                                          whole_s ) -> address|
                                        references: []
                                        Type definitions:
                                        (proxy_address#609:12-25 -> proxy_address)
                                        Range: File "", line 609, characters 12-25
                                        Body Range: File "", line 609, characters 28-71
                                        Content: : |funtype 'v : * . typed_address (
                                        ( ( 'v * nat ) *
                                          address ) ,
                                        unit)|
                                        references:
                                          File "", line 611, characters 78-91 ,
                                          File "", line 619, characters 26-39
                                        Module definitions:

                      references: []


    references: [] |}]
