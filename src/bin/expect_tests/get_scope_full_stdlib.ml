open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 failwith#52:4-12 bool#54:5-9 option#55:8-14 entrypoint#57:14-24 Tezos#59:7-12 get_balance#61:6-17 get_amount#62:6-16 get_now#63:6-13 get_sender#64:6-16 get_source#65:6-16 get_level#66:6-15 get_self_address#67:6-22 get_chain_id#68:6-18 get_total_voting_power#69:6-28 get_min_block_time#70:6-24 voting_power#71:6-18 address#72:6-13 implicit_account#73:6-22 join_tickets#74:6-18 read_ticket#75:6-17 never#77:6-11 pairing_check#78:6-19 set_delegate#79:6-18 self#80:25-29 constant#83:25-33 sapling_empty_state#84:25-44 get_contract_opt#86:25-41 get_contract#88:25-37 get_contract_with_error#92:6-29 create_ticket#95:6-19 transaction#96:6-17 call_view#98:25-34 split_ticket#101:6-18 create_contract#103:25-40 create_contract_uncurried#106:25-50 get_entrypoint_opt#108:25-43 get_entrypoint#111:25-39 emit#114:25-29 sapling_verify_update#117:25-46 Bitwise#121:7-14 and#122:6-10 xor#123:6-9 or#124:6-9 shift_left#125:6-16 shift_right#126:6-17 Big_map#129:7-14 empty#130:16-21 literal#131:25-32 mem#133:6-9 add#134:6-9 remove#135:6-12 update#136:6-12 get_and_update#137:6-20 find_opt#138:6-14 find#139:6-10 Map#143:7-10 empty#144:6-11 size#145:6-10 literal#146:25-32 mem#148:6-9 add#149:6-9 remove#150:6-12 update#151:6-12 get_and_update#152:6-20 find#153:6-10 find_opt#154:6-14 iter#155:6-10 map#156:6-9 fold#157:6-10 Transpiled#161:7-17 map_find_opt#162:6-18 map_add#163:6-13 map_remove#164:6-16 Set#167:7-10 empty#168:6-11 size#169:6-10 cardinal#170:6-14 literal#171:25-32 mem#173:6-9 add#174:6-9 remove#175:6-12 update#176:6-12 iter#177:6-10 fold#178:6-10 fold_desc#179:6-15 filter_map#180:6-16 List#184:7-11 length#185:6-12 size#186:6-10 head_opt#187:6-14 tail_opt#188:6-14 map#190:6-9 iter#191:6-10 fold#192:6-10 fold_left#193:6-15 fold_right#194:6-16 cons#195:6-10 find_opt#196:6-14 filter_map#198:6-16 update#200:6-12 update_with#202:6-17 String#206:7-13 length#207:6-12 concats#208:6-13 concat#210:6-12 sub#211:6-9 Option#214:7-13 unopt#215:6-11 unopt_with_error#217:6-22 map#218:15-18 value#219:6-11 value_exn#220:6-15 is_none#221:6-13 is_some#222:6-13 Bytes#225:7-12 concats#226:6-13 pack#227:6-10 unpack#228:6-12 length#229:6-12 concat#231:6-12 sub#232:6-9 Crypto#235:7-13 blake2b#236:6-13 sha256#237:6-12 sha512#238:6-12 sha3#239:6-10 keccak#240:6-12 hash_key#241:6-14 check#242:6-11 assert#245:4-10 assert_some#246:4-15 assert_none#247:4-15 abs#248:4-7 is_nat#249:4-10 unit#250:14-18 int#251:4-7 nat#252:4-7 bytes#253:4-9 ignore#254:4-10 curry#255:4-9 uncurry#256:4-11 assert_with_error#258:4-21 assert_some_with_error#259:4-26 assert_none_with_error#260:4-26 ediv#261:4-8 michelson_program#263:5-22 typed_address#264:5-18 mutation#265:5-13 michelson_contract#266:5-23 ast_contract#267:5-17 pbt_gen#268:5-12 int64#269:5-10 views#270:5-10 test_exec_error_balance_too_low#272:5-36 test_exec_error#275:5-20 test_exec_result#280:5-21 test_baker_policy#282:5-22 pbt_test#287:8-16 pbt_result#288:8-18 unforged_ticket#290:8-23 module_contract#292:14-29 Test#294:7-11 run#296:6-9 eval#297:6-10 compile_value#299:6-19 get_total_voting_power#300:6-28 failwith#301:6-14 to_contract#302:6-17 set_source#303:6-16 get_storage_of_address#304:6-28 get_balance#305:6-17 print#306:6-11 eprint#307:6-12 get_voting_power#308:6-22 nth_bootstrap_contract#309:6-28 nth_bootstrap_account#310:6-27 get_bootstrap_account#313:6-27 nth_bootstrap_typed_address#314:6-33 last_originations#315:6-23 random#316:6-12 new_account#319:6-17 decompile#320:6-15 bake_until_n_cycle_end#321:6-28 get_time#322:6-14 cast_address#323:6-18 register_delegate#324:6-23 stake#325:6-11 register_constant#326:6-23 to_typed_address#327:6-22 constant_to_michelson_program#328:6-35 parse_michelson#329:6-21 restore_context#330:6-21 save_context#331:6-18 drop_context#332:6-18 to_string#333:6-15 to_json#334:6-13 get_storage#335:6-17 set_baker_policy#340:6-22 set_baker#341:6-15 size#342:6-10 compile_contract#343:6-22 read_contract_from_file#347:6-29 chr#348:6-9 nl#358:6-8 println#359:6-13 set_print_values#362:6-22 unset_print_values#363:6-24 PBT#365:9-12 gen#366:8-11 gen_small#367:8-17 make_test#368:8-17 run#369:8-11 get_last_events_from#383:6-26 transfer#391:6-14 transfer_exn#392:6-18 log#393:6-9 reset_state#397:6-17 reset_state_at#398:6-20 bootstrap_contract#399:6-24 mutate_value#400:6-18 save_mutation#401:6-19 sign#402:6-10 add_account#403:6-17 baker_account#404:6-19 set_big_map#405:6-17 transfer_to_contract#406:6-26 transfer_to_contract_exn#411:6-30 michelson_equal#416:6-21 to_entrypoint#417:6-19 originate_contract#425:6-24 originate#426:6-15 compile_contract_with_views#433:8-35 originate_uncurried#436:6-25 originate_module#443:6-22 compile_contract_from_file#450:6-32 originate_from_file#453:6-25 mutation_test#458:6-19 mutation_test_all#470:6-23 originate_from_file_and_mutate#482:6-36 originate_from_file_and_mutate_all#502:6-40 originate_module_and_mutate#522:6-33 originate_module_and_mutate_all#544:6-37 assert#567:6-12 assert_some#568:6-17 assert_none#569:6-17 assert_with_error#571:6-23 assert_some_with_error#572:6-28 assert_none_with_error#573:6-28 Proxy_ticket#575:9-21 proxy_transfer_contract#576:19-42 proxy_originate_contract#588:19-43 proxy_address#600:12-25 init_transfer#602:8-21 transfer#609:8-16 originate_uncurried#615:8-27 originate#630:8-17  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 failwith#52:4-12 bool#54:5-9 option#55:8-14 entrypoint#57:14-24 Tezos#59:7-12 get_balance#61:6-17 get_amount#62:6-16 get_now#63:6-13 get_sender#64:6-16 get_source#65:6-16 get_level#66:6-15 get_self_address#67:6-22 get_chain_id#68:6-18 get_total_voting_power#69:6-28 get_min_block_time#70:6-24 voting_power#71:6-18 address#72:6-13 implicit_account#73:6-22 join_tickets#74:6-18 read_ticket#75:6-17 never#77:6-11 pairing_check#78:6-19 set_delegate#79:6-18 self#80:25-29 constant#83:25-33 sapling_empty_state#84:25-44 get_contract_opt#86:25-41 get_contract#88:25-37 get_contract_with_error#92:6-29 create_ticket#95:6-19 transaction#96:6-17 call_view#98:25-34 split_ticket#101:6-18 create_contract#103:25-40 create_contract_uncurried#106:25-50 get_entrypoint_opt#108:25-43 get_entrypoint#111:25-39 emit#114:25-29 sapling_verify_update#117:25-46 Bitwise#121:7-14 and#122:6-10 xor#123:6-9 or#124:6-9 shift_left#125:6-16 shift_right#126:6-17 Big_map#129:7-14 empty#130:16-21 literal#131:25-32 mem#133:6-9 add#134:6-9 remove#135:6-12 update#136:6-12 get_and_update#137:6-20 find_opt#138:6-14 find#139:6-10 Map#143:7-10 empty#144:6-11 size#145:6-10 literal#146:25-32 mem#148:6-9 add#149:6-9 remove#150:6-12 update#151:6-12 get_and_update#152:6-20 find#153:6-10 find_opt#154:6-14 iter#155:6-10 map#156:6-9 fold#157:6-10 Transpiled#161:7-17 map_find_opt#162:6-18 map_add#163:6-13 map_remove#164:6-16 Set#167:7-10 empty#168:6-11 size#169:6-10 cardinal#170:6-14 literal#171:25-32 mem#173:6-9 add#174:6-9 remove#175:6-12 update#176:6-12 iter#177:6-10 fold#178:6-10 fold_desc#179:6-15 filter_map#180:6-16 List#184:7-11 length#185:6-12 size#186:6-10 head_opt#187:6-14 tail_opt#188:6-14 map#190:6-9 iter#191:6-10 fold#192:6-10 fold_left#193:6-15 fold_right#194:6-16 cons#195:6-10 find_opt#196:6-14 filter_map#198:6-16 update#200:6-12 update_with#202:6-17 String#206:7-13 length#207:6-12 concats#208:6-13 concat#210:6-12 sub#211:6-9 Option#214:7-13 unopt#215:6-11 unopt_with_error#217:6-22 map#218:15-18 value#219:6-11 value_exn#220:6-15 is_none#221:6-13 is_some#222:6-13 Bytes#225:7-12 concats#226:6-13 pack#227:6-10 unpack#228:6-12 length#229:6-12 concat#231:6-12 sub#232:6-9 Crypto#235:7-13 blake2b#236:6-13 sha256#237:6-12 sha512#238:6-12 sha3#239:6-10 keccak#240:6-12 hash_key#241:6-14 check#242:6-11 assert#245:4-10 assert_some#246:4-15 assert_none#247:4-15 abs#248:4-7 is_nat#249:4-10 unit#250:14-18 int#251:4-7 nat#252:4-7 bytes#253:4-9 ignore#254:4-10 curry#255:4-9 uncurry#256:4-11 assert_with_error#258:4-21 assert_some_with_error#259:4-26 assert_none_with_error#260:4-26 ediv#261:4-8 michelson_program#263:5-22 typed_address#264:5-18 mutation#265:5-13 michelson_contract#266:5-23 ast_contract#267:5-17 pbt_gen#268:5-12 int64#269:5-10 views#270:5-10 test_exec_error_balance_too_low#272:5-36 test_exec_error#275:5-20 test_exec_result#280:5-21 test_baker_policy#282:5-22 pbt_test#287:8-16 pbt_result#288:8-18 unforged_ticket#290:8-23 module_contract#292:14-29 Test#294:7-11 run#296:6-9 eval#297:6-10 compile_value#299:6-19 get_total_voting_power#300:6-28 failwith#301:6-14 to_contract#302:6-17 set_source#303:6-16 get_storage_of_address#304:6-28 get_balance#305:6-17 print#306:6-11 eprint#307:6-12 get_voting_power#308:6-22 nth_bootstrap_contract#309:6-28 nth_bootstrap_account#310:6-27 get_bootstrap_account#313:6-27 nth_bootstrap_typed_address#314:6-33 last_originations#315:6-23 random#316:6-12 new_account#319:6-17 decompile#320:6-15 bake_until_n_cycle_end#321:6-28 get_time#322:6-14 cast_address#323:6-18 register_delegate#324:6-23 stake#325:6-11 register_constant#326:6-23 to_typed_address#327:6-22 constant_to_michelson_program#328:6-35 parse_michelson#329:6-21 restore_context#330:6-21 save_context#331:6-18 drop_context#332:6-18 to_string#333:6-15 to_json#334:6-13 get_storage#335:6-17 set_baker_policy#340:6-22 set_baker#341:6-15 size#342:6-10 compile_contract#343:6-22 read_contract_from_file#347:6-29 chr#348:6-9 nl#358:6-8 println#359:6-13 set_print_values#362:6-22 unset_print_values#363:6-24 PBT#365:9-12 gen#366:8-11 gen_small#367:8-17 make_test#368:8-17 run#369:8-11 get_last_events_from#383:6-26 transfer#391:6-14 transfer_exn#392:6-18 log#393:6-9 reset_state#397:6-17 reset_state_at#398:6-20 bootstrap_contract#399:6-24 mutate_value#400:6-18 save_mutation#401:6-19 sign#402:6-10 add_account#403:6-17 baker_account#404:6-19 set_big_map#405:6-17 transfer_to_contract#406:6-26 transfer_to_contract_exn#411:6-30 michelson_equal#416:6-21 to_entrypoint#417:6-19 originate_contract#425:6-24 originate#426:6-15 compile_contract_with_views#433:8-35 originate_uncurried#436:6-25 originate_module#443:6-22 compile_contract_from_file#450:6-32 originate_from_file#453:6-25 mutation_test#458:6-19 mutation_test_all#470:6-23 originate_from_file_and_mutate#482:6-36 originate_from_file_and_mutate_all#502:6-40 originate_module_and_mutate#522:6-33 originate_module_and_mutate_all#544:6-37 assert#567:6-12 assert_some#568:6-17 assert_none#569:6-17 assert_with_error#571:6-23 assert_some_with_error#572:6-28 assert_none_with_error#573:6-28 Proxy_ticket#575:9-21 proxy_transfer_contract#576:19-42 proxy_originate_contract#588:19-43 proxy_address#600:12-25 init_transfer#602:8-21 transfer#609:8-16 originate_uncurried#615:8-27 originate#630:8-17 a#1:4-5  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 failwith#52:4-12 bool#54:5-9 option#55:8-14 entrypoint#57:14-24 Tezos#59:7-12 get_balance#61:6-17 get_amount#62:6-16 get_now#63:6-13 get_sender#64:6-16 get_source#65:6-16 get_level#66:6-15 get_self_address#67:6-22 get_chain_id#68:6-18 get_total_voting_power#69:6-28 get_min_block_time#70:6-24 voting_power#71:6-18 address#72:6-13 implicit_account#73:6-22 join_tickets#74:6-18 read_ticket#75:6-17 never#77:6-11 pairing_check#78:6-19 set_delegate#79:6-18 self#80:25-29 constant#83:25-33 sapling_empty_state#84:25-44 get_contract_opt#86:25-41 get_contract#88:25-37 get_contract_with_error#92:6-29 create_ticket#95:6-19 transaction#96:6-17 call_view#98:25-34 split_ticket#101:6-18 create_contract#103:25-40 create_contract_uncurried#106:25-50 get_entrypoint_opt#108:25-43 get_entrypoint#111:25-39 emit#114:25-29 sapling_verify_update#117:25-46 Bitwise#121:7-14 and#122:6-10 xor#123:6-9 or#124:6-9 shift_left#125:6-16 shift_right#126:6-17 Big_map#129:7-14 empty#130:16-21 literal#131:25-32 mem#133:6-9 add#134:6-9 remove#135:6-12 update#136:6-12 get_and_update#137:6-20 find_opt#138:6-14 find#139:6-10 Map#143:7-10 empty#144:6-11 size#145:6-10 literal#146:25-32 mem#148:6-9 add#149:6-9 remove#150:6-12 update#151:6-12 get_and_update#152:6-20 find#153:6-10 find_opt#154:6-14 iter#155:6-10 map#156:6-9 fold#157:6-10 Transpiled#161:7-17 map_find_opt#162:6-18 map_add#163:6-13 map_remove#164:6-16 Set#167:7-10 empty#168:6-11 size#169:6-10 cardinal#170:6-14 literal#171:25-32 mem#173:6-9 add#174:6-9 remove#175:6-12 update#176:6-12 iter#177:6-10 fold#178:6-10 fold_desc#179:6-15 filter_map#180:6-16 List#184:7-11 length#185:6-12 size#186:6-10 head_opt#187:6-14 tail_opt#188:6-14 map#190:6-9 iter#191:6-10 fold#192:6-10 fold_left#193:6-15 fold_right#194:6-16 cons#195:6-10 find_opt#196:6-14 filter_map#198:6-16 update#200:6-12 update_with#202:6-17 String#206:7-13 length#207:6-12 concats#208:6-13 concat#210:6-12 sub#211:6-9 Option#214:7-13 unopt#215:6-11 unopt_with_error#217:6-22 map#218:15-18 value#219:6-11 value_exn#220:6-15 is_none#221:6-13 is_some#222:6-13 Bytes#225:7-12 concats#226:6-13 pack#227:6-10 unpack#228:6-12 length#229:6-12 concat#231:6-12 sub#232:6-9 Crypto#235:7-13 blake2b#236:6-13 sha256#237:6-12 sha512#238:6-12 sha3#239:6-10 keccak#240:6-12 hash_key#241:6-14 check#242:6-11 assert#245:4-10 assert_some#246:4-15 assert_none#247:4-15 abs#248:4-7 is_nat#249:4-10 unit#250:14-18 int#251:4-7 nat#252:4-7 bytes#253:4-9 ignore#254:4-10 curry#255:4-9 uncurry#256:4-11 assert_with_error#258:4-21 assert_some_with_error#259:4-26 assert_none_with_error#260:4-26 ediv#261:4-8 michelson_program#263:5-22 typed_address#264:5-18 mutation#265:5-13 michelson_contract#266:5-23 ast_contract#267:5-17 pbt_gen#268:5-12 int64#269:5-10 views#270:5-10 test_exec_error_balance_too_low#272:5-36 test_exec_error#275:5-20 test_exec_result#280:5-21 test_baker_policy#282:5-22 pbt_test#287:8-16 pbt_result#288:8-18 unforged_ticket#290:8-23 module_contract#292:14-29 Test#294:7-11 run#296:6-9 eval#297:6-10 compile_value#299:6-19 get_total_voting_power#300:6-28 failwith#301:6-14 to_contract#302:6-17 set_source#303:6-16 get_storage_of_address#304:6-28 get_balance#305:6-17 print#306:6-11 eprint#307:6-12 get_voting_power#308:6-22 nth_bootstrap_contract#309:6-28 nth_bootstrap_account#310:6-27 get_bootstrap_account#313:6-27 nth_bootstrap_typed_address#314:6-33 last_originations#315:6-23 random#316:6-12 new_account#319:6-17 decompile#320:6-15 bake_until_n_cycle_end#321:6-28 get_time#322:6-14 cast_address#323:6-18 register_delegate#324:6-23 stake#325:6-11 register_constant#326:6-23 to_typed_address#327:6-22 constant_to_michelson_program#328:6-35 parse_michelson#329:6-21 restore_context#330:6-21 save_context#331:6-18 drop_context#332:6-18 to_string#333:6-15 to_json#334:6-13 get_storage#335:6-17 set_baker_policy#340:6-22 set_baker#341:6-15 size#342:6-10 compile_contract#343:6-22 read_contract_from_file#347:6-29 chr#348:6-9 nl#358:6-8 println#359:6-13 set_print_values#362:6-22 unset_print_values#363:6-24 PBT#365:9-12 gen#366:8-11 gen_small#367:8-17 make_test#368:8-17 run#369:8-11 get_last_events_from#383:6-26 transfer#391:6-14 transfer_exn#392:6-18 log#393:6-9 reset_state#397:6-17 reset_state_at#398:6-20 bootstrap_contract#399:6-24 mutate_value#400:6-18 save_mutation#401:6-19 sign#402:6-10 add_account#403:6-17 baker_account#404:6-19 set_big_map#405:6-17 transfer_to_contract#406:6-26 transfer_to_contract_exn#411:6-30 michelson_equal#416:6-21 to_entrypoint#417:6-19 originate_contract#425:6-24 originate#426:6-15 compile_contract_with_views#433:8-35 originate_uncurried#436:6-25 originate_module#443:6-22 compile_contract_from_file#450:6-32 originate_from_file#453:6-25 mutation_test#458:6-19 mutation_test_all#470:6-23 originate_from_file_and_mutate#482:6-36 originate_from_file_and_mutate_all#502:6-40 originate_module_and_mutate#522:6-33 originate_module_and_mutate_all#544:6-37 assert#567:6-12 assert_some#568:6-17 assert_none#569:6-17 assert_with_error#571:6-23 assert_some_with_error#572:6-28 assert_none_with_error#573:6-28 Proxy_ticket#575:9-21 proxy_transfer_contract#576:19-42 proxy_originate_contract#588:19-43 proxy_address#600:12-25 init_transfer#602:8-21 transfer#609:8-16 originate_uncurried#615:8-27 originate#630:8-17 a#1:4-5 c#5:10-11  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 failwith#52:4-12 bool#54:5-9 option#55:8-14 entrypoint#57:14-24 Tezos#59:7-12 get_balance#61:6-17 get_amount#62:6-16 get_now#63:6-13 get_sender#64:6-16 get_source#65:6-16 get_level#66:6-15 get_self_address#67:6-22 get_chain_id#68:6-18 get_total_voting_power#69:6-28 get_min_block_time#70:6-24 voting_power#71:6-18 address#72:6-13 implicit_account#73:6-22 join_tickets#74:6-18 read_ticket#75:6-17 never#77:6-11 pairing_check#78:6-19 set_delegate#79:6-18 self#80:25-29 constant#83:25-33 sapling_empty_state#84:25-44 get_contract_opt#86:25-41 get_contract#88:25-37 get_contract_with_error#92:6-29 create_ticket#95:6-19 transaction#96:6-17 call_view#98:25-34 split_ticket#101:6-18 create_contract#103:25-40 create_contract_uncurried#106:25-50 get_entrypoint_opt#108:25-43 get_entrypoint#111:25-39 emit#114:25-29 sapling_verify_update#117:25-46 Bitwise#121:7-14 and#122:6-10 xor#123:6-9 or#124:6-9 shift_left#125:6-16 shift_right#126:6-17 Big_map#129:7-14 empty#130:16-21 literal#131:25-32 mem#133:6-9 add#134:6-9 remove#135:6-12 update#136:6-12 get_and_update#137:6-20 find_opt#138:6-14 find#139:6-10 Map#143:7-10 empty#144:6-11 size#145:6-10 literal#146:25-32 mem#148:6-9 add#149:6-9 remove#150:6-12 update#151:6-12 get_and_update#152:6-20 find#153:6-10 find_opt#154:6-14 iter#155:6-10 map#156:6-9 fold#157:6-10 Transpiled#161:7-17 map_find_opt#162:6-18 map_add#163:6-13 map_remove#164:6-16 Set#167:7-10 empty#168:6-11 size#169:6-10 cardinal#170:6-14 literal#171:25-32 mem#173:6-9 add#174:6-9 remove#175:6-12 update#176:6-12 iter#177:6-10 fold#178:6-10 fold_desc#179:6-15 filter_map#180:6-16 List#184:7-11 length#185:6-12 size#186:6-10 head_opt#187:6-14 tail_opt#188:6-14 map#190:6-9 iter#191:6-10 fold#192:6-10 fold_left#193:6-15 fold_right#194:6-16 cons#195:6-10 find_opt#196:6-14 filter_map#198:6-16 update#200:6-12 update_with#202:6-17 String#206:7-13 length#207:6-12 concats#208:6-13 concat#210:6-12 sub#211:6-9 Option#214:7-13 unopt#215:6-11 unopt_with_error#217:6-22 map#218:15-18 value#219:6-11 value_exn#220:6-15 is_none#221:6-13 is_some#222:6-13 Bytes#225:7-12 concats#226:6-13 pack#227:6-10 unpack#228:6-12 length#229:6-12 concat#231:6-12 sub#232:6-9 Crypto#235:7-13 blake2b#236:6-13 sha256#237:6-12 sha512#238:6-12 sha3#239:6-10 keccak#240:6-12 hash_key#241:6-14 check#242:6-11 assert#245:4-10 assert_some#246:4-15 assert_none#247:4-15 abs#248:4-7 is_nat#249:4-10 unit#250:14-18 int#251:4-7 nat#252:4-7 bytes#253:4-9 ignore#254:4-10 curry#255:4-9 uncurry#256:4-11 assert_with_error#258:4-21 assert_some_with_error#259:4-26 assert_none_with_error#260:4-26 ediv#261:4-8 michelson_program#263:5-22 typed_address#264:5-18 mutation#265:5-13 michelson_contract#266:5-23 ast_contract#267:5-17 pbt_gen#268:5-12 int64#269:5-10 views#270:5-10 test_exec_error_balance_too_low#272:5-36 test_exec_error#275:5-20 test_exec_result#280:5-21 test_baker_policy#282:5-22 pbt_test#287:8-16 pbt_result#288:8-18 unforged_ticket#290:8-23 module_contract#292:14-29 Test#294:7-11 run#296:6-9 eval#297:6-10 compile_value#299:6-19 get_total_voting_power#300:6-28 failwith#301:6-14 to_contract#302:6-17 set_source#303:6-16 get_storage_of_address#304:6-28 get_balance#305:6-17 print#306:6-11 eprint#307:6-12 get_voting_power#308:6-22 nth_bootstrap_contract#309:6-28 nth_bootstrap_account#310:6-27 get_bootstrap_account#313:6-27 nth_bootstrap_typed_address#314:6-33 last_originations#315:6-23 random#316:6-12 new_account#319:6-17 decompile#320:6-15 bake_until_n_cycle_end#321:6-28 get_time#322:6-14 cast_address#323:6-18 register_delegate#324:6-23 stake#325:6-11 register_constant#326:6-23 to_typed_address#327:6-22 constant_to_michelson_program#328:6-35 parse_michelson#329:6-21 restore_context#330:6-21 save_context#331:6-18 drop_context#332:6-18 to_string#333:6-15 to_json#334:6-13 get_storage#335:6-17 set_baker_policy#340:6-22 set_baker#341:6-15 size#342:6-10 compile_contract#343:6-22 read_contract_from_file#347:6-29 chr#348:6-9 nl#358:6-8 println#359:6-13 set_print_values#362:6-22 unset_print_values#363:6-24 PBT#365:9-12 gen#366:8-11 gen_small#367:8-17 make_test#368:8-17 run#369:8-11 get_last_events_from#383:6-26 transfer#391:6-14 transfer_exn#392:6-18 log#393:6-9 reset_state#397:6-17 reset_state_at#398:6-20 bootstrap_contract#399:6-24 mutate_value#400:6-18 save_mutation#401:6-19 sign#402:6-10 add_account#403:6-17 baker_account#404:6-19 set_big_map#405:6-17 transfer_to_contract#406:6-26 transfer_to_contract_exn#411:6-30 michelson_equal#416:6-21 to_entrypoint#417:6-19 originate_contract#425:6-24 originate#426:6-15 compile_contract_with_views#433:8-35 originate_uncurried#436:6-25 originate_module#443:6-22 compile_contract_from_file#450:6-32 originate_from_file#453:6-25 mutation_test#458:6-19 mutation_test_all#470:6-23 originate_from_file_and_mutate#482:6-36 originate_from_file_and_mutate_all#502:6-40 originate_module_and_mutate#522:6-33 originate_module_and_mutate_all#544:6-37 assert#567:6-12 assert_some#568:6-17 assert_none#569:6-17 assert_with_error#571:6-23 assert_some_with_error#572:6-28 assert_none_with_error#573:6-28 Proxy_ticket#575:9-21 proxy_transfer_contract#576:19-42 proxy_originate_contract#588:19-43 proxy_address#600:12-25 init_transfer#602:8-21 transfer#609:8-16 originate_uncurried#615:8-27 originate#630:8-17 a#1:4-5 c#5:10-11 d#5:26-27  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 failwith#52:4-12 bool#54:5-9 option#55:8-14 entrypoint#57:14-24 Tezos#59:7-12 get_balance#61:6-17 get_amount#62:6-16 get_now#63:6-13 get_sender#64:6-16 get_source#65:6-16 get_level#66:6-15 get_self_address#67:6-22 get_chain_id#68:6-18 get_total_voting_power#69:6-28 get_min_block_time#70:6-24 voting_power#71:6-18 address#72:6-13 implicit_account#73:6-22 join_tickets#74:6-18 read_ticket#75:6-17 never#77:6-11 pairing_check#78:6-19 set_delegate#79:6-18 self#80:25-29 constant#83:25-33 sapling_empty_state#84:25-44 get_contract_opt#86:25-41 get_contract#88:25-37 get_contract_with_error#92:6-29 create_ticket#95:6-19 transaction#96:6-17 call_view#98:25-34 split_ticket#101:6-18 create_contract#103:25-40 create_contract_uncurried#106:25-50 get_entrypoint_opt#108:25-43 get_entrypoint#111:25-39 emit#114:25-29 sapling_verify_update#117:25-46 Bitwise#121:7-14 and#122:6-10 xor#123:6-9 or#124:6-9 shift_left#125:6-16 shift_right#126:6-17 Big_map#129:7-14 empty#130:16-21 literal#131:25-32 mem#133:6-9 add#134:6-9 remove#135:6-12 update#136:6-12 get_and_update#137:6-20 find_opt#138:6-14 find#139:6-10 Map#143:7-10 empty#144:6-11 size#145:6-10 literal#146:25-32 mem#148:6-9 add#149:6-9 remove#150:6-12 update#151:6-12 get_and_update#152:6-20 find#153:6-10 find_opt#154:6-14 iter#155:6-10 map#156:6-9 fold#157:6-10 Transpiled#161:7-17 map_find_opt#162:6-18 map_add#163:6-13 map_remove#164:6-16 Set#167:7-10 empty#168:6-11 size#169:6-10 cardinal#170:6-14 literal#171:25-32 mem#173:6-9 add#174:6-9 remove#175:6-12 update#176:6-12 iter#177:6-10 fold#178:6-10 fold_desc#179:6-15 filter_map#180:6-16 List#184:7-11 length#185:6-12 size#186:6-10 head_opt#187:6-14 tail_opt#188:6-14 map#190:6-9 iter#191:6-10 fold#192:6-10 fold_left#193:6-15 fold_right#194:6-16 cons#195:6-10 find_opt#196:6-14 filter_map#198:6-16 update#200:6-12 update_with#202:6-17 String#206:7-13 length#207:6-12 concats#208:6-13 concat#210:6-12 sub#211:6-9 Option#214:7-13 unopt#215:6-11 unopt_with_error#217:6-22 map#218:15-18 value#219:6-11 value_exn#220:6-15 is_none#221:6-13 is_some#222:6-13 Bytes#225:7-12 concats#226:6-13 pack#227:6-10 unpack#228:6-12 length#229:6-12 concat#231:6-12 sub#232:6-9 Crypto#235:7-13 blake2b#236:6-13 sha256#237:6-12 sha512#238:6-12 sha3#239:6-10 keccak#240:6-12 hash_key#241:6-14 check#242:6-11 assert#245:4-10 assert_some#246:4-15 assert_none#247:4-15 abs#248:4-7 is_nat#249:4-10 unit#250:14-18 int#251:4-7 nat#252:4-7 bytes#253:4-9 ignore#254:4-10 curry#255:4-9 uncurry#256:4-11 assert_with_error#258:4-21 assert_some_with_error#259:4-26 assert_none_with_error#260:4-26 ediv#261:4-8 michelson_program#263:5-22 typed_address#264:5-18 mutation#265:5-13 michelson_contract#266:5-23 ast_contract#267:5-17 pbt_gen#268:5-12 int64#269:5-10 views#270:5-10 test_exec_error_balance_too_low#272:5-36 test_exec_error#275:5-20 test_exec_result#280:5-21 test_baker_policy#282:5-22 pbt_test#287:8-16 pbt_result#288:8-18 unforged_ticket#290:8-23 module_contract#292:14-29 Test#294:7-11 run#296:6-9 eval#297:6-10 compile_value#299:6-19 get_total_voting_power#300:6-28 failwith#301:6-14 to_contract#302:6-17 set_source#303:6-16 get_storage_of_address#304:6-28 get_balance#305:6-17 print#306:6-11 eprint#307:6-12 get_voting_power#308:6-22 nth_bootstrap_contract#309:6-28 nth_bootstrap_account#310:6-27 get_bootstrap_account#313:6-27 nth_bootstrap_typed_address#314:6-33 last_originations#315:6-23 random#316:6-12 new_account#319:6-17 decompile#320:6-15 bake_until_n_cycle_end#321:6-28 get_time#322:6-14 cast_address#323:6-18 register_delegate#324:6-23 stake#325:6-11 register_constant#326:6-23 to_typed_address#327:6-22 constant_to_michelson_program#328:6-35 parse_michelson#329:6-21 restore_context#330:6-21 save_context#331:6-18 drop_context#332:6-18 to_string#333:6-15 to_json#334:6-13 get_storage#335:6-17 set_baker_policy#340:6-22 set_baker#341:6-15 size#342:6-10 compile_contract#343:6-22 read_contract_from_file#347:6-29 chr#348:6-9 nl#358:6-8 println#359:6-13 set_print_values#362:6-22 unset_print_values#363:6-24 PBT#365:9-12 gen#366:8-11 gen_small#367:8-17 make_test#368:8-17 run#369:8-11 get_last_events_from#383:6-26 transfer#391:6-14 transfer_exn#392:6-18 log#393:6-9 reset_state#397:6-17 reset_state_at#398:6-20 bootstrap_contract#399:6-24 mutate_value#400:6-18 save_mutation#401:6-19 sign#402:6-10 add_account#403:6-17 baker_account#404:6-19 set_big_map#405:6-17 transfer_to_contract#406:6-26 transfer_to_contract_exn#411:6-30 michelson_equal#416:6-21 to_entrypoint#417:6-19 originate_contract#425:6-24 originate#426:6-15 compile_contract_with_views#433:8-35 originate_uncurried#436:6-25 originate_module#443:6-22 compile_contract_from_file#450:6-32 originate_from_file#453:6-25 mutation_test#458:6-19 mutation_test_all#470:6-23 originate_from_file_and_mutate#482:6-36 originate_from_file_and_mutate_all#502:6-40 originate_module_and_mutate#522:6-33 originate_module_and_mutate_all#544:6-37 assert#567:6-12 assert_some#568:6-17 assert_none#569:6-17 assert_with_error#571:6-23 assert_some_with_error#572:6-28 assert_none_with_error#573:6-28 Proxy_ticket#575:9-21 proxy_transfer_contract#576:19-42 proxy_originate_contract#588:19-43 proxy_address#600:12-25 init_transfer#602:8-21 transfer#609:8-16 originate_uncurried#615:8-27 originate#630:8-17 a#1:4-5 e#6:9-10  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32

    Variable definitions:
    (failwith#52:4-12 -> failwith)
    Range: File "", line 52, characters 4-12
    Body Range: File "", line 52, characters 34-73
    Content: |unresolved|
    references:
      File "", line 90, characters 27-35 ,
      File "", line 94, characters 27-35 ,
      File "", line 113, characters 27-35 ,
      File "", line 215, characters 79-87 ,
      File "", line 217, characters 103-111 ,
      File "", line 220, characters 83-91 ,
      File "", line 245, characters 49-57 ,
      File "", line 246, characters 72-80 ,
      File "", line 247, characters 87-95 ,
      File "", line 258, characters 66-74 ,
      File "", line 259, characters 96-104 ,
      File "", line 260, characters 111-119
    Mod Path =
    Def Type = Global
    (assert#245:4-10 -> assert)
    Range: File "", line 245, characters 4-10
    Body Range: File "", line 245, characters 31-76
    Content: |core: bool -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (assert_some#246:4-15 -> assert_some)
    Range: File "", line 246, characters 4-15
    Body Range: File "", line 246, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (assert_none#247:4-15 -> assert_none)
    Range: File "", line 247, characters 4-15
    Body Range: File "", line 247, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (abs#248:4-7 -> abs)
    Range: File "", line 248, characters 4-7
    Body Range: File "", line 248, characters 26-62
    Content: |core: int -> nat|
    references: File "", line 421, characters 31-34
    Mod Path =
    Def Type = Global
    (is_nat#249:4-10 -> is_nat)
    Range: File "", line 249, characters 4-10
    Body Range: File "", line 249, characters 36-81
    Content: |core: int -> option (nat)|
    references: []
    Mod Path =
    Def Type = Global
    (unit#250:14-18 -> unit)
    Range: File "", line 250, characters 14-18
    Body Range: File "", line 250, characters 28-48
    Content: |core: unit|
    references: []
    Mod Path =
    Def Type = Global
    (int#251:4-7 -> int)
    Range: File "", line 251, characters 4-7
    Body Range: File "", line 251, characters 44-96
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 313, characters 97-100 ,
      File "", line 351, characters 79-82 ,
      File "", line 353, characters 78-81 ,
      File "", line 355, characters 72-75
    Mod Path =
    Def Type = Global
    (nat#252:4-7 -> nat)
    Range: File "", line 252, characters 4-7
    Body Range: File "", line 252, characters 28-73
    Content: |core: bytes -> nat|
    references: []
    Mod Path =
    Def Type = Global
    (bytes#253:4-9 -> bytes)
    Range: File "", line 253, characters 4-9
    Body Range: File "", line 253, characters 48-104
    Content: |core: ∀ a : * . a -> external_bytes (a)|
    references: []
    Mod Path =
    Def Type = Global
    (ignore#254:4-10 -> ignore)
    Range: File "", line 254, characters 4-10
    Body Range: File "", line 254, characters 37-39
    Content: |core: ∀ a : * . a -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (curry#255:4-9 -> curry)
    Range: File "", line 255, characters 4-9
    Body Range: File "", line 255, characters 62-70
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    Mod Path =
    Def Type = Global
    (uncurry#256:4-11 -> uncurry)
    Range: File "", line 256, characters 4-11
    Body Range: File "", line 256, characters 62-73
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 427, characters 30-37
    Mod Path =
    Def Type = Global
    (assert_with_error#258:4-21 -> assert_with_error)
    Range: File "", line 258, characters 4-21
    Body Range: File "", line 258, characters 48-76
    Content: |unresolved|
    references: []
    Mod Path =
    Def Type = Global
    (assert_some_with_error#259:4-26 -> assert_some_with_error)
    Range: File "", line 259, characters 4-26
    Body Range: File "", line 259, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (assert_none_with_error#260:4-26 -> assert_none_with_error)
    Range: File "", line 260, characters 4-26
    Body Range: File "", line 260, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (ediv#261:4-8 -> ediv)
    Range: File "", line 261, characters 4-8
    Body Range: File "", line 261, characters 61-117
    Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_ediv (a ,
    b)|
    references: []
    Mod Path =
    Def Type = Global
    (a#1:4-5 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    Mod Path =
    Def Type = Global
    (b#3:4-5 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    Mod Path =
    Def Type = Global
    (c#5:10-11 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    Mod Path =
    Def Type = Parameter
    (d#5:26-27 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    Mod Path =
    Def Type = Local
    (e#6:9-10 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    Mod Path =
    Def Type = Local
    Type definitions:
    (string#3:5-11 -> string)
    Range: File "", line 3, characters 5-11
    Body Range: File "", line 3, characters 14-32
    Content: : |string|
    references:
      File "", line 80, characters 44-50 ,
      File "", line 82, characters 48-54 ,
      File "", line 83, characters 48-54 ,
      File "", line 92, characters 58-64 ,
      File "", line 98, characters 51-57 ,
      File "", line 100, characters 62-68 ,
      File "", line 108, characters 58-64 ,
      File "", line 110, characters 65-71 ,
      File "", line 111, characters 54-60 ,
      File "", line 114, characters 44-50 ,
      File "", line 116, characters 61-67 ,
      File "", line 207, characters 18-24 ,
      File "", line 208, characters 20-26 ,
      File "", line 208, characters 35-41 ,
      File "", line 210, characters 19-25 ,
      File "", line 210, characters 33-39 ,
      File "", line 210, characters 43-49 ,
      File "", line 211, characters 35-41 ,
      File "", line 211, characters 45-51 ,
      File "", line 217, characters 52-58 ,
      File "", line 258, characters 38-44 ,
      File "", line 259, characters 56-62 ,
      File "", line 260, characters 56-62 ,
      File "", line 278, characters 13-19 ,
      File "", line 306, characters 17-23 ,
      File "", line 307, characters 18-24 ,
      File "", line 313, characters 56-62 ,
      File "", line 319, characters 31-37 ,
      File "", line 326, characters 50-56 ,
      File "", line 328, characters 41-47 ,
      File "", line 329, characters 27-33 ,
      File "", line 333, characters 35-41 ,
      File "", line 334, characters 33-39 ,
      File "", line 347, characters 36-42 ,
      File "", line 348, characters 22-28 ,
      File "", line 359, characters 19-25 ,
      File "", line 383, characters 76-82 ,
      File "", line 391, characters 140-146 ,
      File "", line 392, characters 135-141 ,
      File "", line 401, characters 25-31 ,
      File "", line 401, characters 50-56 ,
      File "", line 402, characters 17-23 ,
      File "", line 403, characters 23-29 ,
      File "", line 404, characters 25-31 ,
      File "", line 408, characters 12-18 ,
      File "", line 413, characters 14-20 ,
      File "", line 417, characters 38-44 ,
      File "", line 450, characters 39-45 ,
      File "", line 453, characters 32-38 ,
      File "", line 482, characters 52-58 ,
      File "", line 502, characters 56-62 ,
      File "", line 571, characters 40-46 ,
      File "", line 572, characters 58-64 ,
      File "", line 573, characters 58-64
    (bytes#4:5-10 -> bytes)
    Range: File "", line 4, characters 5-10
    Body Range: File "", line 4, characters 13-30
    Content: : |bytes|
    references:
      File "", line 117, characters 121-126 ,
      File "", line 117, characters 219-224 ,
      File "", line 226, characters 20-25 ,
      File "", line 226, characters 34-39 ,
      File "", line 227, characters 30-35 ,
      File "", line 227, characters 70-75 ,
      File "", line 228, characters 27-32 ,
      File "", line 229, characters 18-23 ,
      File "", line 231, characters 19-24 ,
      File "", line 231, characters 32-37 ,
      File "", line 231, characters 41-46 ,
      File "", line 232, characters 35-40 ,
      File "", line 232, characters 44-49 ,
      File "", line 236, characters 19-24 ,
      File "", line 236, characters 28-33 ,
      File "", line 236, characters 71-76 ,
      File "", line 237, characters 18-23 ,
      File "", line 237, characters 27-32 ,
      File "", line 237, characters 69-74 ,
      File "", line 238, characters 18-23 ,
      File "", line 238, characters 27-32 ,
      File "", line 238, characters 69-74 ,
      File "", line 239, characters 16-21 ,
      File "", line 239, characters 25-30 ,
      File "", line 239, characters 65-70 ,
      File "", line 240, characters 18-23 ,
      File "", line 240, characters 27-32 ,
      File "", line 240, characters 69-74 ,
      File "", line 242, characters 43-48 ,
      File "", line 252, characters 13-18 ,
      File "", line 252, characters 57-62 ,
      File "", line 402, characters 30-35
    (int#5:5-8 -> int)
    Range: File "", line 5, characters 5-8
    Body Range: File "", line 5, characters 11-26
    Content: : |int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 14-17 ,
      File "", line 117, characters 130-133 ,
      File "", line 117, characters 228-231 ,
      File "", line 248, characters 13-16 ,
      File "", line 249, characters 16-19 ,
      File "", line 283, characters 16-19 ,
      File "", line 310, characters 33-36 ,
      File "", line 342, characters 38-41 ,
      File "", line 405, characters 34-37 ,
      File "", line 426, characters 127-130 ,
      File "", line 436, characters 136-139 ,
      File "", line 443, characters 134-137 ,
      File "", line 453, characters 108-111 ,
      File "", line 483, characters 78-81 ,
      File "", line 503, characters 82-85 ,
      File "", line 523, characters 90-93 ,
      File "", line 545, characters 94-97
    (nat#6:5-8 -> nat)
    Range: File "", line 6, characters 5-8
    Body Range: File "", line 6, characters 11-26
    Content: : |nat|
    references:
      File "", line 66, characters 30-33 ,
      File "", line 66, characters 67-70 ,
      File "", line 69, characters 43-46 ,
      File "", line 69, characters 93-96 ,
      File "", line 70, characters 39-42 ,
      File "", line 70, characters 85-88 ,
      File "", line 71, characters 37-40 ,
      File "", line 71, characters 84-87 ,
      File "", line 75, characters 60-63 ,
      File "", line 76, characters 66-69 ,
      File "", line 95, characters 42-45 ,
      File "", line 95, characters 114-117 ,
      File "", line 101, characters 48-51 ,
      File "", line 101, characters 54-57 ,
      File "", line 145, characters 41-44 ,
      File "", line 169, characters 34-37 ,
      File "", line 170, characters 38-41 ,
      File "", line 185, characters 38-41 ,
      File "", line 186, characters 36-39 ,
      File "", line 207, characters 28-31 ,
      File "", line 211, characters 15-18 ,
      File "", line 211, characters 25-28 ,
      File "", line 229, characters 27-30 ,
      File "", line 232, characters 15-18 ,
      File "", line 232, characters 25-28 ,
      File "", line 248, characters 20-23 ,
      File "", line 248, characters 57-60 ,
      File "", line 249, characters 23-26 ,
      File "", line 249, characters 69-72 ,
      File "", line 252, characters 22-25 ,
      File "", line 252, characters 66-69 ,
      File "", line 280, characters 35-38 ,
      File "", line 290, characters 86-89 ,
      File "", line 300, characters 43-46 ,
      File "", line 308, characters 41-44 ,
      File "", line 309, characters 34-37 ,
      File "", line 313, characters 33-36 ,
      File "", line 314, characters 50-53 ,
      File "", line 321, characters 34-37 ,
      File "", line 348, characters 15-18 ,
      File "", line 369, characters 48-51 ,
      File "", line 370, characters 31-34 ,
      File "", line 392, characters 69-72 ,
      File "", line 397, characters 23-26 ,
      File "", line 398, characters 40-43 ,
      File "", line 400, characters 33-36 ,
      File "", line 411, characters 77-80 ,
      File "", line 451, characters 90-93 ,
      File "", line 461, characters 30-33 ,
      File "", line 473, characters 58-61 ,
      File "", line 489, characters 90-93 ,
      File "", line 492, characters 30-33 ,
      File "", line 509, characters 90-93 ,
      File "", line 512, characters 58-61 ,
      File "", line 534, characters 30-33 ,
      File "", line 556, characters 58-61 ,
      File "", line 578, characters 26-29 ,
      File "", line 591, characters 28-31 ,
      File "", line 600, characters 35-38 ,
      File "", line 603, characters 33-36 ,
      File "", line 611, characters 29-32 ,
      File "", line 616, characters 28-31 ,
      File "", line 619, characters 35-38 ,
      File "", line 631, characters 28-31 ,
      File "", line 634, characters 35-38
    (unit#7:5-9 -> unit)
    Range: File "", line 7, characters 5-9
    Body Range: File "", line 7, characters 12-28
    Content: : |unit|
    references:
      File "", line 54, characters 12-24 ,
      File "", line 55, characters 17-34 ,
      File "", line 61, characters 24-28 ,
      File "", line 62, characters 23-27 ,
      File "", line 63, characters 20-24 ,
      File "", line 64, characters 23-27 ,
      File "", line 65, characters 23-27 ,
      File "", line 66, characters 22-26 ,
      File "", line 67, characters 29-33 ,
      File "", line 68, characters 25-29 ,
      File "", line 69, characters 35-39 ,
      File "", line 70, characters 31-35 ,
      File "", line 73, characters 41-45 ,
      File "", line 73, characters 102-106 ,
      File "", line 99, characters 12-16 ,
      File "", line 109, characters 12-16 ,
      File "", line 115, characters 12-16 ,
      File "", line 155, characters 36-40 ,
      File "", line 155, characters 61-65 ,
      File "", line 177, characters 30-34 ,
      File "", line 177, characters 50-54 ,
      File "", line 191, characters 30-34 ,
      File "", line 191, characters 51-55 ,
      File "", line 245, characters 24-28 ,
      File "", line 246, characters 42-46 ,
      File "", line 247, characters 42-46 ,
      File "", line 250, characters 21-25 ,
      File "", line 254, characters 30-34 ,
      File "", line 259, characters 66-70 ,
      File "", line 260, characters 66-70 ,
      File "", line 288, characters 21-41 ,
      File "", line 300, characters 35-39 ,
      File "", line 303, characters 33-37 ,
      File "", line 306, characters 27-31 ,
      File "", line 307, characters 28-32 ,
      File "", line 315, characters 29-33 ,
      File "", line 316, characters 28-32 ,
      File "", line 319, characters 23-27 ,
      File "", line 321, characters 41-45 ,
      File "", line 322, characters 21-25 ,
      File "", line 324, characters 42-46 ,
      File "", line 325, characters 40-44 ,
      File "", line 330, characters 27-31 ,
      File "", line 330, characters 35-39 ,
      File "", line 331, characters 24-28 ,
      File "", line 331, characters 32-36 ,
      File "", line 332, characters 24-28 ,
      File "", line 332, characters 32-36 ,
      File "", line 340, characters 50-54 ,
      File "", line 341, characters 32-36 ,
      File "", line 359, characters 29-33 ,
      File "", line 362, characters 28-32 ,
      File "", line 362, characters 36-40 ,
      File "", line 363, characters 30-34 ,
      File "", line 363, characters 38-42 ,
      File "", line 393, characters 29-33 ,
      File "", line 397, characters 45-49 ,
      File "", line 398, characters 62-66 ,
      File "", line 399, characters 90-94 ,
      File "", line 403, characters 43-47 ,
      File "", line 404, characters 58-62 ,
      File "", line 405, characters 62-66 ,
      File "", line 420, characters 20-22 ,
      File "", line 459, characters 31-35 ,
      File "", line 459, characters 47-51 ,
      File "", line 460, characters 20-62 ,
      File "", line 463, characters 44-48 ,
      File "", line 463, characters 98-102 ,
      File "", line 471, characters 31-35 ,
      File "", line 471, characters 47-51 ,
      File "", line 472, characters 20-62 ,
      File "", line 475, characters 44-48 ,
      File "", line 475, characters 98-102 ,
      File "", line 490, characters 31-35 ,
      File "", line 490, characters 47-51 ,
      File "", line 491, characters 20-62 ,
      File "", line 495, characters 44-48 ,
      File "", line 495, characters 103-107 ,
      File "", line 510, characters 31-35 ,
      File "", line 510, characters 47-51 ,
      File "", line 511, characters 20-62 ,
      File "", line 515, characters 44-48 ,
      File "", line 515, characters 103-107 ,
      File "", line 532, characters 31-35 ,
      File "", line 532, characters 47-51 ,
      File "", line 533, characters 20-62 ,
      File "", line 537, characters 44-48 ,
      File "", line 537, characters 103-107 ,
      File "", line 554, characters 31-35 ,
      File "", line 554, characters 47-51 ,
      File "", line 555, characters 20-62 ,
      File "", line 559, characters 44-48 ,
      File "", line 559, characters 103-107 ,
      File "", line 567, characters 26-30 ,
      File "", line 568, characters 44-48 ,
      File "", line 569, characters 44-48 ,
      File "", line 572, characters 68-72 ,
      File "", line 573, characters 68-72 ,
      File "", line 579, characters 20-24 ,
      File "", line 580, characters 27-31 ,
      File "", line 600, characters 52-56 ,
      File "", line 603, characters 51-55 ,
      File "", line 603, characters 76-80
    (operation#10:5-14 -> operation)
    Range: File "", line 10, characters 5-14
    Body Range: File "", line 10, characters 17-38
    Content: : |operation|
    references:
      File "", line 57, characters 38-47 ,
      File "", line 79, characters 43-52 ,
      File "", line 79, characters 95-104 ,
      File "", line 96, characters 65-74 ,
      File "", line 97, characters 87-96 ,
      File "", line 103, characters 67-76 ,
      File "", line 103, characters 131-140 ,
      File "", line 106, characters 76-85 ,
      File "", line 106, characters 140-149 ,
      File "", line 114, characters 62-71 ,
      File "", line 116, characters 91-100 ,
      File "", line 292, characters 44-53 ,
      File "", line 343, characters 48-57 ,
      File "", line 399, characters 50-59 ,
      File "", line 426, characters 42-51 ,
      File "", line 433, characters 61-70 ,
      File "", line 436, characters 51-60 ,
      File "", line 580, characters 10-19 ,
      File "", line 590, characters 39-48 ,
      File "", line 593, characters 10-19 ,
      File "", line 603, characters 59-68 ,
      File "", line 618, characters 39-48 ,
      File "", line 619, characters 60-69 ,
      File "", line 633, characters 40-49 ,
      File "", line 634, characters 60-69
    (tez#11:5-8 -> tez)
    Range: File "", line 11, characters 5-8
    Body Range: File "", line 11, characters 11-26
    Content: : |tez|
    references:
      File "", line 61, characters 32-35 ,
      File "", line 61, characters 71-74 ,
      File "", line 62, characters 31-34 ,
      File "", line 62, characters 69-72 ,
      File "", line 96, characters 41-44 ,
      File "", line 97, characters 67-70 ,
      File "", line 103, characters 115-118 ,
      File "", line 106, characters 124-127 ,
      File "", line 273, characters 52-55 ,
      File "", line 273, characters 74-77 ,
      File "", line 305, characters 34-37 ,
      File "", line 325, characters 33-36 ,
      File "", line 391, characters 58-61 ,
      File "", line 392, characters 62-65 ,
      File "", line 397, characters 33-36 ,
      File "", line 398, characters 50-53 ,
      File "", line 399, characters 83-86 ,
      File "", line 404, characters 44-47 ,
      File "", line 406, characters 66-69 ,
      File "", line 411, characters 70-73 ,
      File "", line 425, characters 79-82 ,
      File "", line 426, characters 75-78 ,
      File "", line 436, characters 84-87 ,
      File "", line 443, characters 82-85 ,
      File "", line 453, characters 70-73 ,
      File "", line 482, characters 89-92 ,
      File "", line 502, characters 93-96 ,
      File "", line 522, characters 95-98 ,
      File "", line 544, characters 99-102
    (address#12:5-12 -> address)
    Range: File "", line 12, characters 5-12
    Body Range: File "", line 12, characters 15-34
    Content: : |address|
    references:
      File "", line 64, characters 31-38 ,
      File "", line 64, characters 73-80 ,
      File "", line 65, characters 31-38 ,
      File "", line 65, characters 73-80 ,
      File "", line 67, characters 37-44 ,
      File "", line 67, characters 85-92 ,
      File "", line 72, characters 42-49 ,
      File "", line 72, characters 87-94 ,
      File "", line 75, characters 45-52 ,
      File "", line 76, characters 51-58 ,
      File "", line 86, characters 56-63 ,
      File "", line 88, characters 52-59 ,
      File "", line 92, characters 44-51 ,
      File "", line 98, characters 72-79 ,
      File "", line 103, characters 143-150 ,
      File "", line 106, characters 152-159 ,
      File "", line 108, characters 71-78 ,
      File "", line 111, characters 67-74 ,
      File "", line 273, characters 23-30 ,
      File "", line 276, characters 36-43 ,
      File "", line 284, characters 18-25 ,
      File "", line 285, characters 17-24 ,
      File "", line 290, characters 54-61 ,
      File "", line 303, characters 22-29 ,
      File "", line 304, characters 34-41 ,
      File "", line 305, characters 23-30 ,
      File "", line 309, characters 41-48 ,
      File "", line 310, characters 40-47 ,
      File "", line 313, characters 40-47 ,
      File "", line 315, characters 38-45 ,
      File "", line 315, characters 47-54 ,
      File "", line 323, characters 35-42 ,
      File "", line 337, characters 12-19 ,
      File "", line 341, characters 21-28 ,
      File "", line 385, characters 21-28 ,
      File "", line 386, characters 45-52 ,
      File "", line 391, characters 20-27 ,
      File "", line 392, characters 24-31 ,
      File "", line 407, characters 12-19 ,
      File "", line 412, characters 14-21 ,
      File "", line 425, characters 86-93 ,
      File "", line 453, characters 77-84 ,
      File "", line 483, characters 47-54 ,
      File "", line 503, characters 51-58 ,
      File "", line 578, characters 33-40 ,
      File "", line 592, characters 22-29 ,
      File "", line 593, characters 27-34 ,
      File "", line 600, characters 42-49 ,
      File "", line 603, characters 40-47 ,
      File "", line 611, characters 36-43 ,
      File "", line 618, characters 67-74 ,
      File "", line 619, characters 42-49 ,
      File "", line 619, characters 77-84 ,
      File "", line 622, characters 68-75 ,
      File "", line 633, characters 68-75 ,
      File "", line 634, characters 42-49 ,
      File "", line 634, characters 77-84 ,
      File "", line 637, characters 68-75
    (signature#13:5-14 -> signature)
    Range: File "", line 13, characters 5-14
    Body Range: File "", line 13, characters 17-38
    Content: : |signature|
    references:
      File "", line 242, characters 27-36 ,
      File "", line 402, characters 39-48
    (key#14:5-8 -> key)
    Range: File "", line 14, characters 5-8
    Body Range: File "", line 14, characters 11-26
    Content: : |key|
    references:
      File "", line 241, characters 20-23 ,
      File "", line 242, characters 17-20 ,
      File "", line 313, characters 50-53 ,
      File "", line 319, characters 40-43 ,
      File "", line 403, characters 36-39 ,
      File "", line 404, characters 34-37
    (key_hash#15:5-13 -> key_hash)
    Range: File "", line 15, characters 5-13
    Body Range: File "", line 15, characters 16-36
    Content: : |key_hash|
    references:
      File "", line 71, characters 25-33 ,
      File "", line 73, characters 29-37 ,
      File "", line 79, characters 24-32 ,
      File "", line 103, characters 93-101 ,
      File "", line 106, characters 102-110 ,
      File "", line 241, characters 27-35 ,
      File "", line 241, characters 74-82 ,
      File "", line 308, characters 29-37 ,
      File "", line 324, characters 30-38 ,
      File "", line 325, characters 18-26 ,
      File "", line 597, characters 54-62
    (timestamp#16:5-14 -> timestamp)
    Range: File "", line 16, characters 5-14
    Body Range: File "", line 16, characters 17-38
    Content: : |timestamp|
    references:
      File "", line 63, characters 28-37 ,
      File "", line 63, characters 69-78 ,
      File "", line 322, characters 29-38 ,
      File "", line 397, characters 92-101 ,
      File "", line 398, characters 24-33
    (list#17:5-9 -> list)
    Range: File "", line 17, characters 5-9
    Body Range: File "", line 17, characters 12-28
    Content: : |list|
    references:
      File "", line 57, characters 48-52 ,
      File "", line 78, characters 55-59 ,
      File "", line 103, characters 77-81 ,
      File "", line 106, characters 86-90 ,
      File "", line 131, characters 57-61 ,
      File "", line 146, characters 57-61 ,
      File "", line 171, characters 49-53 ,
      File "", line 185, characters 30-34 ,
      File "", line 186, characters 28-32 ,
      File "", line 187, characters 32-36 ,
      File "", line 188, characters 32-36 ,
      File "", line 188, characters 43-47 ,
      File "", line 190, characters 42-46 ,
      File "", line 190, characters 52-56 ,
      File "", line 191, characters 44-48 ,
      File "", line 192, characters 47-51 ,
      File "", line 193, characters 60-64 ,
      File "", line 194, characters 53-57 ,
      File "", line 195, characters 36-40 ,
      File "", line 195, characters 46-50 ,
      File "", line 196, characters 48-52 ,
      File "", line 198, characters 56-60 ,
      File "", line 198, characters 66-70 ,
      File "", line 199, characters 31-35 ,
      File "", line 200, characters 50-54 ,
      File "", line 200, characters 60-64 ,
      File "", line 202, characters 59-63 ,
      File "", line 202, characters 69-73 ,
      File "", line 208, characters 27-31 ,
      File "", line 226, characters 26-30 ,
      File "", line 285, characters 25-29 ,
      File "", line 292, characters 54-58 ,
      File "", line 315, characters 55-59 ,
      File "", line 343, characters 58-62 ,
      File "", line 383, characters 88-92 ,
      File "", line 385, characters 34-38 ,
      File "", line 386, characters 37-41 ,
      File "", line 386, characters 63-67 ,
      File "", line 390, characters 33-37 ,
      File "", line 397, characters 37-41 ,
      File "", line 398, characters 54-58 ,
      File "", line 399, characters 60-64 ,
      File "", line 426, characters 52-56 ,
      File "", line 433, characters 71-75 ,
      File "", line 436, characters 61-65 ,
      File "", line 470, characters 78-82 ,
      File "", line 473, characters 47-51 ,
      File "", line 473, characters 80-84 ,
      File "", line 481, characters 38-42 ,
      File "", line 503, characters 109-113 ,
      File "", line 512, characters 47-51 ,
      File "", line 512, characters 80-84 ,
      File "", line 521, characters 38-42 ,
      File "", line 545, characters 121-125 ,
      File "", line 556, characters 47-51 ,
      File "", line 556, characters 80-84 ,
      File "", line 565, characters 38-42 ,
      File "", line 580, characters 20-24 ,
      File "", line 590, characters 49-53 ,
      File "", line 593, characters 20-24 ,
      File "", line 603, characters 69-73 ,
      File "", line 618, characters 49-53 ,
      File "", line 619, characters 70-74 ,
      File "", line 633, characters 50-54 ,
      File "", line 634, characters 70-74
    (big_map#18:5-12 -> big_map)
    Range: File "", line 18, characters 5-12
    Body Range: File "", line 18, characters 15-34
    Content: : |big_map|
    references:
      File "", line 130, characters 42-49 ,
      File "", line 131, characters 72-79 ,
      File "", line 133, characters 41-48 ,
      File "", line 134, characters 49-56 ,
      File "", line 134, characters 67-74 ,
      File "", line 135, characters 44-51 ,
      File "", line 135, characters 62-69 ,
      File "", line 136, characters 59-66 ,
      File "", line 136, characters 77-84 ,
      File "", line 137, characters 67-74 ,
      File "", line 137, characters 96-103 ,
      File "", line 138, characters 46-53 ,
      File "", line 139, characters 42-49 ,
      File "", line 405, characters 51-58
    (map#19:5-8 -> map)
    Range: File "", line 19, characters 5-8
    Body Range: File "", line 19, characters 11-26
    Content: : |map|
    references:
      File "", line 144, characters 32-35 ,
      File "", line 145, characters 34-37 ,
      File "", line 146, characters 72-75 ,
      File "", line 148, characters 41-44 ,
      File "", line 149, characters 49-52 ,
      File "", line 149, characters 63-66 ,
      File "", line 150, characters 44-47 ,
      File "", line 150, characters 58-61 ,
      File "", line 151, characters 59-62 ,
      File "", line 151, characters 73-76 ,
      File "", line 152, characters 67-70 ,
      File "", line 152, characters 92-95 ,
      File "", line 153, characters 42-45 ,
      File "", line 154, characters 46-49 ,
      File "", line 155, characters 54-57 ,
      File "", line 156, characters 52-55 ,
      File "", line 156, characters 66-69 ,
      File "", line 157, characters 59-62 ,
      File "", line 315, characters 61-64
    (set#20:5-8 -> set)
    Range: File "", line 20, characters 5-8
    Body Range: File "", line 20, characters 11-26
    Content: : |set|
    references:
      File "", line 168, characters 25-28 ,
      File "", line 169, characters 27-30 ,
      File "", line 170, characters 31-34 ,
      File "", line 171, characters 59-62 ,
      File "", line 173, characters 34-37 ,
      File "", line 174, characters 34-37 ,
      File "", line 174, characters 43-46 ,
      File "", line 175, characters 37-40 ,
      File "", line 175, characters 46-49 ,
      File "", line 176, characters 48-51 ,
      File "", line 177, characters 43-46 ,
      File "", line 178, characters 46-49 ,
      File "", line 179, characters 51-54 ,
      File "", line 180, characters 56-59 ,
      File "", line 180, characters 65-68 ,
      File "", line 181, characters 30-33 ,
      File "", line 181, characters 106-109
    (contract#21:5-13 -> contract)
    Range: File "", line 21, characters 5-13
    Body Range: File "", line 21, characters 16-36
    Content: : |contract|
    references:
      File "", line 72, characters 30-38 ,
      File "", line 73, characters 46-54 ,
      File "", line 73, characters 107-115 ,
      File "", line 80, characters 56-64 ,
      File "", line 82, characters 60-68 ,
      File "", line 86, characters 70-78 ,
      File "", line 87, characters 74-82 ,
      File "", line 88, characters 66-74 ,
      File "", line 92, characters 70-78 ,
      File "", line 96, characters 53-61 ,
      File "", line 97, characters 75-83 ,
      File "", line 108, characters 84-92 ,
      File "", line 110, characters 98-106 ,
      File "", line 111, characters 80-88 ,
      File "", line 302, characters 60-68 ,
      File "", line 327, characters 41-49 ,
      File "", line 336, characters 14-22 ,
      File "", line 406, characters 43-51 ,
      File "", line 411, characters 47-55 ,
      File "", line 417, characters 77-85 ,
      File "", line 584, characters 22-30
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
      File "", line 68, characters 33-41 ,
      File "", line 68, characters 78-86
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
      File "", line 84, characters 66-79 ,
      File "", line 85, characters 90-103 ,
      File "", line 117, characters 103-116 ,
      File "", line 117, characters 142-155 ,
      File "", line 117, characters 240-253
    (sapling_transaction#28:5-24 -> sapling_transaction)
    Range: File "", line 28, characters 5-24
    Body Range: File "", line 28, characters 27-58
    Content: : |sapling_transaction|
    references: File "", line 117, characters 71-90
    (baker_operation#29:5-20 -> baker_operation)
    Range: File "", line 29, characters 5-20
    Body Range: File "", line 29, characters 23-50
    Content: : |baker_operation|
    references: []
    (bls12_381_g1#30:5-17 -> bls12_381_g1)
    Range: File "", line 30, characters 5-17
    Body Range: File "", line 30, characters 20-44
    Content: : |bls12_381_g1|
    references: File "", line 78, characters 26-38
    (bls12_381_g2#31:5-17 -> bls12_381_g2)
    Range: File "", line 31, characters 5-17
    Body Range: File "", line 31, characters 20-44
    Content: : |bls12_381_g2|
    references: File "", line 78, characters 41-53
    (bls12_381_fr#32:5-17 -> bls12_381_fr)
    Range: File "", line 32, characters 5-17
    Body Range: File "", line 32, characters 20-44
    Content: : |bls12_381_fr|
    references: []
    (never#33:5-10 -> never)
    Range: File "", line 33, characters 5-10
    Body Range: File "", line 33, characters 13-30
    Content: : |never|
    references: File "", line 77, characters 26-31
    (ticket#34:5-11 -> ticket)
    Range: File "", line 34, characters 5-11
    Body Range: File "", line 34, characters 14-32
    Content: : |ticket|
    references:
      File "", line 74, characters 35-41 ,
      File "", line 74, characters 46-52 ,
      File "", line 74, characters 59-65 ,
      File "", line 74, characters 118-124 ,
      File "", line 75, characters 34-40 ,
      File "", line 75, characters 70-76 ,
      File "", line 76, characters 76-82 ,
      File "", line 95, characters 52-58 ,
      File "", line 95, characters 124-130 ,
      File "", line 101, characters 35-41 ,
      File "", line 101, characters 64-70 ,
      File "", line 101, characters 75-81 ,
      File "", line 102, characters 49-55 ,
      File "", line 102, characters 60-66 ,
      File "", line 577, characters 23-29 ,
      File "", line 589, characters 25-31 ,
      File "", line 602, characters 54-60 ,
      File "", line 617, characters 26-32 ,
      File "", line 632, characters 26-32
    (external_bytes#37:5-19 -> external_bytes)
    Range: File "", line 37, characters 5-19
    Body Range: File "", line 37, characters 22-48
    Content: : |external_bytes|
    references:
      File "", line 253, characters 31-45 ,
      File "", line 253, characters 86-100
    (external_int#38:5-17 -> external_int)
    Range: File "", line 38, characters 5-17
    Body Range: File "", line 38, characters 20-44
    Content: : |external_int|
    references:
      File "", line 251, characters 29-41 ,
      File "", line 251, characters 80-92
    (external_ediv#39:5-18 -> external_ediv)
    Range: File "", line 39, characters 5-18
    Body Range: File "", line 39, characters 21-46
    Content: : |external_ediv|
    references:
      File "", line 261, characters 45-58 ,
      File "", line 261, characters 102-115
    (external_and#40:5-17 -> external_and)
    Range: File "", line 40, characters 5-17
    Body Range: File "", line 40, characters 20-44
    Content: : |external_and|
    references:
      File "", line 122, characters 54-66 ,
      File "", line 122, characters 123-135
    (external_or#41:5-16 -> external_or)
    Range: File "", line 41, characters 5-16
    Body Range: File "", line 41, characters 19-42
    Content: : |external_or|
    references:
      File "", line 123, characters 54-65 ,
      File "", line 123, characters 123-134
    (external_xor#42:5-17 -> external_xor)
    Range: File "", line 42, characters 5-17
    Body Range: File "", line 42, characters 20-44
    Content: : |external_xor|
    references:
      File "", line 124, characters 54-66 ,
      File "", line 124, characters 123-135
    (external_lsl#43:5-17 -> external_lsl)
    Range: File "", line 43, characters 5-17
    Body Range: File "", line 43, characters 20-44
    Content: : |external_lsl|
    references:
      File "", line 125, characters 54-66 ,
      File "", line 125, characters 123-135
    (external_lsr#44:5-17 -> external_lsr)
    Range: File "", line 44, characters 5-17
    Body Range: File "", line 44, characters 20-44
    Content: : |external_lsr|
    references:
      File "", line 126, characters 54-66 ,
      File "", line 126, characters 123-135
    (external_map_find_opt#45:5-26 -> external_map_find_opt)
    Range: File "", line 45, characters 5-26
    Body Range: File "", line 45, characters 29-62
    Content: : |external_map_find_opt|
    references:
      File "", line 162, characters 55-76 ,
      File "", line 162, characters 119-140
    (external_map_add#46:5-21 -> external_map_add)
    Range: File "", line 46, characters 5-21
    Body Range: File "", line 46, characters 24-52
    Content: : |external_map_add|
    references:
      File "", line 163, characters 63-79 ,
      File "", line 163, characters 145-161
    (external_map_remove#47:5-24 -> external_map_remove)
    Range: File "", line 47, characters 5-24
    Body Range: File "", line 47, characters 27-58
    Content: : |external_map_remove|
    references:
      File "", line 164, characters 53-72 ,
      File "", line 164, characters 197-216
    (external_map_remove_value#48:5-30 -> external_map_remove_value)
    Range: File "", line 48, characters 5-30
    Body Range: File "", line 48, characters 33-70
    Content: : |external_map_remove_value|
    references: File "", line 164, characters 149-174
    (bool#54:5-9 -> bool)
    Range: File "", line 54, characters 5-9
    Body Range: File "", line 54, characters 12-24
    Content: : |bool|
    references:
      File "", line 78, characters 63-67 ,
      File "", line 78, characters 111-115 ,
      File "", line 133, characters 52-56 ,
      File "", line 148, characters 48-52 ,
      File "", line 173, characters 41-45 ,
      File "", line 176, characters 35-39 ,
      File "", line 196, characters 34-38 ,
      File "", line 202, characters 37-41 ,
      File "", line 221, characters 40-44 ,
      File "", line 222, characters 40-44 ,
      File "", line 242, characters 52-56 ,
      File "", line 242, characters 106-110 ,
      File "", line 245, characters 16-20 ,
      File "", line 258, characters 27-31 ,
      File "", line 287, characters 41-45 ,
      File "", line 368, characters 53-57 ,
      File "", line 416, characters 74-78 ,
      File "", line 567, characters 18-22 ,
      File "", line 571, characters 29-33 ,

    (option#55:8-14 -> option)
    Range: File "", line 55, characters 8-14
    Body Range: File "", line 55, characters 17-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 74, characters 67-73 ,
      File "", line 74, characters 125-131 ,
      File "", line 79, characters 33-39 ,
      File "", line 81, characters 14-20 ,
      File "", line 85, characters 74-80 ,
      File "", line 86, characters 80-86 ,
      File "", line 87, characters 59-65 ,
      File "", line 87, characters 84-90 ,
      File "", line 95, characters 60-66 ,
      File "", line 95, characters 132-138 ,
      File "", line 98, characters 86-92 ,
      File "", line 100, characters 80-86 ,
      File "", line 100, characters 96-102 ,
      File "", line 101, characters 83-89 ,
      File "", line 102, characters 68-74 ,
      File "", line 103, characters 102-108 ,
      File "", line 106, characters 111-117 ,
      File "", line 108, characters 93-99 ,
      File "", line 110, characters 83-89 ,
      File "", line 110, characters 108-114 ,
      File "", line 116, characters 79-85 ,
      File "", line 117, characters 158-164 ,
      File "", line 117, characters 256-262 ,
      File "", line 136, characters 39-45 ,
      File "", line 137, characters 47-53 ,
      File "", line 137, characters 80-86 ,
      File "", line 138, characters 59-65 ,
      File "", line 151, characters 39-45 ,
      File "", line 152, characters 47-53 ,
      File "", line 152, characters 76-82 ,
      File "", line 154, characters 55-61 ,
      File "", line 164, characters 176-182 ,
      File "", line 180, characters 40-46 ,
      File "", line 187, characters 42-48 ,
      File "", line 188, characters 49-55 ,
      File "", line 196, characters 58-64 ,
      File "", line 197, characters 31-37 ,
      File "", line 198, characters 40-46 ,
      File "", line 200, characters 34-40 ,
      File "", line 215, characters 28-34 ,
      File "", line 217, characters 39-45 ,
      File "", line 218, characters 50-56 ,
      File "", line 218, characters 62-68 ,
      File "", line 219, characters 42-48 ,
      File "", line 220, characters 48-54 ,
      File "", line 221, characters 30-36 ,
      File "", line 222, characters 30-36 ,
      File "", line 228, characters 38-44 ,
      File "", line 228, characters 100-106 ,
      File "", line 228, characters 114-120 ,
      File "", line 246, characters 32-38 ,
      File "", line 247, characters 32-38 ,
      File "", line 249, characters 27-33 ,
      File "", line 249, characters 73-79 ,
      File "", line 259, characters 43-49 ,
      File "", line 260, characters 43-49 ,
      File "", line 348, characters 29-35 ,
      File "", line 391, characters 147-153 ,
      File "", line 392, characters 142-148 ,
      File "", line 397, characters 102-108 ,
      File "", line 400, characters 63-69 ,
      File "", line 401, characters 57-63 ,
      File "", line 404, characters 48-54 ,
      File "", line 408, characters 19-25 ,
      File "", line 413, characters 21-27 ,
      File "", line 451, characters 94-100 ,
      File "", line 458, characters 74-80 ,
      File "", line 461, characters 52-58 ,
      File "", line 483, characters 105-111 ,
      File "", line 489, characters 94-100 ,
      File "", line 492, characters 52-58 ,
      File "", line 509, characters 94-100 ,
      File "", line 523, characters 117-123 ,
      File "", line 534, characters 52-58 ,
      File "", line 568, characters 34-40 ,
      File "", line 569, characters 34-40 ,
      File "", line 572, characters 45-51 ,
      File "", line 573, characters 45-51 ,
      File "", line 592, characters 30-36 ,
      File "", line 593, characters 35-41 ,
      File "", line 597, characters 63-69 ,
      File "", line 619, characters 50-56 ,
      File "", line 619, characters 85-91 ,
      File "", line 622, characters 76-82 ,
      File "", line 634, characters 50-56 ,
      File "", line 634, characters 85-91 ,
      File "", line 637, characters 76-82
    (entrypoint#57:14-24 -> entrypoint)
    Range: File "", line 57, characters 14-24
    Body Range: File "", line 57, characters 27-57
    Content: : |funtype 'p : * . funtype 's : * . ( 'p * 's ) -> ( list (operation) *
                                                                   's )|
    references: []
    (michelson_program#263:5-22 -> michelson_program)
    Range: File "", line 263, characters 5-22
    Body Range: File "", line 263, characters 25-54
    Content: : |michelson_program|
    references:
      File "", line 276, characters 16-33 ,
      File "", line 296, characters 44-61 ,
      File "", line 297, characters 30-47 ,
      File "", line 299, characters 39-56 ,
      File "", line 304, characters 45-62 ,
      File "", line 320, characters 30-47 ,
      File "", line 326, characters 29-46 ,
      File "", line 328, characters 51-68 ,
      File "", line 329, characters 37-54 ,
      File "", line 338, characters 12-29 ,
      File "", line 391, characters 34-51 ,
      File "", line 392, characters 38-55 ,
      File "", line 409, characters 12-29 ,
      File "", line 414, characters 14-31 ,
      File "", line 416, characters 28-45 ,
      File "", line 416, characters 53-70 ,
      File "", line 425, characters 55-72 ,
      File "", line 453, characters 45-62 ,
      File "", line 482, characters 65-82 ,
      File "", line 502, characters 69-86
    (typed_address#264:5-18 -> typed_address)
    Range: File "", line 264, characters 5-18
    Body Range: File "", line 264, characters 21-46
    Content: : |typed_address|
    references:
      File "", line 302, characters 41-54 ,
      File "", line 314, characters 64-77 ,
      File "", line 323, characters 53-66 ,
      File "", line 327, characters 60-73 ,
      File "", line 335, characters 41-54 ,
      File "", line 383, characters 54-67 ,
      File "", line 417, characters 58-71 ,
      File "", line 426, characters 90-103 ,
      File "", line 431, characters 19-32 ,
      File "", line 436, characters 99-112 ,
      File "", line 441, characters 19-32 ,
      File "", line 443, characters 97-110 ,
      File "", line 448, characters 19-32 ,
      File "", line 523, characters 51-64 ,
      File "", line 529, characters 21-34 ,
      File "", line 545, characters 55-68 ,
      File "", line 551, characters 21-34 ,
      File "", line 600, characters 58-71 ,
      File "", line 626, characters 55-68 ,
      File "", line 641, characters 55-68
    (mutation#265:5-13 -> mutation)
    Range: File "", line 265, characters 5-13
    Body Range: File "", line 265, characters 16-36
    Content: : |mutation|
    references:
      File "", line 400, characters 53-61 ,
      File "", line 401, characters 38-46 ,
      File "", line 458, characters 64-72 ,
      File "", line 460, characters 35-43 ,
      File "", line 461, characters 42-50 ,
      File "", line 470, characters 68-76 ,
      File "", line 472, characters 35-43 ,
      File "", line 473, characters 37-45 ,
      File "", line 473, characters 70-78 ,
      File "", line 481, characters 28-36 ,
      File "", line 483, characters 95-103 ,
      File "", line 491, characters 35-43 ,
      File "", line 492, characters 42-50 ,
      File "", line 503, characters 99-107 ,
      File "", line 511, characters 35-43 ,
      File "", line 512, characters 37-45 ,
      File "", line 512, characters 70-78 ,
      File "", line 521, characters 28-36 ,
      File "", line 523, characters 107-115 ,
      File "", line 533, characters 35-43 ,
      File "", line 534, characters 42-50 ,
      File "", line 545, characters 111-119 ,
      File "", line 555, characters 35-43 ,
      File "", line 556, characters 37-45 ,
      File "", line 556, characters 70-78 ,
      File "", line 565, characters 28-36
    (michelson_contract#266:5-23 -> michelson_contract)
    Range: File "", line 266, characters 5-23
    Body Range: File "", line 266, characters 26-56
    Content: : |michelson_contract|
    references:
      File "", line 342, characters 16-34 ,
      File "", line 343, characters 70-88 ,
      File "", line 347, characters 46-64 ,
      File "", line 425, characters 30-48 ,
      File "", line 426, characters 106-124 ,
      File "", line 433, characters 98-116 ,
      File "", line 436, characters 115-133 ,
      File "", line 443, characters 113-131 ,
      File "", line 450, characters 49-67 ,
      File "", line 453, characters 87-105 ,
      File "", line 483, characters 57-75 ,
      File "", line 503, characters 61-79 ,
      File "", line 523, characters 68-86 ,
      File "", line 545, characters 72-90
    (ast_contract#267:5-17 -> ast_contract)
    Range: File "", line 267, characters 5-17
    Body Range: File "", line 267, characters 20-44
    Content: : |ast_contract|
    references:
      File "", line 345, characters 16-28 ,
      File "", line 434, characters 18-30 ,
      File "", line 451, characters 16-28 ,
      File "", line 484, characters 25-37 ,
      File "", line 489, characters 16-28 ,
      File "", line 504, characters 25-37 ,
      File "", line 509, characters 16-28 ,
      File "", line 525, characters 25-37 ,
      File "", line 531, characters 16-28 ,
      File "", line 547, characters 25-37 ,
      File "", line 553, characters 16-28
    (pbt_gen#268:5-12 -> pbt_gen)
    Range: File "", line 268, characters 5-12
    Body Range: File "", line 268, characters 15-34
    Content: : |pbt_gen|
    references:
      File "", line 287, characters 23-30 ,
      File "", line 317, characters 14-21 ,
      File "", line 366, characters 25-32 ,
      File "", line 367, characters 31-38 ,
      File "", line 368, characters 34-41
    (int64#269:5-10 -> int64)
    Range: File "", line 269, characters 5-10
    Body Range: File "", line 269, characters 13-30
    Content: : |int64|
    references: []
    (views#270:5-10 -> views)
    Range: File "", line 270, characters 5-10
    Body Range: File "", line 270, characters 13-30
    Content: : |views|
    references:
      File "", line 292, characters 70-75 ,
      File "", line 344, characters 18-23 ,
      File "", line 433, characters 89-94
    (test_exec_error_balance_too_low#272:5-36 -> test_exec_error_balance_too_low)
    Range: File "", line 272, characters 5-36
    Body Range: File "", line 273, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 277, characters 23-54
    (test_exec_error#275:5-20 -> test_exec_error)
    Range: File "", line 275, characters 5-20
    Body Range: File "", line 276, character 4 to line 278, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 280, characters 49-64
    (test_exec_result#280:5-21 -> test_exec_result)
    Range: File "", line 280, characters 5-21
    Body Range: File "", line 280, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 391, characters 65-81 ,
      File "", line 406, characters 73-89 ,
      File "", line 611, characters 47-63
    (test_baker_policy#282:5-22 -> test_baker_policy)
    Range: File "", line 282, characters 5-22
    Body Range: File "", line 283, character 4 to line 285, character 24
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 340, characters 29-46
    (pbt_test#287:8-16 -> pbt_test)
    Range: File "", line 287, characters 8-16
    Body Range: File "", line 287, characters 19-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 368, characters 63-71 ,
      File "", line 369, characters 33-41
    (pbt_result#288:8-18 -> pbt_result)
    Range: File "", line 288, characters 8-18
    Body Range: File "", line 288, characters 21-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 369, characters 57-67 ,
      File "", line 370, characters 39-49 ,
      File "", line 372, characters 84-94 ,
      File "", line 376, characters 96-106 ,
      File "", line 379, characters 68-78
    (unforged_ticket#290:8-23 -> unforged_ticket)
    Range: File "", line 290, characters 8-23
    Body Range: File "", line 290, characters 41-91
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    (module_contract#292:14-29 -> module_contract)
    Range: File "", line 292, characters 14-29
    Body Range: File "", line 292, characters 32-69
    Content: : |funtype 'p : * . funtype 's : * . ( ( 'p * 's ) -> ( list (operation) *
                                                                     's ) *
                                                    views ('s) )|
    references:
      File "", line 443, characters 52-67 ,
      File "", line 522, characters 65-80 ,
      File "", line 544, characters 69-84
    Module definitions:
    (Tezos#59:7-12 -> Tezos)
    Range: File "", line 59, characters 7-12
    Body Range: File "", line 59, character 15 to line 119, character 3
    Content: Members: Variable definitions:
                      (get_balance#61:6-17 -> get_balance)
                      Range: File "", line 61, characters 6-17
                      Body Range: File "", line 61, characters 38-76
                      Content: |core: unit -> tez|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_amount#62:6-16 -> get_amount)
                      Range: File "", line 62, characters 6-16
                      Body Range: File "", line 62, characters 37-74
                      Content: |core: unit -> tez|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_now#63:6-13 -> get_now)
                      Range: File "", line 63, characters 6-13
                      Body Range: File "", line 63, characters 40-80
                      Content: |core: unit -> timestamp|
                      references: File "", line 322, characters 47-54
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_sender#64:6-16 -> get_sender)
                      Range: File "", line 64, characters 6-16
                      Body Range: File "", line 64, characters 41-82
                      Content: |core: unit -> address|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_source#65:6-16 -> get_source)
                      Range: File "", line 65, characters 6-16
                      Body Range: File "", line 65, characters 41-82
                      Content: |core: unit -> address|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_level#66:6-15 -> get_level)
                      Range: File "", line 66, characters 6-15
                      Body Range: File "", line 66, characters 36-72
                      Content: |core: unit -> nat|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_self_address#67:6-22 -> get_self_address)
                      Range: File "", line 67, characters 6-22
                      Body Range: File "", line 67, characters 47-94
                      Content: |core: unit -> address|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_chain_id#68:6-18 -> get_chain_id)
                      Range: File "", line 68, characters 6-18
                      Body Range: File "", line 68, characters 44-88
                      Content: |core: unit -> chain_id|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_total_voting_power#69:6-28 -> get_total_voting_power)
                      Range: File "", line 69, characters 6-28
                      Body Range: File "", line 69, characters 49-98
                      Content: |core: unit -> nat|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_min_block_time#70:6-24 -> get_min_block_time)
                      Range: File "", line 70, characters 6-24
                      Body Range: File "", line 70, characters 45-90
                      Content: |core: unit -> nat|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (voting_power#71:6-18 -> voting_power)
                      Range: File "", line 71, characters 6-18
                      Body Range: File "", line 71, characters 43-89
                      Content: |core: key_hash -> nat|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (address#72:6-13 -> address)
                      Range: File "", line 72, characters 6-13
                      Body Range: File "", line 72, characters 52-96
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 384, characters 21-28
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (implicit_account#73:6-22 -> implicit_account)
                      Range: File "", line 73, characters 6-22
                      Body Range: File "", line 73, characters 57-117
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (join_tickets#74:6-18 -> join_tickets)
                      Range: File "", line 74, characters 6-18
                      Body Range: File "", line 74, characters 76-133
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (read_ticket#75:6-17 -> read_ticket)
                      Range: File "", line 75, characters 6-17
                      Body Range: File "", line 76, characters 4-84
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address *
                                                                    ( a * nat ) ) *
                                                                  ticket (a) )|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (never#77:6-11 -> never)
                      Range: File "", line 77, characters 6-11
                      Body Range: File "", line 77, characters 39-75
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (pairing_check#78:6-19 -> pairing_check)
                      Range: File "", line 78, characters 6-19
                      Body Range: File "", line 78, characters 70-117
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (set_delegate#79:6-18 -> set_delegate)
                      Range: File "", line 79, characters 6-18
                      Body Range: File "", line 79, characters 55-106
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (self#80:25-29 -> self)
                      Range: File "", line 80, characters 25-29
                      Body Range: File "", line 81, character 4 to line 82, character 70
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (constant#83:25-33 -> constant)
                      Range: File "", line 83, characters 25-33
                      Body Range: File "", line 83, characters 62-96
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (sapling_empty_state#84:25-44 -> sapling_empty_state)
                      Range: File "", line 84, characters 25-44
                      Body Range: File "", line 85, characters 4-105
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_contract_opt#86:25-41 -> get_contract_opt)
                      Range: File "", line 86, characters 25-41
                      Body Range: File "", line 87, characters 4-92
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 89, characters 12-28 ,
                        File "", line 93, characters 12-28
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_contract#88:25-37 -> get_contract)
                      Range: File "", line 88, characters 25-37
                      Body Range: File "", line 89, character 4 to line 90, character 68
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_contract_with_error#92:6-29 -> get_contract_with_error)
                      Range: File "", line 92, characters 6-29
                      Body Range: File "", line 93, character 4 to line 94, character 39
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: File "", line 584, characters 39-62
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (create_ticket#95:6-19 -> create_ticket)
                      Range: File "", line 95, characters 6-19
                      Body Range: File "", line 95, characters 69-147
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references:
                        File "", line 582, characters 39-52 ,
                        File "", line 595, characters 39-52
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (transaction#96:6-17 -> transaction)
                      Range: File "", line 96, characters 6-17
                      Body Range: File "", line 97, characters 4-109
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: File "", line 585, characters 21-32
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (call_view#98:25-34 -> call_view)
                      Range: File "", line 98, characters 25-34
                      Body Range: File "", line 99, character 4 to line 100, character 104
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (split_ticket#101:6-18 -> split_ticket)
                      Range: File "", line 101, characters 6-18
                      Body Range: File "", line 102, characters 4-76
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (create_contract#103:25-40 -> create_contract)
                      Range: File "", line 103, characters 25-40
                      Body Range: File "", line 104, character 6 to line 105, character 58
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: File "", line 597, characters 26-41
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (create_contract_uncurried#106:25-50 -> create_contract_uncurried)
                      Range: File "", line 106, characters 25-50
                      Body Range: File "", line 107, characters 6-50
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_entrypoint_opt#108:25-43 -> get_entrypoint_opt)
                      Range: File "", line 108, characters 25-43
                      Body Range: File "", line 109, character 4 to line 110, character 116
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 112, characters 12-30
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_entrypoint#111:25-39 -> get_entrypoint)
                      Range: File "", line 111, characters 25-39
                      Body Range: File "", line 112, character 4 to line 113, character 70
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (emit#114:25-29 -> emit)
                      Range: File "", line 114, characters 25-29
                      Body Range: File "", line 115, character 4 to line 116, character 102
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (sapling_verify_update#117:25-46 -> sapling_verify_update)
                      Range: File "", line 117, characters 25-46
                      Body Range: File "", line 117, characters 167-264
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes *
                        ( int * sapling_state (sap_a) ) ))|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "", line 322, characters 41-46 ,
      File "", line 384, characters 15-20 ,
      File "", line 582, characters 33-38 ,
      File "", line 584, characters 33-38 ,
      File "", line 585, characters 15-20 ,
      File "", line 595, characters 33-38 ,
      File "", line 597, characters 20-25

    (Bitwise#121:7-14 -> Bitwise)
    Range: File "", line 121, characters 7-14
    Body Range: File "", line 121, character 17 to line 127, character 3
    Content: Members: Variable definitions:
                      (and#122:6-10 -> and)
                      Range: File "", line 122, characters 6-10
                      Body Range: File "", line 122, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      (xor#123:6-9 -> xor)
                      Range: File "", line 123, characters 6-9
                      Body Range: File "", line 123, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_or (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      (or#124:6-9 -> or)
                      Range: File "", line 124, characters 6-9
                      Body Range: File "", line 124, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_xor (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      (shift_left#125:6-16 -> shift_left)
                      Range: File "", line 125, characters 6-16
                      Body Range: File "", line 125, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsl (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      (shift_right#126:6-17 -> shift_right)
                      Range: File "", line 126, characters 6-17
                      Body Range: File "", line 126, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsr (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Big_map#129:7-14 -> Big_map)
    Range: File "", line 129, characters 7-14
    Body Range: File "", line 129, character 17 to line 141, character 3
    Content: Members: Variable definitions:
                      (empty#130:16-21 -> empty)
                      Range: File "", line 130, characters 16-21
                      Body Range: File "", line 130, characters 52-81
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (literal#131:25-32 -> literal)
                      Range: File "", line 131, characters 25-32
                      Body Range: File "", line 131, characters 82-116
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (mem#133:6-9 -> mem)
                      Range: File "", line 133, characters 6-9
                      Body Range: File "", line 133, characters 59-88
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (add#134:6-9 -> add)
                      Range: File "", line 134, characters 6-9
                      Body Range: File "", line 134, characters 77-109
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (remove#135:6-12 -> remove)
                      Range: File "", line 135, characters 6-12
                      Body Range: File "", line 135, characters 72-104
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (update#136:6-12 -> update)
                      Range: File "", line 136, characters 6-12
                      Body Range: File "", line 136, characters 87-122
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (get_and_update#137:6-20 -> get_and_update)
                      Range: File "", line 137, characters 6-20
                      Body Range: File "", line 137, characters 106-153
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (find_opt#138:6-14 -> find_opt)
                      Range: File "", line 138, characters 6-14
                      Body Range: File "", line 138, characters 68-102
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (find#139:6-10 -> find)
                      Range: File "", line 139, characters 6-10
                      Body Range: File "", line 139, characters 57-87
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Map#143:7-10 -> Map)
    Range: File "", line 143, characters 7-10
    Body Range: File "", line 143, character 13 to line 159, character 3
    Content: Members: Variable definitions:
                      (empty#144:6-11 -> empty)
                      Range: File "", line 144, characters 6-11
                      Body Range: File "", line 144, characters 38-63
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (size#145:6-10 -> size)
                      Range: File "", line 145, characters 6-10
                      Body Range: File "", line 145, characters 47-74
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (literal#146:25-32 -> literal)
                      Range: File "", line 146, characters 25-32
                      Body Range: File "", line 146, characters 78-108
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (mem#148:6-9 -> mem)
                      Range: File "", line 148, characters 6-9
                      Body Range: File "", line 148, characters 55-84
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (add#149:6-9 -> add)
                      Range: File "", line 149, characters 6-9
                      Body Range: File "", line 149, characters 69-101
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (remove#150:6-12 -> remove)
                      Range: File "", line 150, characters 6-12
                      Body Range: File "", line 150, characters 64-96
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (update#151:6-12 -> update)
                      Range: File "", line 151, characters 6-12
                      Body Range: File "", line 151, characters 79-114
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (get_and_update#152:6-20 -> get_and_update)
                      Range: File "", line 152, characters 6-20
                      Body Range: File "", line 152, characters 98-141
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (find#153:6-10 -> find)
                      Range: File "", line 153, characters 6-10
                      Body Range: File "", line 153, characters 53-83
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (find_opt#154:6-14 -> find_opt)
                      Range: File "", line 154, characters 6-14
                      Body Range: File "", line 154, characters 64-98
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (iter#155:6-10 -> iter)
                      Range: File "", line 155, characters 6-10
                      Body Range: File "", line 155, characters 68-98
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (map#156:6-9 -> map)
                      Range: File "", line 156, characters 6-9
                      Body Range: File "", line 156, characters 72-101
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (fold#157:6-10 -> fold)
                      Range: File "", line 157, characters 6-10
                      Body Range: File "", line 157, characters 78-111
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Transpiled#161:7-17 -> Transpiled)
    Range: File "", line 161, characters 7-17
    Body Range: File "", line 161, character 20 to line 165, character 3
    Content: Members: Variable definitions:
                      (map_find_opt#162:6-18 -> map_find_opt)
                      Range: File "", line 162, characters 6-18
                      Body Range: File "", line 162, characters 79-142
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      Mod Path = "Transpiled"
                      Def Type = Module_field
                      (map_add#163:6-13 -> map_add)
                      Range: File "", line 163, characters 6-13
                      Body Range: File "", line 163, characters 82-163
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      Mod Path = "Transpiled"
                      Def Type = Module_field
                      (map_remove#164:6-16 -> map_remove)
                      Range: File "", line 164, characters 6-16
                      Body Range: File "", line 164, characters 75-218
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Mod Path = "Transpiled"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Set#167:7-10 -> Set)
    Range: File "", line 167, characters 7-10
    Body Range: File "", line 167, character 13 to line 182, character 3
    Content: Members: Variable definitions:
                      (empty#168:6-11 -> empty)
                      Range: File "", line 168, characters 6-11
                      Body Range: File "", line 168, characters 31-56
                      Content: |core: ∀ a : * . set (a)|
                      references: File "", line 181, characters 96-101
                      Mod Path = "Set"
                      Def Type = Module_field
                      (size#169:6-10 -> size)
                      Range: File "", line 169, characters 6-10
                      Body Range: File "", line 169, characters 40-67
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (cardinal#170:6-14 -> cardinal)
                      Range: File "", line 170, characters 6-14
                      Body Range: File "", line 170, characters 44-71
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (literal#171:25-32 -> literal)
                      Range: File "", line 171, characters 25-32
                      Body Range: File "", line 171, characters 65-95
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (mem#173:6-9 -> mem)
                      Range: File "", line 173, characters 6-9
                      Body Range: File "", line 173, characters 48-77
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (add#174:6-9 -> add)
                      Range: File "", line 174, characters 6-9
                      Body Range: File "", line 174, characters 49-78
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: File "", line 181, characters 81-84
                      Mod Path = "Set"
                      Def Type = Module_field
                      (remove#175:6-12 -> remove)
                      Range: File "", line 175, characters 6-12
                      Body Range: File "", line 175, characters 52-84
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (update#176:6-12 -> update)
                      Range: File "", line 176, characters 6-12
                      Body Range: File "", line 176, characters 55-90
                      Content: |unresolved|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (iter#177:6-10 -> iter)
                      Range: File "", line 177, characters 6-10
                      Body Range: File "", line 177, characters 57-87
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (fold#178:6-10 -> fold)
                      Range: File "", line 178, characters 6-10
                      Body Range: File "", line 178, characters 65-98
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (fold_desc#179:6-15 -> fold_desc)
                      Range: File "", line 179, characters 6-15
                      Body Range: File "", line 179, characters 70-108
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: File "", line 181, characters 4-13
                      Mod Path = "Set"
                      Def Type = Module_field
                      (filter_map#180:6-16 -> filter_map)
                      Range: File "", line 180, characters 6-16
                      Body Range: File "", line 181, characters 4-110
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> set (a) -> set (b)|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (List#184:7-11 -> List)
    Range: File "", line 184, characters 7-11
    Body Range: File "", line 184, character 14 to line 204, character 3
    Content: Members: Variable definitions:
                      (length#185:6-12 -> length)
                      Range: File "", line 185, characters 6-12
                      Body Range: File "", line 185, characters 44-73
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (size#186:6-10 -> size)
                      Range: File "", line 186, characters 6-10
                      Body Range: File "", line 186, characters 42-71
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (head_opt#187:6-14 -> head_opt)
                      Range: File "", line 187, characters 6-14
                      Body Range: File "", line 187, characters 51-98
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (tail_opt#188:6-14 -> tail_opt)
                      Range: File "", line 188, characters 6-14
                      Body Range: File "", line 188, characters 58-107
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (map#190:6-9 -> map)
                      Range: File "", line 190, characters 6-9
                      Body Range: File "", line 190, characters 59-90
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 201, characters 4-7 ,
                        File "", line 203, characters 4-7
                      Mod Path = "List"
                      Def Type = Module_field
                      (iter#191:6-10 -> iter)
                      Range: File "", line 191, characters 6-10
                      Body Range: File "", line 191, characters 58-90
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (fold#192:6-10 -> fold)
                      Range: File "", line 192, characters 6-10
                      Body Range: File "", line 192, characters 67-102
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 390, characters 9-13
                      Mod Path = "List"
                      Def Type = Module_field
                      (fold_left#193:6-15 -> fold_left)
                      Range: File "", line 193, characters 6-15
                      Body Range: File "", line 193, characters 72-112
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (fold_right#194:6-16 -> fold_right)
                      Range: File "", line 194, characters 6-16
                      Body Range: File "", line 194, characters 73-114
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 197, characters 4-14 ,
                        File "", line 199, characters 4-14
                      Mod Path = "List"
                      Def Type = Module_field
                      (cons#195:6-10 -> cons)
                      Range: File "", line 195, characters 6-10
                      Body Range: File "", line 195, characters 53-80
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (find_opt#196:6-14 -> find_opt)
                      Range: File "", line 196, characters 6-14
                      Body Range: File "", line 197, characters 4-82
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (filter_map#198:6-16 -> filter_map)
                      Range: File "", line 198, characters 6-16
                      Body Range: File "", line 199, characters 4-100
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (update#200:6-12 -> update)
                      Range: File "", line 200, characters 6-12
                      Body Range: File "", line 201, characters 4-62
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (update_with#202:6-17 -> update_with)
                      Range: File "", line 202, characters 6-17
                      Body Range: File "", line 203, characters 4-48
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 390, characters 4-8

    (String#206:7-13 -> String)
    Range: File "", line 206, characters 7-13
    Body Range: File "", line 206, character 16 to line 212, character 3
    Content: Members: Variable definitions:
                      (length#207:6-12 -> length)
                      Range: File "", line 207, characters 6-12
                      Body Range: File "", line 207, characters 34-57
                      Content: |core: string -> nat|
                      references:
                        File "", line 418, characters 22-28 ,
                        File "", line 421, characters 43-49
                      Mod Path = "String"
                      Def Type = Module_field
                      (concats#208:6-13 -> concats)
                      Range: File "", line 208, characters 6-13
                      Body Range: File "", line 208, characters 44-71
                      Content: |core: list (string) -> string|
                      references: []
                      Mod Path = "String"
                      Def Type = Module_field
                      (concat#210:6-12 -> concat)
                      Range: File "", line 210, characters 6-12
                      Body Range: File "", line 210, characters 52-82
                      Content: |core: string -> string -> string|
                      references: []
                      Mod Path = "String"
                      Def Type = Module_field
                      (sub#211:6-9 -> sub)
                      Range: File "", line 211, characters 6-9
                      Body Range: File "", line 211, characters 54-84
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 419, characters 24-27 ,
                        File "", line 421, characters 23-26
                      Mod Path = "String"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "", line 418, characters 15-21 ,
      File "", line 419, characters 17-23 ,
      File "", line 421, characters 16-22 ,
      File "", line 421, characters 36-42

    (Option#214:7-13 -> Option)
    Range: File "", line 214, characters 7-13
    Body Range: File "", line 214, character 16 to line 223, character 3
    Content: Members: Variable definitions:
                      (unopt#215:6-11 -> unopt)
                      Range: File "", line 215, characters 6-11
                      Body Range: File "", line 215, characters 42-104
                      Content: |core: ∀ a : * . option (a) -> a|
                      references:
                        File "", line 582, characters 26-31 ,
                        File "", line 595, characters 26-31
                      Mod Path = "Option"
                      Def Type = Module_field
                      (unopt_with_error#217:6-22 -> unopt_with_error)
                      Range: File "", line 217, characters 6-22
                      Body Range: File "", line 217, characters 66-113
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      (map#218:15-18 -> map)
                      Range: File "", line 218, characters 15-18
                      Body Range: File "", line 218, characters 71-103
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      (value#219:6-11 -> value)
                      Range: File "", line 219, characters 6-11
                      Body Range: File "", line 219, characters 56-100
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      (value_exn#220:6-15 -> value_exn)
                      Range: File "", line 220, characters 6-15
                      Body Range: File "", line 220, characters 62-109
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      (is_none#221:6-13 -> is_none)
                      Range: File "", line 221, characters 6-13
                      Body Range: File "", line 221, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      (is_some#222:6-13 -> is_some)
                      Range: File "", line 222, characters 6-13
                      Body Range: File "", line 222, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "", line 582, characters 19-25 ,
      File "", line 595, characters 19-25

    (Bytes#225:7-12 -> Bytes)
    Range: File "", line 225, characters 7-12
    Body Range: File "", line 225, character 15 to line 233, character 3
    Content: Members: Variable definitions:
                      (concats#226:6-13 -> concats)
                      Range: File "", line 226, characters 6-13
                      Body Range: File "", line 226, characters 42-69
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (pack#227:6-10 -> pack)
                      Range: File "", line 227, characters 6-10
                      Body Range: File "", line 227, characters 38-77
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (unpack#228:6-12 -> unpack)
                      Range: File "", line 228, characters 6-12
                      Body Range: File "", line 228, characters 47-122
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (length#229:6-12 -> length)
                      Range: File "", line 229, characters 6-12
                      Body Range: File "", line 229, characters 33-56
                      Content: |core: bytes -> nat|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (concat#231:6-12 -> concat)
                      Range: File "", line 231, characters 6-12
                      Body Range: File "", line 231, characters 49-79
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (sub#232:6-9 -> sub)
                      Range: File "", line 232, characters 6-9
                      Body Range: File "", line 232, characters 52-82
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#235:7-13 -> Crypto)
    Range: File "", line 235, characters 7-13
    Body Range: File "", line 235, character 16 to line 243, character 3
    Content: Members: Variable definitions:
                      (blake2b#236:6-13 -> blake2b)
                      Range: File "", line 236, characters 6-13
                      Body Range: File "", line 236, characters 36-78
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (sha256#237:6-12 -> sha256)
                      Range: File "", line 237, characters 6-12
                      Body Range: File "", line 237, characters 35-76
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (sha512#238:6-12 -> sha512)
                      Range: File "", line 238, characters 6-12
                      Body Range: File "", line 238, characters 35-76
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (sha3#239:6-10 -> sha3)
                      Range: File "", line 239, characters 6-10
                      Body Range: File "", line 239, characters 33-72
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (keccak#240:6-12 -> keccak)
                      Range: File "", line 240, characters 6-12
                      Body Range: File "", line 240, characters 35-76
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (hash_key#241:6-14 -> hash_key)
                      Range: File "", line 241, characters 6-14
                      Body Range: File "", line 241, characters 38-84
                      Content: |core: key -> key_hash|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (check#242:6-11 -> check)
                      Range: File "", line 242, characters 6-11
                      Body Range: File "", line 242, characters 59-112
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Test#294:7-11 -> Test)
    Range: File "", line 294, characters 7-11
    Body Range: File "", line 294, character 14 to line 645, character 3
    Content: Members: Variable definitions:
                      (run#296:6-9 -> run)
                      Range: File "", line 296, characters 6-9
                      Body Range: File "", line 296, characters 64-94
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 297, characters 50-53
                      Mod Path = "Test"
                      Def Type = Module_field
                      (eval#297:6-10 -> eval)
                      Range: File "", line 297, characters 6-10
                      Body Range: File "", line 297, characters 50-74
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 299, characters 59-63 ,
                        File "", line 409, characters 32-36 ,
                        File "", line 414, characters 34-38 ,
                        File "", line 428, characters 12-16 ,
                        File "", line 438, characters 12-16 ,
                        File "", line 445, characters 12-16 ,
                        File "", line 524, characters 12-16 ,
                        File "", line 546, characters 12-16
                      Mod Path = "Test"
                      Def Type = Module_field
                      (compile_value#299:6-19 -> compile_value)
                      Range: File "", line 299, characters 6-19
                      Body Range: File "", line 299, characters 59-65
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_total_voting_power#300:6-28 -> get_total_voting_power)
                      Range: File "", line 300, characters 6-28
                      Body Range: File "", line 300, characters 49-96
                      Content: |core: unit -> nat|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (failwith#301:6-14 -> failwith)
                      Range: File "", line 301, characters 6-14
                      Body Range: File "", line 301, characters 40-72
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 567, characters 51-59 ,
                        File "", line 568, characters 74-82 ,
                        File "", line 569, characters 89-97 ,
                        File "", line 571, characters 68-76 ,
                        File "", line 572, characters 98-106 ,
                        File "", line 573, characters 113-121 ,
                        File "", line 628, characters 16-24 ,
                        File "", line 643, characters 16-24
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_contract#302:6-17 -> to_contract)
                      Range: File "", line 302, characters 6-17
                      Body Range: File "", line 302, characters 71-106
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 336, characters 25-36 ,
                        File "", line 384, characters 30-41 ,
                        File "", line 613, characters 28-39 ,
                        File "", line 623, characters 40-51 ,
                        File "", line 638, characters 40-51
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_source#303:6-16 -> set_source)
                      Range: File "", line 303, characters 6-16
                      Body Range: File "", line 303, characters 40-74
                      Content: |core: address -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_storage_of_address#304:6-28 -> get_storage_of_address)
                      Range: File "", line 304, characters 6-28
                      Body Range: File "", line 304, characters 65-111
                      Content: |core: address -> michelson_program|
                      references: File "", line 338, characters 32-54
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_balance#305:6-17 -> get_balance)
                      Range: File "", line 305, characters 6-17
                      Body Range: File "", line 305, characters 40-75
                      Content: |core: address -> tez|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (print#306:6-11 -> print)
                      Range: File "", line 306, characters 6-11
                      Body Range: File "", line 306, characters 34-66
                      Content: |core: string -> unit|
                      references:
                        File "", line 360, characters 4-9 ,
                        File "", line 396, characters 4-9
                      Mod Path = "Test"
                      Def Type = Module_field
                      (eprint#307:6-12 -> eprint)
                      Range: File "", line 307, characters 6-12
                      Body Range: File "", line 307, characters 35-67
                      Content: |core: string -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_voting_power#308:6-22 -> get_voting_power)
                      Range: File "", line 308, characters 6-22
                      Body Range: File "", line 308, characters 47-88
                      Content: |core: key_hash -> nat|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (nth_bootstrap_contract#309:6-28 -> nth_bootstrap_contract)
                      Range: File "", line 309, characters 6-28
                      Body Range: File "", line 309, characters 51-97
                      Content: |core: nat -> address|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (nth_bootstrap_account#310:6-27 -> nth_bootstrap_account)
                      Range: File "", line 310, characters 6-27
                      Body Range: File "", line 311, character 4 to line 312, character 5
                      Content: |core: int -> address|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_bootstrap_account#313:6-27 -> get_bootstrap_account)
                      Range: File "", line 313, characters 6-27
                      Body Range: File "", line 313, characters 65-105
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (nth_bootstrap_typed_address#314:6-33 -> nth_bootstrap_typed_address)
                      Range: File "", line 314, characters 6-33
                      Body Range: File "", line 314, characters 80-131
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (last_originations#315:6-23 -> last_originations)
                      Range: File "", line 315, characters 6-23
                      Body Range: File "", line 315, characters 67-108
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (random#316:6-12 -> random)
                      Range: File "", line 316, characters 6-12
                      Body Range: File "", line 317, character 4 to line 318, character 42
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (new_account#319:6-17 -> new_account)
                      Range: File "", line 319, characters 6-17
                      Body Range: File "", line 319, characters 46-81
                      Content: |core: unit -> ( string * key )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (decompile#320:6-15 -> decompile)
                      Range: File "", line 320, characters 6-15
                      Body Range: File "", line 320, characters 55-88
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 339, characters 5-14
                      Mod Path = "Test"
                      Def Type = Module_field
                      (bake_until_n_cycle_end#321:6-28 -> bake_until_n_cycle_end)
                      Range: File "", line 321, characters 6-28
                      Body Range: File "", line 321, characters 48-94
                      Content: |core: nat -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_time#322:6-14 -> get_time)
                      Range: File "", line 322, characters 6-14
                      Body Range: File "", line 322, characters 41-57
                      Content: |core: unit -> timestamp|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (cast_address#323:6-18 -> cast_address)
                      Range: File "", line 323, characters 6-18
                      Body Range: File "", line 323, characters 69-105
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 431, characters 35-47 ,
                        File "", line 441, characters 35-47 ,
                        File "", line 448, characters 35-47 ,
                        File "", line 529, characters 37-49 ,
                        File "", line 551, characters 37-49 ,
                        File "", line 626, characters 22-34 ,
                        File "", line 641, characters 22-34
                      Mod Path = "Test"
                      Def Type = Module_field
                      (register_delegate#324:6-23 -> register_delegate)
                      Range: File "", line 324, characters 6-23
                      Body Range: File "", line 324, characters 49-91
                      Content: |core: key_hash -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (stake#325:6-11 -> stake)
                      Range: File "", line 325, characters 6-11
                      Body Range: File "", line 325, characters 47-80
                      Content: |core: key_hash -> tez -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (register_constant#326:6-23 -> register_constant)
                      Range: File "", line 326, characters 6-23
                      Body Range: File "", line 326, characters 59-100
                      Content: |core: michelson_program -> string|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_typed_address#327:6-22 -> to_typed_address)
                      Range: File "", line 327, characters 6-22
                      Body Range: File "", line 327, characters 76-116
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (constant_to_michelson_program#328:6-35 -> constant_to_michelson_program)
                      Range: File "", line 328, characters 6-35
                      Body Range: File "", line 328, characters 71-116
                      Content: |core: string -> michelson_program|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (parse_michelson#329:6-21 -> parse_michelson)
                      Range: File "", line 329, characters 6-21
                      Body Range: File "", line 329, characters 57-102
                      Content: |core: string -> michelson_program|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (restore_context#330:6-21 -> restore_context)
                      Range: File "", line 330, characters 6-21
                      Body Range: File "", line 330, characters 42-77
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (save_context#331:6-18 -> save_context)
                      Range: File "", line 331, characters 6-18
                      Body Range: File "", line 331, characters 39-75
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (drop_context#332:6-18 -> drop_context)
                      Range: File "", line 332, characters 6-18
                      Body Range: File "", line 332, characters 39-75
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_string#333:6-15 -> to_string)
                      Range: File "", line 333, characters 6-15
                      Body Range: File "", line 333, characters 44-80
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 351, characters 68-77 ,
                        File "", line 353, characters 67-76 ,
                        File "", line 355, characters 61-70 ,
                        File "", line 395, characters 12-21
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_json#334:6-13 -> to_json)
                      Range: File "", line 334, characters 6-13
                      Body Range: File "", line 334, characters 42-78
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_storage#335:6-17 -> get_storage)
                      Range: File "", line 335, characters 6-17
                      Body Range: File "", line 336, character 4 to line 339, character 21
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references:
                        File "", line 624, characters 12-23 ,
                        File "", line 639, characters 12-23
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_baker_policy#340:6-22 -> set_baker_policy)
                      Range: File "", line 340, characters 6-22
                      Body Range: File "", line 340, characters 57-91
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 341, characters 39-55
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_baker#341:6-15 -> set_baker)
                      Range: File "", line 341, characters 6-15
                      Body Range: File "", line 341, characters 39-70
                      Content: |core: address -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (size#342:6-10 -> size)
                      Range: File "", line 342, characters 6-10
                      Body Range: File "", line 342, characters 44-72
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 430, characters 12-16 ,
                        File "", line 440, characters 12-16 ,
                        File "", line 447, characters 12-16 ,
                        File "", line 456, characters 12-16 ,
                        File "", line 487, characters 14-18 ,
                        File "", line 507, characters 14-18 ,
                        File "", line 528, characters 14-18 ,
                        File "", line 550, characters 14-18
                      Mod Path = "Test"
                      Def Type = Module_field
                      (compile_contract#343:6-22 -> compile_contract)
                      Range: File "", line 343, characters 6-22
                      Body Range: File "", line 344, character 4 to line 346, character 52
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 427, characters 12-28 ,
                        File "", line 437, characters 12-28
                      Mod Path = "Test"
                      Def Type = Module_field
                      (read_contract_from_file#347:6-29 -> read_contract_from_file)
                      Range: File "", line 347, characters 6-29
                      Body Range: File "", line 347, characters 67-115
                      Content: |core: string -> michelson_contract|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (chr#348:6-9 -> chr)
                      Range: File "", line 348, characters 6-9
                      Body Range: File "", line 349, character 4 to line 357, character 10
                      Content: |core: nat -> option (string)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (nl#358:6-8 -> nl)
                      Range: File "", line 358, characters 6-8
                      Body Range: File "", line 358, characters 11-53
                      Content: |unresolved|
                      references: File "", line 360, characters 15-17
                      Mod Path = "Test"
                      Def Type = Module_field
                      (println#359:6-13 -> println)
                      Range: File "", line 359, characters 6-13
                      Body Range: File "", line 360, characters 4-18
                      Content: |core: string -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_print_values#362:6-22 -> set_print_values)
                      Range: File "", line 362, characters 6-22
                      Body Range: File "", line 362, characters 43-100
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (unset_print_values#363:6-24 -> unset_print_values)
                      Range: File "", line 363, characters 6-24
                      Body Range: File "", line 363, characters 45-103
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_last_events_from#383:6-26 -> get_last_events_from)
                      Range: File "", line 383, characters 6-26
                      Body Range: File "", line 384, character 4 to line 390, character 38
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (transfer#391:6-14 -> transfer)
                      Range: File "", line 391, characters 6-14
                      Body Range: File "", line 391, characters 84-162
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (transfer_exn#392:6-18 -> transfer_exn)
                      Range: File "", line 392, characters 6-18
                      Body Range: File "", line 392, characters 75-157
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (log#393:6-9 -> log)
                      Range: File "", line 393, characters 6-9
                      Body Range: File "", line 394, character 4 to line 396, character 11
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 420, characters 25-28
                      Mod Path = "Test"
                      Def Type = Module_field
                      (reset_state#397:6-17 -> reset_state)
                      Range: File "", line 397, characters 6-17
                      Body Range: File "", line 397, characters 52-117
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (reset_state_at#398:6-20 -> reset_state_at)
                      Range: File "", line 398, characters 6-20
                      Body Range: File "", line 398, characters 69-117
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (bootstrap_contract#399:6-24 -> bootstrap_contract)
                      Range: File "", line 399, characters 6-24
                      Body Range: File "", line 399, characters 97-145
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (mutate_value#400:6-18 -> mutate_value)
                      Range: File "", line 400, characters 6-18
                      Body Range: File "", line 400, characters 72-111
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 462, characters 23-35 ,
                        File "", line 474, characters 23-35
                      Mod Path = "Test"
                      Def Type = Module_field
                      (save_mutation#401:6-19 -> save_mutation)
                      Range: File "", line 401, characters 6-19
                      Body Range: File "", line 401, characters 66-106
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (sign#402:6-10 -> sign)
                      Range: File "", line 402, characters 6-10
                      Body Range: File "", line 402, characters 51-83
                      Content: |core: string -> bytes -> signature|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (add_account#403:6-17 -> add_account)
                      Range: File "", line 403, characters 6-17
                      Body Range: File "", line 403, characters 50-88
                      Content: |core: string -> key -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (baker_account#404:6-19 -> baker_account)
                      Range: File "", line 404, characters 6-19
                      Body Range: File "", line 404, characters 65-105
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_big_map#405:6-17 -> set_big_map)
                      Range: File "", line 405, characters 6-17
                      Body Range: File "", line 405, characters 69-107
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (transfer_to_contract#406:6-26 -> transfer_to_contract)
                      Range: File "", line 406, characters 6-26
                      Body Range: File "", line 407, character 4 to line 410, character 61
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: File "", line 613, characters 6-26
                      Mod Path = "Test"
                      Def Type = Module_field
                      (transfer_to_contract_exn#411:6-30 -> transfer_to_contract_exn)
                      Range: File "", line 411, characters 6-30
                      Body Range: File "", line 412, character 6 to line 415, character 67
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references:
                        File "", line 623, characters 14-38 ,
                        File "", line 638, characters 14-38
                      Mod Path = "Test"
                      Def Type = Module_field
                      (michelson_equal#416:6-21 -> michelson_equal)
                      Range: File "", line 416, characters 6-21
                      Body Range: File "", line 416, characters 81-88
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_entrypoint#417:6-19 -> to_entrypoint)
                      Range: File "", line 417, characters 6-19
                      Body Range: File "", line 418, character 4 to line 424, character 44
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_contract#425:6-24 -> originate_contract)
                      Range: File "", line 425, characters 6-24
                      Body Range: File "", line 425, characters 96-135
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 429, characters 12-30 ,
                        File "", line 439, characters 12-30 ,
                        File "", line 446, characters 12-30 ,
                        File "", line 455, characters 12-30 ,
                        File "", line 486, characters 14-32 ,
                        File "", line 506, characters 14-32 ,
                        File "", line 527, characters 14-32 ,
                        File "", line 549, characters 14-32
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate#426:6-15 -> originate)
                      Range: File "", line 426, characters 6-15
                      Body Range: File "", line 427, character 4 to line 432, character 13
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references:
                        File "", line 606, characters 32-41 ,
                        File "", line 622, characters 32-41 ,
                        File "", line 637, characters 32-41
                      Mod Path = "Test"
                      Def Type = Module_field
                      (compile_contract_with_views#433:8-35 -> compile_contract_with_views)
                      Range: File "", line 433, characters 8-35
                      Body Range: File "", line 434, character 6 to line 435, character 54
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 444, characters 12-39
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_uncurried#436:6-25 -> originate_uncurried)
                      Range: File "", line 436, characters 6-25
                      Body Range: File "", line 437, character 4 to line 442, character 13
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_module#443:6-22 -> originate_module)
                      Range: File "", line 443, characters 6-22
                      Body Range: File "", line 444, character 4 to line 449, character 13
                      Content: |core: ∀ p : * . ∀ s : * . module_contract (p ,
                      s) -> s -> tez -> ( typed_address (p ,
                                          s) *
                                          michelson_contract *
                                          int )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (compile_contract_from_file#450:6-32 -> compile_contract_from_file)
                      Range: File "", line 450, characters 6-32
                      Body Range: File "", line 451, character 4 to line 452, character 52
                      Content: |core: string -> michelson_contract|
                      references: File "", line 454, characters 12-38
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_from_file#453:6-25 -> originate_from_file)
                      Range: File "", line 453, characters 6-25
                      Body Range: File "", line 454, character 4 to line 457, character 13
                      Content: |core: string -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (mutation_test#458:6-19 -> mutation_test)
                      Range: File "", line 458, characters 6-19
                      Body Range: File "", line 459, character 4 to line 469, character 19
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (mutation_test_all#470:6-23 -> mutation_test_all)
                      Range: File "", line 470, characters 6-23
                      Body Range: File "", line 471, character 4 to line 481, character 46
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_from_file_and_mutate#482:6-36 -> originate_from_file_and_mutate)
                      Range: File "", line 482, characters 6-36
                      Body Range: File "", line 484, character 4 to line 501, character 19
                      Content: |core: ∀ b : * . string -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_from_file_and_mutate_all#502:6-40 -> originate_from_file_and_mutate_all)
                      Range: File "", line 502, characters 6-40
                      Body Range: File "", line 504, character 4 to line 521, character 46
                      Content: |core: ∀ b : * . string -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_module_and_mutate#522:6-33 -> originate_module_and_mutate)
                      Range: File "", line 522, characters 6-33
                      Body Range: File "", line 524, character 4 to line 543, character 19
                      Content: |core: ∀ p : * . ∀ s : * . ∀ b : * . module_contract (p ,
                      s) -> s -> tez -> typed_address (p ,
                      s) -> michelson_contract -> int -> b -> option (( b *
                                                                        mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_module_and_mutate_all#544:6-37 -> originate_module_and_mutate_all)
                      Range: File "", line 544, characters 6-37
                      Body Range: File "", line 546, character 4 to line 565, character 46
                      Content: |core: ∀ p : * . ∀ s : * . ∀ b : * . module_contract (p ,
                      s) -> s -> tez -> typed_address (p ,
                      s) -> michelson_contract -> int -> b -> list (( b *
                                                                      mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert#567:6-12 -> assert)
                      Range: File "", line 567, characters 6-12
                      Body Range: File "", line 567, characters 33-78
                      Content: |core: bool -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_some#568:6-17 -> assert_some)
                      Range: File "", line 568, characters 6-17
                      Body Range: File "", line 568, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_none#569:6-17 -> assert_none)
                      Range: File "", line 569, characters 6-17
                      Body Range: File "", line 569, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_with_error#571:6-23 -> assert_with_error)
                      Range: File "", line 571, characters 6-23
                      Body Range: File "", line 571, characters 50-78
                      Content: |unresolved|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_some_with_error#572:6-28 -> assert_some_with_error)
                      Range: File "", line 572, characters 6-28
                      Body Range: File "", line 572, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_none_with_error#573:6-28 -> assert_none_with_error)
                      Range: File "", line 573, characters 6-28
                      Body Range: File "", line 573, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:
                      (PBT#365:9-12 -> PBT)
                      Range: File "", line 365, characters 9-12
                      Body Range: File "", line 365, character 15 to line 381, character 5
                      Content: Members: Variable definitions:
                                        (gen#366:8-11 -> gen)
                                        Range: File "", line 366, characters 8-11
                                        Body Range: File "", line 366, characters 35-69
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        Mod Path = "Test""PBT"
                                        Def Type = Module_field
                                        (gen_small#367:8-17 -> gen_small)
                                        Range: File "", line 367, characters 8-17
                                        Body Range: File "", line 367, characters 41-74
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        Mod Path = "Test""PBT"
                                        Def Type = Module_field
                                        (make_test#368:8-17 -> make_test)
                                        Range: File "", line 368, characters 8-17
                                        Body Range: File "", line 368, characters 75-79
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        Mod Path = "Test""PBT"
                                        Def Type = Module_field
                                        (run#369:8-11 -> run)
                                        Range: File "", line 369, characters 8-11
                                        Body Range: File "", line 370, character 6 to line 380, character 7
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Mod Path = "Test""PBT"
                                        Def Type = Module_field
                                        Type definitions:
                                        Module definitions:

                      references: []

                      (Proxy_ticket#575:9-21 -> Proxy_ticket)
                      Range: File "", line 575, characters 9-21
                      Body Range: File "", line 575, character 24 to line 644, character 5
                      Content: Members: Variable definitions:
                                        (proxy_transfer_contract#576:19-42 -> proxy_transfer_contract)
                                        Range: File "", line 576, characters 19-42
                                        Body Range: File "", line 581, character 6 to line 586, character 14
                                        Content: |core: ∀ vt : * . ∀ whole_p : * . ticket (vt) -> whole_p ->
                                        ( ( vt * nat ) *
                                          address ) -> unit -> ( list (operation) *
                                                                 unit )|
                                        references:
                                          File "", line 604, characters 8-31
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (proxy_originate_contract#588:19-43 -> proxy_originate_contract)
                                        Range: File "", line 588, characters 19-43
                                        Body Range: File "", line 594, character 6 to line 598, character 21
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * . ticket (vt) -> whole_s -> vp -> whole_s ->
                                        ( list (operation) *
                                          whole_s ) -> ( vt * nat ) -> option (address) ->
                                        ( list (operation) *
                                          option (address) )|
                                        references:
                                          File "", line 620, characters 8-32 ,
                                          File "", line 635, characters 8-32
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (init_transfer#602:8-21 -> init_transfer)
                                        Range: File "", line 602, characters 8-21
                                        Body Range: File "", line 603, character 6 to line 607, character 17
                                        Content: |core: ∀ vt : * . ∀ whole_p : * . ticket (vt) -> whole_p -> proxy_address (vt)|
                                        references: []
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (transfer#609:8-16 -> transfer)
                                        Range: File "", line 609, characters 8-16
                                        Body Range: File "", line 612, character 6 to line 613, character 84
                                        Content: |core: ∀ vt : * . proxy_address (vt) ->
                                        ( ( vt * nat ) *
                                          address ) -> test_exec_result|
                                        references: []
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (originate_uncurried#615:8-27 -> originate_uncurried)
                                        Range: File "", line 615, characters 8-27
                                        Body Range: File "", line 619, character 6 to line 628, character 7
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * .
                                        ( vt *
                                          nat ) -> ticket (vt) -> whole_s ->
                                        ( vp *
                                          whole_s ) -> ( list (operation) *
                                                         whole_s ) -> address|
                                        references: []
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (originate#630:8-17 -> originate)
                                        Range: File "", line 630, characters 8-17
                                        Body Range: File "", line 634, character 6 to line 643, character 7
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * .
                                        ( vt *
                                          nat ) -> ticket (vt) -> whole_s -> vp -> whole_s ->
                                        ( list (operation) *
                                          whole_s ) -> address|
                                        references: []
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        Type definitions:
                                        (proxy_address#600:12-25 -> proxy_address)
                                        Range: File "", line 600, characters 12-25
                                        Body Range: File "", line 600, characters 28-57
                                        Content: : |funtype 'v : * . typed_address (
                                        ( ( 'v * nat ) *
                                          address ) ,
                                        unit)|
                                        references:
                                          File "", line 602, characters 78-91 ,
                                          File "", line 610, characters 26-39
                                        Module definitions:

                      references: []


    references: [] |}]
