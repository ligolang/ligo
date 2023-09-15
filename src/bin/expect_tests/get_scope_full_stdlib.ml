open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 dynamic_entrypoint#50:5-23 entrypoint#52:14-24 failwith#54:4-12 bool#56:5-9 option#57:8-14 Tezos#60:7-12 get_balance#62:6-17 get_amount#63:6-16 get_now#64:6-13 get_sender#65:6-16 get_source#66:6-16 get_level#67:6-15 get_self_address#68:6-22 get_chain_id#69:6-18 get_total_voting_power#70:6-28 get_min_block_time#71:6-24 voting_power#72:6-18 address#73:6-13 implicit_account#74:6-22 join_tickets#75:6-18 read_ticket#76:6-17 never#78:6-11 pairing_check#79:6-19 set_delegate#80:6-18 self#81:25-29 constant#84:25-33 sapling_empty_state#85:25-44 get_contract_opt#87:25-41 get_contract#89:25-37 get_contract_with_error#93:6-29 create_ticket#96:6-19 transaction#101:6-17 call_view#104:25-34 split_ticket#107:6-18 create_contract#109:25-40 create_contract_uncurried#112:25-50 get_entrypoint_opt#114:25-43 get_entrypoint#117:25-39 emit#120:25-29 sapling_verify_update#123:25-46 Bitwise#127:7-14 and#128:6-10 xor#129:6-9 or#130:6-9 shift_left#131:6-16 shift_right#132:6-17 Big_map#135:7-14 empty#136:16-21 literal#137:25-32 mem#139:6-9 add#140:6-9 remove#141:6-12 update#142:6-12 get_and_update#143:6-20 find_opt#144:6-14 find#145:6-10 Map#149:7-10 empty#150:6-11 size#151:6-10 literal#152:25-32 mem#154:6-9 add#155:6-9 remove#156:6-12 update#157:6-12 get_and_update#158:6-20 find#159:6-10 find_opt#160:6-14 iter#161:6-10 map#162:6-9 fold#163:6-10 Transpiled#167:7-17 map_find_opt#168:6-18 map_add#169:6-13 map_remove#170:6-16 Set#173:7-10 empty#174:6-11 size#175:6-10 cardinal#176:6-14 literal#177:25-32 mem#179:6-9 add#180:6-9 remove#181:6-12 update#182:6-12 iter#183:6-10 fold#184:6-10 fold_desc#185:6-15 filter_map#186:6-16 List#190:7-11 length#191:6-12 size#192:6-10 head_opt#193:6-14 tail_opt#194:6-14 map#196:6-9 iter#197:6-10 fold#198:6-10 fold_left#199:6-15 fold_right#200:6-16 cons#201:6-10 find_opt#202:6-14 filter_map#204:6-16 update#206:6-12 update_with#208:6-17 String#212:7-13 length#213:6-12 concats#214:6-13 concat#216:6-12 sub#217:6-9 Option#220:7-13 unopt#221:6-11 unopt_with_error#223:6-22 map#224:15-18 value#225:6-11 value_exn#226:6-15 is_none#227:6-13 is_some#228:6-13 Bytes#231:7-12 concats#232:6-13 pack#233:6-10 unpack#234:6-12 length#235:6-12 concat#237:6-12 sub#238:6-9 Crypto#241:7-13 blake2b#242:6-13 sha256#243:6-12 sha512#244:6-12 sha3#245:6-10 keccak#246:6-12 hash_key#247:6-14 check#248:6-11 assert#251:4-10 assert_some#252:4-15 assert_none#253:4-15 abs#254:4-7 is_nat#255:4-10 unit#256:14-18 int#257:4-7 nat#258:4-7 bytes#259:4-9 ignore#260:4-10 curry#261:4-9 uncurry#262:4-11 assert_with_error#264:4-21 assert_some_with_error#265:4-26 assert_none_with_error#266:4-26 ediv#267:4-8 dynamic_entrypoints#269:5-24 Dynamic_entrypoints#270:7-26 cast_dynamic_entrypoint#272:17-40 set#275:6-9 get#284:6-9 set_bytes#296:6-15 michelson_program#305:5-22 typed_address#306:5-18 mutation#307:5-13 michelson_contract#308:5-23 ast_contract#309:5-17 pbt_gen#310:5-12 int64#311:5-10 views#312:5-10 test_exec_error_balance_too_low#314:5-36 test_exec_error#319:5-20 test_exec_result#324:5-21 test_baker_policy#328:5-22 pbt_test#333:8-16 pbt_result#334:8-18 unforged_ticket#336:8-23 module_contract#338:14-29 Test#340:7-11 run#342:6-9 eval#343:6-10 compile_value#345:6-19 get_total_voting_power#346:6-28 failwith#347:6-14 to_contract#348:6-17 set_source#349:6-16 get_storage_of_address#350:6-28 get_balance#351:6-17 print#352:6-11 eprint#353:6-12 get_voting_power#354:6-22 nth_bootstrap_contract#355:6-28 nth_bootstrap_account#356:6-27 get_bootstrap_account#359:6-27 nth_bootstrap_typed_address#360:6-33 last_originations#361:6-23 random#362:6-12 new_account#365:6-17 decompile#366:6-15 bake_until_n_cycle_end#367:6-28 get_time#368:6-14 cast_address#369:6-18 register_delegate#370:6-23 stake#371:6-11 register_constant#372:6-23 to_typed_address#373:6-22 constant_to_michelson_program#374:6-35 parse_michelson#375:6-21 restore_context#376:6-21 save_context#377:6-18 drop_context#378:6-18 to_string#379:6-15 to_json#380:6-13 get_storage#381:6-17 set_baker_policy#386:6-22 set_baker#387:6-15 size#388:6-10 compile_contract#389:6-22 read_contract_from_file#393:6-29 chr#394:6-9 nl#404:6-8 println#405:6-13 set_print_values#408:6-22 unset_print_values#409:6-24 PBT#411:9-12 gen#412:8-11 gen_small#413:8-17 make_test#414:8-17 run#415:8-11 get_last_events_from#429:6-26 transfer#437:6-14 transfer_exn#438:6-18 log#439:6-9 reset_state#443:6-17 reset_state_at#444:6-20 bootstrap_contract#445:6-24 mutate_value#446:6-18 save_mutation#447:6-19 sign#448:6-10 add_account#449:6-17 baker_account#450:6-19 set_big_map#451:6-17 transfer_to_contract#452:6-26 transfer_to_contract_exn#457:6-30 michelson_equal#462:6-21 to_entrypoint#463:6-19 storage_with_dynamic_entrypoints#471:6-38 originate_contract#477:6-24 originate#478:6-15 compile_contract_with_views#485:8-35 originate_uncurried#488:6-25 originate_module#495:6-22 compile_contract_from_file#502:6-32 originate_from_file#505:6-25 mutation_test#510:6-19 mutation_test_all#522:6-23 originate_from_file_and_mutate#534:6-36 originate_from_file_and_mutate_all#554:6-40 originate_module_and_mutate#574:6-33 originate_module_and_mutate_all#596:6-37 assert#619:6-12 assert_some#620:6-17 assert_none#621:6-17 assert_with_error#623:6-23 assert_some_with_error#624:6-28 assert_none_with_error#625:6-28 Proxy_ticket#627:9-21 proxy_transfer_contract#628:19-42 proxy_originate_contract#640:19-43 proxy_address#652:12-25 init_transfer#654:8-21 transfer#661:8-16 originate_uncurried#667:8-27 originate#682:8-17  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 dynamic_entrypoint#50:5-23 entrypoint#52:14-24 failwith#54:4-12 bool#56:5-9 option#57:8-14 Tezos#60:7-12 get_balance#62:6-17 get_amount#63:6-16 get_now#64:6-13 get_sender#65:6-16 get_source#66:6-16 get_level#67:6-15 get_self_address#68:6-22 get_chain_id#69:6-18 get_total_voting_power#70:6-28 get_min_block_time#71:6-24 voting_power#72:6-18 address#73:6-13 implicit_account#74:6-22 join_tickets#75:6-18 read_ticket#76:6-17 never#78:6-11 pairing_check#79:6-19 set_delegate#80:6-18 self#81:25-29 constant#84:25-33 sapling_empty_state#85:25-44 get_contract_opt#87:25-41 get_contract#89:25-37 get_contract_with_error#93:6-29 create_ticket#96:6-19 transaction#101:6-17 call_view#104:25-34 split_ticket#107:6-18 create_contract#109:25-40 create_contract_uncurried#112:25-50 get_entrypoint_opt#114:25-43 get_entrypoint#117:25-39 emit#120:25-29 sapling_verify_update#123:25-46 Bitwise#127:7-14 and#128:6-10 xor#129:6-9 or#130:6-9 shift_left#131:6-16 shift_right#132:6-17 Big_map#135:7-14 empty#136:16-21 literal#137:25-32 mem#139:6-9 add#140:6-9 remove#141:6-12 update#142:6-12 get_and_update#143:6-20 find_opt#144:6-14 find#145:6-10 Map#149:7-10 empty#150:6-11 size#151:6-10 literal#152:25-32 mem#154:6-9 add#155:6-9 remove#156:6-12 update#157:6-12 get_and_update#158:6-20 find#159:6-10 find_opt#160:6-14 iter#161:6-10 map#162:6-9 fold#163:6-10 Transpiled#167:7-17 map_find_opt#168:6-18 map_add#169:6-13 map_remove#170:6-16 Set#173:7-10 empty#174:6-11 size#175:6-10 cardinal#176:6-14 literal#177:25-32 mem#179:6-9 add#180:6-9 remove#181:6-12 update#182:6-12 iter#183:6-10 fold#184:6-10 fold_desc#185:6-15 filter_map#186:6-16 List#190:7-11 length#191:6-12 size#192:6-10 head_opt#193:6-14 tail_opt#194:6-14 map#196:6-9 iter#197:6-10 fold#198:6-10 fold_left#199:6-15 fold_right#200:6-16 cons#201:6-10 find_opt#202:6-14 filter_map#204:6-16 update#206:6-12 update_with#208:6-17 String#212:7-13 length#213:6-12 concats#214:6-13 concat#216:6-12 sub#217:6-9 Option#220:7-13 unopt#221:6-11 unopt_with_error#223:6-22 map#224:15-18 value#225:6-11 value_exn#226:6-15 is_none#227:6-13 is_some#228:6-13 Bytes#231:7-12 concats#232:6-13 pack#233:6-10 unpack#234:6-12 length#235:6-12 concat#237:6-12 sub#238:6-9 Crypto#241:7-13 blake2b#242:6-13 sha256#243:6-12 sha512#244:6-12 sha3#245:6-10 keccak#246:6-12 hash_key#247:6-14 check#248:6-11 assert#251:4-10 assert_some#252:4-15 assert_none#253:4-15 abs#254:4-7 is_nat#255:4-10 unit#256:14-18 int#257:4-7 nat#258:4-7 bytes#259:4-9 ignore#260:4-10 curry#261:4-9 uncurry#262:4-11 assert_with_error#264:4-21 assert_some_with_error#265:4-26 assert_none_with_error#266:4-26 ediv#267:4-8 dynamic_entrypoints#269:5-24 Dynamic_entrypoints#270:7-26 cast_dynamic_entrypoint#272:17-40 set#275:6-9 get#284:6-9 set_bytes#296:6-15 michelson_program#305:5-22 typed_address#306:5-18 mutation#307:5-13 michelson_contract#308:5-23 ast_contract#309:5-17 pbt_gen#310:5-12 int64#311:5-10 views#312:5-10 test_exec_error_balance_too_low#314:5-36 test_exec_error#319:5-20 test_exec_result#324:5-21 test_baker_policy#328:5-22 pbt_test#333:8-16 pbt_result#334:8-18 unforged_ticket#336:8-23 module_contract#338:14-29 Test#340:7-11 run#342:6-9 eval#343:6-10 compile_value#345:6-19 get_total_voting_power#346:6-28 failwith#347:6-14 to_contract#348:6-17 set_source#349:6-16 get_storage_of_address#350:6-28 get_balance#351:6-17 print#352:6-11 eprint#353:6-12 get_voting_power#354:6-22 nth_bootstrap_contract#355:6-28 nth_bootstrap_account#356:6-27 get_bootstrap_account#359:6-27 nth_bootstrap_typed_address#360:6-33 last_originations#361:6-23 random#362:6-12 new_account#365:6-17 decompile#366:6-15 bake_until_n_cycle_end#367:6-28 get_time#368:6-14 cast_address#369:6-18 register_delegate#370:6-23 stake#371:6-11 register_constant#372:6-23 to_typed_address#373:6-22 constant_to_michelson_program#374:6-35 parse_michelson#375:6-21 restore_context#376:6-21 save_context#377:6-18 drop_context#378:6-18 to_string#379:6-15 to_json#380:6-13 get_storage#381:6-17 set_baker_policy#386:6-22 set_baker#387:6-15 size#388:6-10 compile_contract#389:6-22 read_contract_from_file#393:6-29 chr#394:6-9 nl#404:6-8 println#405:6-13 set_print_values#408:6-22 unset_print_values#409:6-24 PBT#411:9-12 gen#412:8-11 gen_small#413:8-17 make_test#414:8-17 run#415:8-11 get_last_events_from#429:6-26 transfer#437:6-14 transfer_exn#438:6-18 log#439:6-9 reset_state#443:6-17 reset_state_at#444:6-20 bootstrap_contract#445:6-24 mutate_value#446:6-18 save_mutation#447:6-19 sign#448:6-10 add_account#449:6-17 baker_account#450:6-19 set_big_map#451:6-17 transfer_to_contract#452:6-26 transfer_to_contract_exn#457:6-30 michelson_equal#462:6-21 to_entrypoint#463:6-19 storage_with_dynamic_entrypoints#471:6-38 originate_contract#477:6-24 originate#478:6-15 compile_contract_with_views#485:8-35 originate_uncurried#488:6-25 originate_module#495:6-22 compile_contract_from_file#502:6-32 originate_from_file#505:6-25 mutation_test#510:6-19 mutation_test_all#522:6-23 originate_from_file_and_mutate#534:6-36 originate_from_file_and_mutate_all#554:6-40 originate_module_and_mutate#574:6-33 originate_module_and_mutate_all#596:6-37 assert#619:6-12 assert_some#620:6-17 assert_none#621:6-17 assert_with_error#623:6-23 assert_some_with_error#624:6-28 assert_none_with_error#625:6-28 Proxy_ticket#627:9-21 proxy_transfer_contract#628:19-42 proxy_originate_contract#640:19-43 proxy_address#652:12-25 init_transfer#654:8-21 transfer#661:8-16 originate_uncurried#667:8-27 originate#682:8-17 a#1:4-5  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 dynamic_entrypoint#50:5-23 entrypoint#52:14-24 failwith#54:4-12 bool#56:5-9 option#57:8-14 Tezos#60:7-12 get_balance#62:6-17 get_amount#63:6-16 get_now#64:6-13 get_sender#65:6-16 get_source#66:6-16 get_level#67:6-15 get_self_address#68:6-22 get_chain_id#69:6-18 get_total_voting_power#70:6-28 get_min_block_time#71:6-24 voting_power#72:6-18 address#73:6-13 implicit_account#74:6-22 join_tickets#75:6-18 read_ticket#76:6-17 never#78:6-11 pairing_check#79:6-19 set_delegate#80:6-18 self#81:25-29 constant#84:25-33 sapling_empty_state#85:25-44 get_contract_opt#87:25-41 get_contract#89:25-37 get_contract_with_error#93:6-29 create_ticket#96:6-19 transaction#101:6-17 call_view#104:25-34 split_ticket#107:6-18 create_contract#109:25-40 create_contract_uncurried#112:25-50 get_entrypoint_opt#114:25-43 get_entrypoint#117:25-39 emit#120:25-29 sapling_verify_update#123:25-46 Bitwise#127:7-14 and#128:6-10 xor#129:6-9 or#130:6-9 shift_left#131:6-16 shift_right#132:6-17 Big_map#135:7-14 empty#136:16-21 literal#137:25-32 mem#139:6-9 add#140:6-9 remove#141:6-12 update#142:6-12 get_and_update#143:6-20 find_opt#144:6-14 find#145:6-10 Map#149:7-10 empty#150:6-11 size#151:6-10 literal#152:25-32 mem#154:6-9 add#155:6-9 remove#156:6-12 update#157:6-12 get_and_update#158:6-20 find#159:6-10 find_opt#160:6-14 iter#161:6-10 map#162:6-9 fold#163:6-10 Transpiled#167:7-17 map_find_opt#168:6-18 map_add#169:6-13 map_remove#170:6-16 Set#173:7-10 empty#174:6-11 size#175:6-10 cardinal#176:6-14 literal#177:25-32 mem#179:6-9 add#180:6-9 remove#181:6-12 update#182:6-12 iter#183:6-10 fold#184:6-10 fold_desc#185:6-15 filter_map#186:6-16 List#190:7-11 length#191:6-12 size#192:6-10 head_opt#193:6-14 tail_opt#194:6-14 map#196:6-9 iter#197:6-10 fold#198:6-10 fold_left#199:6-15 fold_right#200:6-16 cons#201:6-10 find_opt#202:6-14 filter_map#204:6-16 update#206:6-12 update_with#208:6-17 String#212:7-13 length#213:6-12 concats#214:6-13 concat#216:6-12 sub#217:6-9 Option#220:7-13 unopt#221:6-11 unopt_with_error#223:6-22 map#224:15-18 value#225:6-11 value_exn#226:6-15 is_none#227:6-13 is_some#228:6-13 Bytes#231:7-12 concats#232:6-13 pack#233:6-10 unpack#234:6-12 length#235:6-12 concat#237:6-12 sub#238:6-9 Crypto#241:7-13 blake2b#242:6-13 sha256#243:6-12 sha512#244:6-12 sha3#245:6-10 keccak#246:6-12 hash_key#247:6-14 check#248:6-11 assert#251:4-10 assert_some#252:4-15 assert_none#253:4-15 abs#254:4-7 is_nat#255:4-10 unit#256:14-18 int#257:4-7 nat#258:4-7 bytes#259:4-9 ignore#260:4-10 curry#261:4-9 uncurry#262:4-11 assert_with_error#264:4-21 assert_some_with_error#265:4-26 assert_none_with_error#266:4-26 ediv#267:4-8 dynamic_entrypoints#269:5-24 Dynamic_entrypoints#270:7-26 cast_dynamic_entrypoint#272:17-40 set#275:6-9 get#284:6-9 set_bytes#296:6-15 michelson_program#305:5-22 typed_address#306:5-18 mutation#307:5-13 michelson_contract#308:5-23 ast_contract#309:5-17 pbt_gen#310:5-12 int64#311:5-10 views#312:5-10 test_exec_error_balance_too_low#314:5-36 test_exec_error#319:5-20 test_exec_result#324:5-21 test_baker_policy#328:5-22 pbt_test#333:8-16 pbt_result#334:8-18 unforged_ticket#336:8-23 module_contract#338:14-29 Test#340:7-11 run#342:6-9 eval#343:6-10 compile_value#345:6-19 get_total_voting_power#346:6-28 failwith#347:6-14 to_contract#348:6-17 set_source#349:6-16 get_storage_of_address#350:6-28 get_balance#351:6-17 print#352:6-11 eprint#353:6-12 get_voting_power#354:6-22 nth_bootstrap_contract#355:6-28 nth_bootstrap_account#356:6-27 get_bootstrap_account#359:6-27 nth_bootstrap_typed_address#360:6-33 last_originations#361:6-23 random#362:6-12 new_account#365:6-17 decompile#366:6-15 bake_until_n_cycle_end#367:6-28 get_time#368:6-14 cast_address#369:6-18 register_delegate#370:6-23 stake#371:6-11 register_constant#372:6-23 to_typed_address#373:6-22 constant_to_michelson_program#374:6-35 parse_michelson#375:6-21 restore_context#376:6-21 save_context#377:6-18 drop_context#378:6-18 to_string#379:6-15 to_json#380:6-13 get_storage#381:6-17 set_baker_policy#386:6-22 set_baker#387:6-15 size#388:6-10 compile_contract#389:6-22 read_contract_from_file#393:6-29 chr#394:6-9 nl#404:6-8 println#405:6-13 set_print_values#408:6-22 unset_print_values#409:6-24 PBT#411:9-12 gen#412:8-11 gen_small#413:8-17 make_test#414:8-17 run#415:8-11 get_last_events_from#429:6-26 transfer#437:6-14 transfer_exn#438:6-18 log#439:6-9 reset_state#443:6-17 reset_state_at#444:6-20 bootstrap_contract#445:6-24 mutate_value#446:6-18 save_mutation#447:6-19 sign#448:6-10 add_account#449:6-17 baker_account#450:6-19 set_big_map#451:6-17 transfer_to_contract#452:6-26 transfer_to_contract_exn#457:6-30 michelson_equal#462:6-21 to_entrypoint#463:6-19 storage_with_dynamic_entrypoints#471:6-38 originate_contract#477:6-24 originate#478:6-15 compile_contract_with_views#485:8-35 originate_uncurried#488:6-25 originate_module#495:6-22 compile_contract_from_file#502:6-32 originate_from_file#505:6-25 mutation_test#510:6-19 mutation_test_all#522:6-23 originate_from_file_and_mutate#534:6-36 originate_from_file_and_mutate_all#554:6-40 originate_module_and_mutate#574:6-33 originate_module_and_mutate_all#596:6-37 assert#619:6-12 assert_some#620:6-17 assert_none#621:6-17 assert_with_error#623:6-23 assert_some_with_error#624:6-28 assert_none_with_error#625:6-28 Proxy_ticket#627:9-21 proxy_transfer_contract#628:19-42 proxy_originate_contract#640:19-43 proxy_address#652:12-25 init_transfer#654:8-21 transfer#661:8-16 originate_uncurried#667:8-27 originate#682:8-17 a#1:4-5 c#5:10-11  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 dynamic_entrypoint#50:5-23 entrypoint#52:14-24 failwith#54:4-12 bool#56:5-9 option#57:8-14 Tezos#60:7-12 get_balance#62:6-17 get_amount#63:6-16 get_now#64:6-13 get_sender#65:6-16 get_source#66:6-16 get_level#67:6-15 get_self_address#68:6-22 get_chain_id#69:6-18 get_total_voting_power#70:6-28 get_min_block_time#71:6-24 voting_power#72:6-18 address#73:6-13 implicit_account#74:6-22 join_tickets#75:6-18 read_ticket#76:6-17 never#78:6-11 pairing_check#79:6-19 set_delegate#80:6-18 self#81:25-29 constant#84:25-33 sapling_empty_state#85:25-44 get_contract_opt#87:25-41 get_contract#89:25-37 get_contract_with_error#93:6-29 create_ticket#96:6-19 transaction#101:6-17 call_view#104:25-34 split_ticket#107:6-18 create_contract#109:25-40 create_contract_uncurried#112:25-50 get_entrypoint_opt#114:25-43 get_entrypoint#117:25-39 emit#120:25-29 sapling_verify_update#123:25-46 Bitwise#127:7-14 and#128:6-10 xor#129:6-9 or#130:6-9 shift_left#131:6-16 shift_right#132:6-17 Big_map#135:7-14 empty#136:16-21 literal#137:25-32 mem#139:6-9 add#140:6-9 remove#141:6-12 update#142:6-12 get_and_update#143:6-20 find_opt#144:6-14 find#145:6-10 Map#149:7-10 empty#150:6-11 size#151:6-10 literal#152:25-32 mem#154:6-9 add#155:6-9 remove#156:6-12 update#157:6-12 get_and_update#158:6-20 find#159:6-10 find_opt#160:6-14 iter#161:6-10 map#162:6-9 fold#163:6-10 Transpiled#167:7-17 map_find_opt#168:6-18 map_add#169:6-13 map_remove#170:6-16 Set#173:7-10 empty#174:6-11 size#175:6-10 cardinal#176:6-14 literal#177:25-32 mem#179:6-9 add#180:6-9 remove#181:6-12 update#182:6-12 iter#183:6-10 fold#184:6-10 fold_desc#185:6-15 filter_map#186:6-16 List#190:7-11 length#191:6-12 size#192:6-10 head_opt#193:6-14 tail_opt#194:6-14 map#196:6-9 iter#197:6-10 fold#198:6-10 fold_left#199:6-15 fold_right#200:6-16 cons#201:6-10 find_opt#202:6-14 filter_map#204:6-16 update#206:6-12 update_with#208:6-17 String#212:7-13 length#213:6-12 concats#214:6-13 concat#216:6-12 sub#217:6-9 Option#220:7-13 unopt#221:6-11 unopt_with_error#223:6-22 map#224:15-18 value#225:6-11 value_exn#226:6-15 is_none#227:6-13 is_some#228:6-13 Bytes#231:7-12 concats#232:6-13 pack#233:6-10 unpack#234:6-12 length#235:6-12 concat#237:6-12 sub#238:6-9 Crypto#241:7-13 blake2b#242:6-13 sha256#243:6-12 sha512#244:6-12 sha3#245:6-10 keccak#246:6-12 hash_key#247:6-14 check#248:6-11 assert#251:4-10 assert_some#252:4-15 assert_none#253:4-15 abs#254:4-7 is_nat#255:4-10 unit#256:14-18 int#257:4-7 nat#258:4-7 bytes#259:4-9 ignore#260:4-10 curry#261:4-9 uncurry#262:4-11 assert_with_error#264:4-21 assert_some_with_error#265:4-26 assert_none_with_error#266:4-26 ediv#267:4-8 dynamic_entrypoints#269:5-24 Dynamic_entrypoints#270:7-26 cast_dynamic_entrypoint#272:17-40 set#275:6-9 get#284:6-9 set_bytes#296:6-15 michelson_program#305:5-22 typed_address#306:5-18 mutation#307:5-13 michelson_contract#308:5-23 ast_contract#309:5-17 pbt_gen#310:5-12 int64#311:5-10 views#312:5-10 test_exec_error_balance_too_low#314:5-36 test_exec_error#319:5-20 test_exec_result#324:5-21 test_baker_policy#328:5-22 pbt_test#333:8-16 pbt_result#334:8-18 unforged_ticket#336:8-23 module_contract#338:14-29 Test#340:7-11 run#342:6-9 eval#343:6-10 compile_value#345:6-19 get_total_voting_power#346:6-28 failwith#347:6-14 to_contract#348:6-17 set_source#349:6-16 get_storage_of_address#350:6-28 get_balance#351:6-17 print#352:6-11 eprint#353:6-12 get_voting_power#354:6-22 nth_bootstrap_contract#355:6-28 nth_bootstrap_account#356:6-27 get_bootstrap_account#359:6-27 nth_bootstrap_typed_address#360:6-33 last_originations#361:6-23 random#362:6-12 new_account#365:6-17 decompile#366:6-15 bake_until_n_cycle_end#367:6-28 get_time#368:6-14 cast_address#369:6-18 register_delegate#370:6-23 stake#371:6-11 register_constant#372:6-23 to_typed_address#373:6-22 constant_to_michelson_program#374:6-35 parse_michelson#375:6-21 restore_context#376:6-21 save_context#377:6-18 drop_context#378:6-18 to_string#379:6-15 to_json#380:6-13 get_storage#381:6-17 set_baker_policy#386:6-22 set_baker#387:6-15 size#388:6-10 compile_contract#389:6-22 read_contract_from_file#393:6-29 chr#394:6-9 nl#404:6-8 println#405:6-13 set_print_values#408:6-22 unset_print_values#409:6-24 PBT#411:9-12 gen#412:8-11 gen_small#413:8-17 make_test#414:8-17 run#415:8-11 get_last_events_from#429:6-26 transfer#437:6-14 transfer_exn#438:6-18 log#439:6-9 reset_state#443:6-17 reset_state_at#444:6-20 bootstrap_contract#445:6-24 mutate_value#446:6-18 save_mutation#447:6-19 sign#448:6-10 add_account#449:6-17 baker_account#450:6-19 set_big_map#451:6-17 transfer_to_contract#452:6-26 transfer_to_contract_exn#457:6-30 michelson_equal#462:6-21 to_entrypoint#463:6-19 storage_with_dynamic_entrypoints#471:6-38 originate_contract#477:6-24 originate#478:6-15 compile_contract_with_views#485:8-35 originate_uncurried#488:6-25 originate_module#495:6-22 compile_contract_from_file#502:6-32 originate_from_file#505:6-25 mutation_test#510:6-19 mutation_test_all#522:6-23 originate_from_file_and_mutate#534:6-36 originate_from_file_and_mutate_all#554:6-40 originate_module_and_mutate#574:6-33 originate_module_and_mutate_all#596:6-37 assert#619:6-12 assert_some#620:6-17 assert_none#621:6-17 assert_with_error#623:6-23 assert_some_with_error#624:6-28 assert_none_with_error#625:6-28 Proxy_ticket#627:9-21 proxy_transfer_contract#628:19-42 proxy_originate_contract#640:19-43 proxy_address#652:12-25 init_transfer#654:8-21 transfer#661:8-16 originate_uncurried#667:8-27 originate#682:8-17 a#1:4-5 c#5:10-11 d#5:26-27  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ string#3:5-11 bytes#4:5-10 int#5:5-8 nat#6:5-8 unit#7:5-9 operation#10:5-14 tez#11:5-8 address#12:5-12 signature#13:5-14 key#14:5-8 key_hash#15:5-13 timestamp#16:5-14 list#17:5-9 big_map#18:5-12 map#19:5-8 set#20:5-8 contract#21:5-13 michelson_or#22:5-17 michelson_pair#23:5-19 chain_id#24:5-13 baker_hash#25:5-15 pvss_key#26:5-13 sapling_state#27:5-18 sapling_transaction#28:5-24 baker_operation#29:5-20 bls12_381_g1#30:5-17 bls12_381_g2#31:5-17 bls12_381_fr#32:5-17 never#33:5-10 ticket#34:5-11 external_bytes#37:5-19 external_int#38:5-17 external_ediv#39:5-18 external_and#40:5-17 external_or#41:5-16 external_xor#42:5-17 external_lsl#43:5-17 external_lsr#44:5-17 external_map_find_opt#45:5-26 external_map_add#46:5-21 external_map_remove#47:5-24 external_map_remove_value#48:5-30 dynamic_entrypoint#50:5-23 entrypoint#52:14-24 failwith#54:4-12 bool#56:5-9 option#57:8-14 Tezos#60:7-12 get_balance#62:6-17 get_amount#63:6-16 get_now#64:6-13 get_sender#65:6-16 get_source#66:6-16 get_level#67:6-15 get_self_address#68:6-22 get_chain_id#69:6-18 get_total_voting_power#70:6-28 get_min_block_time#71:6-24 voting_power#72:6-18 address#73:6-13 implicit_account#74:6-22 join_tickets#75:6-18 read_ticket#76:6-17 never#78:6-11 pairing_check#79:6-19 set_delegate#80:6-18 self#81:25-29 constant#84:25-33 sapling_empty_state#85:25-44 get_contract_opt#87:25-41 get_contract#89:25-37 get_contract_with_error#93:6-29 create_ticket#96:6-19 transaction#101:6-17 call_view#104:25-34 split_ticket#107:6-18 create_contract#109:25-40 create_contract_uncurried#112:25-50 get_entrypoint_opt#114:25-43 get_entrypoint#117:25-39 emit#120:25-29 sapling_verify_update#123:25-46 Bitwise#127:7-14 and#128:6-10 xor#129:6-9 or#130:6-9 shift_left#131:6-16 shift_right#132:6-17 Big_map#135:7-14 empty#136:16-21 literal#137:25-32 mem#139:6-9 add#140:6-9 remove#141:6-12 update#142:6-12 get_and_update#143:6-20 find_opt#144:6-14 find#145:6-10 Map#149:7-10 empty#150:6-11 size#151:6-10 literal#152:25-32 mem#154:6-9 add#155:6-9 remove#156:6-12 update#157:6-12 get_and_update#158:6-20 find#159:6-10 find_opt#160:6-14 iter#161:6-10 map#162:6-9 fold#163:6-10 Transpiled#167:7-17 map_find_opt#168:6-18 map_add#169:6-13 map_remove#170:6-16 Set#173:7-10 empty#174:6-11 size#175:6-10 cardinal#176:6-14 literal#177:25-32 mem#179:6-9 add#180:6-9 remove#181:6-12 update#182:6-12 iter#183:6-10 fold#184:6-10 fold_desc#185:6-15 filter_map#186:6-16 List#190:7-11 length#191:6-12 size#192:6-10 head_opt#193:6-14 tail_opt#194:6-14 map#196:6-9 iter#197:6-10 fold#198:6-10 fold_left#199:6-15 fold_right#200:6-16 cons#201:6-10 find_opt#202:6-14 filter_map#204:6-16 update#206:6-12 update_with#208:6-17 String#212:7-13 length#213:6-12 concats#214:6-13 concat#216:6-12 sub#217:6-9 Option#220:7-13 unopt#221:6-11 unopt_with_error#223:6-22 map#224:15-18 value#225:6-11 value_exn#226:6-15 is_none#227:6-13 is_some#228:6-13 Bytes#231:7-12 concats#232:6-13 pack#233:6-10 unpack#234:6-12 length#235:6-12 concat#237:6-12 sub#238:6-9 Crypto#241:7-13 blake2b#242:6-13 sha256#243:6-12 sha512#244:6-12 sha3#245:6-10 keccak#246:6-12 hash_key#247:6-14 check#248:6-11 assert#251:4-10 assert_some#252:4-15 assert_none#253:4-15 abs#254:4-7 is_nat#255:4-10 unit#256:14-18 int#257:4-7 nat#258:4-7 bytes#259:4-9 ignore#260:4-10 curry#261:4-9 uncurry#262:4-11 assert_with_error#264:4-21 assert_some_with_error#265:4-26 assert_none_with_error#266:4-26 ediv#267:4-8 dynamic_entrypoints#269:5-24 Dynamic_entrypoints#270:7-26 cast_dynamic_entrypoint#272:17-40 set#275:6-9 get#284:6-9 set_bytes#296:6-15 michelson_program#305:5-22 typed_address#306:5-18 mutation#307:5-13 michelson_contract#308:5-23 ast_contract#309:5-17 pbt_gen#310:5-12 int64#311:5-10 views#312:5-10 test_exec_error_balance_too_low#314:5-36 test_exec_error#319:5-20 test_exec_result#324:5-21 test_baker_policy#328:5-22 pbt_test#333:8-16 pbt_result#334:8-18 unforged_ticket#336:8-23 module_contract#338:14-29 Test#340:7-11 run#342:6-9 eval#343:6-10 compile_value#345:6-19 get_total_voting_power#346:6-28 failwith#347:6-14 to_contract#348:6-17 set_source#349:6-16 get_storage_of_address#350:6-28 get_balance#351:6-17 print#352:6-11 eprint#353:6-12 get_voting_power#354:6-22 nth_bootstrap_contract#355:6-28 nth_bootstrap_account#356:6-27 get_bootstrap_account#359:6-27 nth_bootstrap_typed_address#360:6-33 last_originations#361:6-23 random#362:6-12 new_account#365:6-17 decompile#366:6-15 bake_until_n_cycle_end#367:6-28 get_time#368:6-14 cast_address#369:6-18 register_delegate#370:6-23 stake#371:6-11 register_constant#372:6-23 to_typed_address#373:6-22 constant_to_michelson_program#374:6-35 parse_michelson#375:6-21 restore_context#376:6-21 save_context#377:6-18 drop_context#378:6-18 to_string#379:6-15 to_json#380:6-13 get_storage#381:6-17 set_baker_policy#386:6-22 set_baker#387:6-15 size#388:6-10 compile_contract#389:6-22 read_contract_from_file#393:6-29 chr#394:6-9 nl#404:6-8 println#405:6-13 set_print_values#408:6-22 unset_print_values#409:6-24 PBT#411:9-12 gen#412:8-11 gen_small#413:8-17 make_test#414:8-17 run#415:8-11 get_last_events_from#429:6-26 transfer#437:6-14 transfer_exn#438:6-18 log#439:6-9 reset_state#443:6-17 reset_state_at#444:6-20 bootstrap_contract#445:6-24 mutate_value#446:6-18 save_mutation#447:6-19 sign#448:6-10 add_account#449:6-17 baker_account#450:6-19 set_big_map#451:6-17 transfer_to_contract#452:6-26 transfer_to_contract_exn#457:6-30 michelson_equal#462:6-21 to_entrypoint#463:6-19 storage_with_dynamic_entrypoints#471:6-38 originate_contract#477:6-24 originate#478:6-15 compile_contract_with_views#485:8-35 originate_uncurried#488:6-25 originate_module#495:6-22 compile_contract_from_file#502:6-32 originate_from_file#505:6-25 mutation_test#510:6-19 mutation_test_all#522:6-23 originate_from_file_and_mutate#534:6-36 originate_from_file_and_mutate_all#554:6-40 originate_module_and_mutate#574:6-33 originate_module_and_mutate_all#596:6-37 assert#619:6-12 assert_some#620:6-17 assert_none#621:6-17 assert_with_error#623:6-23 assert_some_with_error#624:6-28 assert_none_with_error#625:6-28 Proxy_ticket#627:9-21 proxy_transfer_contract#628:19-42 proxy_originate_contract#640:19-43 proxy_address#652:12-25 init_transfer#654:8-21 transfer#661:8-16 originate_uncurried#667:8-27 originate#682:8-17 a#1:4-5 e#6:9-10  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32

    Variable definitions:
    (failwith#54:4-12 -> failwith)
    Range: File "", line 54, characters 4-12
    Body Range: File "", line 54, characters 34-73
    Content: |unresolved|
    references:
      File "", line 91, characters 27-35 ,
      File "", line 95, characters 27-35 ,
      File "", line 119, characters 27-35 ,
      File "", line 221, characters 79-87 ,
      File "", line 223, characters 103-111 ,
      File "", line 226, characters 83-91 ,
      File "", line 251, characters 49-57 ,
      File "", line 252, characters 72-80 ,
      File "", line 253, characters 87-95 ,
      File "", line 264, characters 66-74 ,
      File "", line 265, characters 96-104 ,
      File "", line 266, characters 111-119
    Mod Path =
    Def Type = Global
    (assert#251:4-10 -> assert)
    Range: File "", line 251, characters 4-10
    Body Range: File "", line 251, characters 31-76
    Content: |core: bool -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (assert_some#252:4-15 -> assert_some)
    Range: File "", line 252, characters 4-15
    Body Range: File "", line 252, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (assert_none#253:4-15 -> assert_none)
    Range: File "", line 253, characters 4-15
    Body Range: File "", line 253, characters 49-116
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (abs#254:4-7 -> abs)
    Range: File "", line 254, characters 4-7
    Body Range: File "", line 254, characters 26-62
    Content: |core: int -> nat|
    references: File "", line 467, characters 31-34
    Mod Path =
    Def Type = Global
    (is_nat#255:4-10 -> is_nat)
    Range: File "", line 255, characters 4-10
    Body Range: File "", line 255, characters 36-81
    Content: |core: int -> option (nat)|
    references: []
    Mod Path =
    Def Type = Global
    (unit#256:14-18 -> unit)
    Range: File "", line 256, characters 14-18
    Body Range: File "", line 256, characters 28-48
    Content: |core: unit|
    references: []
    Mod Path =
    Def Type = Global
    (int#257:4-7 -> int)
    Range: File "", line 257, characters 4-7
    Body Range: File "", line 257, characters 44-96
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 359, characters 97-100 ,
      File "", line 397, characters 79-82 ,
      File "", line 399, characters 78-81 ,
      File "", line 401, characters 72-75
    Mod Path =
    Def Type = Global
    (nat#258:4-7 -> nat)
    Range: File "", line 258, characters 4-7
    Body Range: File "", line 258, characters 28-73
    Content: |core: bytes -> nat|
    references: []
    Mod Path =
    Def Type = Global
    (bytes#259:4-9 -> bytes)
    Range: File "", line 259, characters 4-9
    Body Range: File "", line 259, characters 48-104
    Content: |core: ∀ a : * . a -> external_bytes (a)|
    references: []
    Mod Path =
    Def Type = Global
    (ignore#260:4-10 -> ignore)
    Range: File "", line 260, characters 4-10
    Body Range: File "", line 260, characters 37-39
    Content: |core: ∀ a : * . a -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (curry#261:4-9 -> curry)
    Range: File "", line 261, characters 4-9
    Body Range: File "", line 261, characters 62-70
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    Mod Path =
    Def Type = Global
    (uncurry#262:4-11 -> uncurry)
    Range: File "", line 262, characters 4-11
    Body Range: File "", line 262, characters 62-73
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 479, characters 30-37
    Mod Path =
    Def Type = Global
    (assert_with_error#264:4-21 -> assert_with_error)
    Range: File "", line 264, characters 4-21
    Body Range: File "", line 264, characters 48-76
    Content: |unresolved|
    references: []
    Mod Path =
    Def Type = Global
    (assert_some_with_error#265:4-26 -> assert_some_with_error)
    Range: File "", line 265, characters 4-26
    Body Range: File "", line 265, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (assert_none_with_error#266:4-26 -> assert_none_with_error)
    Range: File "", line 266, characters 4-26
    Body Range: File "", line 266, characters 73-121
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    Mod Path =
    Def Type = Global
    (ediv#267:4-8 -> ediv)
    Range: File "", line 267, characters 4-8
    Body Range: File "", line 267, characters 61-117
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
      File "", line 81, characters 44-50 ,
      File "", line 83, characters 48-54 ,
      File "", line 84, characters 48-54 ,
      File "", line 93, characters 58-64 ,
      File "", line 104, characters 51-57 ,
      File "", line 106, characters 62-68 ,
      File "", line 114, characters 58-64 ,
      File "", line 116, characters 65-71 ,
      File "", line 117, characters 54-60 ,
      File "", line 120, characters 44-50 ,
      File "", line 122, characters 61-67 ,
      File "", line 213, characters 18-24 ,
      File "", line 214, characters 20-26 ,
      File "", line 214, characters 35-41 ,
      File "", line 216, characters 19-25 ,
      File "", line 216, characters 33-39 ,
      File "", line 216, characters 43-49 ,
      File "", line 217, characters 35-41 ,
      File "", line 217, characters 45-51 ,
      File "", line 223, characters 52-58 ,
      File "", line 264, characters 38-44 ,
      File "", line 265, characters 56-62 ,
      File "", line 266, characters 56-62 ,
      File "", line 321, characters 13-19 ,
      File "", line 352, characters 17-23 ,
      File "", line 353, characters 18-24 ,
      File "", line 359, characters 56-62 ,
      File "", line 365, characters 31-37 ,
      File "", line 372, characters 50-56 ,
      File "", line 374, characters 41-47 ,
      File "", line 375, characters 27-33 ,
      File "", line 379, characters 35-41 ,
      File "", line 380, characters 33-39 ,
      File "", line 393, characters 36-42 ,
      File "", line 394, characters 22-28 ,
      File "", line 405, characters 19-25 ,
      File "", line 429, characters 76-82 ,
      File "", line 437, characters 140-146 ,
      File "", line 438, characters 135-141 ,
      File "", line 447, characters 25-31 ,
      File "", line 447, characters 50-56 ,
      File "", line 448, characters 17-23 ,
      File "", line 449, characters 23-29 ,
      File "", line 450, characters 25-31 ,
      File "", line 454, characters 12-18 ,
      File "", line 459, characters 14-20 ,
      File "", line 463, characters 38-44 ,
      File "", line 502, characters 39-45 ,
      File "", line 505, characters 32-38 ,
      File "", line 534, characters 52-58 ,
      File "", line 554, characters 56-62 ,
      File "", line 623, characters 40-46 ,
      File "", line 624, characters 58-64 ,
      File "", line 625, characters 58-64
    (bytes#4:5-10 -> bytes)
    Range: File "", line 4, characters 5-10
    Body Range: File "", line 4, characters 13-30
    Content: : |bytes|
    references:
      File "", line 123, characters 121-126 ,
      File "", line 123, characters 219-224 ,
      File "", line 232, characters 20-25 ,
      File "", line 232, characters 34-39 ,
      File "", line 233, characters 30-35 ,
      File "", line 233, characters 70-75 ,
      File "", line 234, characters 27-32 ,
      File "", line 235, characters 18-23 ,
      File "", line 237, characters 19-24 ,
      File "", line 237, characters 32-37 ,
      File "", line 237, characters 41-46 ,
      File "", line 238, characters 35-40 ,
      File "", line 238, characters 44-49 ,
      File "", line 242, characters 19-24 ,
      File "", line 242, characters 28-33 ,
      File "", line 242, characters 71-76 ,
      File "", line 243, characters 18-23 ,
      File "", line 243, characters 27-32 ,
      File "", line 243, characters 69-74 ,
      File "", line 244, characters 18-23 ,
      File "", line 244, characters 27-32 ,
      File "", line 244, characters 69-74 ,
      File "", line 245, characters 16-21 ,
      File "", line 245, characters 25-30 ,
      File "", line 245, characters 65-70 ,
      File "", line 246, characters 18-23 ,
      File "", line 246, characters 27-32 ,
      File "", line 246, characters 69-74 ,
      File "", line 248, characters 43-48 ,
      File "", line 258, characters 13-18 ,
      File "", line 258, characters 57-62 ,
      File "", line 269, characters 32-37 ,
      File "", line 298, characters 9-14 ,
      File "", line 448, characters 30-35
    (int#5:5-8 -> int)
    Range: File "", line 5, characters 5-8
    Body Range: File "", line 5, characters 11-26
    Content: : |int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 14-17 ,
      File "", line 123, characters 130-133 ,
      File "", line 123, characters 228-231 ,
      File "", line 254, characters 13-16 ,
      File "", line 255, characters 16-19 ,
      File "", line 330, characters 16-19 ,
      File "", line 356, characters 33-36 ,
      File "", line 388, characters 38-41 ,
      File "", line 451, characters 34-37 ,
      File "", line 478, characters 127-130 ,
      File "", line 488, characters 136-139 ,
      File "", line 495, characters 137-140 ,
      File "", line 505, characters 108-111 ,
      File "", line 535, characters 78-81 ,
      File "", line 555, characters 82-85 ,
      File "", line 575, characters 90-93 ,
      File "", line 597, characters 94-97
    (nat#6:5-8 -> nat)
    Range: File "", line 6, characters 5-8
    Body Range: File "", line 6, characters 11-26
    Content: : |nat|
    references:
      File "", line 67, characters 30-33 ,
      File "", line 67, characters 67-70 ,
      File "", line 70, characters 43-46 ,
      File "", line 70, characters 93-96 ,
      File "", line 71, characters 39-42 ,
      File "", line 71, characters 85-88 ,
      File "", line 72, characters 37-40 ,
      File "", line 72, characters 84-87 ,
      File "", line 76, characters 60-63 ,
      File "", line 77, characters 66-69 ,
      File "", line 96, characters 42-45 ,
      File "", line 96, characters 114-117 ,
      File "", line 107, characters 48-51 ,
      File "", line 107, characters 54-57 ,
      File "", line 151, characters 41-44 ,
      File "", line 175, characters 34-37 ,
      File "", line 176, characters 38-41 ,
      File "", line 191, characters 38-41 ,
      File "", line 192, characters 36-39 ,
      File "", line 213, characters 28-31 ,
      File "", line 217, characters 15-18 ,
      File "", line 217, characters 25-28 ,
      File "", line 235, characters 27-30 ,
      File "", line 238, characters 15-18 ,
      File "", line 238, characters 25-28 ,
      File "", line 254, characters 20-23 ,
      File "", line 254, characters 57-60 ,
      File "", line 255, characters 23-26 ,
      File "", line 255, characters 69-72 ,
      File "", line 258, characters 22-25 ,
      File "", line 258, characters 66-69 ,
      File "", line 269, characters 28-31 ,
      File "", line 272, characters 85-88 ,
      File "", line 326, characters 15-18 ,
      File "", line 336, characters 86-89 ,
      File "", line 346, characters 43-46 ,
      File "", line 354, characters 41-44 ,
      File "", line 355, characters 34-37 ,
      File "", line 359, characters 33-36 ,
      File "", line 360, characters 50-53 ,
      File "", line 367, characters 34-37 ,
      File "", line 394, characters 15-18 ,
      File "", line 415, characters 48-51 ,
      File "", line 416, characters 31-34 ,
      File "", line 438, characters 69-72 ,
      File "", line 443, characters 23-26 ,
      File "", line 444, characters 40-43 ,
      File "", line 446, characters 33-36 ,
      File "", line 457, characters 77-80 ,
      File "", line 503, characters 90-93 ,
      File "", line 513, characters 30-33 ,
      File "", line 525, characters 58-61 ,
      File "", line 541, characters 90-93 ,
      File "", line 544, characters 30-33 ,
      File "", line 561, characters 90-93 ,
      File "", line 564, characters 58-61 ,
      File "", line 586, characters 30-33 ,
      File "", line 608, characters 58-61 ,
      File "", line 630, characters 26-29 ,
      File "", line 643, characters 28-31 ,
      File "", line 652, characters 35-38 ,
      File "", line 655, characters 33-36 ,
      File "", line 663, characters 29-32 ,
      File "", line 668, characters 28-31 ,
      File "", line 671, characters 35-38 ,
      File "", line 683, characters 28-31 ,
      File "", line 686, characters 35-38
    (unit#7:5-9 -> unit)
    Range: File "", line 7, characters 5-9
    Body Range: File "", line 7, characters 12-28
    Content: : |unit|
    references:
      File "", line 62, characters 24-28 ,
      File "", line 63, characters 23-27 ,
      File "", line 64, characters 20-24 ,
      File "", line 65, characters 23-27 ,
      File "", line 66, characters 23-27 ,
      File "", line 67, characters 22-26 ,
      File "", line 68, characters 29-33 ,
      File "", line 69, characters 25-29 ,
      File "", line 70, characters 35-39 ,
      File "", line 71, characters 31-35 ,
      File "", line 74, characters 41-45 ,
      File "", line 74, characters 102-106 ,
      File "", line 105, characters 12-16 ,
      File "", line 115, characters 12-16 ,
      File "", line 121, characters 12-16 ,
      File "", line 161, characters 36-40 ,
      File "", line 161, characters 61-65 ,
      File "", line 183, characters 30-34 ,
      File "", line 183, characters 50-54 ,
      File "", line 197, characters 30-34 ,
      File "", line 197, characters 51-55 ,
      File "", line 251, characters 24-28 ,
      File "", line 252, characters 42-46 ,
      File "", line 253, characters 42-46 ,
      File "", line 256, characters 21-25 ,
      File "", line 260, characters 30-34 ,
      File "", line 265, characters 66-70 ,
      File "", line 266, characters 66-70 ,
      File "", line 346, characters 35-39 ,
      File "", line 349, characters 33-37 ,
      File "", line 352, characters 27-31 ,
      File "", line 353, characters 28-32 ,
      File "", line 361, characters 29-33 ,
      File "", line 362, characters 28-32 ,
      File "", line 365, characters 23-27 ,
      File "", line 367, characters 41-45 ,
      File "", line 368, characters 21-25 ,
      File "", line 370, characters 42-46 ,
      File "", line 371, characters 40-44 ,
      File "", line 376, characters 27-31 ,
      File "", line 376, characters 35-39 ,
      File "", line 377, characters 24-28 ,
      File "", line 377, characters 32-36 ,
      File "", line 378, characters 24-28 ,
      File "", line 378, characters 32-36 ,
      File "", line 386, characters 50-54 ,
      File "", line 387, characters 32-36 ,
      File "", line 405, characters 29-33 ,
      File "", line 408, characters 28-32 ,
      File "", line 408, characters 36-40 ,
      File "", line 409, characters 30-34 ,
      File "", line 409, characters 38-42 ,
      File "", line 439, characters 29-33 ,
      File "", line 443, characters 45-49 ,
      File "", line 444, characters 62-66 ,
      File "", line 445, characters 90-94 ,
      File "", line 449, characters 43-47 ,
      File "", line 450, characters 58-62 ,
      File "", line 451, characters 62-66 ,
      File "", line 466, characters 20-22 ,
      File "", line 511, characters 31-35 ,
      File "", line 511, characters 47-51 ,
      File "", line 515, characters 44-48 ,
      File "", line 515, characters 98-102 ,
      File "", line 523, characters 31-35 ,
      File "", line 523, characters 47-51 ,
      File "", line 527, characters 44-48 ,
      File "", line 527, characters 98-102 ,
      File "", line 542, characters 31-35 ,
      File "", line 542, characters 47-51 ,
      File "", line 547, characters 44-48 ,
      File "", line 547, characters 103-107 ,
      File "", line 562, characters 31-35 ,
      File "", line 562, characters 47-51 ,
      File "", line 567, characters 44-48 ,
      File "", line 567, characters 103-107 ,
      File "", line 584, characters 31-35 ,
      File "", line 584, characters 47-51 ,
      File "", line 589, characters 44-48 ,
      File "", line 589, characters 103-107 ,
      File "", line 606, characters 31-35 ,
      File "", line 606, characters 47-51 ,
      File "", line 611, characters 44-48 ,
      File "", line 611, characters 103-107 ,
      File "", line 619, characters 26-30 ,
      File "", line 620, characters 44-48 ,
      File "", line 621, characters 44-48 ,
      File "", line 624, characters 68-72 ,
      File "", line 625, characters 68-72 ,
      File "", line 631, characters 20-24 ,
      File "", line 632, characters 27-31 ,
      File "", line 652, characters 52-56 ,
      File "", line 655, characters 51-55 ,
      File "", line 655, characters 76-80 ,

    (operation#10:5-14 -> operation)
    Range: File "", line 10, characters 5-14
    Body Range: File "", line 10, characters 17-38
    Content: : |operation|
    references:
      File "", line 52, characters 39-48 ,
      File "", line 80, characters 43-52 ,
      File "", line 80, characters 95-104 ,
      File "", line 101, characters 65-74 ,
      File "", line 102, characters 80-89 ,
      File "", line 109, characters 67-76 ,
      File "", line 109, characters 131-140 ,
      File "", line 112, characters 76-85 ,
      File "", line 112, characters 140-149 ,
      File "", line 120, characters 62-71 ,
      File "", line 122, characters 91-100 ,
      File "", line 292, characters 33-42 ,
      File "", line 338, characters 44-53 ,
      File "", line 389, characters 48-57 ,
      File "", line 445, characters 50-59 ,
      File "", line 478, characters 42-51 ,
      File "", line 485, characters 61-70 ,
      File "", line 488, characters 51-60 ,
      File "", line 632, characters 10-19 ,
      File "", line 642, characters 39-48 ,
      File "", line 645, characters 10-19 ,
      File "", line 655, characters 59-68 ,
      File "", line 670, characters 39-48 ,
      File "", line 671, characters 60-69 ,
      File "", line 685, characters 40-49 ,
      File "", line 686, characters 60-69
    (tez#11:5-8 -> tez)
    Range: File "", line 11, characters 5-8
    Body Range: File "", line 11, characters 11-26
    Content: : |tez|
    references:
      File "", line 62, characters 32-35 ,
      File "", line 62, characters 71-74 ,
      File "", line 63, characters 31-34 ,
      File "", line 63, characters 69-72 ,
      File "", line 101, characters 41-44 ,
      File "", line 102, characters 60-63 ,
      File "", line 109, characters 115-118 ,
      File "", line 112, characters 124-127 ,
      File "", line 315, characters 23-26 ,
      File "", line 317, characters 20-23 ,
      File "", line 351, characters 34-37 ,
      File "", line 371, characters 33-36 ,
      File "", line 437, characters 58-61 ,
      File "", line 438, characters 62-65 ,
      File "", line 443, characters 33-36 ,
      File "", line 444, characters 50-53 ,
      File "", line 445, characters 83-86 ,
      File "", line 450, characters 44-47 ,
      File "", line 452, characters 66-69 ,
      File "", line 457, characters 70-73 ,
      File "", line 477, characters 79-82 ,
      File "", line 478, characters 75-78 ,
      File "", line 488, characters 84-87 ,
      File "", line 495, characters 85-88 ,
      File "", line 505, characters 70-73 ,
      File "", line 534, characters 89-92 ,
      File "", line 554, characters 93-96 ,
      File "", line 574, characters 98-101 ,
      File "", line 596, characters 102-105
    (address#12:5-12 -> address)
    Range: File "", line 12, characters 5-12
    Body Range: File "", line 12, characters 15-34
    Content: : |address|
    references:
      File "", line 65, characters 31-38 ,
      File "", line 65, characters 73-80 ,
      File "", line 66, characters 31-38 ,
      File "", line 66, characters 73-80 ,
      File "", line 68, characters 37-44 ,
      File "", line 68, characters 85-92 ,
      File "", line 73, characters 42-49 ,
      File "", line 73, characters 87-94 ,
      File "", line 76, characters 45-52 ,
      File "", line 77, characters 51-58 ,
      File "", line 87, characters 56-63 ,
      File "", line 89, characters 52-59 ,
      File "", line 93, characters 44-51 ,
      File "", line 104, characters 72-79 ,
      File "", line 109, characters 143-150 ,
      File "", line 112, characters 152-159 ,
      File "", line 114, characters 71-78 ,
      File "", line 117, characters 67-74 ,
      File "", line 316, characters 23-30 ,
      File "", line 322, characters 36-43 ,
      File "", line 329, characters 18-25 ,
      File "", line 331, characters 17-24 ,
      File "", line 336, characters 54-61 ,
      File "", line 349, characters 22-29 ,
      File "", line 350, characters 34-41 ,
      File "", line 351, characters 23-30 ,
      File "", line 355, characters 41-48 ,
      File "", line 356, characters 40-47 ,
      File "", line 359, characters 40-47 ,
      File "", line 361, characters 38-45 ,
      File "", line 361, characters 47-54 ,
      File "", line 369, characters 35-42 ,
      File "", line 383, characters 12-19 ,
      File "", line 387, characters 21-28 ,
      File "", line 431, characters 21-28 ,
      File "", line 432, characters 45-52 ,
      File "", line 437, characters 20-27 ,
      File "", line 438, characters 24-31 ,
      File "", line 453, characters 12-19 ,
      File "", line 458, characters 14-21 ,
      File "", line 477, characters 86-93 ,
      File "", line 505, characters 77-84 ,
      File "", line 535, characters 47-54 ,
      File "", line 555, characters 51-58 ,
      File "", line 630, characters 33-40 ,
      File "", line 644, characters 22-29 ,
      File "", line 645, characters 27-34 ,
      File "", line 652, characters 42-49 ,
      File "", line 655, characters 40-47 ,
      File "", line 663, characters 36-43 ,
      File "", line 670, characters 67-74 ,
      File "", line 671, characters 42-49 ,
      File "", line 671, characters 77-84 ,
      File "", line 674, characters 68-75 ,
      File "", line 685, characters 68-75 ,
      File "", line 686, characters 42-49 ,
      File "", line 686, characters 77-84 ,
      File "", line 689, characters 68-75
    (signature#13:5-14 -> signature)
    Range: File "", line 13, characters 5-14
    Body Range: File "", line 13, characters 17-38
    Content: : |signature|
    references:
      File "", line 248, characters 27-36 ,
      File "", line 448, characters 39-48
    (key#14:5-8 -> key)
    Range: File "", line 14, characters 5-8
    Body Range: File "", line 14, characters 11-26
    Content: : |key|
    references:
      File "", line 247, characters 20-23 ,
      File "", line 248, characters 17-20 ,
      File "", line 359, characters 50-53 ,
      File "", line 365, characters 40-43 ,
      File "", line 449, characters 36-39 ,
      File "", line 450, characters 34-37
    (key_hash#15:5-13 -> key_hash)
    Range: File "", line 15, characters 5-13
    Body Range: File "", line 15, characters 16-36
    Content: : |key_hash|
    references:
      File "", line 72, characters 25-33 ,
      File "", line 74, characters 29-37 ,
      File "", line 80, characters 24-32 ,
      File "", line 109, characters 93-101 ,
      File "", line 112, characters 102-110 ,
      File "", line 247, characters 27-35 ,
      File "", line 247, characters 74-82 ,
      File "", line 354, characters 29-37 ,
      File "", line 370, characters 30-38 ,
      File "", line 371, characters 18-26 ,
      File "", line 649, characters 54-62
    (timestamp#16:5-14 -> timestamp)
    Range: File "", line 16, characters 5-14
    Body Range: File "", line 16, characters 17-38
    Content: : |timestamp|
    references:
      File "", line 64, characters 28-37 ,
      File "", line 64, characters 69-78 ,
      File "", line 368, characters 29-38 ,
      File "", line 443, characters 92-101 ,
      File "", line 444, characters 24-33
    (list#17:5-9 -> list)
    Range: File "", line 17, characters 5-9
    Body Range: File "", line 17, characters 12-28
    Content: : |list|
    references:
      File "", line 52, characters 49-53 ,
      File "", line 79, characters 55-59 ,
      File "", line 109, characters 77-81 ,
      File "", line 112, characters 86-90 ,
      File "", line 137, characters 57-61 ,
      File "", line 152, characters 57-61 ,
      File "", line 177, characters 49-53 ,
      File "", line 191, characters 30-34 ,
      File "", line 192, characters 28-32 ,
      File "", line 193, characters 32-36 ,
      File "", line 194, characters 32-36 ,
      File "", line 194, characters 43-47 ,
      File "", line 196, characters 42-46 ,
      File "", line 196, characters 52-56 ,
      File "", line 197, characters 44-48 ,
      File "", line 198, characters 47-51 ,
      File "", line 199, characters 60-64 ,
      File "", line 200, characters 53-57 ,
      File "", line 201, characters 36-40 ,
      File "", line 201, characters 46-50 ,
      File "", line 202, characters 48-52 ,
      File "", line 204, characters 56-60 ,
      File "", line 204, characters 66-70 ,
      File "", line 205, characters 31-35 ,
      File "", line 206, characters 50-54 ,
      File "", line 206, characters 60-64 ,
      File "", line 208, characters 59-63 ,
      File "", line 208, characters 69-73 ,
      File "", line 214, characters 27-31 ,
      File "", line 232, characters 26-30 ,
      File "", line 292, characters 43-47 ,
      File "", line 331, characters 25-29 ,
      File "", line 338, characters 54-58 ,
      File "", line 361, characters 55-59 ,
      File "", line 389, characters 58-62 ,
      File "", line 429, characters 88-92 ,
      File "", line 431, characters 34-38 ,
      File "", line 432, characters 37-41 ,
      File "", line 432, characters 63-67 ,
      File "", line 436, characters 33-37 ,
      File "", line 443, characters 37-41 ,
      File "", line 444, characters 54-58 ,
      File "", line 445, characters 60-64 ,
      File "", line 478, characters 52-56 ,
      File "", line 485, characters 71-75 ,
      File "", line 488, characters 61-65 ,
      File "", line 522, characters 78-82 ,
      File "", line 525, characters 47-51 ,
      File "", line 525, characters 80-84 ,
      File "", line 533, characters 38-42 ,
      File "", line 555, characters 109-113 ,
      File "", line 564, characters 47-51 ,
      File "", line 564, characters 80-84 ,
      File "", line 573, characters 38-42 ,
      File "", line 597, characters 121-125 ,
      File "", line 608, characters 47-51 ,
      File "", line 608, characters 80-84 ,
      File "", line 617, characters 38-42 ,
      File "", line 632, characters 20-24 ,
      File "", line 642, characters 49-53 ,
      File "", line 645, characters 20-24 ,
      File "", line 655, characters 69-73 ,
      File "", line 670, characters 49-53 ,
      File "", line 671, characters 70-74 ,
      File "", line 685, characters 50-54 ,
      File "", line 686, characters 70-74
    (big_map#18:5-12 -> big_map)
    Range: File "", line 18, characters 5-12
    Body Range: File "", line 18, characters 15-34
    Content: : |big_map|
    references:
      File "", line 136, characters 42-49 ,
      File "", line 137, characters 72-79 ,
      File "", line 139, characters 41-48 ,
      File "", line 140, characters 49-56 ,
      File "", line 140, characters 67-74 ,
      File "", line 141, characters 44-51 ,
      File "", line 141, characters 62-69 ,
      File "", line 142, characters 59-66 ,
      File "", line 142, characters 77-84 ,
      File "", line 143, characters 67-74 ,
      File "", line 143, characters 96-103 ,
      File "", line 144, characters 46-53 ,
      File "", line 145, characters 42-49 ,
      File "", line 269, characters 39-46 ,
      File "", line 451, characters 51-58
    (map#19:5-8 -> map)
    Range: File "", line 19, characters 5-8
    Body Range: File "", line 19, characters 11-26
    Content: : |map|
    references:
      File "", line 150, characters 32-35 ,
      File "", line 151, characters 34-37 ,
      File "", line 152, characters 72-75 ,
      File "", line 154, characters 41-44 ,
      File "", line 155, characters 49-52 ,
      File "", line 155, characters 63-66 ,
      File "", line 156, characters 44-47 ,
      File "", line 156, characters 58-61 ,
      File "", line 157, characters 59-62 ,
      File "", line 157, characters 73-76 ,
      File "", line 158, characters 67-70 ,
      File "", line 158, characters 92-95 ,
      File "", line 159, characters 42-45 ,
      File "", line 160, characters 46-49 ,
      File "", line 161, characters 54-57 ,
      File "", line 162, characters 52-55 ,
      File "", line 162, characters 66-69 ,
      File "", line 163, characters 59-62 ,
      File "", line 361, characters 61-64
    (set#20:5-8 -> set)
    Range: File "", line 20, characters 5-8
    Body Range: File "", line 20, characters 11-26
    Content: : |set|
    references:
      File "", line 174, characters 25-28 ,
      File "", line 175, characters 27-30 ,
      File "", line 176, characters 31-34 ,
      File "", line 177, characters 59-62 ,
      File "", line 179, characters 34-37 ,
      File "", line 180, characters 34-37 ,
      File "", line 180, characters 43-46 ,
      File "", line 181, characters 37-40 ,
      File "", line 181, characters 46-49 ,
      File "", line 182, characters 48-51 ,
      File "", line 183, characters 43-46 ,
      File "", line 184, characters 46-49 ,
      File "", line 185, characters 51-54 ,
      File "", line 186, characters 56-59 ,
      File "", line 186, characters 65-68 ,
      File "", line 187, characters 30-33 ,
      File "", line 187, characters 106-109
    (contract#21:5-13 -> contract)
    Range: File "", line 21, characters 5-13
    Body Range: File "", line 21, characters 16-36
    Content: : |contract|
    references:
      File "", line 73, characters 30-38 ,
      File "", line 74, characters 46-54 ,
      File "", line 74, characters 107-115 ,
      File "", line 81, characters 56-64 ,
      File "", line 83, characters 60-68 ,
      File "", line 87, characters 70-78 ,
      File "", line 88, characters 74-82 ,
      File "", line 89, characters 66-74 ,
      File "", line 93, characters 70-78 ,
      File "", line 101, characters 53-61 ,
      File "", line 102, characters 68-76 ,
      File "", line 114, characters 84-92 ,
      File "", line 116, characters 98-106 ,
      File "", line 117, characters 80-88 ,
      File "", line 348, characters 60-68 ,
      File "", line 373, characters 41-49 ,
      File "", line 382, characters 14-22 ,
      File "", line 452, characters 43-51 ,
      File "", line 457, characters 47-55 ,
      File "", line 463, characters 77-85 ,
      File "", line 636, characters 22-30
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
      File "", line 69, characters 33-41 ,
      File "", line 69, characters 78-86
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
      File "", line 85, characters 66-79 ,
      File "", line 86, characters 90-103 ,
      File "", line 123, characters 103-116 ,
      File "", line 123, characters 142-155 ,
      File "", line 123, characters 240-253
    (sapling_transaction#28:5-24 -> sapling_transaction)
    Range: File "", line 28, characters 5-24
    Body Range: File "", line 28, characters 27-58
    Content: : |sapling_transaction|
    references: File "", line 123, characters 71-90
    (baker_operation#29:5-20 -> baker_operation)
    Range: File "", line 29, characters 5-20
    Body Range: File "", line 29, characters 23-50
    Content: : |baker_operation|
    references: []
    (bls12_381_g1#30:5-17 -> bls12_381_g1)
    Range: File "", line 30, characters 5-17
    Body Range: File "", line 30, characters 20-44
    Content: : |bls12_381_g1|
    references: File "", line 79, characters 26-38
    (bls12_381_g2#31:5-17 -> bls12_381_g2)
    Range: File "", line 31, characters 5-17
    Body Range: File "", line 31, characters 20-44
    Content: : |bls12_381_g2|
    references: File "", line 79, characters 41-53
    (bls12_381_fr#32:5-17 -> bls12_381_fr)
    Range: File "", line 32, characters 5-17
    Body Range: File "", line 32, characters 20-44
    Content: : |bls12_381_fr|
    references: []
    (never#33:5-10 -> never)
    Range: File "", line 33, characters 5-10
    Body Range: File "", line 33, characters 13-30
    Content: : |never|
    references: File "", line 78, characters 26-31
    (ticket#34:5-11 -> ticket)
    Range: File "", line 34, characters 5-11
    Body Range: File "", line 34, characters 14-32
    Content: : |ticket|
    references:
      File "", line 75, characters 35-41 ,
      File "", line 75, characters 46-52 ,
      File "", line 75, characters 59-65 ,
      File "", line 75, characters 118-124 ,
      File "", line 76, characters 34-40 ,
      File "", line 76, characters 70-76 ,
      File "", line 77, characters 76-82 ,
      File "", line 96, characters 52-58 ,
      File "", line 96, characters 124-130 ,
      File "", line 107, characters 35-41 ,
      File "", line 107, characters 64-70 ,
      File "", line 107, characters 75-81 ,
      File "", line 108, characters 49-55 ,
      File "", line 108, characters 60-66 ,
      File "", line 629, characters 23-29 ,
      File "", line 641, characters 25-31 ,
      File "", line 654, characters 54-60 ,
      File "", line 669, characters 26-32 ,
      File "", line 684, characters 26-32
    (external_bytes#37:5-19 -> external_bytes)
    Range: File "", line 37, characters 5-19
    Body Range: File "", line 37, characters 22-48
    Content: : |external_bytes|
    references:
      File "", line 259, characters 31-45 ,
      File "", line 259, characters 86-100
    (external_int#38:5-17 -> external_int)
    Range: File "", line 38, characters 5-17
    Body Range: File "", line 38, characters 20-44
    Content: : |external_int|
    references:
      File "", line 257, characters 29-41 ,
      File "", line 257, characters 80-92
    (external_ediv#39:5-18 -> external_ediv)
    Range: File "", line 39, characters 5-18
    Body Range: File "", line 39, characters 21-46
    Content: : |external_ediv|
    references:
      File "", line 267, characters 45-58 ,
      File "", line 267, characters 102-115
    (external_and#40:5-17 -> external_and)
    Range: File "", line 40, characters 5-17
    Body Range: File "", line 40, characters 20-44
    Content: : |external_and|
    references:
      File "", line 128, characters 54-66 ,
      File "", line 128, characters 123-135
    (external_or#41:5-16 -> external_or)
    Range: File "", line 41, characters 5-16
    Body Range: File "", line 41, characters 19-42
    Content: : |external_or|
    references:
      File "", line 129, characters 54-65 ,
      File "", line 129, characters 123-134
    (external_xor#42:5-17 -> external_xor)
    Range: File "", line 42, characters 5-17
    Body Range: File "", line 42, characters 20-44
    Content: : |external_xor|
    references:
      File "", line 130, characters 54-66 ,
      File "", line 130, characters 123-135
    (external_lsl#43:5-17 -> external_lsl)
    Range: File "", line 43, characters 5-17
    Body Range: File "", line 43, characters 20-44
    Content: : |external_lsl|
    references:
      File "", line 131, characters 54-66 ,
      File "", line 131, characters 123-135
    (external_lsr#44:5-17 -> external_lsr)
    Range: File "", line 44, characters 5-17
    Body Range: File "", line 44, characters 20-44
    Content: : |external_lsr|
    references:
      File "", line 132, characters 54-66 ,
      File "", line 132, characters 123-135
    (external_map_find_opt#45:5-26 -> external_map_find_opt)
    Range: File "", line 45, characters 5-26
    Body Range: File "", line 45, characters 29-62
    Content: : |external_map_find_opt|
    references:
      File "", line 168, characters 55-76 ,
      File "", line 168, characters 119-140
    (external_map_add#46:5-21 -> external_map_add)
    Range: File "", line 46, characters 5-21
    Body Range: File "", line 46, characters 24-52
    Content: : |external_map_add|
    references:
      File "", line 169, characters 63-79 ,
      File "", line 169, characters 145-161
    (external_map_remove#47:5-24 -> external_map_remove)
    Range: File "", line 47, characters 5-24
    Body Range: File "", line 47, characters 27-58
    Content: : |external_map_remove|
    references:
      File "", line 170, characters 53-72 ,
      File "", line 170, characters 197-216
    (external_map_remove_value#48:5-30 -> external_map_remove_value)
    Range: File "", line 48, characters 5-30
    Body Range: File "", line 48, characters 33-70
    Content: : |external_map_remove_value|
    references: File "", line 170, characters 149-174
    (dynamic_entrypoint#50:5-23 -> dynamic_entrypoint)
    Range: File "", line 50, characters 5-23
    Body Range: File "", line 50, characters 26-56
    Content: : |dynamic_entrypoint|
    references:
      File "", line 272, characters 63-81 ,
      File "", line 276, characters 15-33 ,
      File "", line 285, characters 15-33 ,
      File "", line 297, characters 15-33
    (entrypoint#52:14-24 -> entrypoint)
    Range: File "", line 52, characters 14-24
    Body Range: File "", line 52, characters 27-58
    Content: : |funtype 'p : * . funtype 's : * . 'p -> 's -> ( list (operation) *
                                                                's )|
    references:
      File "", line 277, characters 16-26 ,
      File "", line 287, characters 16-26
    (bool#56:5-9 -> bool)
    Range: File "", line 56, characters 5-9
    Body Range: File "", line 56, characters 12-24
    Content: : |bool|
    references:
      File "", line 79, characters 63-67 ,
      File "", line 79, characters 111-115 ,
      File "", line 139, characters 52-56 ,
      File "", line 154, characters 48-52 ,
      File "", line 179, characters 41-45 ,
      File "", line 182, characters 35-39 ,
      File "", line 202, characters 34-38 ,
      File "", line 208, characters 37-41 ,
      File "", line 227, characters 40-44 ,
      File "", line 228, characters 40-44 ,
      File "", line 248, characters 52-56 ,
      File "", line 248, characters 106-110 ,
      File "", line 251, characters 16-20 ,
      File "", line 264, characters 27-31 ,
      File "", line 333, characters 41-45 ,
      File "", line 414, characters 53-57 ,
      File "", line 462, characters 74-78 ,
      File "", line 619, characters 18-22 ,
      File "", line 623, characters 29-33 ,

    (option#57:8-14 -> option)
    Range: File "", line 57, characters 8-14
    Body Range: File "", line 57, characters 17-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 75, characters 67-73 ,
      File "", line 75, characters 125-131 ,
      File "", line 80, characters 33-39 ,
      File "", line 82, characters 14-20 ,
      File "", line 86, characters 74-80 ,
      File "", line 87, characters 80-86 ,
      File "", line 88, characters 59-65 ,
      File "", line 88, characters 84-90 ,
      File "", line 96, characters 60-66 ,
      File "", line 96, characters 132-138 ,
      File "", line 104, characters 86-92 ,
      File "", line 106, characters 80-86 ,
      File "", line 106, characters 96-102 ,
      File "", line 107, characters 83-89 ,
      File "", line 108, characters 68-74 ,
      File "", line 109, characters 102-108 ,
      File "", line 112, characters 111-117 ,
      File "", line 114, characters 93-99 ,
      File "", line 116, characters 83-89 ,
      File "", line 116, characters 108-114 ,
      File "", line 122, characters 79-85 ,
      File "", line 123, characters 158-164 ,
      File "", line 123, characters 256-262 ,
      File "", line 142, characters 39-45 ,
      File "", line 143, characters 47-53 ,
      File "", line 143, characters 80-86 ,
      File "", line 144, characters 59-65 ,
      File "", line 157, characters 39-45 ,
      File "", line 158, characters 47-53 ,
      File "", line 158, characters 76-82 ,
      File "", line 160, characters 55-61 ,
      File "", line 170, characters 176-182 ,
      File "", line 186, characters 40-46 ,
      File "", line 193, characters 42-48 ,
      File "", line 194, characters 49-55 ,
      File "", line 202, characters 58-64 ,
      File "", line 203, characters 31-37 ,
      File "", line 204, characters 40-46 ,
      File "", line 206, characters 34-40 ,
      File "", line 221, characters 28-34 ,
      File "", line 223, characters 39-45 ,
      File "", line 224, characters 50-56 ,
      File "", line 224, characters 62-68 ,
      File "", line 225, characters 42-48 ,
      File "", line 226, characters 48-54 ,
      File "", line 227, characters 30-36 ,
      File "", line 228, characters 30-36 ,
      File "", line 234, characters 38-44 ,
      File "", line 234, characters 100-106 ,
      File "", line 234, characters 114-120 ,
      File "", line 252, characters 32-38 ,
      File "", line 253, characters 32-38 ,
      File "", line 255, characters 27-33 ,
      File "", line 255, characters 73-79 ,
      File "", line 265, characters 43-49 ,
      File "", line 266, characters 43-49 ,
      File "", line 277, characters 27-33 ,
      File "", line 287, characters 27-33 ,
      File "", line 292, characters 53-59 ,
      File "", line 298, characters 15-21 ,
      File "", line 338, characters 98-104 ,
      File "", line 394, characters 29-35 ,
      File "", line 437, characters 147-153 ,
      File "", line 438, characters 142-148 ,
      File "", line 443, characters 102-108 ,
      File "", line 446, characters 63-69 ,
      File "", line 447, characters 57-63 ,
      File "", line 450, characters 48-54 ,
      File "", line 454, characters 19-25 ,
      File "", line 459, characters 21-27 ,
      File "", line 503, characters 94-100 ,
      File "", line 510, characters 74-80 ,
      File "", line 513, characters 52-58 ,
      File "", line 535, characters 105-111 ,
      File "", line 541, characters 94-100 ,
      File "", line 544, characters 52-58 ,
      File "", line 561, characters 94-100 ,
      File "", line 575, characters 117-123 ,
      File "", line 586, characters 52-58 ,
      File "", line 620, characters 34-40 ,
      File "", line 621, characters 34-40 ,
      File "", line 624, characters 45-51 ,
      File "", line 625, characters 45-51 ,
      File "", line 644, characters 30-36 ,
      File "", line 645, characters 35-41 ,
      File "", line 649, characters 63-69 ,
      File "", line 671, characters 50-56 ,
      File "", line 671, characters 85-91 ,
      File "", line 674, characters 76-82 ,
      File "", line 686, characters 50-56 ,
      File "", line 686, characters 85-91 ,
      File "", line 689, characters 76-82
    (dynamic_entrypoints#269:5-24 -> dynamic_entrypoints)
    Range: File "", line 269, characters 5-24
    Body Range: File "", line 269, characters 27-38
    Content: : |big_map (nat ,
    bytes)|
    references:
      File "", line 278, characters 9-28 ,
      File "", line 279, characters 9-28 ,
      File "", line 286, characters 9-28 ,
      File "", line 299, characters 9-28 ,
      File "", line 300, characters 9-28 ,
      File "", line 338, characters 78-97 ,
      File "", line 472, characters 67-86
    (michelson_program#305:5-22 -> michelson_program)
    Range: File "", line 305, characters 5-22
    Body Range: File "", line 305, characters 25-54
    Content: : |michelson_program|
    references:
      File "", line 322, characters 16-33 ,
      File "", line 342, characters 44-61 ,
      File "", line 343, characters 30-47 ,
      File "", line 345, characters 39-56 ,
      File "", line 350, characters 45-62 ,
      File "", line 366, characters 30-47 ,
      File "", line 372, characters 29-46 ,
      File "", line 374, characters 51-68 ,
      File "", line 375, characters 37-54 ,
      File "", line 384, characters 12-29 ,
      File "", line 437, characters 34-51 ,
      File "", line 438, characters 38-55 ,
      File "", line 455, characters 12-29 ,
      File "", line 460, characters 14-31 ,
      File "", line 462, characters 28-45 ,
      File "", line 462, characters 53-70 ,
      File "", line 477, characters 55-72 ,
      File "", line 505, characters 45-62 ,
      File "", line 534, characters 65-82 ,
      File "", line 554, characters 69-86
    (typed_address#306:5-18 -> typed_address)
    Range: File "", line 306, characters 5-18
    Body Range: File "", line 306, characters 21-46
    Content: : |typed_address|
    references:
      File "", line 348, characters 41-54 ,
      File "", line 360, characters 64-77 ,
      File "", line 369, characters 53-66 ,
      File "", line 373, characters 60-73 ,
      File "", line 381, characters 41-54 ,
      File "", line 429, characters 54-67 ,
      File "", line 463, characters 58-71 ,
      File "", line 478, characters 90-103 ,
      File "", line 483, characters 19-32 ,
      File "", line 488, characters 99-112 ,
      File "", line 493, characters 19-32 ,
      File "", line 495, characters 100-113 ,
      File "", line 500, characters 19-32 ,
      File "", line 575, characters 51-64 ,
      File "", line 581, characters 21-34 ,
      File "", line 597, characters 55-68 ,
      File "", line 603, characters 21-34 ,
      File "", line 652, characters 58-71 ,
      File "", line 678, characters 55-68 ,
      File "", line 693, characters 55-68
    (mutation#307:5-13 -> mutation)
    Range: File "", line 307, characters 5-13
    Body Range: File "", line 307, characters 16-36
    Content: : |mutation|
    references:
      File "", line 446, characters 53-61 ,
      File "", line 447, characters 38-46 ,
      File "", line 510, characters 64-72 ,
      File "", line 512, characters 35-43 ,
      File "", line 513, characters 42-50 ,
      File "", line 522, characters 68-76 ,
      File "", line 524, characters 35-43 ,
      File "", line 525, characters 37-45 ,
      File "", line 525, characters 70-78 ,
      File "", line 533, characters 28-36 ,
      File "", line 535, characters 95-103 ,
      File "", line 543, characters 35-43 ,
      File "", line 544, characters 42-50 ,
      File "", line 555, characters 99-107 ,
      File "", line 563, characters 35-43 ,
      File "", line 564, characters 37-45 ,
      File "", line 564, characters 70-78 ,
      File "", line 573, characters 28-36 ,
      File "", line 575, characters 107-115 ,
      File "", line 585, characters 35-43 ,
      File "", line 586, characters 42-50 ,
      File "", line 597, characters 111-119 ,
      File "", line 607, characters 35-43 ,
      File "", line 608, characters 37-45 ,
      File "", line 608, characters 70-78 ,
      File "", line 617, characters 28-36
    (michelson_contract#308:5-23 -> michelson_contract)
    Range: File "", line 308, characters 5-23
    Body Range: File "", line 308, characters 26-56
    Content: : |michelson_contract|
    references:
      File "", line 388, characters 16-34 ,
      File "", line 389, characters 70-88 ,
      File "", line 393, characters 46-64 ,
      File "", line 477, characters 30-48 ,
      File "", line 478, characters 106-124 ,
      File "", line 485, characters 98-116 ,
      File "", line 488, characters 115-133 ,
      File "", line 495, characters 116-134 ,
      File "", line 502, characters 49-67 ,
      File "", line 505, characters 87-105 ,
      File "", line 535, characters 57-75 ,
      File "", line 555, characters 61-79 ,
      File "", line 575, characters 68-86 ,
      File "", line 597, characters 72-90
    (ast_contract#309:5-17 -> ast_contract)
    Range: File "", line 309, characters 5-17
    Body Range: File "", line 309, characters 20-44
    Content: : |ast_contract|
    references:
      File "", line 391, characters 16-28 ,
      File "", line 486, characters 18-30 ,
      File "", line 503, characters 16-28 ,
      File "", line 536, characters 25-37 ,
      File "", line 541, characters 16-28 ,
      File "", line 556, characters 25-37 ,
      File "", line 561, characters 16-28 ,
      File "", line 577, characters 25-37 ,
      File "", line 583, characters 16-28 ,
      File "", line 599, characters 25-37 ,
      File "", line 605, characters 16-28
    (pbt_gen#310:5-12 -> pbt_gen)
    Range: File "", line 310, characters 5-12
    Body Range: File "", line 310, characters 15-34
    Content: : |pbt_gen|
    references:
      File "", line 333, characters 23-30 ,
      File "", line 363, characters 14-21 ,
      File "", line 412, characters 25-32 ,
      File "", line 413, characters 31-38 ,
      File "", line 414, characters 34-41
    (int64#311:5-10 -> int64)
    Range: File "", line 311, characters 5-10
    Body Range: File "", line 311, characters 13-30
    Content: : |int64|
    references: []
    (views#312:5-10 -> views)
    Range: File "", line 312, characters 5-10
    Body Range: File "", line 312, characters 13-30
    Content: : |views|
    references:
      File "", line 338, characters 70-75 ,
      File "", line 390, characters 18-23 ,
      File "", line 485, characters 89-94
    (test_exec_error_balance_too_low#314:5-36 -> test_exec_error_balance_too_low)
    Range: File "", line 314, characters 5-36
    Body Range: File "", line 315, character 2 to line 317, character 25
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez({ name: contract_balance }, { name: contract_too_low }, { name: spend_request })]|
    references: File "", line 320, characters 23-54
    (test_exec_error#319:5-20 -> test_exec_error)
    Range: File "", line 319, characters 5-20
    Body Range: File "", line 320, character 4 to line 322, character 43
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )({ name: Balance_too_low }, { name: Other }, { name: Rejected })]|
    references: File "", line 325, characters 12-27
    (test_exec_result#324:5-21 -> test_exec_result)
    Range: File "", line 324, characters 5-21
    Body Range: File "", line 325, character 4 to line 326, character 18
    Content: : |sum[Fail -> test_exec_error ,
                    Success -> nat({ name: Fail }, { name: Success })]|
    references:
      File "", line 437, characters 65-81 ,
      File "", line 452, characters 73-89 ,
      File "", line 663, characters 47-63
    (test_baker_policy#328:5-22 -> test_baker_policy)
    Range: File "", line 328, characters 5-22
    Body Range: File "", line 329, character 4 to line 331, character 24
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)({ name: By_account }, { name: By_round }, { name: Excluding })]|
    references: File "", line 386, characters 29-46
    (pbt_test#333:8-16 -> pbt_test)
    Range: File "", line 333, characters 8-16
    Body Range: File "", line 333, characters 19-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 414, characters 63-71 ,
      File "", line 415, characters 33-41
    (pbt_result#334:8-18 -> pbt_result)
    Range: File "", line 334, characters 8-18
    Body Range: File "", line 334, characters 21-41
    Content: : |funtype 'a : * . sum[Fail -> 'a ,
                                     Success -> unit({ name: Fail }, { name: Success })]|
    references:
      File "", line 415, characters 57-67 ,
      File "", line 416, characters 39-49 ,
      File "", line 418, characters 84-94 ,
      File "", line 422, characters 96-106 ,
      File "", line 425, characters 68-78
    (unforged_ticket#336:8-23 -> unforged_ticket)
    Range: File "", line 336, characters 8-23
    Body Range: File "", line 336, characters 41-91
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    (module_contract#338:14-29 -> module_contract)
    Range: File "", line 338, characters 14-29
    Body Range: File "", line 338, characters 32-97
    Content: : |funtype 'p : * . funtype 's : * . ( ( 'p * 's ) -> ( list (operation) *
                                                                     's ) *
                                                    views ('s) *
                                                    option (dynamic_entrypoints) )|
    references:
      File "", line 471, characters 80-95 ,
      File "", line 495, characters 55-70 ,
      File "", line 574, characters 68-83 ,
      File "", line 596, characters 72-87
    Module definitions:
    (Tezos#60:7-12 -> Tezos)
    Range: File "", line 60, characters 7-12
    Body Range: File "", line 60, character 15 to line 125, character 3
    Content: Members: Variable definitions:
                      (get_balance#62:6-17 -> get_balance)
                      Range: File "", line 62, characters 6-17
                      Body Range: File "", line 62, characters 38-76
                      Content: |core: unit -> tez|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_amount#63:6-16 -> get_amount)
                      Range: File "", line 63, characters 6-16
                      Body Range: File "", line 63, characters 37-74
                      Content: |core: unit -> tez|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_now#64:6-13 -> get_now)
                      Range: File "", line 64, characters 6-13
                      Body Range: File "", line 64, characters 40-80
                      Content: |core: unit -> timestamp|
                      references: File "", line 368, characters 47-54
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_sender#65:6-16 -> get_sender)
                      Range: File "", line 65, characters 6-16
                      Body Range: File "", line 65, characters 41-82
                      Content: |core: unit -> address|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_source#66:6-16 -> get_source)
                      Range: File "", line 66, characters 6-16
                      Body Range: File "", line 66, characters 41-82
                      Content: |core: unit -> address|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_level#67:6-15 -> get_level)
                      Range: File "", line 67, characters 6-15
                      Body Range: File "", line 67, characters 36-72
                      Content: |core: unit -> nat|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_self_address#68:6-22 -> get_self_address)
                      Range: File "", line 68, characters 6-22
                      Body Range: File "", line 68, characters 47-94
                      Content: |core: unit -> address|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_chain_id#69:6-18 -> get_chain_id)
                      Range: File "", line 69, characters 6-18
                      Body Range: File "", line 69, characters 44-88
                      Content: |core: unit -> chain_id|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_total_voting_power#70:6-28 -> get_total_voting_power)
                      Range: File "", line 70, characters 6-28
                      Body Range: File "", line 70, characters 49-98
                      Content: |core: unit -> nat|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_min_block_time#71:6-24 -> get_min_block_time)
                      Range: File "", line 71, characters 6-24
                      Body Range: File "", line 71, characters 45-90
                      Content: |core: unit -> nat|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (voting_power#72:6-18 -> voting_power)
                      Range: File "", line 72, characters 6-18
                      Body Range: File "", line 72, characters 43-89
                      Content: |core: key_hash -> nat|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (address#73:6-13 -> address)
                      Range: File "", line 73, characters 6-13
                      Body Range: File "", line 73, characters 52-96
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 430, characters 21-28
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (implicit_account#74:6-22 -> implicit_account)
                      Range: File "", line 74, characters 6-22
                      Body Range: File "", line 74, characters 57-117
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (join_tickets#75:6-18 -> join_tickets)
                      Range: File "", line 75, characters 6-18
                      Body Range: File "", line 75, characters 76-133
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (read_ticket#76:6-17 -> read_ticket)
                      Range: File "", line 76, characters 6-17
                      Body Range: File "", line 77, characters 4-84
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address *
                                                                    ( a * nat ) ) *
                                                                  ticket (a) )|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (never#78:6-11 -> never)
                      Range: File "", line 78, characters 6-11
                      Body Range: File "", line 78, characters 39-75
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (pairing_check#79:6-19 -> pairing_check)
                      Range: File "", line 79, characters 6-19
                      Body Range: File "", line 79, characters 70-117
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (set_delegate#80:6-18 -> set_delegate)
                      Range: File "", line 80, characters 6-18
                      Body Range: File "", line 80, characters 55-106
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (self#81:25-29 -> self)
                      Range: File "", line 81, characters 25-29
                      Body Range: File "", line 82, character 4 to line 83, character 70
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (constant#84:25-33 -> constant)
                      Range: File "", line 84, characters 25-33
                      Body Range: File "", line 84, characters 62-96
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (sapling_empty_state#85:25-44 -> sapling_empty_state)
                      Range: File "", line 85, characters 25-44
                      Body Range: File "", line 86, characters 4-105
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_contract_opt#87:25-41 -> get_contract_opt)
                      Range: File "", line 87, characters 25-41
                      Body Range: File "", line 88, characters 4-92
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 90, characters 12-28 ,
                        File "", line 94, characters 12-28
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_contract#89:25-37 -> get_contract)
                      Range: File "", line 89, characters 25-37
                      Body Range: File "", line 90, character 4 to line 91, character 68
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_contract_with_error#93:6-29 -> get_contract_with_error)
                      Range: File "", line 93, characters 6-29
                      Body Range: File "", line 94, character 4 to line 95, character 39
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: File "", line 636, characters 39-62
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (create_ticket#96:6-19 -> create_ticket)
                      Range: File "", line 96, characters 6-19
                      Body Range: File "", line 96, characters 69-147
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references:
                        File "", line 634, characters 39-52 ,
                        File "", line 647, characters 39-52
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (transaction#101:6-17 -> transaction)
                      Range: File "", line 101, characters 6-17
                      Body Range: File "", line 102, characters 4-102
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: File "", line 637, characters 21-32
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (call_view#104:25-34 -> call_view)
                      Range: File "", line 104, characters 25-34
                      Body Range: File "", line 105, character 4 to line 106, character 104
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (split_ticket#107:6-18 -> split_ticket)
                      Range: File "", line 107, characters 6-18
                      Body Range: File "", line 108, characters 4-76
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (create_contract#109:25-40 -> create_contract)
                      Range: File "", line 109, characters 25-40
                      Body Range: File "", line 110, character 6 to line 111, character 58
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: File "", line 649, characters 26-41
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (create_contract_uncurried#112:25-50 -> create_contract_uncurried)
                      Range: File "", line 112, characters 25-50
                      Body Range: File "", line 113, characters 6-50
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_entrypoint_opt#114:25-43 -> get_entrypoint_opt)
                      Range: File "", line 114, characters 25-43
                      Body Range: File "", line 115, character 4 to line 116, character 116
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 118, characters 12-30
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (get_entrypoint#117:25-39 -> get_entrypoint)
                      Range: File "", line 117, characters 25-39
                      Body Range: File "", line 118, character 4 to line 119, character 70
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (emit#120:25-29 -> emit)
                      Range: File "", line 120, characters 25-29
                      Body Range: File "", line 121, character 4 to line 122, character 102
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      (sapling_verify_update#123:25-46 -> sapling_verify_update)
                      Range: File "", line 123, characters 25-46
                      Body Range: File "", line 123, characters 167-264
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes *
                        ( int * sapling_state (sap_a) ) ))|
                      references: []
                      Mod Path = "Tezos"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "", line 368, characters 41-46 ,
      File "", line 430, characters 15-20 ,
      File "", line 634, characters 33-38 ,
      File "", line 636, characters 33-38 ,
      File "", line 637, characters 15-20 ,
      File "", line 647, characters 33-38 ,
      File "", line 649, characters 20-25

    (Bitwise#127:7-14 -> Bitwise)
    Range: File "", line 127, characters 7-14
    Body Range: File "", line 127, character 17 to line 133, character 3
    Content: Members: Variable definitions:
                      (and#128:6-10 -> and)
                      Range: File "", line 128, characters 6-10
                      Body Range: File "", line 128, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      (xor#129:6-9 -> xor)
                      Range: File "", line 129, characters 6-9
                      Body Range: File "", line 129, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_or (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      (or#130:6-9 -> or)
                      Range: File "", line 130, characters 6-9
                      Body Range: File "", line 130, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_xor (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      (shift_left#131:6-16 -> shift_left)
                      Range: File "", line 131, characters 6-16
                      Body Range: File "", line 131, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsl (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      (shift_right#132:6-17 -> shift_right)
                      Range: File "", line 132, characters 6-17
                      Body Range: File "", line 132, characters 69-144
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_lsr (a ,
                      b)|
                      references: []
                      Mod Path = "Bitwise"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Big_map#135:7-14 -> Big_map)
    Range: File "", line 135, characters 7-14
    Body Range: File "", line 135, character 17 to line 147, character 3
    Content: Members: Variable definitions:
                      (empty#136:16-21 -> empty)
                      Range: File "", line 136, characters 16-21
                      Body Range: File "", line 136, characters 52-81
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (literal#137:25-32 -> literal)
                      Range: File "", line 137, characters 25-32
                      Body Range: File "", line 137, characters 82-116
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (mem#139:6-9 -> mem)
                      Range: File "", line 139, characters 6-9
                      Body Range: File "", line 139, characters 59-88
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (add#140:6-9 -> add)
                      Range: File "", line 140, characters 6-9
                      Body Range: File "", line 140, characters 77-109
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (remove#141:6-12 -> remove)
                      Range: File "", line 141, characters 6-12
                      Body Range: File "", line 141, characters 72-104
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (update#142:6-12 -> update)
                      Range: File "", line 142, characters 6-12
                      Body Range: File "", line 142, characters 87-122
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references:
                        File "", line 282, characters 14-20 ,
                        File "", line 302, characters 14-20
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (get_and_update#143:6-20 -> get_and_update)
                      Range: File "", line 143, characters 6-20
                      Body Range: File "", line 143, characters 106-153
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (find_opt#144:6-14 -> find_opt)
                      Range: File "", line 144, characters 6-14
                      Body Range: File "", line 144, characters 68-102
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: File "", line 289, characters 28-36
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      (find#145:6-10 -> find)
                      Range: File "", line 145, characters 6-10
                      Body Range: File "", line 145, characters 57-87
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      Mod Path = "Big_map"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "", line 282, characters 6-13 ,
      File "", line 289, characters 20-27 ,
      File "", line 302, characters 6-13

    (Map#149:7-10 -> Map)
    Range: File "", line 149, characters 7-10
    Body Range: File "", line 149, character 13 to line 165, character 3
    Content: Members: Variable definitions:
                      (empty#150:6-11 -> empty)
                      Range: File "", line 150, characters 6-11
                      Body Range: File "", line 150, characters 38-63
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (size#151:6-10 -> size)
                      Range: File "", line 151, characters 6-10
                      Body Range: File "", line 151, characters 47-74
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (literal#152:25-32 -> literal)
                      Range: File "", line 152, characters 25-32
                      Body Range: File "", line 152, characters 78-108
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (mem#154:6-9 -> mem)
                      Range: File "", line 154, characters 6-9
                      Body Range: File "", line 154, characters 55-84
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (add#155:6-9 -> add)
                      Range: File "", line 155, characters 6-9
                      Body Range: File "", line 155, characters 69-101
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (remove#156:6-12 -> remove)
                      Range: File "", line 156, characters 6-12
                      Body Range: File "", line 156, characters 64-96
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (update#157:6-12 -> update)
                      Range: File "", line 157, characters 6-12
                      Body Range: File "", line 157, characters 79-114
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (get_and_update#158:6-20 -> get_and_update)
                      Range: File "", line 158, characters 6-20
                      Body Range: File "", line 158, characters 98-141
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (find#159:6-10 -> find)
                      Range: File "", line 159, characters 6-10
                      Body Range: File "", line 159, characters 53-83
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (find_opt#160:6-14 -> find_opt)
                      Range: File "", line 160, characters 6-14
                      Body Range: File "", line 160, characters 64-98
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (iter#161:6-10 -> iter)
                      Range: File "", line 161, characters 6-10
                      Body Range: File "", line 161, characters 68-98
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (map#162:6-9 -> map)
                      Range: File "", line 162, characters 6-9
                      Body Range: File "", line 162, characters 72-101
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      Mod Path = "Map"
                      Def Type = Module_field
                      (fold#163:6-10 -> fold)
                      Range: File "", line 163, characters 6-10
                      Body Range: File "", line 163, characters 78-111
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

    (Transpiled#167:7-17 -> Transpiled)
    Range: File "", line 167, characters 7-17
    Body Range: File "", line 167, character 20 to line 171, character 3
    Content: Members: Variable definitions:
                      (map_find_opt#168:6-18 -> map_find_opt)
                      Range: File "", line 168, characters 6-18
                      Body Range: File "", line 168, characters 79-142
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      Mod Path = "Transpiled"
                      Def Type = Module_field
                      (map_add#169:6-13 -> map_add)
                      Range: File "", line 169, characters 6-13
                      Body Range: File "", line 169, characters 82-163
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      Mod Path = "Transpiled"
                      Def Type = Module_field
                      (map_remove#170:6-16 -> map_remove)
                      Range: File "", line 170, characters 6-16
                      Body Range: File "", line 170, characters 75-218
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Mod Path = "Transpiled"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Set#173:7-10 -> Set)
    Range: File "", line 173, characters 7-10
    Body Range: File "", line 173, character 13 to line 188, character 3
    Content: Members: Variable definitions:
                      (empty#174:6-11 -> empty)
                      Range: File "", line 174, characters 6-11
                      Body Range: File "", line 174, characters 31-56
                      Content: |core: ∀ a : * . set (a)|
                      references: File "", line 187, characters 96-101
                      Mod Path = "Set"
                      Def Type = Module_field
                      (size#175:6-10 -> size)
                      Range: File "", line 175, characters 6-10
                      Body Range: File "", line 175, characters 40-67
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (cardinal#176:6-14 -> cardinal)
                      Range: File "", line 176, characters 6-14
                      Body Range: File "", line 176, characters 44-71
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (literal#177:25-32 -> literal)
                      Range: File "", line 177, characters 25-32
                      Body Range: File "", line 177, characters 65-95
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (mem#179:6-9 -> mem)
                      Range: File "", line 179, characters 6-9
                      Body Range: File "", line 179, characters 48-77
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (add#180:6-9 -> add)
                      Range: File "", line 180, characters 6-9
                      Body Range: File "", line 180, characters 49-78
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: File "", line 187, characters 81-84
                      Mod Path = "Set"
                      Def Type = Module_field
                      (remove#181:6-12 -> remove)
                      Range: File "", line 181, characters 6-12
                      Body Range: File "", line 181, characters 52-84
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (update#182:6-12 -> update)
                      Range: File "", line 182, characters 6-12
                      Body Range: File "", line 182, characters 55-90
                      Content: |unresolved|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (iter#183:6-10 -> iter)
                      Range: File "", line 183, characters 6-10
                      Body Range: File "", line 183, characters 57-87
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (fold#184:6-10 -> fold)
                      Range: File "", line 184, characters 6-10
                      Body Range: File "", line 184, characters 65-98
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      (fold_desc#185:6-15 -> fold_desc)
                      Range: File "", line 185, characters 6-15
                      Body Range: File "", line 185, characters 70-108
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: File "", line 187, characters 4-13
                      Mod Path = "Set"
                      Def Type = Module_field
                      (filter_map#186:6-16 -> filter_map)
                      Range: File "", line 186, characters 6-16
                      Body Range: File "", line 187, characters 4-110
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> set (a) -> set (b)|
                      references: []
                      Mod Path = "Set"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (List#190:7-11 -> List)
    Range: File "", line 190, characters 7-11
    Body Range: File "", line 190, character 14 to line 210, character 3
    Content: Members: Variable definitions:
                      (length#191:6-12 -> length)
                      Range: File "", line 191, characters 6-12
                      Body Range: File "", line 191, characters 44-73
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (size#192:6-10 -> size)
                      Range: File "", line 192, characters 6-10
                      Body Range: File "", line 192, characters 42-71
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (head_opt#193:6-14 -> head_opt)
                      Range: File "", line 193, characters 6-14
                      Body Range: File "", line 193, characters 51-98
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (tail_opt#194:6-14 -> tail_opt)
                      Range: File "", line 194, characters 6-14
                      Body Range: File "", line 194, characters 58-107
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (map#196:6-9 -> map)
                      Range: File "", line 196, characters 6-9
                      Body Range: File "", line 196, characters 59-90
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 207, characters 4-7 ,
                        File "", line 209, characters 4-7
                      Mod Path = "List"
                      Def Type = Module_field
                      (iter#197:6-10 -> iter)
                      Range: File "", line 197, characters 6-10
                      Body Range: File "", line 197, characters 58-90
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (fold#198:6-10 -> fold)
                      Range: File "", line 198, characters 6-10
                      Body Range: File "", line 198, characters 67-102
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 436, characters 9-13
                      Mod Path = "List"
                      Def Type = Module_field
                      (fold_left#199:6-15 -> fold_left)
                      Range: File "", line 199, characters 6-15
                      Body Range: File "", line 199, characters 72-112
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (fold_right#200:6-16 -> fold_right)
                      Range: File "", line 200, characters 6-16
                      Body Range: File "", line 200, characters 73-114
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 203, characters 4-14 ,
                        File "", line 205, characters 4-14
                      Mod Path = "List"
                      Def Type = Module_field
                      (cons#201:6-10 -> cons)
                      Range: File "", line 201, characters 6-10
                      Body Range: File "", line 201, characters 53-80
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (find_opt#202:6-14 -> find_opt)
                      Range: File "", line 202, characters 6-14
                      Body Range: File "", line 203, characters 4-82
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (filter_map#204:6-16 -> filter_map)
                      Range: File "", line 204, characters 6-16
                      Body Range: File "", line 205, characters 4-100
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (update#206:6-12 -> update)
                      Range: File "", line 206, characters 6-12
                      Body Range: File "", line 207, characters 4-62
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      (update_with#208:6-17 -> update_with)
                      Range: File "", line 208, characters 6-17
                      Body Range: File "", line 209, characters 4-48
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Mod Path = "List"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 436, characters 4-8

    (String#212:7-13 -> String)
    Range: File "", line 212, characters 7-13
    Body Range: File "", line 212, character 16 to line 218, character 3
    Content: Members: Variable definitions:
                      (length#213:6-12 -> length)
                      Range: File "", line 213, characters 6-12
                      Body Range: File "", line 213, characters 34-57
                      Content: |core: string -> nat|
                      references:
                        File "", line 464, characters 22-28 ,
                        File "", line 467, characters 43-49
                      Mod Path = "String"
                      Def Type = Module_field
                      (concats#214:6-13 -> concats)
                      Range: File "", line 214, characters 6-13
                      Body Range: File "", line 214, characters 44-71
                      Content: |core: list (string) -> string|
                      references: []
                      Mod Path = "String"
                      Def Type = Module_field
                      (concat#216:6-12 -> concat)
                      Range: File "", line 216, characters 6-12
                      Body Range: File "", line 216, characters 52-82
                      Content: |core: string -> string -> string|
                      references: []
                      Mod Path = "String"
                      Def Type = Module_field
                      (sub#217:6-9 -> sub)
                      Range: File "", line 217, characters 6-9
                      Body Range: File "", line 217, characters 54-84
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 465, characters 24-27 ,
                        File "", line 467, characters 23-26
                      Mod Path = "String"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "", line 464, characters 15-21 ,
      File "", line 465, characters 17-23 ,
      File "", line 467, characters 16-22 ,
      File "", line 467, characters 36-42

    (Option#220:7-13 -> Option)
    Range: File "", line 220, characters 7-13
    Body Range: File "", line 220, character 16 to line 229, character 3
    Content: Members: Variable definitions:
                      (unopt#221:6-11 -> unopt)
                      Range: File "", line 221, characters 6-11
                      Body Range: File "", line 221, characters 42-104
                      Content: |core: ∀ a : * . option (a) -> a|
                      references:
                        File "", line 634, characters 26-31 ,
                        File "", line 647, characters 26-31
                      Mod Path = "Option"
                      Def Type = Module_field
                      (unopt_with_error#223:6-22 -> unopt_with_error)
                      Range: File "", line 223, characters 6-22
                      Body Range: File "", line 223, characters 66-113
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      (map#224:15-18 -> map)
                      Range: File "", line 224, characters 15-18
                      Body Range: File "", line 224, characters 71-103
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references:
                        File "", line 281, characters 25-28 ,
                        File "", line 290, characters 13-16
                      Mod Path = "Option"
                      Def Type = Module_field
                      (value#225:6-11 -> value)
                      Range: File "", line 225, characters 6-11
                      Body Range: File "", line 225, characters 56-100
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      (value_exn#226:6-15 -> value_exn)
                      Range: File "", line 226, characters 6-15
                      Body Range: File "", line 226, characters 62-109
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: File "", line 293, characters 17-26
                      Mod Path = "Option"
                      Def Type = Module_field
                      (is_none#227:6-13 -> is_none)
                      Range: File "", line 227, characters 6-13
                      Body Range: File "", line 227, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      (is_some#228:6-13 -> is_some)
                      Range: File "", line 228, characters 6-13
                      Body Range: File "", line 228, characters 47-92
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      Mod Path = "Option"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "", line 281, characters 18-24 ,
      File "", line 290, characters 6-12 ,
      File "", line 293, characters 10-16 ,
      File "", line 634, characters 19-25 ,
      File "", line 647, characters 19-25

    (Bytes#231:7-12 -> Bytes)
    Range: File "", line 231, characters 7-12
    Body Range: File "", line 231, character 15 to line 239, character 3
    Content: Members: Variable definitions:
                      (concats#232:6-13 -> concats)
                      Range: File "", line 232, characters 6-13
                      Body Range: File "", line 232, characters 42-69
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (pack#233:6-10 -> pack)
                      Range: File "", line 233, characters 6-10
                      Body Range: File "", line 233, characters 38-77
                      Content: |core: ∀ a : * . a -> bytes|
                      references: File "", line 281, characters 35-39
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (unpack#234:6-12 -> unpack)
                      Range: File "", line 234, characters 6-12
                      Body Range: File "", line 234, characters 47-122
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: File "", line 292, characters 68-74
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (length#235:6-12 -> length)
                      Range: File "", line 235, characters 6-12
                      Body Range: File "", line 235, characters 33-56
                      Content: |core: bytes -> nat|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (concat#237:6-12 -> concat)
                      Range: File "", line 237, characters 6-12
                      Body Range: File "", line 237, characters 49-79
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      (sub#238:6-9 -> sub)
                      Range: File "", line 238, characters 6-9
                      Body Range: File "", line 238, characters 52-82
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      Mod Path = "Bytes"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references:
      File "", line 281, characters 29-34 ,
      File "", line 292, characters 62-67

    (Crypto#241:7-13 -> Crypto)
    Range: File "", line 241, characters 7-13
    Body Range: File "", line 241, character 16 to line 249, character 3
    Content: Members: Variable definitions:
                      (blake2b#242:6-13 -> blake2b)
                      Range: File "", line 242, characters 6-13
                      Body Range: File "", line 242, characters 36-78
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (sha256#243:6-12 -> sha256)
                      Range: File "", line 243, characters 6-12
                      Body Range: File "", line 243, characters 35-76
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (sha512#244:6-12 -> sha512)
                      Range: File "", line 244, characters 6-12
                      Body Range: File "", line 244, characters 35-76
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (sha3#245:6-10 -> sha3)
                      Range: File "", line 245, characters 6-10
                      Body Range: File "", line 245, characters 33-72
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (keccak#246:6-12 -> keccak)
                      Range: File "", line 246, characters 6-12
                      Body Range: File "", line 246, characters 35-76
                      Content: |core: bytes -> bytes|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (hash_key#247:6-14 -> hash_key)
                      Range: File "", line 247, characters 6-14
                      Body Range: File "", line 247, characters 38-84
                      Content: |core: key -> key_hash|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      (check#248:6-11 -> check)
                      Range: File "", line 248, characters 6-11
                      Body Range: File "", line 248, characters 59-112
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      Mod Path = "Crypto"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Dynamic_entrypoints#270:7-26 -> Dynamic_entrypoints)
    Range: File "", line 270, characters 7-26
    Body Range: File "", line 270, character 29 to line 303, character 3
    Content: Members: Variable definitions:
                      (cast_dynamic_entrypoint#272:17-40 -> cast_dynamic_entrypoint)
                      Range: File "", line 272, characters 17-40
                      Body Range: File "", line 273, characters 4-46
                      Content: |core: ∀ p : * . ∀ s : * . dynamic_entrypoint (p ,
                      s) -> nat|
                      references:
                        File "", line 282, characters 22-45 ,
                        File "", line 289, characters 38-61 ,
                        File "", line 302, characters 22-45
                      Mod Path = "Dynamic_entrypoints"
                      Def Type = Module_field
                      (set#275:6-9 -> set)
                      Range: File "", line 275, characters 6-9
                      Body Range: File "", line 281, character 6 to line 282, character 59
                      Content: |core: ∀ p : * . ∀ s : * . dynamic_entrypoint (p ,
                      s) -> option (entrypoint (p ,
                      s)) -> dynamic_entrypoints -> dynamic_entrypoints|
                      references: []
                      Mod Path = "Dynamic_entrypoints"
                      Def Type = Module_field
                      (get#284:6-9 -> get)
                      Range: File "", line 284, characters 6-9
                      Body Range: File "", line 289, character 6 to line 294, character 15
                      Content: |core: ∀ p : * . ∀ s : * . dynamic_entrypoint (p ,
                      s) -> dynamic_entrypoints -> option (entrypoint (p ,
                      s))|
                      references: []
                      Mod Path = "Dynamic_entrypoints"
                      Def Type = Module_field
                      (set_bytes#296:6-15 -> set_bytes)
                      Range: File "", line 296, characters 6-15
                      Body Range: File "", line 302, characters 6-59
                      Content: |core: ∀ p : * . ∀ s : * . dynamic_entrypoint (p ,
                      s) -> option (bytes) -> dynamic_entrypoints -> dynamic_entrypoints|
                      references: []
                      Mod Path = "Dynamic_entrypoints"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:

    references: []

    (Test#340:7-11 -> Test)
    Range: File "", line 340, characters 7-11
    Body Range: File "", line 340, character 14 to line 697, character 3
    Content: Members: Variable definitions:
                      (run#342:6-9 -> run)
                      Range: File "", line 342, characters 6-9
                      Body Range: File "", line 342, characters 64-94
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 343, characters 50-53
                      Mod Path = "Test"
                      Def Type = Module_field
                      (eval#343:6-10 -> eval)
                      Range: File "", line 343, characters 6-10
                      Body Range: File "", line 343, characters 50-74
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 345, characters 59-63 ,
                        File "", line 455, characters 32-36 ,
                        File "", line 460, characters 34-38 ,
                        File "", line 480, characters 12-16 ,
                        File "", line 490, characters 12-16 ,
                        File "", line 497, characters 12-16 ,
                        File "", line 576, characters 12-16 ,
                        File "", line 598, characters 12-16
                      Mod Path = "Test"
                      Def Type = Module_field
                      (compile_value#345:6-19 -> compile_value)
                      Range: File "", line 345, characters 6-19
                      Body Range: File "", line 345, characters 59-65
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_total_voting_power#346:6-28 -> get_total_voting_power)
                      Range: File "", line 346, characters 6-28
                      Body Range: File "", line 346, characters 49-96
                      Content: |core: unit -> nat|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (failwith#347:6-14 -> failwith)
                      Range: File "", line 347, characters 6-14
                      Body Range: File "", line 347, characters 40-72
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 476, characters 14-22 ,
                        File "", line 619, characters 51-59 ,
                        File "", line 620, characters 74-82 ,
                        File "", line 621, characters 89-97 ,
                        File "", line 623, characters 68-76 ,
                        File "", line 624, characters 98-106 ,
                        File "", line 625, characters 113-121 ,
                        File "", line 680, characters 16-24 ,
                        File "", line 695, characters 16-24
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_contract#348:6-17 -> to_contract)
                      Range: File "", line 348, characters 6-17
                      Body Range: File "", line 348, characters 71-106
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 382, characters 25-36 ,
                        File "", line 430, characters 30-41 ,
                        File "", line 665, characters 28-39 ,
                        File "", line 675, characters 40-51 ,
                        File "", line 690, characters 40-51
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_source#349:6-16 -> set_source)
                      Range: File "", line 349, characters 6-16
                      Body Range: File "", line 349, characters 40-74
                      Content: |core: address -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_storage_of_address#350:6-28 -> get_storage_of_address)
                      Range: File "", line 350, characters 6-28
                      Body Range: File "", line 350, characters 65-111
                      Content: |core: address -> michelson_program|
                      references: File "", line 384, characters 32-54
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_balance#351:6-17 -> get_balance)
                      Range: File "", line 351, characters 6-17
                      Body Range: File "", line 351, characters 40-75
                      Content: |core: address -> tez|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (print#352:6-11 -> print)
                      Range: File "", line 352, characters 6-11
                      Body Range: File "", line 352, characters 34-66
                      Content: |core: string -> unit|
                      references:
                        File "", line 406, characters 4-9 ,
                        File "", line 442, characters 4-9
                      Mod Path = "Test"
                      Def Type = Module_field
                      (eprint#353:6-12 -> eprint)
                      Range: File "", line 353, characters 6-12
                      Body Range: File "", line 353, characters 35-67
                      Content: |core: string -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_voting_power#354:6-22 -> get_voting_power)
                      Range: File "", line 354, characters 6-22
                      Body Range: File "", line 354, characters 47-88
                      Content: |core: key_hash -> nat|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (nth_bootstrap_contract#355:6-28 -> nth_bootstrap_contract)
                      Range: File "", line 355, characters 6-28
                      Body Range: File "", line 355, characters 51-97
                      Content: |core: nat -> address|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (nth_bootstrap_account#356:6-27 -> nth_bootstrap_account)
                      Range: File "", line 356, characters 6-27
                      Body Range: File "", line 357, character 4 to line 358, character 5
                      Content: |core: int -> address|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_bootstrap_account#359:6-27 -> get_bootstrap_account)
                      Range: File "", line 359, characters 6-27
                      Body Range: File "", line 359, characters 65-105
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (nth_bootstrap_typed_address#360:6-33 -> nth_bootstrap_typed_address)
                      Range: File "", line 360, characters 6-33
                      Body Range: File "", line 360, characters 80-131
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (last_originations#361:6-23 -> last_originations)
                      Range: File "", line 361, characters 6-23
                      Body Range: File "", line 361, characters 67-108
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (random#362:6-12 -> random)
                      Range: File "", line 362, characters 6-12
                      Body Range: File "", line 363, character 4 to line 364, character 42
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (new_account#365:6-17 -> new_account)
                      Range: File "", line 365, characters 6-17
                      Body Range: File "", line 365, characters 46-81
                      Content: |core: unit -> ( string * key )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (decompile#366:6-15 -> decompile)
                      Range: File "", line 366, characters 6-15
                      Body Range: File "", line 366, characters 55-88
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 385, characters 5-14
                      Mod Path = "Test"
                      Def Type = Module_field
                      (bake_until_n_cycle_end#367:6-28 -> bake_until_n_cycle_end)
                      Range: File "", line 367, characters 6-28
                      Body Range: File "", line 367, characters 48-94
                      Content: |core: nat -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_time#368:6-14 -> get_time)
                      Range: File "", line 368, characters 6-14
                      Body Range: File "", line 368, characters 41-57
                      Content: |core: unit -> timestamp|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (cast_address#369:6-18 -> cast_address)
                      Range: File "", line 369, characters 6-18
                      Body Range: File "", line 369, characters 69-105
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 483, characters 35-47 ,
                        File "", line 493, characters 35-47 ,
                        File "", line 500, characters 35-47 ,
                        File "", line 581, characters 37-49 ,
                        File "", line 603, characters 37-49 ,
                        File "", line 678, characters 22-34 ,
                        File "", line 693, characters 22-34
                      Mod Path = "Test"
                      Def Type = Module_field
                      (register_delegate#370:6-23 -> register_delegate)
                      Range: File "", line 370, characters 6-23
                      Body Range: File "", line 370, characters 49-91
                      Content: |core: key_hash -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (stake#371:6-11 -> stake)
                      Range: File "", line 371, characters 6-11
                      Body Range: File "", line 371, characters 47-80
                      Content: |core: key_hash -> tez -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (register_constant#372:6-23 -> register_constant)
                      Range: File "", line 372, characters 6-23
                      Body Range: File "", line 372, characters 59-100
                      Content: |core: michelson_program -> string|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_typed_address#373:6-22 -> to_typed_address)
                      Range: File "", line 373, characters 6-22
                      Body Range: File "", line 373, characters 76-116
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (constant_to_michelson_program#374:6-35 -> constant_to_michelson_program)
                      Range: File "", line 374, characters 6-35
                      Body Range: File "", line 374, characters 71-116
                      Content: |core: string -> michelson_program|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (parse_michelson#375:6-21 -> parse_michelson)
                      Range: File "", line 375, characters 6-21
                      Body Range: File "", line 375, characters 57-102
                      Content: |core: string -> michelson_program|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (restore_context#376:6-21 -> restore_context)
                      Range: File "", line 376, characters 6-21
                      Body Range: File "", line 376, characters 42-77
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (save_context#377:6-18 -> save_context)
                      Range: File "", line 377, characters 6-18
                      Body Range: File "", line 377, characters 39-75
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (drop_context#378:6-18 -> drop_context)
                      Range: File "", line 378, characters 6-18
                      Body Range: File "", line 378, characters 39-75
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_string#379:6-15 -> to_string)
                      Range: File "", line 379, characters 6-15
                      Body Range: File "", line 379, characters 44-80
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 397, characters 68-77 ,
                        File "", line 399, characters 67-76 ,
                        File "", line 401, characters 61-70 ,
                        File "", line 441, characters 12-21
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_json#380:6-13 -> to_json)
                      Range: File "", line 380, characters 6-13
                      Body Range: File "", line 380, characters 42-78
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_storage#381:6-17 -> get_storage)
                      Range: File "", line 381, characters 6-17
                      Body Range: File "", line 382, character 4 to line 385, character 21
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references:
                        File "", line 676, characters 12-23 ,
                        File "", line 691, characters 12-23
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_baker_policy#386:6-22 -> set_baker_policy)
                      Range: File "", line 386, characters 6-22
                      Body Range: File "", line 386, characters 57-91
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 387, characters 39-55
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_baker#387:6-15 -> set_baker)
                      Range: File "", line 387, characters 6-15
                      Body Range: File "", line 387, characters 39-70
                      Content: |core: address -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (size#388:6-10 -> size)
                      Range: File "", line 388, characters 6-10
                      Body Range: File "", line 388, characters 44-72
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 482, characters 12-16 ,
                        File "", line 492, characters 12-16 ,
                        File "", line 499, characters 12-16 ,
                        File "", line 508, characters 12-16 ,
                        File "", line 539, characters 14-18 ,
                        File "", line 559, characters 14-18 ,
                        File "", line 580, characters 14-18 ,
                        File "", line 602, characters 14-18
                      Mod Path = "Test"
                      Def Type = Module_field
                      (compile_contract#389:6-22 -> compile_contract)
                      Range: File "", line 389, characters 6-22
                      Body Range: File "", line 390, character 4 to line 392, character 52
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 479, characters 12-28 ,
                        File "", line 489, characters 12-28
                      Mod Path = "Test"
                      Def Type = Module_field
                      (read_contract_from_file#393:6-29 -> read_contract_from_file)
                      Range: File "", line 393, characters 6-29
                      Body Range: File "", line 393, characters 67-115
                      Content: |core: string -> michelson_contract|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (chr#394:6-9 -> chr)
                      Range: File "", line 394, characters 6-9
                      Body Range: File "", line 395, character 4 to line 403, character 10
                      Content: |core: nat -> option (string)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (nl#404:6-8 -> nl)
                      Range: File "", line 404, characters 6-8
                      Body Range: File "", line 404, characters 11-53
                      Content: |unresolved|
                      references: File "", line 406, characters 15-17
                      Mod Path = "Test"
                      Def Type = Module_field
                      (println#405:6-13 -> println)
                      Range: File "", line 405, characters 6-13
                      Body Range: File "", line 406, characters 4-18
                      Content: |core: string -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_print_values#408:6-22 -> set_print_values)
                      Range: File "", line 408, characters 6-22
                      Body Range: File "", line 408, characters 43-100
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (unset_print_values#409:6-24 -> unset_print_values)
                      Range: File "", line 409, characters 6-24
                      Body Range: File "", line 409, characters 45-103
                      Content: |core: unit -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (get_last_events_from#429:6-26 -> get_last_events_from)
                      Range: File "", line 429, characters 6-26
                      Body Range: File "", line 430, character 4 to line 436, character 38
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (transfer#437:6-14 -> transfer)
                      Range: File "", line 437, characters 6-14
                      Body Range: File "", line 437, characters 84-162
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (transfer_exn#438:6-18 -> transfer_exn)
                      Range: File "", line 438, characters 6-18
                      Body Range: File "", line 438, characters 75-157
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (log#439:6-9 -> log)
                      Range: File "", line 439, characters 6-9
                      Body Range: File "", line 440, character 4 to line 442, character 11
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 466, characters 25-28
                      Mod Path = "Test"
                      Def Type = Module_field
                      (reset_state#443:6-17 -> reset_state)
                      Range: File "", line 443, characters 6-17
                      Body Range: File "", line 443, characters 52-117
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (reset_state_at#444:6-20 -> reset_state_at)
                      Range: File "", line 444, characters 6-20
                      Body Range: File "", line 444, characters 69-117
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (bootstrap_contract#445:6-24 -> bootstrap_contract)
                      Range: File "", line 445, characters 6-24
                      Body Range: File "", line 445, characters 97-145
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (mutate_value#446:6-18 -> mutate_value)
                      Range: File "", line 446, characters 6-18
                      Body Range: File "", line 446, characters 72-111
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 514, characters 23-35 ,
                        File "", line 526, characters 23-35
                      Mod Path = "Test"
                      Def Type = Module_field
                      (save_mutation#447:6-19 -> save_mutation)
                      Range: File "", line 447, characters 6-19
                      Body Range: File "", line 447, characters 66-106
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (sign#448:6-10 -> sign)
                      Range: File "", line 448, characters 6-10
                      Body Range: File "", line 448, characters 51-83
                      Content: |core: string -> bytes -> signature|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (add_account#449:6-17 -> add_account)
                      Range: File "", line 449, characters 6-17
                      Body Range: File "", line 449, characters 50-88
                      Content: |core: string -> key -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (baker_account#450:6-19 -> baker_account)
                      Range: File "", line 450, characters 6-19
                      Body Range: File "", line 450, characters 65-105
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (set_big_map#451:6-17 -> set_big_map)
                      Range: File "", line 451, characters 6-17
                      Body Range: File "", line 451, characters 69-107
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (transfer_to_contract#452:6-26 -> transfer_to_contract)
                      Range: File "", line 452, characters 6-26
                      Body Range: File "", line 453, character 4 to line 456, character 61
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: File "", line 665, characters 6-26
                      Mod Path = "Test"
                      Def Type = Module_field
                      (transfer_to_contract_exn#457:6-30 -> transfer_to_contract_exn)
                      Range: File "", line 457, characters 6-30
                      Body Range: File "", line 458, character 6 to line 461, character 67
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references:
                        File "", line 675, characters 14-38 ,
                        File "", line 690, characters 14-38
                      Mod Path = "Test"
                      Def Type = Module_field
                      (michelson_equal#462:6-21 -> michelson_equal)
                      Range: File "", line 462, characters 6-21
                      Body Range: File "", line 462, characters 81-88
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (to_entrypoint#463:6-19 -> to_entrypoint)
                      Range: File "", line 463, characters 6-19
                      Body Range: File "", line 464, character 4 to line 470, character 44
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (storage_with_dynamic_entrypoints#471:6-38 -> storage_with_dynamic_entrypoints)
                      Range: File "", line 471, characters 6-38
                      Body Range: File "", line 472, character 4 to line 476, character 5
                      Content: |unresolved|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_contract#477:6-24 -> originate_contract)
                      Range: File "", line 477, characters 6-24
                      Body Range: File "", line 477, characters 96-135
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 481, characters 12-30 ,
                        File "", line 491, characters 12-30 ,
                        File "", line 498, characters 12-30 ,
                        File "", line 507, characters 12-30 ,
                        File "", line 538, characters 14-32 ,
                        File "", line 558, characters 14-32 ,
                        File "", line 579, characters 14-32 ,
                        File "", line 601, characters 14-32
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate#478:6-15 -> originate)
                      Range: File "", line 478, characters 6-15
                      Body Range: File "", line 479, character 4 to line 484, character 13
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references:
                        File "", line 658, characters 32-41 ,
                        File "", line 674, characters 32-41 ,
                        File "", line 689, characters 32-41
                      Mod Path = "Test"
                      Def Type = Module_field
                      (compile_contract_with_views#485:8-35 -> compile_contract_with_views)
                      Range: File "", line 485, characters 8-35
                      Body Range: File "", line 486, character 6 to line 487, character 54
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 496, characters 12-39
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_uncurried#488:6-25 -> originate_uncurried)
                      Range: File "", line 488, characters 6-25
                      Body Range: File "", line 489, character 4 to line 494, character 13
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_module#495:6-22 -> originate_module)
                      Range: File "", line 495, characters 6-22
                      Body Range: File "", line 496, character 4 to line 501, character 13
                      Content: |core: ∀ p : * . ∀ s : * . module_contract (p ,
                      s) -> s -> tez -> ( typed_address (p ,
                                          s) *
                                          michelson_contract *
                                          int )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (compile_contract_from_file#502:6-32 -> compile_contract_from_file)
                      Range: File "", line 502, characters 6-32
                      Body Range: File "", line 503, character 4 to line 504, character 52
                      Content: |core: string -> michelson_contract|
                      references: File "", line 506, characters 12-38
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_from_file#505:6-25 -> originate_from_file)
                      Range: File "", line 505, characters 6-25
                      Body Range: File "", line 506, character 4 to line 509, character 13
                      Content: |core: string -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (mutation_test#510:6-19 -> mutation_test)
                      Range: File "", line 510, characters 6-19
                      Body Range: File "", line 511, character 4 to line 521, character 19
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (mutation_test_all#522:6-23 -> mutation_test_all)
                      Range: File "", line 522, characters 6-23
                      Body Range: File "", line 523, character 4 to line 533, character 46
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_from_file_and_mutate#534:6-36 -> originate_from_file_and_mutate)
                      Range: File "", line 534, characters 6-36
                      Body Range: File "", line 536, character 4 to line 553, character 19
                      Content: |core: ∀ b : * . string -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_from_file_and_mutate_all#554:6-40 -> originate_from_file_and_mutate_all)
                      Range: File "", line 554, characters 6-40
                      Body Range: File "", line 556, character 4 to line 573, character 46
                      Content: |core: ∀ b : * . string -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_module_and_mutate#574:6-33 -> originate_module_and_mutate)
                      Range: File "", line 574, characters 6-33
                      Body Range: File "", line 576, character 4 to line 595, character 19
                      Content: |core: ∀ p : * . ∀ s : * . ∀ b : * . module_contract (p ,
                      s) -> s -> tez -> typed_address (p ,
                      s) -> michelson_contract -> int -> b -> option (( b *
                                                                        mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (originate_module_and_mutate_all#596:6-37 -> originate_module_and_mutate_all)
                      Range: File "", line 596, characters 6-37
                      Body Range: File "", line 598, character 4 to line 617, character 46
                      Content: |core: ∀ p : * . ∀ s : * . ∀ b : * . module_contract (p ,
                      s) -> s -> tez -> typed_address (p ,
                      s) -> michelson_contract -> int -> b -> list (( b *
                                                                      mutation ))|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert#619:6-12 -> assert)
                      Range: File "", line 619, characters 6-12
                      Body Range: File "", line 619, characters 33-78
                      Content: |core: bool -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_some#620:6-17 -> assert_some)
                      Range: File "", line 620, characters 6-17
                      Body Range: File "", line 620, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_none#621:6-17 -> assert_none)
                      Range: File "", line 621, characters 6-17
                      Body Range: File "", line 621, characters 51-118
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_with_error#623:6-23 -> assert_with_error)
                      Range: File "", line 623, characters 6-23
                      Body Range: File "", line 623, characters 50-78
                      Content: |unresolved|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_some_with_error#624:6-28 -> assert_some_with_error)
                      Range: File "", line 624, characters 6-28
                      Body Range: File "", line 624, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      (assert_none_with_error#625:6-28 -> assert_none_with_error)
                      Range: File "", line 625, characters 6-28
                      Body Range: File "", line 625, characters 75-123
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      Mod Path = "Test"
                      Def Type = Module_field
                      Type definitions:
                      Module definitions:
                      (PBT#411:9-12 -> PBT)
                      Range: File "", line 411, characters 9-12
                      Body Range: File "", line 411, character 15 to line 427, character 5
                      Content: Members: Variable definitions:
                                        (gen#412:8-11 -> gen)
                                        Range: File "", line 412, characters 8-11
                                        Body Range: File "", line 412, characters 35-69
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        Mod Path = "Test""PBT"
                                        Def Type = Module_field
                                        (gen_small#413:8-17 -> gen_small)
                                        Range: File "", line 413, characters 8-17
                                        Body Range: File "", line 413, characters 41-74
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        Mod Path = "Test""PBT"
                                        Def Type = Module_field
                                        (make_test#414:8-17 -> make_test)
                                        Range: File "", line 414, characters 8-17
                                        Body Range: File "", line 414, characters 75-79
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        Mod Path = "Test""PBT"
                                        Def Type = Module_field
                                        (run#415:8-11 -> run)
                                        Range: File "", line 415, characters 8-11
                                        Body Range: File "", line 416, character 6 to line 426, character 7
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Mod Path = "Test""PBT"
                                        Def Type = Module_field
                                        Type definitions:
                                        Module definitions:

                      references: []

                      (Proxy_ticket#627:9-21 -> Proxy_ticket)
                      Range: File "", line 627, characters 9-21
                      Body Range: File "", line 627, character 24 to line 696, character 5
                      Content: Members: Variable definitions:
                                        (proxy_transfer_contract#628:19-42 -> proxy_transfer_contract)
                                        Range: File "", line 628, characters 19-42
                                        Body Range: File "", line 633, character 6 to line 638, character 14
                                        Content: |core: ∀ vt : * . ∀ whole_p : * . ticket (vt) -> whole_p ->
                                        ( ( vt * nat ) *
                                          address ) -> unit -> ( list (operation) *
                                                                 unit )|
                                        references:
                                          File "", line 656, characters 8-31
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (proxy_originate_contract#640:19-43 -> proxy_originate_contract)
                                        Range: File "", line 640, characters 19-43
                                        Body Range: File "", line 646, character 6 to line 650, character 21
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * . ticket (vt) -> whole_s -> vp -> whole_s ->
                                        ( list (operation) *
                                          whole_s ) -> ( vt * nat ) -> option (address) ->
                                        ( list (operation) *
                                          option (address) )|
                                        references:
                                          File "", line 672, characters 8-32 ,
                                          File "", line 687, characters 8-32
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (init_transfer#654:8-21 -> init_transfer)
                                        Range: File "", line 654, characters 8-21
                                        Body Range: File "", line 655, character 6 to line 659, character 17
                                        Content: |core: ∀ vt : * . ∀ whole_p : * . ticket (vt) -> whole_p -> proxy_address (vt)|
                                        references: []
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (transfer#661:8-16 -> transfer)
                                        Range: File "", line 661, characters 8-16
                                        Body Range: File "", line 664, character 6 to line 665, character 84
                                        Content: |core: ∀ vt : * . proxy_address (vt) ->
                                        ( ( vt * nat ) *
                                          address ) -> test_exec_result|
                                        references: []
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (originate_uncurried#667:8-27 -> originate_uncurried)
                                        Range: File "", line 667, characters 8-27
                                        Body Range: File "", line 671, character 6 to line 680, character 7
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * .
                                        ( vt *
                                          nat ) -> ticket (vt) -> whole_s ->
                                        ( vp *
                                          whole_s ) -> ( list (operation) *
                                                         whole_s ) -> address|
                                        references: []
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        (originate#682:8-17 -> originate)
                                        Range: File "", line 682, characters 8-17
                                        Body Range: File "", line 686, character 6 to line 695, character 7
                                        Content: |core: ∀ vt : * . ∀ whole_s : * . ∀ vp : * .
                                        ( vt *
                                          nat ) -> ticket (vt) -> whole_s -> vp -> whole_s ->
                                        ( list (operation) *
                                          whole_s ) -> address|
                                        references: []
                                        Mod Path = "Test""Proxy_ticket"
                                        Def Type = Module_field
                                        Type definitions:
                                        (proxy_address#652:12-25 -> proxy_address)
                                        Range: File "", line 652, characters 12-25
                                        Body Range: File "", line 652, characters 28-57
                                        Content: : |funtype 'v : * . typed_address (
                                        ( ( 'v * nat ) *
                                          address ) ,
                                        unit)|
                                        references:
                                          File "", line 654, characters 78-91 ,
                                          File "", line 662, characters 26-39
                                        Module definitions:

                      references: []


    references: [] |}]
