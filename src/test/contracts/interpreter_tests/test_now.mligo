let under_test = "./contract_under_test/now_contract.mligo"

let test =
  let init_storage = Test.compile_expression (Some under_test) [%cameligo ({| test_ts |} : ligo_program) ] in
  let (addr,code,_) = Test.originate under_test "main" init_storage in
  
  let u = Test.log "storage at origination" in
  let st = Test.get_storage addr in
  let u = Test.log st in

  let u = Test.log "setting now at:" in
  let u = Test.set_now ("2010-01-01t10:10:10Z" : timestamp) in

  let param = Test.compile_expression (None : string option) [%cameligo ({| () |} : ligo_program)] in
  let u = Test.transfer_exn addr param 10n in

  let u = Test.log "storage after calling" in
  let st = Test.get_storage addr in
  let u = Test.log st in

  true
