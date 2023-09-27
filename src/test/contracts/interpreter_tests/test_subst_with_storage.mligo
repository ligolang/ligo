#import "./contract_under_test/contract_record_storage_ty.mligo" "C"

let test =
  let init_storage = {foo = 0 ; bar = "bar"} in
  let orig = Test.originate (contract_of C) init_storage 0tez in
  let store = Test.get_storage orig.addr in
  Test.eval store.foo
