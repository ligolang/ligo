#include "./contract_under_test/contract_record_storage_ty.mligo"
let cut = "./contract_under_test/contract_record_storage_ty.mligo"

let test =
  let orig : (parameter, storage) origination_result = Test.originate_from_file cut {foo = 0 ; bar = "bar"} 0tez in
  let store : storage = Test.get_storage orig.addr in
  let ovens_map = store.foo in
  assert (ovens_map = 0)
